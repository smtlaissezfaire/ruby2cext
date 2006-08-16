
require "rubynode"
require "ruby2cext/parser"
require "ruby2cext/error"
require "ruby2cext/tools"
require "ruby2cext/c_function"

module Ruby2CExtension

	class Compiler

		attr_reader :name, :verbose

		def initialize(name, verbose = false)
			@name = name
			@verbose = verbose
			@funs = []
			@funs_reuseable = {}
			@toplevel_funs = []
			@sym_man = Tools::SymbolManager.new
			@global_man = Tools::GlobalManager.new
			@uniq_names = Tools::UniqueNames.new
			@helpers = {}
			@plugins = []
			@preprocessors = {}
		end

		def to_c_code
			plugins_global = @plugins.map { |plugin| plugin.global_c_code }
			plugins_init = @plugins.map { |plugin| plugin.init_c_code }
			res = [
				"#include <ruby.h>",
				"#include <node.h>",
				"#include <env.h>",
				"#include <st.h>",
				"extern VALUE ruby_top_self;",
				"static VALUE org_ruby_top_self;",
				@sym_man.to_c_code,
				@global_man.to_c_code,
			]
			res.concat(@helpers.values.sort)
			res.concat(plugins_global)
			res.concat(@funs)
			res << "void Init_#{@name}() {"
			res << "org_ruby_top_self = ruby_top_self;"
			# just to be sure
			res << "rb_global_variable(&org_ruby_top_self);"
			res << "init_syms();"
			res << "init_globals();"
			res << "NODE *cref = rb_node_newnode(NODE_CREF, rb_cObject, 0, 0);"
			res.concat(plugins_init)
			@toplevel_funs.each { |f| res << "#{f}(ruby_top_self, cref);" }
			res << "}"
			res.join("\n").split("\n").map { |l| l.strip }.reject { |l| l.empty? }.join("\n")
		end

		def add_toplevel(function_name)
			@toplevel_funs << function_name
		end

		# non destructive: node_tree will not be changed
		def compile_toplevel_function(node_tree, private_vmode = true)
			CFunction::ToplevelScope.compile(self, node_tree, private_vmode)
		end

		def rb_file_to_toplevel_functions(source_str, file_name)
			res = []
			hash = Parser.parse_string(source_str, file_name)
			# abb all BEGIN blocks, if available
			if (beg_tree = hash[:begin])
				beg_tree = beg_tree.transform(:include_node => true)
				if beg_tree.first == :block
					beg_tree.last.each { |s| res << compile_toplevel_function(s, false) }
				else
					res << compile_toplevel_function(beg_tree, false)
				end
			end
			# add toplevel scope
			if (tree = hash[:tree])
				res << compile_toplevel_function(tree.transform(:include_node => true))
			end
			res
		end

		def add_rb_file(source_str, file_name)
			rb_file_to_toplevel_functions(source_str, file_name).each { |fn|
				add_toplevel(fn)
			}
		end

		# uniq name
		def un(str)
			@uniq_names.get(str)
		end
		def sym(sym)
			@sym_man.get(sym)
		end
		def global_const(str, register_gc = true)
			@global_man.get(str, true, register_gc)
		end
		def global_var(str)
			@global_man.get(str, false, true)
		end

		def log(str, force = false)
			if verbose || force
				puts str
			end
		end

		def add_helper(str)
			@helpers[str] ||= str
		end

		def add_fun(code, base_name)
			unless (name = @funs_reuseable[code])
				name = un(base_name)
				lines = code.split("\n")
				unless lines.shift =~ /^\s*static / # first line needs static
					raise Ruby2CExtError::Bug, "trying to add a non static function"
				end
				if lines.grep(/^\s*static /).empty? # only reuseably without static variables
					@funs_reuseable[code] = name
				end
				unless code.sub!("FUNNAME", name)
					raise Ruby2CExtError::Bug, "trying to add a function without FUNNAME"
				end
				@funs << code
			end
			name
		end

		def add_plugin(plugin_class, *args)
			@plugins << plugin_class.new(self, *args)
		end

		# preprocessors can be added by plugins. preprocessors are procs that
		# take two arguments: the current cfun and the node (tree) to
		# preprocess (which will have type node_type)
		#
		# The proc can either return a (modified) node (tree) or string. If a
		# node (tree) is returned then that will be translated as usual, if a
		# string is returned, that string will be the result
		#
		# Example, a preprocessor that replaces 23 with 42:
		# add_preprocessor(:lit) { |cfun, node|
		#   node.last[:lit] == 23 ? [:lit, {:lit=>42}] : node
		# }
		#
		# Another way to do the same:
		# add_preprocessor(:lit) { |cfun, node|
		#   node.last[:lit] == 23 ? cfun.comp_lit(:lit=>42) : node
		# }
		#
		# If multiple preprocessors are added for the same node type then they
		# will be called after each other with the result of the previous one
		# unless it is a string, then the following preprocessors are ignored
		def add_preprocessor(node_type, &pp_proc)
			(@preprocessors[node_type] ||= []) << pp_proc
		end

		def preprocessors_for(node_type)
			@preprocessors[node_type]
		end

		class << self
			def compile_ruby_to_c(source_str, name, file_name, verbose = false)
				c = self.new(name, verbose)

				# TODO: require plugins conditionally via options
				require "ruby2cext/plugins/warnings"
				c.add_plugin(Plugins::Warnings)
				require "ruby2cext/plugins/const_cache"
				c.add_plugin(Plugins::ConstCache)
				require "ruby2cext/plugins/inline_methods"
				c.add_plugin(Plugins::InlineMethods)
				require "ruby2cext/plugins/builtin_methods"
				c.add_plugin(Plugins::BuiltinMethods, Plugins::BuiltinMethods::SUPPORTED_BUILTINS)
				require "ruby2cext/plugins/require_include"
				c.add_plugin(Plugins::RequireInclude, file_name, ["."])

				c.add_rb_file(source_str, file_name)
				c.to_c_code
			end

			def compile_c_file_to_dllib(file_basename, name, verbose = false)
				require "rbconfig"

				conf = ::Config::CONFIG
				ldshared = conf["LDSHARED"]
				cflags = [conf["CCDLFLAGS"], conf["CFLAGS"], conf["ARCH_FLAG"]].join(" ")
				hdrdir = conf["archdir"]
				dlext = conf["DLEXT"]
				cmd = "#{ldshared} #{cflags} -I. -I #{hdrdir} -o #{file_basename}.#{dlext} #{file_basename}.c"
				if RUBY_PLATFORM =~ /mswin32/
					cmd << " -link /INCREMENTAL:no /EXPORT:Init_#{name}"
				end
				puts cmd if verbose
				unless system(cmd) # run it
					raise "error while executing '#{cmd}'"
				end
			end

			def compile_file(file_name, only_c = false, verbose = false)
				bn = File.basename(file_name)
				unless bn =~ /\A(.*)\.rb\w?\z/
					raise "#{file_name} is no ruby file"
				end
				name = $1;
				unless name =~ /\A\w+\z/
					raise "'#{name}' is not a valid extension name"
				end
				file_name = File.join(File.dirname(file_name), bn)
				puts "reading #{file_name}" if verbose
				source_str = IO.read(file_name)
				puts "translating #{file_name} to C" if verbose
				c_code = compile_ruby_to_c(source_str, name, file_name, verbose)
				file_basename = File.join(File.dirname(file_name), name)
				puts "writing #{file_basename}.c" if verbose
				File.open("#{file_basename}.c", "w") { |f| f.puts(c_code) }
				unless only_c
					puts "compiling #{file_basename}.c" if verbose
					compile_c_file_to_dllib(file_basename, name, verbose)
				end
			end

			def usage
				puts <<EOS
Usage: #$0 [options] file.rb ...

Translates the given Ruby file into an equivalent C extension. The result is
stored in file.c. It will then be compiled into a shared object file, unless
the option --only-c is given.

If multiple files are given, each file will be handled separately.

Options:
  -c  --only-c   only translate to C
  -v  --verbose  print status messages
  -h  --help     print this help
EOS
			end

			def run
				require 'getoptlong'

				opts = GetoptLong.new(
					[ '--only-c',  '-c', GetoptLong::NO_ARGUMENT ],
					[ '--verbose', '-v', GetoptLong::NO_ARGUMENT ],
					[ '--help',    '-h', GetoptLong::NO_ARGUMENT ]
				)

				only_c = verbose = false
				begin
					opts.each do |opt, arg|
						case opt
						when "--only-c"
							only_c = true
						when "--verbose"
							verbose = true
						when "--help"
							usage
							exit
						end
					end
					if ARGV.empty?
						warn "No files given"
						raise
					end
				rescue
					puts
					usage
					exit 1
				end

				begin
					ARGV.each { |fn|
						compile_file(fn, only_c, verbose)
					}
				rescue RuntimeError, SyntaxError => e
					warn e
					exit 1
				end
			end
		end
	end

end

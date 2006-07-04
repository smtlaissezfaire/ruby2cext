
require "rubynode"
require "ruby2cext/parser"
require "ruby2cext/tools"
require "ruby2cext/c_function"

module Ruby2CExtension

	class Compiler
		def initialize(name)
			@name = name
			@funs = []
			@toplevel_funs = []
			@sym_man = Tools::SymbolManager.new
			@global_man = Tools::GlobalManager.new
			@uniq_names = Tools::UniqueNames.new
			@helpers = {}
		end

		def to_c_code
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
			res.concat(@funs)
			res << "void Init_#{@name}() {"
			res << "org_ruby_top_self = ruby_top_self;"
			# just to be sure
			res << "rb_global_variable(&org_ruby_top_self);"
			res << "init_syms();"
			res << "init_globals();"
			res << "NODE *cref = rb_node_newnode(NODE_CREF, rb_cObject, 0, 0);"
			@toplevel_funs.each { |f| res << "#{f}(ruby_top_self, cref);" }
			res << "}"
			res.join("\n").split("\n").map { |l| l.strip }.reject { |l| l.empty? }.join("\n")
		end

		# non destructive: node_tree will not be changed
		def add_toplevel(node_tree)
			@toplevel_funs << CFunction::ToplevelScope.compile(self, node_tree)
		end

		# uniq name
		def un(str)
			@uniq_names.get(str)
		end
		def sym(sym)
			@sym_man.get(sym)
		end
		def global(str)
			@global_man.get(str)
		end

		def add_helper(str)
			@helpers[str] ||= str
		end

		def add_fun(str)
			@funs << str
		end

		class << self
			def compile_ruby_to_c(source_str, name, file_name)
				c = self.new(name)
				hash = Parser.parse_string(source_str, file_name)
				# abb all BEGIN blocks, if available
				if (beg_tree = hash[:begin])
					beg_tree = beg_tree.transform(:include_node => true)
					if beg_tree.first == :block
						beg_tree.last.each { |s| c.add_toplevel(s) }
					else
						c.add_toplevel(beg_tree)
					end
				end
				# add toplevel scope
				if (tree = hash[:tree])
					c.add_toplevel(tree.transform(:include_node => true))
				end
				c.to_c_code
			end

			def compile_c_file_to_dllib(file_basename, name, verbose = false)
				require "rbconfig"

				conf = ::Config::CONFIG
				ldshared = conf["LDSHARED"]
				cflags = [conf["CCDLFLAGS"], conf["CFLAGS"], conf["ARCH_FLAG"]].join(" ")
				hdrdir = conf["archdir"]
				dlext = conf["DLEXT"]
				libs = [conf["LIBRUBYARG"], conf["LIBS"], conf["DLDLIBS"]].join(" ")
				cmd = "#{ldshared} #{cflags} -I. -I #{hdrdir} -o #{file_basename}.#{dlext} #{file_basename}.c #{libs}"
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
				c_code = compile_ruby_to_c(source_str, name, file_name)
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


require "digest/sha1"
require "ruby2cext/compiler"
require "ruby2cext/error"
require "ruby2cext/version"

module Ruby2CExtension

	class Eval2C

		attr_reader :path, :prefix, :plugins, :logger, :force_recompile

		DEFAULT_PATH = File.join(File.expand_path(ENV["HOME"] || ENV["USERPROFILE"] || ENV["HOMEPATH"] || "."), ".ruby2cext")

		def initialize(options = {})
			unless (@path = options[:path])
				@path = DEFAULT_PATH
				Dir.mkdir(@path, 0700) unless File.exists?(@path)
			end
			@path = File.expand_path(@path)
			unless File.directory?(@path)
				raise Ruby2CExtError, "#{@path} is no directory"
			end
			unless File.stat(@path).mode & 022 == 0 # no writing for group and others
				warn "Ruby2CExtension::Eval2C warning: #{@path} is insecure"
			end
			@prefix = options[:prefix] || "eval2c"
			@plugins = options[:plugins] || {:optimizations => :all}
			@logger = options[:logger]
			@force_recompile = options[:force_recompile]
			@done = {}
			@digest_extra = Ruby2CExtension::FULL_VERSION_STRING + @plugins.inspect.split(//).sort.join("")
		end

		def compile_to_proc(code_str)
			name = "#{prefix}_#{Digest::SHA1.hexdigest(code_str + @digest_extra)}"
			gvar_name = "$__#{name}__"
			dl_filename = File.join(path, "#{name}.#{Compiler::DLEXT}")
			if !File.exists?(dl_filename) || (force_recompile && !@done[name])
				c = Compiler.new(name, logger)
				c.add_plugins(plugins)
				c.add_rb_file("BEGIN { #{gvar_name} = proc { #{code_str} } }", name) # BEGIN {} to get public vmode
				c_filename = File.join(path, "#{name}.c")
				File.open(c_filename, "w") { |f| f.puts(c.to_c_code) }
				unless Compiler.compile_c_file_to_dllib(c_filename, logger) == dl_filename
					raise Ruby2CExtError::Bug, "unexpected return value from compile_c_file_to_dllib"
				end
				@done[name] = true
			end
			require dl_filename
			eval(gvar_name) # return the proc
		end

		def module_eval(mod, code_str)
			mod.module_eval(&compile_to_proc(code_str))
		end
		alias :class_eval :module_eval

		def instance_eval(object, code_str)
			object.instance_eval(&compile_to_proc(code_str))
		end

		def toplevel_eval(code_str)
			compile_to_proc(code_str).call
		end

	end

end


require "digest/sha1"
require "ruby2cext/compiler"
require "ruby2cext/error"

module Ruby2CExtension

	class Eval2C

		attr_reader :path, :prefix, :plugins, :logger, :force_recompile

		def initialize(path = ".", options = {})
			@path = File.expand_path(path)
			@prefix = options[:prefix] || "eval2c"
			@plugins = options[:plugins] || {:optimizations => :all}
			@logger = options[:logger]
			@force_recompile = options[:force_recompile]
			@done = {}
		end

		def compile_to_proc(code_str)
			name = "#{prefix}_#{Digest::SHA1.hexdigest(code_str)}"
			gvar_name = "$__#{name}__"
			dl_filename = File.join(path, "#{name}.#{Compiler::DLEXT}")
			if !File.exists?(dl_filename) || (force_recompile && !@done[name])
				c = Compiler.new(name, logger)
				c.add_plugins(plugins)
				c.add_rb_file("BEGIN { #{gvar_name} = proc { #{code_str} } }", name)
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

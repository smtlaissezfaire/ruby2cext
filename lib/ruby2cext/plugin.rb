
module Ruby2CExtension

	class Plugin
		attr_reader :compiler

		def initialize(compiler)
			@compiler = compiler
		end

		# C code returned by this method will be inserted into the final C file
		# between the helpers and the C functions
		def global_c_code
			nil
		end

		# C code returned by this method will be inserted into the Init_*()
		# function before the calling of the toplevel scopes
		def init_c_code
			nil
		end
	end

end

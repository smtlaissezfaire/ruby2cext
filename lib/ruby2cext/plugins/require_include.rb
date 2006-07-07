
require "ruby2cext/plugin"

module Ruby2CExtension::Plugins

	class RequireInclude < Ruby2CExtension::Plugin

		attr_reader :include_paths
		def initialize(compiler, include_paths)
			super(compiler)
			@include_paths = include_paths
			done = {}
			compiler.add_preprocessor(:fcall) { |cfun, node|
				hash = node.last
				if hash[:mid] == :require &&
						(args = hash[:args]) &&
						args.first == :array &&
						(args = args.last).size == 1 &&
						Array === args[0] &&
						args[0].first == :str &&
						(file = search_file(args[0].last[:lit]))
					unless done[File.expand_path(file)]
						done[File.expand_path(file)] = true
						cfun.compiler.log "including require'd file: #{file}"
						cfun.compiler.add_rb_file(IO.read(file), file)
					end
					"Qtrue"
				else
					node
				end
			}
		end

		def search_file(req_str)
			req_str = req_str.dup
			unless req_str =~ /\.rb\z/
				req_str << ".rb"
			end
			res = false
			include_paths.map { |path|
				File.join(path, req_str)
			}.find { |file|
				File.exists? file
			}
		end
	end

end

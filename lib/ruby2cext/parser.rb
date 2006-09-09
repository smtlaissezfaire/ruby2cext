
require "rubynode"

module Ruby2CExtension

	# not really a parser, uses rubynode
	module Parser
		def self.parse_string(str, file_name = "(parse)")
			res = {}
			# for the first parsing use original str, because it doesn't matter
			# for BEGIN stuff and we get better exceptions this way.
			if (tmp = str.parse_begin_to_nodes(file_name, 1))
				res[:begin] = tmp
			end
			# now wrap str in a class scope and strip the class node of
			# afterwards, to get a clean scope in the result. src should
			# not have syntax errors if str didn't.
			src = "class Object\n#{str}\nend"
			begin
				old_verb = $VERBOSE
				# turn warnings of here to avoid the repetition of parse warnings
				$VERBOSE = nil
				if (tmp = src.parse_to_nodes(file_name, 0))
					tmp = tmp.nd_next.nd_body
					if tmp.type == :scope && tmp.nd_next
						res[:tree] = tmp
					end
				end
			ensure
				$VERBOSE = old_verb
			end
			res
		end
	end

end

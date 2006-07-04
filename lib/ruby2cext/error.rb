
module Ruby2CExtension

	class Ruby2CExtError < StandardError
		class NotSupported < self
		end

		class Bug < self
			def initialize(msg)
				super("BUG! #{msg}")
			end
		end
	end

end

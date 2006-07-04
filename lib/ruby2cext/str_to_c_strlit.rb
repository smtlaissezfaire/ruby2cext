
class String
	C_STRLIT_MAP = (0..255).map { |b| b.chr.inspect[1..-2] }.freeze
	def to_c_strlit
		# this might be a bit slow, but #inspect escapes ruby specific stuff,
		# that generates warnings in C (e.g. '#$' => "\#$")
		map = C_STRLIT_MAP
		res = ""
		each_byte { |b| res << map[b] }
		"\"#{res}\""
	end
end

unless $".empty?
	raise '$" is not empty'
end

classes = [
	NilClass,
	#Class, Module,
	Float, String, Regexp, Array, Fixnum, Hash, Bignum,
	#Struct, ???
	#File, rb_cIO and rb_cFile conflict
	TrueClass, FalseClass,
	#MatchData,
	Symbol,
].sort_by { |c| c.name }

def method_info(meth, klass)
	res =
		case meth.inspect
		when /: (\w+)\((\w+)\)#(.+)>/
			[$1.to_sym, $3.to_sym, meth.arity, $2.to_sym]
		when /: (\w+)#(.+)>/
			[$1.to_sym, $2.to_sym, meth.arity]
		else
			p meth
			raise
		end
	if res[0] != klass.name.to_sym
		p res
		raise
	end
	res[1..-1]
end

taboo_methods =
	(Object.instance_methods - [
		"==", "===", "=~", "__id__", "class",
		"clone", # ?
		"dup", "eql?", "equal?",
		"freeze", "frozen?",
		"hash", "inspect",
		"instance_of?",
		"instance_variable_get", "instance_variable_set", "instance_variables",
		"is_a?", "kind_of?",
		"method", "nil?", "object_id",
		"taint", "tainted?", "to_a", "to_s", "untaint"
	]) +
	(Enumerable.instance_methods - ["entries", "include?", "member?", "to_a"])+
	(Precision.instance_methods)+
	["each"]

first = true
common = []
all = classes.map { |klass|
	meths = ((klass.instance_methods - taboo_methods).map { |ms|
		method_info(klass.instance_method(ms), klass)
	}.select { |mi| mi[1] >= -1 }.sort_by { |mi| [mi[2].to_s, mi[1], mi[0].to_s] })
	if first
		common = meths
		first = false
	else
		common &= meths
	end
	[klass, meths]
}
all = all.map { |(kl, meth)| [kl, meth - common] }
puts "Common:"
puts(common.map { |mi| " #{mi.inspect}," })
require "pp"
pp all

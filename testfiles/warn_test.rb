eval("1")
method(:eval).arity
class A
	define_method(:evxx, method(:eval))
end
def foo
	local_variables +
		[binding]
end

class A
	p=proc{}
	instance_eval(&p)
	module_eval(&p)
	class_eval(&p)
	define_method(:xx, &p)
	instance_eval("1")
	module_eval("1")
	class_eval("1")
	define_method(:yy) {}
end

callcc(&proc{})
callcc { |cc| cc }

# no warnings below
class A
	instance_eval { 1 }
	module_eval { 1 }
	class_eval { 1 }
end

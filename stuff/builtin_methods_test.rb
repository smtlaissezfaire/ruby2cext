
$:.unshift("../lib")
require "ruby2cext"
require "ruby2cext/plugins/builtin_methods"

include Ruby2CExtension

NIL_OP = [:nil, {}]

calls = []

Plugins::BuiltinMethods::METHODS.each { |k, v|
	v.each { |arr|
		args = (arr[1] == 0) ? false : [:array, [NIL_OP] * arr[1]]
		calls << [:call, {:mid=>arr[0], :recv=>NIL_OP, :args=>args}]
	}
}

c = Compiler.new("bm_test", false)
c.add_plugin(Plugins::BuiltinMethods, Plugins::BuiltinMethods::SUPPORTED_BUILTINS)
c.add_toplevel(c.compile_toplevel_function([:scope, {
	:tbl=>nil,
	:next=> [:block, calls],
	:rval=>false
}]))
c = c.to_c_code
c=c.split("\n")[0..-3]
lookups = c.join("\n").scan(/= (builtinopt_method_lookup.rb_.*);/).flatten
p lookups.size
lookups.each { |l|
	c << "if (!(#{l})) rb_warn(\"#{l} failed\");"
}
c << "}"
File.open("bm_test.c", "w") { |f|
	f.puts c.join("\n")
}
Compiler.compile_c_file_to_dllib("bm_test", "bm_test", true)


require "ruby2cext/error"
require "ruby2cext/plugin"

module Ruby2CExtension::Plugins

	class BuiltinMethods < Ruby2CExtension::Plugin
		# for public methods of builtin types with a fixed arity, which don't do anything with blocks

		SUPPORTED_BUILTINS = [:Array, :Bignum, :FalseClass, :Fixnum, :Float, :Hash, :NilClass, :Regexp, :String, :Symbol, :TrueClass]

		COMMON_METHODS = [ # all supported builtins use these methods from Kernel
			 [:__id__, 0, :Kernel],
			 [:class, 0, :Kernel],
			 [:clone, 0, :Kernel],
			 [:dup, 0, :Kernel],
			 [:freeze, 0, :Kernel],
			 [:instance_variables, 0, :Kernel],
			 [:object_id, 0, :Kernel],
			 [:taint, 0, :Kernel],
			 [:tainted?, 0, :Kernel],
			 [:untaint, 0, :Kernel],
			 [:equal?, 1, :Kernel],
			 [:instance_of?, 1, :Kernel],
			 [:instance_variable_get, 1, :Kernel],
			 [:is_a?, 1, :Kernel],
			 [:kind_of?, 1, :Kernel],
			 [:method, 1, :Kernel],
			 [:instance_variable_set, 2, :Kernel],
		]

		METHODS = {
			:Array => [
				[:clear, 0],
				[:compact, 0],
				[:compact!, 0],
				[:empty?, 0],
				[:flatten, 0],
				[:flatten!, 0],
				[:frozen?, 0],
				[:hash, 0],
				[:inspect, 0],
				[:length, 0],
				[:nitems, 0],
				[:pop, 0],
				[:reverse, 0],
				[:reverse!, 0],
				[:shift, 0],
				[:size, 0],
				[:to_a, 0],
				[:to_ary, 0],
				[:to_s, 0],
				[:transpose, 0],
				[:uniq, 0],
				[:uniq!, 0],
				[:&, 1],
				[:|, 1],
				[:*, 1],
				[:+, 1],
				[:-, 1],
				[:<<, 1],
				[:<=>, 1],
				[:==, 1],
				[:assoc, 1],
				[:at, 1],
				[:concat, 1],
				[:delete_at, 1],
				[:eql?, 1],
				[:include?, 1],
				[:index, 1],
				[:rassoc, 1],
				[:replace, 1],
				[:rindex, 1],
				[:entries, 0, :Enumerable],
				[:member?, 1, :Enumerable],
				[:nil?, 0, :Kernel],
				[:===, 1, :Kernel],
				[:=~, 1, :Kernel],
			],
			:Bignum => [
				[:-@, 0],
				[:abs, 0],
				[:hash, 0],
				[:size, 0],
				[:to_f, 0],
				[:~, 0],
				[:%, 1, nil, [:Fixnum, :Bignum]],
				[:&, 1],
				[:*, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:**, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:+, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:-, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:/, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:<<, 1],
				[:<=>, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:==, 1],
				[:>>, 1],
				[:[], 1],
				[:^, 1],
				[:coerce, 1],
				[:div, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:divmod, 1, nil, [:Fixnum, :Bignum]],
				[:eql?, 1],
				[:modulo, 1, nil, [:Fixnum, :Bignum]],
				[:quo, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:remainder, 1, nil, [:Fixnum, :Bignum]],
				[:|, 1],
				[:<, 1, :Comparable],
				[:<=, 1, :Comparable],
				[:>, 1, :Comparable],
				[:>=, 1, :Comparable],
				[:between?, 2, :Comparable],
				[:ceil, 0, :Integer],
				[:chr, 0, :Integer],
				[:floor, 0, :Integer],
				[:integer?, 0, :Integer],
				[:next, 0, :Integer],
				[:round, 0, :Integer],
				[:succ, 0, :Integer],
				[:to_i, 0, :Integer],
				[:to_int, 0, :Integer],
				[:truncate, 0, :Integer],
				[:+@, 0, :Numeric],
				[:nonzero?, 0, :Numeric],
				[:zero?, 0, :Numeric],
				[:frozen?, 0, :Kernel],
				[:inspect, 0, :Kernel],
				[:nil?, 0, :Kernel],
				[:to_a, 0, :Kernel],
				[:===, 1, :Kernel],
				[:=~, 1, :Kernel],
			],
			:FalseClass => [
				[:to_s, 0],
				[:&, 1],
				[:^, 1],
				[:|, 1],
				[:frozen?, 0, :Kernel],
				[:hash, 0, :Kernel],
				[:inspect, 0, :Kernel],
				[:nil?, 0, :Kernel],
				[:to_a, 0, :Kernel],
				[:==, 1, :Kernel],
				[:===, 1, :Kernel],
				[:=~, 1, :Kernel],
				[:eql?, 1, :Kernel],
			],
			:Fixnum => [
				[:-@, 0],
				[:abs, 0],
				[:id2name, 0],
				[:size, 0],
				[:to_sym, 0],
				[:to_f, 0],
				[:zero?, 0],
				[:~, 0],
				[:+, 1, nil, [:Fixnum, :Float]],
				[:-, 1, nil, [:Fixnum, :Float]],
				[:*, 1, nil, [:Fixnum, :Float]],
				[:**, 1, nil, [:Fixnum, :Float]],
				[:/, 1, nil, [:Fixnum]],
				[:div, 1, nil, [:Fixnum]],
				[:%, 1, nil, [:Fixnum]],
				[:modulo, 1, nil, [:Fixnum]],
				[:divmod, 1, nil, [:Fixnum]],
				[:quo, 1, nil, [:Fixnum]],
				[:<=>, 1, nil, [:Fixnum]],
				[:>, 1, nil, [:Fixnum]],
				[:>=, 1, nil, [:Fixnum]],
				[:<, 1, nil, [:Fixnum]],
				[:<=, 1, nil, [:Fixnum]],
				[:==, 1],
				[:&, 1],
				[:|, 1],
				[:^, 1],
				[:[], 1],
				[:<<, 1],
				[:>>, 1],
				[:between?, 2, :Comparable],
				[:+@, 0, :Numeric],
				[:nonzero?, 0, :Numeric],
				[:coerce, 1, :Numeric],
				[:eql?, 1, :Numeric],
				[:remainder, 1, :Numeric],
				[:ceil, 0, :Integer],
				[:chr, 0, :Integer],
				[:floor, 0, :Integer],
				[:integer?, 0, :Integer],
				[:next, 0, :Integer],
				[:round, 0, :Integer],
				[:succ, 0, :Integer],
				[:to_i, 0, :Integer],
				[:to_int, 0, :Integer],
				[:truncate, 0, :Integer],
				[:frozen?, 0, :Kernel],
				[:hash, 0, :Kernel],
				[:inspect, 0, :Kernel],
				[:nil?, 0, :Kernel],
				[:to_a, 0, :Kernel],
				[:===, 1, :Kernel],
				[:=~, 1, :Kernel],
			],
			:Float => [
				[:-@, 0],
				[:to_s, 0],
				[:hash, 0],
				[:to_f, 0],
				[:abs, 0],
				[:zero?, 0],
				[:to_i, 0],
				[:to_int, 0],
				[:floor, 0],
				[:ceil, 0],
				[:round, 0],
				[:truncate, 0],
				[:nan?, 0],
				[:infinite?, 0],
				[:finite?, 0],
				[:coerce, 1],
				[:eql?, 1],
				[:==, 1],
				[:+, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:-, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:*, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:/, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:%, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:modulo, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:divmod, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:**, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:<=>, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:>, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:>=, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:<, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:<=, 1, nil, [:Fixnum, :Bignum, :Float]],
				[:between?, 2, :Comparable],
				[:+@, 0, :Numeric],
				[:integer?, 0, :Numeric],
				[:nonzero?, 0, :Numeric],
				[:div, 1, :Numeric],
				[:quo, 1, :Numeric],
				[:remainder, 1, :Numeric],
				[:frozen?, 0, :Kernel],
				[:inspect, 0, :Kernel],
				[:nil?, 0, :Kernel],
				[:to_a, 0, :Kernel],
				[:===, 1, :Kernel],
				[:=~, 1, :Kernel],
			],
			:Hash => [
				[:clear, 0],
				[:default_proc, 0],
				[:empty?, 0],
				[:inspect, 0],
				[:invert, 0],
				[:keys, 0],
				[:length, 0],
				[:rehash, 0],
				[:shift, 0],
				[:size, 0],
				[:to_a, 0],
				[:to_hash, 0],
				[:to_s, 0],
				[:values, 0],
				[:==, 1],
				[:[], 1],
				[:default=, 1],
				[:delete, 1],
				[:has_key?, 1],
				[:has_value?, 1],
				[:include?, 1],
				[:index, 1],
				[:key?, 1],
				[:member?, 1],
				[:replace, 1],
				[:value?, 1],
				[:[]=, 2],
				[:store, 2],
				[:entries, 0, :Enumerable],
				[:frozen?, 0, :Kernel],
				[:hash, 0, :Kernel],
				[:nil?, 0, :Kernel],
				[:===, 1, :Kernel],
				[:=~, 1, :Kernel],
				[:eql?, 1, :Kernel],
			],
			:NilClass => [
				[:inspect, 0],
				[:nil?, 0],
				[:to_a, 0],
				[:to_f, 0],
				[:to_i, 0],
				[:to_s, 0],
				[:&, 1],
				[:^, 1],
				[:|, 1],
				[:frozen?, 0, :Kernel],
				[:hash, 0, :Kernel],
				[:==, 1, :Kernel],
				[:===, 1, :Kernel],
				[:=~, 1, :Kernel],
				[:eql?, 1, :Kernel],
			],
			:Regexp => [
				[:casefold?, 0],
				[:hash, 0],
				[:inspect, 0],
				[:kcode, 0],
				[:options, 0],
				[:source, 0],
				[:to_s, 0],
				[:~, 0],
				[:==, 1],
				[:===, 1],
				[:=~, 1],
				[:eql?, 1],
				[:match, 1],
				[:frozen?, 0, :Kernel],
				[:nil?, 0, :Kernel],
				[:to_a, 0, :Kernel],
			],
			:String => [
				[:capitalize, 0],
				[:capitalize!, 0],
				[:chop, 0],
				[:chop!, 0],
				[:downcase, 0],
				[:downcase!, 0],
				[:dump, 0],
				[:empty?, 0],
				[:hash, 0],
				[:hex, 0],
				[:inspect, 0],
				[:intern, 0],
				[:length, 0],
				[:lstrip, 0],
				[:lstrip!, 0],
				[:next, 0],
				[:next!, 0],
				[:oct, 0],
				[:reverse, 0],
				[:reverse!, 0],
				[:rstrip, 0],
				[:rstrip!, 0],
				[:size, 0],
				[:strip, 0],
				[:strip!, 0],
				[:succ, 0],
				[:succ!, 0],
				[:swapcase, 0],
				[:swapcase!, 0],
				[:to_f, 0],
				[:to_s, 0],
				[:to_str, 0],
				[:to_sym, 0],
				[:upcase, 0],
				[:upcase!, 0],
				[:%, 1],
				[:*, 1],
				[:+, 1],
				[:<<, 1],
				[:<=>, 1],
				[:==, 1],
				[:=~, 1],
				[:casecmp, 1],
				[:concat, 1],
				[:crypt, 1],
				[:eql?, 1],
				[:include?, 1],
				[:match, 1],
				[:replace, 1],
				[:insert, 2],
				[:tr, 2],
				[:tr!, 2],
				[:tr_s, 2],
				[:tr_s!, 2],
				[:<, 1, :Comparable],
				[:<=, 1, :Comparable],
				[:>, 1, :Comparable],
				[:>=, 1, :Comparable],
				[:between?, 2, :Comparable],
				[:entries, 0, :Enumerable],
				[:to_a, 0, :Enumerable],
				[:member?, 1, :Enumerable],
				[:frozen?, 0, :Kernel],
				[:nil?, 0, :Kernel],
				[:===, 1, :Kernel],
			],
			:Symbol => [
				[:id2name, 0],
				[:inspect, 0],
				[:to_i, 0],
				[:to_int, 0],
				[:to_s, 0],
				[:to_sym, 0],
				[:===, 1],
				[:frozen?, 0, :Kernel],
				[:hash, 0, :Kernel],
				[:nil?, 0, :Kernel],
				[:to_a, 0, :Kernel],
				[:==, 1, :Kernel],
				[:=~, 1, :Kernel],
				[:eql?, 1, :Kernel],
			],
			:TrueClass => [
				[:to_s, 0],
				[:&, 1],
				[:^, 1],
				[:|, 1],
				[:frozen?, 0, :Kernel],
				[:hash, 0, :Kernel],
				[:inspect, 0, :Kernel],
				[:nil?, 0, :Kernel],
				[:to_a, 0, :Kernel],
				[:==, 1, :Kernel],
				[:===, 1, :Kernel],
				[:=~, 1, :Kernel],
				[:eql?, 1, :Kernel],
			],
		}

		METHOD_NAME_MAPPINGS = Hash.new { |h, k|
			case k.to_s
			when /\A\w+\z/
				h[k] = "buitinoptmeth_#{k}"
			when /\A\w+\?\z/
				h[k] = "buitinoptmeth_#{k.to_s[0..-2]}__pred"
			when /\A\w+!\z/
				h[k] = "buitinoptmeth_#{k.to_s[0..-2]}__bang"
			when /\A\w+=\z/
				h[k] = "buitinoptmeth_#{k.to_s[0..-2]}__assign"
			else
				raise Ruby2CExtension::Ruby2CExtError::Bug, "unexpected method name: #{k.inspect}"
			end
		}
		METHOD_NAME_MAPPINGS.merge!({
			:+@  => "buitinoptop_uplus",
			:-@  => "buitinoptop_uminus",
			:+   => "buitinoptop_plus",
			:-   => "buitinoptop_minus",
			:*   => "buitinoptop_mul",
			:/   => "buitinoptop_div",
			:**  => "buitinoptop_pow",
			:%   => "buitinoptop_mod",
			:~   => "buitinoptop_rev",
			:==  => "buitinoptop_equal",
			:=== => "buitinoptop_eqq",
			:=~  => "buitinoptop_match",
			:<=> => "buitinoptop_cmp",
			:>   => "buitinoptop_gt",
			:>=  => "buitinoptop_ge",
			:<   => "buitinoptop_lt",
			:<=  => "buitinoptop_le",
			:&   => "buitinoptop_and",
			:|   => "buitinoptop_or",
			:^   => "buitinoptop_xor",
			:[]  => "buitinoptop_aref",
			:[]= => "buitinoptop_aset",
			:<<  => "buitinoptop_lshift",
			:>>  => "buitinoptop_rshift",
		})

		BUILTIN_TYPE_MAP = Hash.new { |h, k|
			h[k] = "T_#{k}".upcase
		}
		BUILTIN_TYPE_MAP.merge!({
			:NilClass => "T_NIL",
			:TrueClass => "T_TRUE",
			:FalseClass => "T_FALSE",
		})

		BUILTIN_C_VAR_MAP = Hash.new { |h, k|
			h[k] = "rb_#{Module.const_get(k).instance_of?(Module) ? "m" : "c"}#{k}"
		}

		attr_reader :methods, :function_names

		def initialize(compiler, builtins)
			super(compiler)
			builtins = SUPPORTED_BUILTINS & builtins # "sort" and unique
			@methods = {} # [meth_sym, arity] =>
			              # [[type, impl. class/mod, types of first arg or nil,
			              # meth_tbl_data: [idx, [C class var, impl. C class/mod var, id, arity]] (initialized to nil)], ...]
			@function_names = {} # [meth_sym, arity] => name # initialized on first use
			@method_tbl_size = 0
			builtins.each { |builtin|
				(METHODS[builtin] + COMMON_METHODS).each { |arr|
					(@methods[arr[0, 2]] ||= []) << [builtin, arr[2] || builtin, arr[3], nil]
				}
			}
			compiler.add_preprocessor(:call) { |cfun, node|
				handle_call(cfun, node.last, node)
			}
		end

		def handle_call(cfun, hash, node)
			args = []
			if hash[:args]
				if hash[:args].first == :array
					args = hash[:args].last
				else
					return node
				end
			end
			if (fun = get_function(hash[:mid], args.size))
				cfun.instance_eval {
					recv = comp(hash[:recv])
					if args.empty?
						"#{fun}(#{recv})"
					else
						c_scope_res {
							l "VALUE recv = #{recv};"
							build_c_arr(args, "argv")
							"#{fun}(recv, argv)"
						}
					end
				}
			else
				node
			end
		end

		def get_function(method, arity)
			ma = [method, arity]
			if (fn = function_names[ma])
				return fn
			end
			if (m = methods[ma])
				fn = function_names[ma] = "#{METHOD_NAME_MAPPINGS[method]}__#{arity}"
				m.each { |meth_arr|
					meth_arr[3] = [@method_tbl_size, [BUILTIN_C_VAR_MAP[meth_arr[0]], BUILTIN_C_VAR_MAP[meth_arr[1]], compiler.sym(method), arity]]
					@method_tbl_size += 1
				}
				fn
			else
				nil
			end
		end

		METHOD_LOOKUP_CODE = %{
			static BUILTINOPT_FP buitinopt_method_lookup(VALUE klass, VALUE origin, ID mid, long arity) {
				NODE *body;
				while (klass != origin) {
					if (TYPE(klass) == T_ICLASS && RBASIC(klass)->klass == origin) break;
					if (st_lookup(RCLASS(klass)->m_tbl, mid, (st_data_t *)&body)) return NULL;
					klass = RCLASS(klass)->super;
					if (!klass) return NULL;
				}
				if (st_lookup(RCLASS(klass)->m_tbl, mid, (st_data_t *)&body)) {
					body = body->nd_body;
					if (nd_type(body) == NODE_FBODY) body = body->nd_head;
					if (nd_type(body) == NODE_CFUNC && body->nd_argc == arity) {
						return body->nd_cfnc;
					}
				}
				return NULL;
			}
		}

		def global_c_code
			unless function_names.empty?
				res = []
				res << "typedef VALUE (*BUILTINOPT_FP)(ANYARGS);"
				res << "static BUILTINOPT_FP buitinopt_method_tbl[#{@method_tbl_size}];"
				res << METHOD_LOOKUP_CODE
				function_names.sort_by { |ma, name| name }.each { |ma, name|
					method, arity = *ma
					res << "static VALUE #{name}(VALUE recv#{arity > 0 ? ", VALUE *argv" : ""}) {"
					res << "static int lookup_done = 0;"
					res << "if (!lookup_done) {"
					res << "lookup_done = 1;"
					methods[ma].each { |m|
						res << "buitinopt_method_tbl[#{m[3][0]}] = buitinopt_method_lookup(#{m[3][1].join(", ")});"
					}
					res << "}"
					res << "switch(TYPE(recv)) {"
					methods[ma].each { |m|
						res << "case #{BUILTIN_TYPE_MAP[m[0]]}:"
						if (other = m[2])
							if arity != 1
								raise Ruby2CExtension::Ruby2CExtError::Bug, "arity must be 1 for arg type check"
							end
							res << "switch(TYPE(argv[0])) {"
							res << other.map { |o| "case #{BUILTIN_TYPE_MAP[o]}:" }.join("\n")
							res << "if (buitinopt_method_tbl[#{m[3][0]}]) return (*(buitinopt_method_tbl[#{m[3][0]}]))(recv, argv[0]);"
							res << "default:\ngoto std_call;"
							res << "}"
						else
							res << "if (buitinopt_method_tbl[#{m[3][0]}])"
							args = (0...arity).map { |i| "argv[#{i}]" }.join(", ")
							args = ", " + args unless args.empty?
							res << "return (*(buitinopt_method_tbl[#{m[3][0]}]))(recv#{args});"
							res << "else goto std_call;"
						end
					}
					res << "default:\nstd_call:"
					res << "return rb_funcall3(recv, #{compiler.sym(method)}, #{arity}, #{arity > 0 ? "argv" : "0"});"
					res << "}\n}"
				}
				res.join("\n")
			end
		end

	end

end


require "ruby2cext/error"
require "ruby2cext/plugin"

module Ruby2CExtension::Plugins

	class BuiltinMethods < Ruby2CExtension::Plugin
		# for public methods of builtin types with a fixed arity, which don't do anything with blocks

		SUPPORTED_BUILTINS = [:Fixnum, :Float]

		METHODS = {
			:Fixnum => [
				[:-@, 0],
				[:abs, 0],
				[:~, 0],
				[:to_f, 0],
				[:zero?, 0],
				[:+, 1, [:Fixnum, :Float]],
				[:-, 1, [:Fixnum, :Float]],
				[:*, 1, [:Fixnum, :Float]],
				[:**, 1, [:Fixnum, :Float]],
				[:/, 1, [:Fixnum]],
				[:div, 1, [:Fixnum]],
				[:%, 1, [:Fixnum]],
				[:modulo, 1, [:Fixnum]],
				[:divmod, 1, [:Fixnum]],
				[:quo, 1, [:Fixnum]],
				[:<=>, 1, [:Fixnum]],
				[:>, 1, [:Fixnum]],
				[:>=, 1, [:Fixnum]],
				[:<, 1, [:Fixnum]],
				[:<=, 1, [:Fixnum]],
				[:==, 1],
				[:&, 1],
				[:|, 1],
				[:^, 1],
				[:[], 1],
				[:<<, 1],
				[:>>, 1],
				# TODO: some more methods from Number/Integer??? (safety check ?!!)
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
				[:+, 1, [:Fixnum, :Bignum, :Float]],
				[:-, 1, [:Fixnum, :Bignum, :Float]],
				[:*, 1, [:Fixnum, :Bignum, :Float]],
				[:/, 1, [:Fixnum, :Bignum, :Float]],
				[:%, 1, [:Fixnum, :Bignum, :Float]],
				[:modulo, 1, [:Fixnum, :Bignum, :Float]],
				[:divmod, 1, [:Fixnum, :Bignum, :Float]],
				[:**, 1, [:Fixnum, :Bignum, :Float]],
				[:<=>, 1, [:Fixnum, :Bignum, :Float]],
				[:>, 1, [:Fixnum, :Bignum, :Float]],
				[:>=, 1, [:Fixnum, :Bignum, :Float]],
				[:<, 1, [:Fixnum, :Bignum, :Float]],
				[:<=, 1, [:Fixnum, :Bignum, :Float]],
			],
		}

		METHOD_NAME_MAPPINGS = Hash.new { |h, k|
			case k.to_s
			when /\A\w+\z/
				h[k] = "buitinoptmeth_#{k}"
			when /\A\w+\?\z/
				h[k] = "buitinoptmeth_#{k}__pred"
			when /\A\w+!\z/
				h[k] = "buitinoptmeth_#{k}__bang"
			else
				raise Ruby2CExtension::Ruby2CExtError::Bug, "unexpected method name: #{l.inspect}"
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
			:<=> => "buitinoptop_cmp",
			:>   => "buitinoptop_gt",
			:>=  => "buitinoptop_ge",
			:<   => "buitinoptop_lt",
			:<=  => "buitinoptop_le",
			:&   => "buitinoptop_and",
			:|   => "buitinoptop_or",
			:^   => "buitinoptop_xor",
			:[]  => "buitinoptop_aref",
			:<<  => "buitinoptop_lshift",
			:>>  => "buitinoptop_rshift",
		})

		BUILTIN_TYPE_MAP = Hash.new { |h, k|
			h[k] = "T_#{k}".upcase
		}

		attr_reader :methods, :function_names

		def initialize(compiler, builtins)
			super(compiler)
			builtins = SUPPORTED_BUILTINS & builtins # "sort" and unique
			@methods = {} # [meth_sym, arity] => [[type, other types or nil, tbl_idx (initialized to nil)], ...]
			@function_names = {} # [meth_sym, arity] => name # initialized on first use
			@method_tbl = [] # [[C class var, id, arity], ...]
			builtins.each { |builtin|
				METHODS[builtin].each { |arr|
					(@methods[arr[0, 2]] ||= []) << [builtin, arr[2], nil]
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
					meth_arr[2] = @method_tbl.size
					@method_tbl << ["rb_c#{meth_arr[0]}", compiler.sym(method), arity]
				}
				fn
			else
				nil
			end
		end

		def global_c_code
			if function_names.empty?
				nil
			else
				res = []
				res << "typedef VALUE (*BUILTINOPT_FP)(ANYARGS);"
				res << "static BUILTINOPT_FP buitinopt_method_tbl[#{@method_tbl.size}];"
				res << %{
					static BUILTINOPT_FP buitinopt_method_lookup(VALUE klass, ID mid, long arity) {
						NODE *body;
						if (st_lookup(RCLASS(klass)->m_tbl, mid, (st_data_t *)&body)) {
							body = body->nd_body;
							if (nd_type(body) == NODE_CFUNC && body->nd_argc == arity) {
								return body->nd_cfnc;
							}
						}
						return NULL;
					}
				}
				res << "static void init_builtinopt() {"
				@method_tbl.each_with_index { |meth_arr, idx|
					res << "buitinopt_method_tbl[#{idx}] = buitinopt_method_lookup(#{meth_arr.join(", ")});"
				}
				res << "}"
				function_names.sort_by { |ma, name| name }.each { |ma, name|
					method, arity = *ma
					res << "static VALUE #{name}(VALUE recv#{arity > 0 ? ", VALUE *argv" : ""}) {"
					res << "switch(TYPE(recv)) {"
					methods[ma].each { |m|
						res << "case #{BUILTIN_TYPE_MAP[m[0]]}:"
						if (other = m[1])
							if arity != 1
								raise Ruby2CExtension::Ruby2CExtError::Bug, "arity must be 1 for arg type check"
							end
							res << "switch(TYPE(argv[0])) {"
							res << other.map { |o| "case #{BUILTIN_TYPE_MAP[o]}:" }.join("\n")
							res << "if (buitinopt_method_tbl[#{m[2]}]) return (*(buitinopt_method_tbl[#{m[2]}]))(recv, argv[0]);"
							res << "default:\ngoto std_call;"
							res << "}"
						else
							res << "if (buitinopt_method_tbl[#{m[2]}])"
							args = (0...arity).map { |i| "argv[#{i}]" }.join(", ")
							args = ", " + args unless args.empty?
							res << "return (*(buitinopt_method_tbl[#{m[2]}]))(recv#{args});"
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

		def init_c_code
			if function_names.empty?
				nil
			else
				"init_builtinopt();"
			end
		end

	end

end

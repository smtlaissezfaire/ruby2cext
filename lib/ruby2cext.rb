
require "rubynode"

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
					res[:tree] = tmp.nd_next.nd_body
				end
			ensure
				$VERBOSE = old_verb
			end
			res
		end
	end

	class Ruby2CExtError < StandardError
		class NotSupported < self
		end

		class Bug < self
			def initialize(msg)
				super("BUG! #{msg}")
			end
		end
	end

	module Tools
		# keeps track of used symbols and provides global variables that hold the corresponding ID
		class SymbolManager
			def initialize
				@syms = {}
			end

			# returns the var name for sym
			def get(sym)
				idx = (@syms[sym] ||= @syms.size)
				"sym[#{idx}] /* #{sym.to_s.gsub(/[^\w<=>+\-*\/!?@$|&~.]/, "_").gsub("*/", "__")} */"
			end

			def to_c_code(init_fun_name = "init_syms")
				res = []
				res << "static ID sym[#{@syms.size}];" unless @syms.empty?
				res << "static void #{init_fun_name}() {"
				@syms.sort_by { |sym, idx| idx }.each { |sym, idx|
					res << "sym[#{idx}] = rb_intern(#{sym.to_s.to_c_strlit});"
				}
				res << "}"
				res.join("\n")
			end
		end

		class GlobalManager
			def initialize
				@cnt = 0
				@src = []
			end

			# returns the var name for sym
			def get(str)
				name = "global[#{@cnt}]"
				@cnt += 1
				@src << "#{name} = #{str};" << "rb_global_variable(&(#{name}));"
				name
			end

			def to_c_code(init_fun_name = "init_globals")
				res = []
				res << "static VALUE global[#{@cnt}];" if @cnt > 0
				res << "static void #{init_fun_name}() {"
				res.concat(@src)
				res << "}"
				res.join("\n")
			end
		end

		class UniqueNames
			def initialize
				@prefix_cnt = Hash.new(0)
			end
			def get(prefix)
				"#{prefix}_#{@prefix_cnt[prefix]+=1}"
			end
		end

		module EnsureNodeTypeMixin
			def ensure_node_type(node, expected_types)
				expected_types = [expected_types] unless Array === expected_types
				unless node && expected_types.index(node.first)
					raise Ruby2CExtError, "unexpected node type: expected #{expected_types.join(" or ")}, found #{node.inspect}"
				end
			end
		end

	end

	module CommonNodeComp

		# the comp methods need lots of methods from CFunction::Base or subclasses to work, e.g.:
		# l, assign_res, get_self, get_class, get_cbase, get_cvar_cbase,
		# scope (with get_lvar, get_lvar_idx, get_dvar, get_dvar_curr, vmode, vmode_def_fun)
		# un, sym, global, add_helper
		# ...

		include Tools::EnsureNodeTypeMixin

		def c_scope
			l "{"
			yield
			l "}"
		end
		def c_scope_res
			l "{"
			assign_res(yield)
			l "}"
			"res"
		end

		def c_if(cond)
			l "if (#{cond}) {"
			yield
			l "}"
		end
		def c_else
			l "else {"
			yield
			l "}"
		end
		def c_for(exprs)
			l "for (#{exprs}) {"
			yield
			l "}"
		end

		def make_block(block)
			if block
				block.first == :block ? block : [:block, [block]]
			else
				[:block, []]
			end
		end
		def set_vmode(mid)
			if Scopes::Scope::VMODES.include? mid
				scope.vmode = mid
			end
		end

		def handle_method_args(arg, block_arg)
			ensure_node_type(arg, :args)
			# handle arg
			arg = arg.last
			cnt = arg[:cnt]
			opt = make_block(arg[:opt]).last
			rest = arg[:rest] - 2
			need_wrong_arg_num_helper = false
			if opt.empty? && rest < 0
				l "if (argc != #{cnt}) wrong_arg_num(argc, #{cnt});"
				need_wrong_arg_num_helper = true
			else
				if cnt > 0
					l "if (argc < #{cnt}) wrong_arg_num(argc, #{cnt});"
					need_wrong_arg_num_helper = true
				end
				if rest < 0
					l "if (argc > #{cnt + opt.size}) wrong_arg_num(argc, #{cnt + opt.size});"
					need_wrong_arg_num_helper = true
				end
			end
			if need_wrong_arg_num_helper
				add_helper <<-EOC
					static void wrong_arg_num(int argc, int exp) {
						rb_raise(rb_eArgError, "wrong number of arguments (%d for %d)", argc, exp);
					}
				EOC
			end
			(0...cnt).each { |i|
				l "#{scope.get_lvar_idx(i)} = argv[#{i}];"
			}
			opt.each_with_index { |asgn, i|
				c_if("argc > #{cnt + i}") {
					l "#{scope.get_lvar_idx(cnt + i)} = argv[#{cnt + i}];"
				}
				c_else {
					l "#{comp(asgn)};"
				}
			}
			if rest >= 0
				sum = cnt + opt.size
				c_if("argc > #{sum}") {
					l "#{scope.get_lvar_idx(rest)} = rb_ary_new4(argc-#{sum}, argv+#{sum});"
				}
				c_else {
					l "#{scope.get_lvar_idx(rest)} = rb_ary_new();"
				}
			end

			# handle block arg if available
			if block_arg
				ensure_node_type(block_arg, :block_arg)
				c_if("rb_block_given_p()") {
					l "#{scope.get_lvar(block_arg.last[:vid])} = rb_block_proc();"
				}
				# no else, lvars are initialized to nil
			end
		end

		def comp(node)
			case node
			when false
				"Qnil"
			when String
				node
			else
				l "/* #{node.first} */"
				begin
					send("comp_#{node.first}", node.last)
				rescue Ruby2CExtError => e
					if Hash === node.last
						n = node.last[:node]
						# add file and line to message
						raise "#{n.file}:#{n.line}: #{e}"
					else
						raise # reraise
					end
				end
			end
		end

		def comp_block(exprs)
			return "Qnil" if exprs.empty?
			last = exprs.last
			exprs[0..-2].each { |ex|
				l "#{comp(ex)};"
			}
			comp(last)
		end

		def comp_vcall(hash)
			set_vmode(hash[:mid])
			"rb_funcall2(#{get_self}, #{sym(hash[:mid])}, 0, 0)"
		end

		def build_c_arr(arr, var_name)
			l "VALUE #{var_name}[#{arr.size}];"
			arr.each_with_index { |n, i|
				l "#{var_name}[#{i}] = #{comp(n)};"
			}
		end
		def build_args(args)
			if args.first == :array
				l "const int argc = #{args.last.size};"
				build_c_arr(args.last, "argv")
			else
				l "int argc; VALUE *argv;"
				c_scope {
					l "VALUE args;"
					l "args = #{comp(args)};"
					l "if (TYPE(args) != T_ARRAY) args = rb_ary_to_ary(args);"
					l "argc = RARRAY(args)->len;"
					l "argv = ALLOCA_N(VALUE, argc);"
					l "MEMCPY(argv, RARRAY(args)->ptr, VALUE, argc);"
				}
			end
		end

		NON_ITER_PROC = proc { |str, args, arg_types| str % args }

		def do_funcall(recv, mid, args, allow_private, iter_proc)
			iter_proc ||= NON_ITER_PROC
			fun = "rb_funcall#{allow_private ? 2 : 3}"
			if args
				c_scope_res {
					l "VALUE recv = #{recv};"
					build_args(args)
					iter_proc["#{fun}(%s, #{sym(mid)}, %s, %s)", %w[recv argc argv], %w[VALUE int VALUE*]]
				}
			else
				set_vmode(mid) if allow_private
				iter_proc["#{fun}(%s, #{sym(mid)}, 0, 0)", [recv], %w[VALUE]]
			end
		end

		def comp_fcall(hash, &iter_proc)
			do_funcall(get_self, hash[:mid], hash[:args], true, iter_proc)
		end

		def comp_call(hash, &iter_proc)
			do_funcall(comp(hash[:recv]), hash[:mid], hash[:args], false, iter_proc)
		end

		def comp_attrasgn(hash)
			fun = "rb_funcall#{hash[:recv] == 0 ? 2 : 3}"
			recv = (hash[:recv] == 0 ? get_self : comp(hash[:recv]))
			c_scope_res {
				l "VALUE recv = #{recv};"
				build_args(hash[:args])
				l "#{fun}(recv, #{sym(hash[:mid])}, argc, argv);"
				"argv[argc - 1]"
			}
		end

		def helper_super_allowed_check
			# last_func is set to 0 by rb_require_safe
			add_helper <<-EOC
				static void super_allowed_check() {
					if (!ruby_frame->last_func) {
						rb_raise(rb_eNoMethodError, "super called outside of method");
					}
				}
			EOC
		end

		def comp_zsuper(hash, &iter_proc)
			iter_proc ||= NON_ITER_PROC
			helper_super_allowed_check
			l "super_allowed_check();"
			iter_proc["rb_call_super(ruby_frame->argc, ruby_frame->argv)", [], []]
		end

		def comp_super(hash, &iter_proc)
			iter_proc ||= NON_ITER_PROC
			helper_super_allowed_check
			l "super_allowed_check();"
			args = hash[:args]
			if args
				c_scope_res {
					build_args(args)
					"rb_call_super(argc, argv)"
					iter_proc["rb_call_super(%s, %s)", %w[argc argv], %w[int VALUE*]]
				}
			else
				iter_proc["rb_call_super(0, 0)", [], []]
			end
		end

		def comp_lit(hash)
			case (l = hash[:lit])
			when Fixnum
				"LONG2FIX(#{l.inspect})"
			when Symbol
				"ID2SYM(#{sym(l)})"
			when Bignum
				global("rb_cstr_to_inum(#{l.to_s.to_c_strlit}, 10, Qfalse)")
			when Float
				global("rb_float_new(%.40e)" % l)
			when Range
				global("rb_range_new(#{comp_lit(:lit=>l.first)}, #{comp_lit(:lit=>l.last)}, Q#{l.exclude_end?})")
			when Regexp
				s = l.source
				global("rb_reg_new(#{s.to_c_strlit}, #{s.size}, #{l.options})")
			else
				raise Ruby2CExtError::Bug, "unsupported literal type: #{l.inspect}"
			end
		end

		def comp_nil(hash); "Qnil"; end
		def comp_false(hash); "Qfalse"; end
		def comp_true(hash); "Qtrue"; end
		def comp_self(hash); get_self; end

		def comp_argscat(hash)
			c_scope_res {
				l "VALUE head, body;"
				l "head = #{comp(hash[:head])};"
				l "body = #{comp(hash[:body])};"
				l "body = (NIL_P(body) ? rb_ary_new3(1, Qnil) : rb_Array(body));"
				"rb_ary_concat(head, body)"
			}
		end
		def comp_argspush(hash)
			# argspush is used in a[2,*a]=4 for example
			c_scope_res {
				l "VALUE head;"
				l "head = rb_ary_dup(#{comp(hash[:head])});"
				"rb_ary_push(head, #{comp(hash[:body])})"
			}
		end

		def comp_begin(hash)
			comp(make_block(hash[:body]))
		end

		def comp_if(hash)
			cond = comp(hash[:cond])
			if !hash[:body] && !hash[:else]
				l cond + ";"
				"Qnil"
			else
				c_if("RTEST(#{cond})") {
					assign_res(comp(hash[:body]))
				}
				c_else {
					assign_res(comp(hash[:else]))
				}
				"res"
			end
		end

		def comp_postexe(hash, &iter_proc)
			raise Ruby2CExtError, "postexe only allowed with iter" unless iter_proc
			c_scope {
				l "static int done = 0;"
				c_if("!done") {
					l "done = 1;"
					# compile as at_exit (rb_f_END() is static)
					l do_funcall(get_self, :at_exit, false, true, iter_proc)
				}
			}
			"Qnil"
		end

		def comp_match(hash)
			"rb_reg_match2(#{comp_lit(hash)})"
		end
		def comp_match2(hash)
			c_scope_res {
				l "VALUE recv;"
				l "recv = #{comp(hash[:recv])};"
				"rb_reg_match(recv, #{comp(hash[:value])})"
			}
		end
		def comp_match3(hash)
			c_scope_res {
				l "VALUE recv, val;"
				l "recv = #{comp(hash[:recv])};"
				l "val = #{comp(hash[:value])};"
				"(TYPE(val) == T_STRING ? rb_reg_match(recv, val) : rb_funcall(val, #{sym(:=~)}, 1, recv))"
			}
		end

		def handle_when(hash, test_str)
			ensure_node_type(head = hash[:head], :array)
			c_scope_res {
				l "int when_handle = 1;"
				head.last.each { |check|
					if check.first == :when
						l "VALUE arr; long i;"
						l "arr = #{comp(check.last[:head])};"
						l "if (TYPE(arr) != T_ARRAY) arr = rb_ary_to_ary(arr);"
						c_for("i = 0; i < RARRAY(arr)->len; ++i") {
							c_if("RTEST(#{test_str % "RARRAY(arr)->ptr[i]"})") {
								l "arr = Qfalse;"
								l "break;"
							}
						}
						l "if (RTEST(arr)) {"
					else
						l "if (!RTEST(#{test_str % comp(check)})) {"
					end
				}
				l "when_handle = 0;" # here all checks failed
				head.last.size.times { l "}" }
				c_if("when_handle") {
					assign_res(comp(hash[:body]))
				}
				c_else {
					if hash[:next] && hash[:next].first == :when
						assign_res(handle_when(hash[:next].last, test_str))
					else
						assign_res(comp(hash[:next]))
					end
				}
				"res"
			}
		end
		def comp_when(hash)
			handle_when(hash, "%s")
		end
		def comp_case(hash)
			ensure_node_type(hash[:body], :when)
			c_scope_res {
				l "VALUE case_val;"
				l "case_val = #{comp(hash[:head])};"
				handle_when(hash[:body].last, "rb_funcall2(%s, #{sym(:===)}, 1, &case_val)")
			}
		end

		def comp_while(hash, is_until = false)
			redo_lbl = un("while_redo")
			next_lbl = un("while_next")
			break_lbl = un("while_break")
			c_scope_res {
				l "VALUE while_res = Qnil;"
				c_for(";;") {
					l "if (#{is_until ? "" : "!"}RTEST(#{comp(hash[:cond])})) break;" if hash[:state] != 0
					l "#{redo_lbl}:"
					push_while(redo_lbl, next_lbl, break_lbl)
					l "#{comp(hash[:body])};"
					pop_while
					l "#{next_lbl}: ;"
					l "if (#{is_until ? "" : "!"}RTEST(#{comp(hash[:cond])})) break;" if hash[:state] == 0
				}
				l "#{break_lbl}:"
				"while_res"
			}
		end
		def comp_until(hash)
			comp_while(hash, true)
		end

		def handle_iter(iter, bl_fun, closure)
			ensure_node_type(iter, [:call, :fcall, :super, :zsuper, :postexe]) # attrasgn ???
			l "do {"
			block_calls = 0
			assign_res(send("comp_#{iter.first}", iter.last) { |str, args, arg_types|
				block_calls += 1
				c_scope_res {
					l "VALUE iter_data[#{args.size + 1}];"
					l "iter_data[0] = Qfalse;"
					args.each_with_index { |arg, i|
						l "iter_data[#{i + 1}] = (VALUE)(#{arg});"
					}
					iter_data_cast = []
					arg_types.each_with_index { |arg_type, i|
						iter_data_cast << "(#{arg_type})(iter_data[#{i + 1}])"
					}
					it_fun = un("iterate")
					compiler.add_fun(<<-EOC.chomp % iter_data_cast)
						static VALUE #{it_fun}(VALUE data) {
							VALUE *iter_data = (VALUE*)data;
							ruby_top_self = org_ruby_top_self;
							if (iter_data[0]) return Qundef;
							iter_data[0] = Qtrue;
							return #{str};
						}
					EOC
					# hack to make instance_eval etc. work (rb_iterate() uses ruby_top_self as block.self)
					# this _should_ be save, because rb_trap_immediate should always be 0 here ...
					l "ruby_top_self = Qundef;"
					"rb_iterate(#{it_fun}, (VALUE)iter_data, #{bl_fun}, #{closure})"
				}
			})
			raise Ruby2CExtError::Bug, "internal error while compiling iter" unless block_calls == 1
			l "}"
			l "while (res == Qundef);"
			"res"
		end

		def comp_block_pass(hash)
			# TODO: is just a workaround/hack, does not work with instance_eval etc.
			c_scope_res {
				l "VALUE proc;"
				l "proc = #{comp(hash[:body])};"
				c_if("NIL_P(proc)") {
					assign_res(comp(hash[:iter]))
				}
				c_else {
					# rb_obj_is_proc is static in eval.c, so we just convert and hope the best...
					add_helper <<-EOC
						#define PROC_TSHIFT (FL_USHIFT+1)
						#define PROC_TMASK  (FL_USER1|FL_USER2|FL_USER3)
						static VALUE obj_to_proc(VALUE proc) {
							VALUE tmp;
							tmp = rb_check_convert_type(proc, T_DATA, "Proc", "to_proc");
							if (rb_class_real(CLASS_OF(tmp)) != rb_cProc)
								rb_raise(rb_eTypeError, "wrong argument type %s (expected Proc)", rb_obj_classname(proc));
							proc = tmp;
							if (ruby_safe_level >= 1 && OBJ_TAINTED(proc) &&
								ruby_safe_level > ((RBASIC(proc)->flags & PROC_TMASK) >> PROC_TSHIFT))
								rb_raise(rb_eSecurityError, "Insecure: tainted block value");
							return proc;
						}
					EOC
					l "proc = obj_to_proc(proc);"
					add_helper <<-EOC
						static VALUE block_pass_helper_block(VALUE bl_val, VALUE proc, VALUE self) {
							if (ruby_current_node->nd_state != 1) {
								if (bl_val == Qundef) bl_val = rb_ary_new2(0);
								else {
									VALUE tmp = rb_check_array_type(bl_val);
									bl_val = (NIL_P(tmp) ? rb_ary_new3(1, bl_val) : tmp);
								}
							}
							if (RARRAY(bl_val)->len == 0) return rb_funcall3(proc, #{sym(:call)}, 0, 0);
							else {
								int argc = RARRAY(bl_val)->len;
								VALUE *argv = ALLOCA_N(VALUE, argc);
								MEMCPY(argv, RARRAY(bl_val)->ptr, VALUE, argc);
								return rb_funcall3(proc, #{sym(:call)}, argc, argv);
							}
						}
						EOC
					assign_res(handle_iter(hash[:iter], "block_pass_helper_block", "proc"))
				}
				"res"
			}
		end

		def comp_for(hash)
			# transform to equivalent iter node
			hash = hash.dup
			hash[:iter] = [:call, {:args=>false, :mid=>:each, :recv=>hash[:iter]}]
			comp_iter(hash)
		end

		def comp_iter(hash)
			bl_fun, need_clos = CFunction::Block.compile(self, make_block(hash[:body]), hash[:var])
			handle_iter(hash[:iter], bl_fun, need_clos ? get_closure_ary_var : "Qnil")
		end

		def comp_break(hash)
			if (lbl = in_while?(:break))
				l "while_res = #{comp(hash[:stts])};"
				l "goto #{lbl};"
			elsif in_block?
				raise Ruby2CExtError::NotSupported, "break with a value is not supported in a block" if hash[:stts]
				l "rb_iter_break();"
			else
				raise Ruby2CExtError::NotSupported, "break is not supported here"
			end
			"Qnil"
		end
		def comp_next(hash)
			if (lbl = in_while?(:next))
				# hash[:stts] is silently ignored (as ruby does)
				l "goto #{lbl};"
			elsif in_block?
				l "return #{comp(hash[:stts])};"
			else
				raise Ruby2CExtError::NotSupported, "next is not supported here"
			end
			"Qnil"
		end
		def comp_redo(hash)
			if (lbl = (in_while?(:redo) || in_block?(:redo)))
				l "goto #{lbl};"
			else
				raise Ruby2CExtError::NotSupported, "redo is not supported here"
			end
			"Qnil"
		end
		def comp_retry(hash)
			l "rb_jump_tag(0x4 /* TAG_RETRY */);"
			"Qnil"
		end
		def comp_return(hash)
			if return_allowed?
				l "return #{comp(hash[:stts])};"
			else
				raise Ruby2CExtError::NotSupported, "return is not supported here"
			end
			"Qnil"
		end

		def comp_splat(hash)
			assign_res(comp(hash[:head]))
			"(NIL_P(res) ? rb_ary_new3(1, Qnil) : rb_Array(res))"
		end
		def comp_to_ary(hash)
			"rb_ary_to_ary(#{comp(hash[:head])})"
		end
		def comp_svalue(hash)
			assign_res(comp(hash[:head]))
			"(RARRAY(res)->len == 1 ? RARRAY(res)->ptr[0] : (RARRAY(res)->len == 0 ? Qnil : res))"
		end

		def comp_yield(hash)
			val = (hash[:head] ? comp(hash[:head]) : "Qundef")
			"rb_yield#{hash[:state] == 0 ? "" : "_splat"}(#{val})"
		end

		def comp_rescue(hash)
			ensure_node_type(resb = hash[:resq], :resbody)
			# compile the real body
			body = un("rescue_body")
			CFunction::Wrap.compile(self, body) { |cf|
				cf.instance_eval {
					l "#{get_wrap_ptr}->state = 1;"
					comp(hash[:head])
				}
			}
			res_bodies = un("rescue_resbodies")
			# now all the resbodies in one c function
			CFunction::Wrap.compile(self, res_bodies) { |cf|
				cf.instance_eval {
					cnt = 0
					while resb
						cnt += 1
						args = resb.last[:args]
						unless args
							l "if (rb_obj_is_kind_of(ruby_errinfo, rb_eStandardError)) {"
						else
							add_helper <<-EOC
								static void rescue_mod_check(VALUE mod) {
									if (!rb_obj_is_kind_of(mod, rb_cModule)) {
										rb_raise(rb_eTypeError, "class or module required for rescue clause");
									}
								}
							EOC
							if args.first == :array # TODO
								# ruby doesn't handle this case specially, but it might be faster this way
								# (on the other side: we might miss exceptions (for not evaluated entries of the array))
								args.last.each { |ex|
									assign_res(comp(ex))
									l "rescue_mod_check(res);"
									l "if (!RTEST(rb_funcall(res, #{sym(:===)}, 1, ruby_errinfo))) {"
								}
								# non did match
								assign_res("Qfalse")
								# close all ifs
								args.last.size.times { l "}" }
							else
								c_scope {
									l "int i = 0;"
									# TODO: ruby has BEGIN_CALLARGS protection here, is this really necessary???
									build_args(args)
									assign_res("Qfalse")
									c_for("; i < argc; ++i") {
										l "rescue_mod_check(argv[i]);"
										c_if("RTEST(rb_funcall(argv[i], #{sym(:===)}, 1, ruby_errinfo))") {
											assign_res("Qtrue");
											l "break;"
										}
									}
								}
							end
							l "if (res) {"
						end
						l "#{get_wrap_ptr}->state = 0;"
						assign_res(comp(resb.last[:body]))
						l "}"
						l "else {"
						resb = resb.last[:head]
					end
					# we are in the last else, if the exception wasn't handled, then reraise
					l "rb_jump_tag(0x6 /* TAG_RAISE */);"
					# close all elses
					cnt.times { l "}" }
				}
				"res"
			}
			# now call rb_rescue2 with the two bodies (and handle else if necessary)
			c_scope_res {
				l "long save_state = #{get_wrap_ptr}->state;"
				wp = "(VALUE)#{get_wrap_ptr}"
				assign_res("rb_rescue2(#{body}, #{wp}, #{res_bodies}, #{wp}, rb_eException, (VALUE)0)")
				if els = hash[:else]
					c_if("#{get_wrap_ptr}->state == 1") {
						assign_res(comp(els))
					}
				end
				l "#{get_wrap_ptr}->state = save_state;"
				"res"
			}
		end

		def comp_ensure(hash)
			b = un("ensure_body")
			e = un("ensure_ensure")
			CFunction::Wrap.compile(self, b) { |cf| cf.comp(hash[:head]) }
			CFunction::Wrap.compile(self, e) { |cf| cf.comp(hash[:ensr]) }
			"rb_ensure(#{b}, (VALUE)#{get_wrap_ptr}, #{e}, (VALUE)#{get_wrap_ptr})"
		end

		def comp_and(hash)
			assign_res(comp(hash[:first]))
			c_if("RTEST(res)") {
				assign_res(comp(hash[:second]))
			}
			"res"
		end
		def comp_or(hash)
			assign_res(comp(hash[:first]))
			c_if("!RTEST(res)") {
				assign_res(comp(hash[:second]))
			}
			"res"
		end
		def comp_not(hash)
			"(RTEST(#{comp(hash[:body])}) ? Qfalse : Qtrue)"
		end

		def handle_dot(hash, dot3)
			c_scope_res {
				l "VALUE beg;"
				l "beg = #{comp(hash[:beg])};"
				"rb_range_new(beg, #{comp(hash[:end])}, Q#{dot3})"
			}
		end
		def comp_dot2(hash)
			handle_dot(hash, false)
		end
		def comp_dot3(hash)
			handle_dot(hash, true)
		end

		def handle_flip(hash, flip3)
			flip_var = scope.get_lvar_idx(hash[:cnt] - 2)
			c_if("RTEST(#{flip_var})") {
				l "if (RTEST(#{comp(hash[:end])})) #{flip_var} = Qfalse;"
				assign_res("Qtrue")
			}
			c_else {
				if flip3
					assign_res("(RTEST(#{comp(hash[:beg])}) ? Qtrue : Qfalse)")
					l "#{flip_var} = res;"
				else
					assign_res(c_scope_res {
						l "VALUE beg_res;"
						l "beg_res = (RTEST(#{comp(hash[:beg])}) ? Qtrue : Qfalse);"
						c_if("beg_res") {
							l "#{flip_var} = (RTEST(#{comp(hash[:end])}) ? Qfalse : Qtrue);"
						}
						"beg_res"
					})
				end
			}
			"res"
		end
		def comp_flip2(hash)
			handle_flip(hash, false)
		end
		def comp_flip3(hash)
			handle_flip(hash, true)
		end

		def comp_op_asgn1(hash)
			ensure_node_type(hash[:args], :array)
			c_scope_res {
				l "VALUE recv, val;"
				l "recv = #{comp(hash[:recv])};"
				c_scope_res {
					build_args([:array, hash[:args].last[1..-1]])
					l "val = rb_funcall2(recv, #{sym(:[])}, argc-1, argv);"
					mid = hash[:mid]
					if Symbol === mid
						l "val = rb_funcall(val, #{sym(mid)}, 1, #{comp(hash[:args].last.first)});"
						l "{"
					else
						# mid == 0 is OR, mid == 1 is AND
						l "if (#{mid == 0 ? "!" : ""}RTEST(val)) {"
						l "val = #{comp(hash[:args].last.first)};"
					end
					l "argv[argc-1] = val;"
					l "rb_funcall2(recv, #{sym(:[]=)}, argc, argv);"
					l "}"
					"val"
				}
			}
		end
		def comp_op_asgn2(hash)
			ensure_node_type(ids = hash[:next], :op_asgn2)
			ids = ids.last
			c_scope_res {
				l "VALUE recv, val;"
				l "recv = #{comp(hash[:recv])};"
				l "val = rb_funcall(recv, #{sym(ids[:vid])}, 0);"
				mid = ids[:mid]
				if Symbol === mid
					l "val = rb_funcall(val, #{sym(mid)}, 1, #{comp(hash[:value])});"
					l "{"
				else
					# mid == 0 is OR, mid == 1 is AND
					l "if (#{mid == 0 ? "!" : ""}RTEST(val)) {"
					l "val = #{comp(hash[:value])};"
				end
				l "rb_funcall2(recv, #{sym(ids[:aid])}, 1, &val);"
				l "}"
				"val"
			}
		end

		def comp_op_asgn_and(hash)
			assign_res(comp(hash[:head]))
			c_if("RTEST(res)") {
				assign_res(comp(hash[:value]))
			}
			"res"
		end
		def comp_op_asgn_or(hash)
			if Symbol === (aid = hash[:aid])
				def_test =
					case aid.to_s
					when /\A@@/
						"rb_cvar_defined(#{get_cvar_cbase}, #{sym(aid)})"
					when /\A@/
						"rb_ivar_defined(#{get_self}, #{sym(aid)})"
					when /\A\$/
						"rb_gvar_defined(rb_global_entry(#{sym(aid)}))"
					else
						raise Ruby2CExtError::Bug, "unexpected aid for op_asgn_or: #{aid.inspect}"
					end
				c_if(def_test) {
					assign_res(comp(hash[:head]))
				}
				c_else {
					assign_res("Qnil")
				}
			else
				assign_res(comp(hash[:head]))
			end
			c_if("!RTEST(res)") {
				assign_res(comp(hash[:value]))
			}
			"res"
		end

		def comp_masgn(hash)
			c_scope_res {
				l "VALUE ma_val;"
				l "long ma_len;"
				l "ma_val = #{comp(hash[:value])};"
				l "ma_len = RARRAY(ma_val)->len;"
				head_len = 0
				if (head = hash[:head])
					ensure_node_type(head, :array)
					head_len = head.last.size
					head.last.each_with_index { |asgn, i|
						handle_assign(asgn, "(#{i} < ma_len ? RARRAY(ma_val)->ptr[#{i}] : Qnil)", false)
					}
				end
				if ((args = hash[:args]) && (args != -1))
					handle_assign(args, "(#{head_len} < ma_len ? rb_ary_new4(ma_len-#{head_len}, " +
						"RARRAY(ma_val)->ptr+#{head_len}) : rb_ary_new2(0))", false)
				end
				"ma_val"
			}
		end

		def handle_assign(asgn_node, val, undef_check = true)
			c_scope {
				if undef_check
					l "VALUE as_val;"
					l "as_val = #{comp(val)};"
					l "if (as_val == Qundef) as_val = Qnil;"
					val = "as_val"
				end
				dup_node = [asgn_node.first, asgn_node.last.dup]
				case asgn_node.first
				when :lasgn, :dasgn, :dasgn_curr, :iasgn, :gasgn, :cdecl, :cvdecl, :cvasgn
					if dup_node.last[:value]
						raise Ruby2CExtError, "unexpected value in #{asgn_node.first} node in handle_assign"
					end
					dup_node.last[:value] = val
					l "#{comp(dup_node)};"
				when :masgn
					c_scope {
						l "VALUE as_ma_val;"
						l "VALUE tmp;" if (ma_head = asgn_node.last[:head])
						l "as_ma_val = #{comp(val)};"
						# adapted from svalue_to_mrhs()
						if ma_head
							l "tmp = rb_check_array_type(as_ma_val);"
							l "as_ma_val = (NIL_P(tmp) ? rb_ary_new3(1, as_ma_val) : tmp);"
						else
							l "as_ma_val = rb_ary_new3(1, as_ma_val);"
						end
						dup_node.last[:value] = "as_ma_val"
						l "#{comp(dup_node)};"
					}
				when :attrasgn # TODO: can :call also appear here ???
					c_scope {
						l "VALUE as_aa_val;"
						l "as_aa_val = #{comp(val)};"
						if asgn_node.last[:args]
							dup_node.last[:args] = [:argspush, {:body => "as_aa_val", :head => asgn_node.last[:args]}]
						else
							dup_node.last[:args] = [:array, ["as_aa_val"]]
						end
						l "#{comp(dup_node)};"
					}
				else
					raise Ruby2CExtError::Bug, "unexpected assign node type: #{asgn_node.first}"
				end
			}
		end

		def comp_lasgn(hash)
			"(#{scope.get_lvar(hash[:vid])} = #{comp(hash[:value])})"
		end
		def comp_dasgn(hash)
			"(#{scope.get_dvar(hash[:vid])} = #{comp(hash[:value])})"
		end
		def comp_dasgn_curr(hash)
			"(#{scope.get_dvar_curr(hash[:vid])} = #{comp(hash[:value])})"
		end
		def comp_gasgn(hash)
			assign_res(comp(hash[:value]))
			# TODO: store global entries somewhere ??? (would save one st_lookup)
			l "rb_gvar_set(rb_global_entry(#{sym(hash[:vid])}), res);"
			"res"
		end
		def comp_iasgn(hash)
			assign_res(comp(hash[:value]))
			l "rb_ivar_set(#{get_self}, #{sym(hash[:vid])}, res);"
			"res"
		end

		def helper_class_module_check
			add_helper <<-EOC
				static void class_module_check(VALUE klass) {
					switch (TYPE(klass)) {
						case T_CLASS:
						case T_MODULE:
							break;
						default:
							rb_raise(rb_eTypeError, "%s is not a class/module", RSTRING(rb_obj_as_string(klass))->ptr);
					}
				}
			EOC
		end

		def make_class_prefix(node)
			ensure_node_type(node, [:colon2, :colon3])
			if node.first == :colon2
				hash = node.last
				if hash[:head]
					assign_res(comp(hash[:head]))
					l "class_module_check(res);"
					"res"
				else
					get_cbase
				end
			else
				"rb_cObject"
			end
		end

		def comp_cdecl(hash)
			c_scope_res {
				l "VALUE val;"
				l "val = #{comp(hash[:value])};"
				if Symbol === hash[:vid]
					l "rb_const_set(#{get_cbase}, #{sym(hash[:vid])}, val);"
				else
					l "rb_const_set(#{make_class_prefix(hash[:else])}, #{sym(hash[:else].last[:mid])}, val);"
				end
				"val"
			}
		end
		def comp_cvasgn(hash, decl = false)
			assign_res(comp(hash[:value]))
			l "rb_cvar_set(#{get_cvar_cbase}, #{sym(hash[:vid])}, res, Q#{decl});"
			"res"
		end
		def comp_cvdecl(hash)
			comp_cvasgn(hash, true)
		end

		def comp_lvar(hash)
			"#{scope.get_lvar(hash[:vid])}"
		end
		def comp_dvar(hash)
			scope.get_dvar(hash[:vid])
		end
		def comp_gvar(hash)
			# store global entries somewhere ??? (would save one st_lookup)
			"rb_gvar_get(rb_global_entry(#{sym(hash[:vid])}))"
		end
		def comp_ivar(hash)
			"rb_ivar_get(#{get_self}, #{sym(hash[:vid])})"
		end
		def comp_const(hash)
			add_helper <<-EOC
				static VALUE const_get(ID id, NODE *cref) {
					NODE *cbase = cref;
					VALUE result;
					while (cbase && cbase->nd_next) {
						VALUE klass = cbase->nd_clss;
						while (RCLASS(klass)->iv_tbl && st_lookup(RCLASS(klass)->iv_tbl, id, &result)) {
							if (result == Qundef) {
								if (!RTEST(rb_autoload_load(klass, id))) break;
								continue;
							}
							return result;
						}
						cbase = cbase->nd_next;
					}
					return rb_const_get(cref->nd_clss, id);
				}
			EOC
			"const_get(#{sym(hash[:vid])}, #{get_cref})"
		end
		def comp_cvar(hash)
			"rb_cvar_get(#{get_cvar_cbase}, #{sym(hash[:vid])})"
		end
		def comp_colon2(hash)
			mid = hash[:mid]
			if mid.to_s[0,1].downcase != mid.to_s[0,1] # then it is a constant
				helper_class_module_check
				c_scope_res {
					l "VALUE tmp_class;"
					l "tmp_class = #{comp(hash[:head])};"
					l "class_module_check(tmp_class);"
					"rb_const_get_from(tmp_class, #{sym(mid)})"
				}
			else
				"rb_funcall(#{comp(hash[:head])}, #{sym(mid)}, 0, 0)"
			end
		end
		def comp_colon3(hash)
			"rb_const_get_from(rb_cObject, #{sym(hash[:mid])})"
		end

		def comp_nth_ref(hash)
			"rb_reg_nth_match(#{hash[:nth]}, rb_backref_get())"
		end
		def comp_back_ref(hash)
			case hash[:nth]
			when ?&
				"rb_reg_last_match(rb_backref_get())"
			when ?`
				"rb_reg_match_pre(rb_backref_get())"
			when ?'
				"rb_reg_match_post(rb_backref_get())"
			when ?+
				"rb_reg_match_last(rb_backref_get())"
			else
				raise Ruby2CExtError, "unexpected back-ref type: '#{hash[:nth].chr}'"
			end
		end

		def comp_array(arr)
			c_scope_res {
				l "VALUE ary = rb_ary_new2(#{arr.size});"
				arr.each_with_index { |n, i|
					l "RARRAY(ary)->ptr[#{i}] = #{comp(n)};"
					l "RARRAY(ary)->len = #{i+1};"
				}
				"ary"
			}
		end
		def comp_zarray(hash)
			"rb_ary_new()"
		end

		def comp_hash(hash)
			if (arr = hash[:head])
				ensure_node_type(arr, :array)
				arr = arr.last
				raise Ruby2CExtError, "odd number list for hash" unless arr.size % 2 == 0
				c_scope_res {
					l "VALUE key, hash = rb_hash_new();"
					arr.each_with_index { |n, i|
						if i % 2 == 0
							l "key = #{comp(n)};"
						else
							l "rb_hash_aset(hash, key, #{comp(n)});"
						end
					}
					"hash"
				}
			else
				"rb_hash_new()"
			end
		end

		def comp_str(hash)
			lit = global("rb_str_new(#{hash[:lit].to_c_strlit}, #{hash[:lit].size})")
			"rb_str_new3(#{lit})"
		end
		def comp_evstr(hash)
			"rb_obj_as_string(#{comp(hash[:body])})"
		end
		def handle_dyn_str(hash)
			ensure_node_type(hash[:next], :array)
			c_scope_res {
				l "VALUE str, str2;"
				l "str = #{comp_str(hash)};"
				hash[:next].last.each { |node|
					l "str2 = #{comp(node)};"
					l "rb_str_append(str, str2);"
					l "OBJ_INFECT(str, str2);"
				}
				"str"
			}
		end
		def comp_dstr(hash)
			handle_dyn_str(hash)
		end
		def comp_dxstr(hash)
			handle_dyn_str(hash)
			"rb_funcall(#{get_self}, '`', 1, res)"
		end
		def comp_dsym(hash)
			handle_dyn_str(hash)
			"rb_str_intern(res)"
		end
		def comp_dregx(hash)
			handle_dyn_str(hash)
			"rb_reg_new(RSTRING(res)->ptr, RSTRING(res)->len, #{hash[:cflag]})"
		end
		def comp_dregx_once(hash)
			c_scope_res {
				l "static int done = 0;"
				l "static VALUE regx;"
				c_if("!done") {
					assign_res(comp_dregx(hash))
					# other thread might have been faster
					c_if("!done") {
						l "regx = res;"
						l "done = 1;"
					}
				}
				"regx"
			}
		end
		def comp_xstr(hash)
			"rb_funcall(#{get_self}, '`', 1, #{comp_str(hash)})"
		end

		def comp_defn(hash)
			add_helper <<-EOC
				static void class_nil_check(VALUE klass) {
				    if (NIL_P(klass)) rb_raise(rb_eTypeError, "no class/module to add method");
				}
			EOC
			l "class_nil_check(#{get_class});" # can happen in instance_eval for Fixnum/Symbol
			CFunction::Method.compile(self, hash[:defn], scope.vmode_def_fun, get_class, hash[:mid])
			"Qnil"
		end

		def comp_defs(hash)
			add_helper <<-EOC
				static void defs_allowed(VALUE recv, ID mid) {
					if (ruby_safe_level >= 4 && !OBJ_TAINTED(recv))
						rb_raise(rb_eSecurityError, "Insecure: can't define singleton method");
					if (OBJ_FROZEN(recv)) rb_error_frozen("object");
					if (ruby_safe_level >= 4) {
						NODE *body = 0;
						VALUE klass = rb_singleton_class(recv);
						if (st_lookup(RCLASS(klass)->m_tbl, mid, (st_data_t *)&body))
						rb_raise(rb_eSecurityError, "redefining method prohibited");
					}
				}
			EOC
			c_scope_res {
				l "VALUE recv;"
				l "recv = #{comp(hash[:recv])};"
				l "defs_allowed(recv, #{sym(hash[:mid])});"
				CFunction::Method.compile(self, hash[:defn], "rb_define_singleton_method", "recv", hash[:mid])
				"Qnil"
			}
		end

		def comp_undef(hash)
			l "rb_undef(#{get_class}, #{sym(hash[:mid])});"
			"Qnil"
		end

		def comp_alias(hash)
			l "rb_alias(#{get_class}, #{sym(hash[:new])}, #{sym(hash[:old])});"
			"Qnil"
		end
		def comp_valias(hash)
			l "rb_alias_variable(#{sym(hash[:new])}, #{sym(hash[:old])});"
			"Qnil"
		end

		def comp_class(hash)
			add_helper <<-EOC
				static VALUE class_prep(VALUE prefix, VALUE super, ID cname) {
					VALUE klass;
					if (rb_const_defined_at(prefix, cname)) {
						klass = rb_const_get_at(prefix, cname);
						if (TYPE(klass) != T_CLASS) rb_raise(rb_eTypeError, "%s is not a class", rb_id2name(cname));
						if (super) {
							VALUE tmp = rb_class_real(RCLASS(klass)->super);
							if (tmp != super) rb_raise(rb_eTypeError, "superclass mismatch for class %s", rb_id2name(cname));
						}
						if (ruby_safe_level >= 4) rb_raise(rb_eSecurityError, "extending class prohibited");
					}
					else {
						if (!super) super = rb_cObject;
						klass = rb_define_class_id(cname, super);
						rb_set_class_path(klass, prefix, rb_id2name(cname));
						rb_const_set(prefix, cname, klass);
						rb_class_inherited(super, klass);
					}
					return klass;
				}
			EOC
			sup = hash[:super]
			c_scope_res {
				l "VALUE prefix, tmp_class;"
				l "VALUE super;" if sup
				l "prefix = #{make_class_prefix(hash[:cpath])};"
				l "super = #{comp(sup)};" if sup
				l "tmp_class = class_prep(prefix, #{sup ? "super" : "0"}, #{sym(hash[:cpath].last[:mid])});"
				CFunction::ClassModuleScope.compile(self, hash[:body], "tmp_class")
			}
		end

		def comp_module(hash)
			add_helper <<-EOC
				static VALUE module_prep(VALUE prefix, ID cname) {
					VALUE module;
					if (rb_const_defined_at(prefix, cname)) {
						module = rb_const_get_at(prefix, cname);
						if (TYPE(module) != T_MODULE) rb_raise(rb_eTypeError, "%s is not a module", rb_id2name(cname));
						if (ruby_safe_level >= 4) rb_raise(rb_eSecurityError, "extending module prohibited");
					}
					else {
						module = rb_define_module_id(cname);
						rb_set_class_path(module, prefix, rb_id2name(cname));
						rb_const_set(prefix, cname, module);
					}
					return module;
				}
			EOC
			c_scope_res {
				l "VALUE prefix, tmp_module;"
				l "prefix = #{make_class_prefix(hash[:cpath])};"
				l "tmp_module = module_prep(prefix, #{sym(hash[:cpath].last[:mid])});"
				CFunction::ClassModuleScope.compile(self, hash[:body], "tmp_module")
			}
		end

		def comp_sclass(hash)
			add_helper <<-EOC
				static void sclass_check(VALUE obj) {
					if (FIXNUM_P(obj) || SYMBOL_P(obj)) rb_raise(rb_eTypeError, "no virtual class for %s", rb_obj_classname(obj));
					if (ruby_safe_level >= 4 && !OBJ_TAINTED(obj)) rb_raise(rb_eSecurityError, "Insecure: can't extend object");
				}
			EOC
			c_scope_res {
				l "VALUE tmp_sclass;"
				l "tmp_sclass = #{comp(hash[:recv])};"
				l "sclass_check(tmp_sclass);"
				l "tmp_sclass = rb_singleton_class(tmp_sclass);"
				CFunction::ClassModuleScope.compile(self, hash[:body], "tmp_sclass")
			}
		end

		def comp_defined(hash)
			head = hash[:head]
			hhash = head.last
			res =
			case head.first
			when :match2, :match3
				'"method"'
			when :yield
				'(rb_block_given_p() ? "yield" : 0)'
			when :self, :nil, :true, :false
				head.first.to_s.to_c_strlit
			when :op_asgn1, :op_asgn2, :masgn, :lasgn, :dasgn, :dasgn_curr,
				:gasgn, :iasgn, :cdecl, :cvdecl, :cvasgn  # :attrset can never be parsed
				'"assignment"'
			when :lvar
				'"local-variable"'
			when :dvar
				'"local-variable(in-block)"'
			when :gvar
				"(rb_gvar_defined(rb_global_entry(#{sym(hhash[:vid])})) ? \"global-variable\" : 0)"
			when :ivar
				"(rb_ivar_defined(#{get_self}, #{sym(hhash[:vid])}) ? \"instance-variable\" : 0)"
			when :const
				add_helper <<-EOC
					static VALUE const_defined(ID id, NODE *cref) {
						NODE *cbase = cref;
						VALUE result;
						while (cbase && cbase->nd_next) {
							VALUE klass = cbase->nd_clss;
							if (RCLASS(klass)->iv_tbl && st_lookup(RCLASS(klass)->iv_tbl, id, &result)) {
								if (result == Qundef && NIL_P(rb_autoload_p(klass, id))) return Qfalse;
								return Qtrue;
							}
							cbase = cbase->nd_next;
						}
						return rb_const_defined(cref->nd_clss, id);
					}
				EOC
				"(const_defined(#{sym(hhash[:vid])}, #{get_cref}) ? \"constant\" : 0)"
			when :cvar
				"(rb_cvar_defined(#{get_cvar_cbase}, #{sym(hhash[:vid])}) ? \"class variable\" : 0)"
			when :colon3
				"(rb_const_defined_from(rb_cObject, #{sym(hhash[:mid])}) ? \"constant\" : 0)"
			when :nth_ref
				"(RTEST(rb_reg_nth_defined(#{hhash[:nth]}, rb_backref_get())) ? \"$#{hhash[:nth]}\" : 0)"
			when :back_ref
				"(RTEST(rb_reg_nth_defined(0, rb_backref_get())) ? \"$#{hhash[:nth].chr}\" : 0)"
			else
				raise Ruby2CExtError::NotSupported, "defined? with node type #{head.first} is not supported"
			end
			if res[0,1] == '"' # just a string
				"rb_str_new2(#{res})"
			else
				c_scope_res {
					l "char * def_desc;"
					l "def_desc = #{res};"
					"(def_desc ? rb_str_new2(def_desc) : Qnil)"
				}
			end
		end
	end

	module Scopes
		class Scope
			VMODES = [:public, :private, :protected, :module_function]

			def initialize(tbl, top_level = false)
				@vmode = top_level ? :private : :public
				@tbl = tbl || []
				@closure_tbl = nil # must be set by user
			end
			attr_reader :tbl
			attr_accessor :vmode, :need_heap, :closure_tbl

			def get_lvar_idx(i)
				raise Ruby2CExtError, "wrong lvar index: #{i}" unless i >= 0 && i < tbl.size
				"lvar[#{i}]"
			end
			def get_lvar(sym)
				raise Ruby2CExtError, "unknown lvar: #{sym}" unless (i = tbl.index(sym))
				get_lvar_idx(i)
			end
			def get_lvar_ary
				self.need_heap = true
				"lvar_ary"
			end
			def get_dvar(arg)
				raise Ruby2CExtError, "dvars not available here"
			end
			alias get_dvar_curr get_dvar
			alias get_dvar_ary get_dvar

			def vmode_def_fun
				case vmode
				when :protected, :private
					"rb_define_#{vmode}_method"
				when :public
					"rb_define_method"
				when :module_function
					"rb_define_module_function"
				else
					raise Ruby2CExtError::Bug, "unknown vmode: #{@vmode}"
				end
			end

			def new_dyna_scope
				DynaScope.new(self, nil, 1)
			end

			def var_ptr_for_wrap
				tbl.empty? ? nil : "lvar"
			end

			def init_c_code
				return nil if tbl.empty?
				res = []
				if need_heap
					res << "VALUE *lvar;"
					res << "VALUE lvar_ary = rb_ary_new2(#{tbl.size});"
					res << "rb_mem_clear(RARRAY(lvar_ary)->ptr, #{tbl.size});"
					res << "RARRAY(lvar_ary)->len = #{tbl.size};"
					res << "lvar = RARRAY(lvar_ary)->ptr;"
				else
					res << "VALUE lvar[#{tbl.size}];\nrb_mem_clear(lvar, #{tbl.size});"
				end
				res.join("\n")
			end
		end

		class DynaScope
			def initialize(base_scope, outer_scope, depth)
				@base_scope = base_scope
				@outer_scope = outer_scope
				@depth = depth
				@closure_tbl = nil # must be set by user
				@tbl = []
			end
			attr_reader :tbl, :base_scope, :outer_scope, :depth
			attr_accessor :need_heap, :need_closure, :closure_tbl

			def method_missing(meth, *arg)
				super unless meth.to_s =~ /vmode/
				base_scope.send(meth, *arg)
			end

			def outer_closure_tbl
				(outer_scope || base_scope).closure_tbl
			end
			def add_closure_need(need)
				# need is in [1, self.depth) or :lvar
				unless need == :lvar || (1...depth) === need
					raise Ruby2CExtError::Bug, "unexpected needed closure vars: #{need}"
				end
				self.need_closure = true
				outer_closure_tbl << need unless outer_closure_tbl.include? need
			end
			def get_closure_ary(clos_idx)
				add_closure_need(clos_idx)
				"closure[#{outer_closure_tbl.index(clos_idx)}]"
			end
			def get_closure_var(clos_idx, var_idx)
				"RARRAY(#{get_closure_ary(clos_idx)})->ptr[#{var_idx}]"
			end

			def get_lvar_idx(i)
				raise Ruby2CExtError, "wrong lvar index: #{i}" unless i >= 0 && i < base_scope.tbl.size
				get_closure_var(:lvar, i)
			end
			def get_lvar(sym)
				raise Ruby2CExtError, "unknown lvar: #{sym}" unless (i = base_scope.tbl.index(sym))
				get_lvar_idx(i)
			end
			def get_lvar_ary
				get_closure_ary(:lvar)
			end

			def get_dvar(sym)
				if tbl.include?(sym)
					return get_dvar_curr(sym)
				end
				cur = self
				while (cur = cur.outer_scope)
					if (i = cur.tbl.index(sym))
						return get_closure_var(cur.depth, i)
					end
				end
				raise Ruby2CExtError, "unexpected dvar: #{sym}"
			end
			def get_dvar_curr(sym)
				unless (i = tbl.index(sym))
					i = tbl.size
					tbl << sym
				end
				"dvar[#{i}]"
			end
			def get_dvar_ary(idx)
				if idx == depth
					self.need_heap = true
					"dvar_ary"
				else
					get_closure_ary(idx)
				end
			end

			def new_dyna_scope
				DynaScope.new(base_scope, self, depth + 1)
			end

			def var_ptr_for_wrap
				tbl.empty? ? nil : "dvar"
			end

			def init_c_code
				return nil if tbl.empty?
				res = []
				if need_heap
					res << "VALUE *dvar;"
					res << "VALUE dvar_ary = rb_ary_new2(#{tbl.size});"
					res << "rb_mem_clear(RARRAY(dvar_ary)->ptr, #{tbl.size});"
					res << "RARRAY(dvar_ary)->len = #{tbl.size};"
					res << "dvar = RARRAY(dvar_ary)->ptr;"
				else
					res << "VALUE dvar[#{tbl.size}];\nrb_mem_clear(dvar, #{tbl.size});"
				end
				res.join("\n")
			end
		end

		class WrappedScope
			def initialize(base_scope)
				@base_scope = base_scope
			end

			# redirect to @base_scope
			def method_missing(meth, *arg)
				res = @base_scope.send(meth, *arg)
				if String === res
					case res
					when "lvar_ary"
						raise Ruby2CExtError::Bug, "unexpected need for lvar_ary in WrappedScope"
					when "dvar_ary"
						raise Ruby2CExtError::Bug, "unexpected need for dvar_ary in WrappedScope"
					when /\bclosure\b/
						res.gsub!(/\bclosure\b/, "(wrap_ptr->closure)")
					when /\b[ld]var\b/
						res.gsub!(/\b[ld]var\b/, "(wrap_ptr->var)")
					when /^rb_define/
						# nothing
					else
						raise Ruby2CExtError::Bug, "unexpected string returned from #{@base_scope.class}##{meth}: #{res}"
					end
				end
				res
			end

			def var_ptr_for_wrap
				raise Ruby2CExtError::Bug, "unexpected call of var_ptr_for_wrap"
			end

			def init_c_code
				raise Ruby2CExtError::Bug, "unexpected call of init_c_code"
			end
		end
	end

	module CFunction
		# contains all different Types of C functions that are compiled from ruby nodes

		class Base
			include CommonNodeComp
			extend Tools::EnsureNodeTypeMixin
			attr_reader :scope, :compiler, :closure_tbl
			attr_accessor :need_res, :need_self, :need_cref, :need_class, :need_wrap
			def initialize(compiler, scope)
				@compiler = compiler
				@scope = scope
				@closure_tbl = []
				@scope.closure_tbl = @closure_tbl
				@lines = []
				@while_stack = []
			end

			# some redirects to compiler
			def un(str); compiler.un(str); end
			def sym(sym); compiler.sym(sym); end
			def global(str); compiler.global(str); end
			def add_helper(str); compiler.add_helper(str); end

			def get_lines
				@lines.join("\n")
			end
			def l(line) # add_line
				# ignore lines with only whitespace or only alnum chars (variable name)
				unless line =~ /\A\s*\z/ || (line =~ /\A(\w*);?\z/ && !(%w[break continue].include? $1))
					@lines << line
				end
			end

			def push_while(redo_lbl, next_lbl, break_lbl)
				@while_stack << [redo_lbl, next_lbl, break_lbl]
			end
			def pop_while
				raise Ruby2CExtError::Bug, "popped from empty while stack" if @while_stack.empty?
				@while_stack.pop
			end
			def in_while?(lbl_type)
				return false if @while_stack.empty?
				case lbl_type
				when :redo
					@while_stack.last[0]
				when :next
					@while_stack.last[1]
				when :break
					@while_stack.last[2]
				else
					false
				end
			end

			def in_block?(lbl_type = nil)
				false
			end

			def return_allowed?
				false # subclass
			end

			def need_closure_ptr
				false # only needed in Block
			end

			def assign_res(str)
				self.need_res = true
				unless str.strip == "res"
					l "res = #{str};"
				end
			end
			def get_self
				self.need_self = true
				"self"
			end
			def get_cref
				self.need_cref = true
				get_cref_impl # subclass
			end
			def get_class
				self.need_class = true
				"s_class"
			end
			def get_cbase
				"(#{get_cref}->nd_clss)"
			end
			def get_cvar_cbase
				# there is always at least one real class in the cref chain
				add_helper <<-EOC
					static VALUE cvar_cbase(NODE *cref) {
						while (FL_TEST(cref->nd_clss, FL_SINGLETON)) { cref = cref->nd_next; }
						return cref->nd_clss;
					}
				EOC
				"cvar_cbase(#{get_cref})"
			end

			def get_closure_ary_var
				"my_closure_ary"
			end

			def get_wrap_ptr
				"(&the_wrap)"
			end

			def add_closure_need(sym)
				closure_tbl << sym unless closure_tbl.include? sym
			end

			def closure_buid_c_code
				if closure_tbl.empty?
					nil
				else
					res = ["#{get_closure_ary_var} = rb_ary_new2(#{closure_tbl.size});"]
					closure_tbl.each_with_index { |entry, idx|
						case entry
						when Integer
							res << "RARRAY(#{get_closure_ary_var})->ptr[#{idx}] = #{scope.get_dvar_ary(entry)};"
						when :lvar
							res << "RARRAY(#{get_closure_ary_var})->ptr[#{idx}] = #{scope.get_lvar_ary};"
						when :self
							res << "RARRAY(#{get_closure_ary_var})->ptr[#{idx}] = #{get_self};"
						when :class
							res << "RARRAY(#{get_closure_ary_var})->ptr[#{idx}] = #{get_class};"
						when :cref
							add_helper <<-EOC
								static void cref_data_mark(NODE *n) {
									rb_gc_mark((VALUE)n);
								}
							EOC
							res << "RARRAY(#{get_closure_ary_var})->ptr[#{idx}] = " +
								"Data_Wrap_Struct(rb_cObject, cref_data_mark, 0, #{get_cref});"
						else
							raise Ruby2CExtError::Bug, "unexpected closure_tbl entry: #{entry.inspect}"
						end
					}
					res << "RARRAY(#{get_closure_ary_var})->len = #{closure_tbl.size};"
					res.join("\n")
				end
			end

			def wrap_buid_c_code
				add_helper <<-EOC
					struct wrap {
						VALUE self;
						VALUE s_class;
						NODE *cref;
						VALUE my_closure_ary;
						VALUE *closure;
						VALUE *var;
						long state;
					};
				EOC
				res = []
				res << "the_wrap.self = #{get_self};" if need_self
				res << "the_wrap.s_class = #{get_class};" if need_class
				res << "the_wrap.cref = #{get_cref};" if need_cref
				res << "the_wrap.my_closure_ary = #{get_closure_ary_var};" unless closure_tbl.empty?
				res << "the_wrap.closure = closure;" if need_closure_ptr
				res << "the_wrap.var = #{scope.var_ptr_for_wrap};" if scope.var_ptr_for_wrap
				res.compact.join("\n")
			end

			def init_c_code
				cb_c_code = closure_buid_c_code # must be called before the rest because it might change self or scope
				res = []
				res << "VALUE res;" if need_res
				res << "VALUE s_class = (#{get_cref})->nd_clss;" if need_class
				res << "VALUE #{get_closure_ary_var};" if cb_c_code
				res << "struct wrap the_wrap;" if need_wrap
				res << scope.init_c_code
				res << cb_c_code if cb_c_code
				res << wrap_buid_c_code if need_wrap
				res.compact.join("\n")
			end
		end

		class ClassModuleScope < Base
			def self.compile(outer, scope_node, class_mod_var)
				ensure_node_type(scope_node, :scope)
				cf = self.new(outer.compiler, Scopes::Scope.new(scope_node.last[:tbl]))
				fname = cf.un("class_module_scope")
				cf.instance_eval {
					block = make_block(scope_node.last[:next])
					l "return #{comp(block)};"
				}
				body = "#{cf.init_c_code}\n#{cf.get_lines}"
				args = []
				args << "VALUE self" if cf.need_self
				args << "NODE *cref" if cf.need_cref
				sig = "static VALUE #{fname}(#{args.join(", ")}) {"
				cf.compiler.add_fun("#{sig}\n#{body}\n}")
				outer.instance_eval {
					args = []
					args << class_mod_var if cf.need_self
					args << "NEW_NODE(NODE_CREF, #{class_mod_var}, 0, #{get_cref})" if cf.need_cref
					assign_res("#{fname}(#{args.join(", ")})")
				}
				"res"
			end

			def get_cref_impl
				"cref"
			end

		end

		class ToplevelScope < ClassModuleScope
			def self.compile(compiler, scope_node)
				ensure_node_type(scope_node, :scope)
				cf = self.new(compiler, Scopes::Scope.new(scope_node.last[:tbl], true))
				fname = cf.un("toplevel_scope")
				cf.instance_eval {
					block = make_block(scope_node.last[:next])
					l "#{comp(block)};"
				}
				body = "#{cf.init_c_code}\n#{cf.get_lines}"
				sig = "static void #{fname}(VALUE self, NODE *cref) {"
				cf.compiler.add_fun("#{sig}\n#{body}\n}")
				fname
			end
		end

		class Method < Base
			def self.compile(outer, scope_node, def_fun, class_var, mid)
				ensure_node_type(scope_node, :scope)
				cf = self.new(outer.compiler, Scopes::Scope.new(scope_node.last[:tbl]))
				fname = cf.un("method")
				cf.instance_eval {
					block = [:block, make_block(scope_node.last[:next]).last.dup] # dup the block to allow modification
					arg = block.last.shift
					ba = nil
					unless block.last.empty? || block.last.first.first != :block_arg
						ba = block.last.shift
					end
					handle_method_args(arg, ba)
					l "return #{comp(block)};"
				}
				body = "#{cf.init_c_code}\n#{cf.get_lines}"
				sig = "static VALUE #{fname}(int argc, VALUE *argv, VALUE self) {"
				cf.compiler.add_fun("#{sig}\n#{body}\n}")
				if cf.need_cref
					outer.instance_eval {
						add_helper <<-EOC
							static void def_only_once(ID mid) {
								rb_raise(rb_eTypeError, "def for \\"%s\\" can only be used once", rb_id2name(mid));
							}
						EOC
						c_scope {
							l "static int done = 0;"
							l "if (done) def_only_once(#{sym(mid)});"
							l "#{cf.cref_global_var} = (VALUE)(#{get_cref});"
							l "done = 1;"
							l "#{def_fun}(#{class_var}, #{mid.to_s.to_c_strlit}, #{fname}, -1);"
						}
					}
				else
					outer.instance_eval {
						l "#{def_fun}(#{class_var}, #{mid.to_s.to_c_strlit}, #{fname}, -1);"
					}
				end
				"Qnil"
			end

			def return_allowed?
				true
			end

			def get_cref_impl
				@cref_global_var ||= global("Qfalse")
				"(RNODE(#{@cref_global_var}))"
			end
			attr_reader :cref_global_var
		end

		class Block < Base
			def self.compile(outer, block_node, var_node)
				ensure_node_type(block_node, :block)
				cf = self.new(outer.compiler, outer.scope.new_dyna_scope)
				fname = cf.un("block")
				cf.instance_eval {
					if Array === var_node
						if var_node.first == :masgn
							dup_hash = var_node.last.dup
							c_if("ruby_current_node->nd_state != 1") { # 1 is YIELD_FUNC_AVALUE
								# do "svalue_to_mrhs"
								c_if("bl_val == Qundef") {
									l "bl_val = rb_ary_new2(0);"
								}
								c_else {
									#if dup_hash[:head] # TODO
										l "VALUE tmp = rb_check_array_type(bl_val);"
										l "bl_val = (NIL_P(tmp) ? rb_ary_new3(1, bl_val) : tmp);"
									#else
									#	l "bl_val = rb_ary_new3(1, bl_val);"
									#end
								}
							}
							dup_hash[:value] = "bl_val"
							comp_masgn(dup_hash)
						else
							c_if("ruby_current_node->nd_state == 1") { # 1 is YIELD_FUNC_AVALUE
								# do "avalue_to_svalue"
								l "if (RARRAY(bl_val)->len == 0) bl_val = Qnil;"
								l "else if (RARRAY(bl_val)->len == 1) bl_val = RARRAY(bl_val)->ptr[0];"
							}
							handle_assign(var_node, "bl_val")
						end
					end
					l "block_redo:"
					l "return #{comp_block(block_node.last)};"
				}
				body = "#{cf.init_c_code(outer)}\n#{cf.get_lines}"
				sig = "static VALUE #{fname}(VALUE bl_val, VALUE closure_ary, VALUE bl_self) {"
				cf.compiler.add_fun("#{sig}\n#{body}\n}")
				[fname, cf.need_closure_ptr]
			end

			def init_c_code(outer)
				cb_c_code = closure_buid_c_code # must be called before the rest because it might change self or scope
				outer.add_closure_need(:self) if need_self
				outer.add_closure_need(:class) if need_class
				outer.add_closure_need(:cref) if need_cref
				res = []
				res << "VALUE res;" if need_res
				if need_closure_ptr
					res << "VALUE *closure = RARRAY(closure_ary)->ptr;"
				end
				res << "VALUE self = (bl_self == Qundef ? closure[#{outer.closure_tbl.index(:self)}] : bl_self);" if need_self
				res << "VALUE s_class = (bl_self == Qundef ? closure[#{outer.closure_tbl.index(:class)}] : ruby_class);" if need_class
				if need_cref
					# see #define Data_Get_Struct
					res << "NODE *cref = (Check_Type(closure[#{outer.closure_tbl.index(:cref)}]," +
						" T_DATA), (NODE*)DATA_PTR(closure[#{outer.closure_tbl.index(:cref)}]));"
				end
				res << "VALUE #{get_closure_ary_var};" if cb_c_code
				res << "struct wrap the_wrap;" if need_wrap
				res << scope.init_c_code
				res << cb_c_code if cb_c_code
				res << wrap_buid_c_code if need_wrap
				res.compact.join("\n")
			end

			def in_block?(lbl_type = nil)
				case lbl_type
				when :redo
					"block_redo"
				else
					true
				end
			end

			def need_closure_ptr
				scope.need_closure || need_self || need_class || need_cref
			end

			def get_cref_impl
				"cref"
			end
		end

		class Wrap < Base

			def self.compile(outer, fname)
				cf = self.new(outer)
				cf.l "return #{yield cf};"
				body = "#{cf.init_c_code}\n#{cf.get_lines}"
				sig = "static VALUE #{fname}(struct wrap *wrap_ptr) {"
				cf.compiler.add_fun("#{sig}\n#{body}\n}")
				outer.need_wrap = true
				nil
			end

			attr_reader :base_cfun

			# the following attr_accessors from Base are redefined, to redirect to base_cfun
			[:need_self, :need_cref, :need_class, :need_wrap].each { |a|
				define_method(a) { base_cfun.send(a) }
				asgn_sym = :"#{a}="
				define_method(asgn_sym) { |arg| base_cfun.send(asgn_sym, arg) }
			}

			def initialize(cfun_to_wrap)
				if Wrap === cfun_to_wrap
					@base_cfun = cfun_to_wrap.base_cfun
				else
					@base_cfun = cfun_to_wrap
				end
				@compiler = base_cfun.compiler
				@scope = Scopes::WrappedScope.new(base_cfun.scope)
				@closure_tbl = base_cfun.closure_tbl
				@lines = []
				@while_stack = []
			end

			# TODO: def in_while?(lbl_type), so that it also checks in base_cfun ...

			def in_block?(lbl_type = nil)
				# TODO: ask base_cfun ...
				false
			end

			def return_allowed?
				# TODO: ask base_cfun ...
				false # subclass
			end

			def get_self
				self.need_self = true
				"(wrap_ptr->self)"
			end
			def get_cref_impl
				"(wrap_ptr->cref)"
			end
			def get_class
				self.need_class = true
				"(wrap_ptr->s_class)"
			end

			def get_closure_ary_var
				"(wrap_ptr->my_closure_ary)"
			end

			def get_wrap_ptr
				"wrap_ptr"
			end

			[:closure_buid_c_code, :wrap_buid_c_code, :need_closure_ptr].each { |m|
				define_method(m) {
					raise Ruby2CExtError::Bug, "the method #{m} may not be called for an instance of Wrap"
				}
			}

			def init_c_code
				res = []
				res << "VALUE res;" if need_res
				res.compact.join("\n")
			end
		end

	end

	class Compiler
		def initialize(name)
			@name = name
			@funs = []
			@toplevel_funs = []
			@sym_man = Tools::SymbolManager.new
			@global_man = Tools::GlobalManager.new
			@uniq_names = Tools::UniqueNames.new
			@helpers = {}
		end

		def to_c_code
			res = [
				"#include <ruby.h>",
				"#include <node.h>",
				"#include <env.h>",
				"#include <st.h>",
				"extern VALUE ruby_top_self;",
				"static VALUE org_ruby_top_self;",
				@sym_man.to_c_code,
				@global_man.to_c_code,
			]
			res.concat(@helpers.values.sort)
			res.concat(@funs)
			res << "void Init_#{@name}() {"
			res << "org_ruby_top_self = ruby_top_self;"
			# just to be sure
			res << "rb_global_variable(&org_ruby_top_self);"
			res << "init_syms();"
			res << "init_globals();"
			res << "NODE *cref = rb_node_newnode(NODE_CREF, rb_cObject, 0, 0);"
			@toplevel_funs.each { |f| res << "#{f}(ruby_top_self, cref);" }
			res << "}"
			res.join("\n").split("\n").map { |l| l.strip }.reject { |l| l.empty? }.join("\n")
		end

		# non destructive: node_tree will not be changed
		def add_toplevel(node_tree)
			@toplevel_funs << CFunction::ToplevelScope.compile(self, node_tree)
		end

		# uniq name
		def un(str)
			@uniq_names.get(str)
		end
		def sym(sym)
			@sym_man.get(sym)
		end
		def global(str)
			@global_man.get(str)
		end

		def add_helper(str)
			@helpers[str] ||= str
		end

		def add_fun(str)
			@funs << str
		end

		class << self
			def compile_ruby_to_c(source_str, name, file_name)
				c = self.new(name)
				hash = Parser.parse_string(source_str, file_name)
				# abb all BEGIN blocks, if available
				if (beg_tree = hash[:begin])
					beg_tree = beg_tree.transform(:include_node => true)
					if beg_tree.first == :block
						beg_tree.last.each { |s| c.add_toplevel(s) }
					else
						c.add_toplevel(beg_tree)
					end
				end
				# add toplevel scope
				if (tree = hash[:tree])
					c.add_toplevel(tree.transform(:include_node => true))
				end
				c.to_c_code
			end

			def compile_c_file_to_dllib(file_basename, name, verbose = false)
				require "rbconfig"

				conf = ::Config::CONFIG
				ldshared = conf["LDSHARED"]
				cflags = [conf["CCDLFLAGS"], conf["CFLAGS"], conf["ARCH_FLAG"]].join(" ")
				hdrdir = conf["archdir"]
				dlext = conf["DLEXT"]
				libs = [conf["LIBRUBYARG"], conf["LIBS"], conf["DLDLIBS"]].join(" ")
				cmd = "#{ldshared} #{cflags} -I. -I #{hdrdir} -o #{file_basename}.#{dlext} #{file_basename}.c #{libs}"
				if RUBY_PLATFORM =~ /mswin32/
					cmd << " -link /INCREMENTAL:no /EXPORT:Init_#{name}"
				end
				puts cmd if verbose
				unless system(cmd) # run it
					raise "error while executing '#{cmd}'"
				end
			end

			def compile_file(file_name, only_c = false, verbose = false)
				bn = File.basename(file_name)
				unless bn =~ /\A(.*)\.rb\w?\z/
					raise "#{file_name} is no ruby file"
				end
				name = $1;
				unless name =~ /\A\w+\z/
					raise "'#{name}' is not a valid extension name"
				end
				file_name = File.join(File.dirname(file_name), bn)
				puts "reading #{file_name}" if verbose
				source_str = IO.read(file_name)
				puts "compiling #{file_name} to C" if verbose
				c_code = compile_ruby_to_c(source_str, name, file_name)
				file_basename = File.join(File.dirname(file_name), name)
				puts "writing #{file_basename}.c" if verbose
				File.open("#{file_basename}.c", "w") { |f| f.puts(c_code) }
				unless only_c
					puts "compiling #{file_basename}.c" if verbose
					compile_c_file_to_dllib(file_basename, name, verbose)
				end
			end

			def usage
				puts <<EOS
Usage: #$0 [options] file.rb ...

Translates the given Ruby file into an equivalent C extension. The result is
stored in file.c. It will then be compiled into a shared object file, unless
the option --only-c is given.

If multiple files are given, each file will be handled separately.

Options:
  -c  --only-c   only translate to C
  -v  --verbose  print status messages
  -h  --help     print this help
EOS
			end

			def run
				require 'getoptlong'

				opts = GetoptLong.new(
					[ '--only-c',  '-c', GetoptLong::NO_ARGUMENT ],
					[ '--verbose', '-v', GetoptLong::NO_ARGUMENT ],
					[ '--help',    '-h', GetoptLong::NO_ARGUMENT ]
				)

				only_c = verbose = false
				begin
					opts.each do |opt, arg|
						case opt
						when "--only-c"
							only_c = true
						when "--verbose"
							verbose = true
						when "--help"
							usage
							exit
						end
					end
					if ARGV.empty?
						warn "No files given"
						raise
					end
				rescue
					puts
					usage
					exit 1
				end

				begin
					ARGV.each { |fn|
						compile_file(fn, only_c, verbose)
					}
				rescue RuntimeError, SyntaxError => e
					warn e
					exit 1
				end
			end
		end
	end

end

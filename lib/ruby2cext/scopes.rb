
require "ruby2cext/error"

module Ruby2CExtension

	module Scopes
		class Scope
			VMODES = [:public, :private, :protected, :module_function]

			def initialize(tbl, private_vmode = false)
				@vmode = private_vmode ? :private : :public
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

end


module Ruby2CExtension::Plugins

module Util

    def deduce_type(node)
        while true
            Array === node or return
            case node.first
            when :lit
                return Kernel.const_get(node.last[:lit].class.name)
            when :nil
                return NilClass
            when :false
                return FalseClass
            when :true
                return TrueClass
            when :str, :dstr
                return String
            when :dsym
                return Symbol
            when :array, :zarray, :masgn
                return Array
            when :hash
                return Hash
            when :dregx, :dregx_once
                return Regexp
            when :dasn_curr, :iasgn, :cvdecl, :gasgn, :cdecl
                node = node.last[:value]
            when :attrasgn
                args = node.last[:args]
                Array === args or return
                case args.first
                when :array
                    node = args.last.last
                when :argspush
                    node = args.last[:body]
                else
                    return
                end
            else
                return
            end
        end
    end

    def values(cfun, nreused, *rvalues) # :yield: *vals
        header = "{"
        cfun.l(header)
        res = 0
        reuse = 0
        vals = []
        res_assign = ""
        res_val = ""
        direct = /\A\s*(\w+(\s*\[\s*\d+\s*\])?|\d+)\s*\Z/
        rvalues.each { |rvalue|
            nreused -= 1
            val = rvalue && cfun.comp(rvalue)
            if val == "res"
                if res_assign
                    header.concat("VALUE res#{res};")
                    res += 1
                end
                var = "res#{res}"
                cfun.l(res_assign = "#{var} = #{val};")
                res_val = val = var
            elsif nreused >= 0 and val and val !~ direct
                var = "reuse#{reuse}"
                header.concat("VALUE #{var};")
                cfun.l("#{var} = #{val};")
                val = var
            end
            vals << val
        }
        res_assign.replace("")
        res_val.replace("res")
        if header.size == 1
            header.replace("")
            yield(*vals)
        else
            res_rvalue = yield(*vals)
            cfun.assign_res(res_rvalue) unless res_rvalue=="res"
            cfun.l "}"
            "res"
        end
    end

    def split_args(arity, extra_allowed, args)
        data = args.last
        required = []
        remaining = nil
        case args.first
        when :array
            if data.size>arity
                required = data[0, arity]
                remaining = [:array, data[arity, data.size-arity]]
            else
                required = data
            end
        when :splat
            remaining = args
        when :argscat
            required = split_args(arity, true, data[:head]) or return nil
            remaining = [:argscat, {:head => required.pop, :body => data[:body]}]
        when :argspush
            if extra_allowed
                required = split_args(arity, true, data[:head]) or return nil
                remaining = [:argspush, {:head => required.pop, :body => data[:body]}]
            else
                required = split_args(arity-1, false, data[:head]) or return nil
                required << data[:body]
            end
        else
            return nil
        end
        if required.size<arity
            nil
        elsif extra_allowed
            remaining ||= [:array, []]
            required << remaining
        elsif remaining
            nil
        else
            required
        end
    end

end

end



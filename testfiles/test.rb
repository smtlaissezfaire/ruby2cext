# this file contains code that uses many different node types and everything in
# here works with Ruby2CExtension

BEGIN {puts "begin1"}
BEGIN {puts "begin2"}

p self
puts
p 1,1.2,(p 23;-1),:sym,1...1,1..1
p [1,2],"oij\0iuj","",[1,[2,[3,4]]]
p 1,*[2,3]
p({:a=>[1], :b=>[2]}.values_at(:a, :b))
p [1,2,3].index(2)
a=b="hello"
puts a, b + " you"
p(@a=7)
p($a=6)
xx=$a=$b=@c=@d=xxx=5
p [$a,$b,@c,@d,xx,xxx]
for a in 0..1
	a = (a == 0 ? :a : :b)
	unless a == :b
		puts "a"
	else
		puts "not a"
	end
end
p(if puts
else
end)
def m(a)
	p a
end
m 1
p method(:m)

p [1111111111111222222333345342323, /ab/i, /cd/m, /xx/n]

a="hello"
def a.a
	p self
end
a.a

public

def fib(n)
	if n < 2
		n
	else
		fib(n - 1) + fib(n - 2)
	end
end

p fib(10)

case
when 1 == 1
	p "woo"
when *[false, true]
	p 1
else
	p "uhh"
end

for a in 0...7
	case a
	when 0
		p "null"
	when 3, *[1,2]
		p "1 2 3"
	when 4, 5
		p "4 5"
	else
		p 6
	end
end

a = 1,3
p a
p [__FILE__, __LINE__]

a = [1,nil,3]

a[0]+=2
a[1]||=4
a[2]&&=5

p a

a = Struct.new(:aa).new

a.aa=3
a.aa+=2
p a.aa
a.aa||=raise
p a.aa
a.aa&&=8
p a.aa

Object::AAA=5
BBB=6
p [AAA,BBB,::AAA,::BBB,Object::AAA,Object::Object::BBB]

p [:"a#{1+2}b#{"123".inspect}", "str#{:sym}xx", /a#{2}b#{3}c/, /a#{2}b#{3}c/o, `echo #{2+3}`, `echo 23`.chomp, /null\000null/]

def ct
	def argv
		::ARGV
	end
end

ct
p argv
ct

class AA < String
	def rev
		reverse
	end
end
p AA.new("abc").rev

class A
	@@var = :A
	C = :CA
end

class B
	@@var = :B
	C = :CB

	a = A.new
	class ::A
		p [@@var, C] # => [:A, :CA]
		def foo
			[@@var, C]
		end
	end
	def a.bar
		[@@var, C]
	end
	class << a
		p [@@var, C] # => [:B, :CB]
		def baz
			[@@var, C]
		end
	end
	p a.foo # => [:A, :CA]
	p a.bar # => [:B, :CB]
	p a.baz # => [:B, :CB]
	p [@@var, ::A::C, C, class << a;C;end] # => [:B, :CA, :CB, :CB]
	class << a
		@@var = :ASing # this changes B's @@var
		C = :CASing # this creates a new C for a's sing. class
	end
	p [@@var, ::A::C, C, class << a;C;end] # => [:ASing, :CA, :CB, :CASing]
	p a.foo # => [:A, :CA]
	p a.bar # => [:ASing, :CB]
	p a.baz # => [:ASing, :CASing]
end

class F; FF = 3;end
FF = 4
class G < F; p FF; end # should be 3, not 4

def pt
	p Proc.new[]
end
pt { 'Proc.new ok!' }

a,@b,$c,(D,*e),*f = [1,2,3,[4,5,6,7],8,9,10]
p [a,@b,$c,D,e,f]
a,((@b,$c),(DDD,*e)),*f = [1,[[2,3],[4,5,6,7]],8,9,10]
p [a,@b,$c,DDD,e,f]
a,b = 2
p [a,b]
*b=1,2
p b

a=[1,2,3]
a[5],a[7]=4, 6
p a
class << a
	attr_accessor :c, :d
end
a.c,a.d=7,8
p [a.c,a.d]

p [1,2,3].map { |a| a+1 }

def yield_t(a)
	yield a, a+1
end

p(yield_t(23) { |a, b| a+b })

p "abc".instance_eval { reverse }
p proc { |xx| xx }[24]

END { p "END" }
def end_t
	at_exit { p "at_exit" }
end
end_t
end_t

class SupA
	def test
		yield 23
	end
end
class SupB < SupA
	def test
		super
		super()
		super { |b| p b+1 }
		super() { |b| p b+2 }
	end
end
SupB.new.test { |a| p a }

for i in 1..9
	p i
end

def sqrt(x)
	x = x.to_f
	average = lambda { |a, b| (a+b)/2 }
	impr    = lambda { |g| average[g, x/g] }
	good_en = lambda { |g| (g*g - x).abs < 0.001 }
	try     = lambda { |g| good_en[g] ? g : try[impr[g]] }
	try[1.0]
end

p sqrt(2)


def my_while(cond)
	if cond
		yield
		retry
	end
end

i=3
my_while((i-=1)>0) { p i }

p (11..20).map { |i| (i%4==0)..(i%3==0) ? i : nil }
p (11..20).map { |i| (i%4==0)...(i%3==0) ? i : nil }

"a" =~ /(a)/
p [
	defined? ""=~//,
	defined? yield,
	defined? self,
	defined? nil,
	defined? true,
	defined? false,
	defined? a=5,
	defined? a,
	defined? $a,
	defined? $udef,
	defined? @c,
	defined? @udef,
	defined? A,
	defined? Udef,
	defined? @@udef,
	defined? ::A,
	defined? ::Udef,
	defined? $1,
	defined? $9,
	defined? $',
]

class AAAAAA
	xx=5
	proc { |yy|
		begin
			@@a||=xx
			@@a||=yy
			@a||=5
			@a||=6
			$f||=5
			$f||=6
			a||=5
			a||=6
			p [@@a, @a, $f, a]
			@@a&&=5
			@@a&&=6
			@a&&=5
			@a&&=6
			$f&&=5
			$f&&=6
			a&&=5
			a&&=6
			p [@@a, @a, $f, a]
		ensure
			p "in ensure"
		end
	}[6]
end

class AEx < RuntimeError;end
class BEx < RuntimeError;end
class CEx < RuntimeError;end

ex = AEx
p(begin
  p "in body"
  ex && raise(ex.new)
  p "not raised"
rescue TypeError, AEx
	p "in AEx"
	ex = BEx
	retry
rescue *[TypeError, BEx] => b
	p "in BEx #{b}"
	ex = CEx
	retry
rescue
	p "in rescue"
	ex = nil
	retry
else
	"res"
ensure
	p "in ensure"
end)

ex = AEx
begin
	begin
		p "in body"
		raise ex
	rescue AEx
		p "in AEx"
		ex = BEx
		retry
	end
rescue BEx
	p "in BEx"
else
	p "in else: BUG!"
end

begin
	begin
		raise
	rescue 1
		2
	end
rescue => e
	p e
end


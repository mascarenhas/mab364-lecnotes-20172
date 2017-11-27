import proto._

run(parse(
  """
fun counter(n)
  object (n)
    fun inc(n)
      @0 := @0 + n
    end
    fun dec(n)
      @0 := @0 - n
    end
  end
end

let c1 = counter(read),
 c2 = counter(read) in
  print(c1.inc(4));
  print(c2.dec(2))
end
  """), Stream(1, 2))

run(parse(
  """
fun counter(n)
  object (n)
    fun inc(n)
      @0 := @0 + n
    end
    fun dec(n)
      @0 := self.inc(-n)
    end
  end
end

let c1 = counter(read),
 c2 = counter(read) in
  print(c1.inc(4));
  print(c2.dec(2))
end
  """), Stream(1, 2))

run(parse(
  """
fun counter(n)
  object (n)
    fun inc(n)
      @0 := @0 + n
    end
    fun dec(n)
      self.inc(-n)
    end
  end
end
fun dcounter(o)
  object (o)
    fun inc(n)
      @0.inc(n*2)
    end
  end
end
let c1 = counter(0),
    c2 = dcounter(c1) in
  print(c1.inc(5));
  print(c2.inc(2))
end
  """), Stream(1, 2))

run(parse(
  """
fun counter(n)
  object (n)
    fun inc(n)
      @0 := @0 + n
    end
    fun dec(n)
      self.inc(-n)
    end
  end
end
fun hcounter(o)
  object () extends o
    fun inc(n)
      super.inc(n * 2)
    end
  end
end
let c1 = counter(0),
    c2 = hcounter(c1) in
  print(c1.inc(2));
  print(c2.dec(3))
end
  """), Stream(1, 2))

run(parse(
  """
fun vazia()
  object ()
    fun imprime() 0 end
    fun map(f) self end
    fun match(cv, cc)
      cv.apply()
    end
  end
end

fun cons(h, t)
  object (h, t)
    fun imprime()
      print(@0); @1.imprime()
    end
    fun map(f)
      cons(f.apply(@0), @1.map(f))
    end
    fun match(cv, cc)
      cc.apply(@0, @1)
    end
  end
end

fun imprime(l)
  l.match(object ()
    fun apply() 0 end
  end,
  object()
    fun apply(h, t)
      print(h);
      imprime(t)
    end
  end)
end

fun map(l, f)
  l.match(object (l)
    fun apply() @0 end
  end,
  object (f)
    fun apply(h, t)
      cons(@0.apply(h), map(t, @0))
    end
  end)
end

let l1 = cons(2, cons(3, vazia())),
    l2 = l1.map(object ()
                  fun apply(o) o * o end
                end),
    l3 = map(l2, object ()
                   fun apply(o) o * o end
                 end) in
  l1.imprime(); l2.imprime();
  imprime(l1); imprime(l2);
  imprime(l3)
end

  """), Stream())

run(parse(
  """
fun tracer(f)
  object () extends f
    fun apply(x)
      print(x);
      super.apply(x)
    end
  end
end

let fat = object ()
            fun apply(n)
              if n < 2 then
                1
              else
                n * self.apply(
                       n - 1)
              end
            end
          end in
  print(fat.apply(5));
  (tracer(fat)).apply(5)
end
  """), Stream())

def tracer[A, B](f: (A => B) => A => B): A => B = {
  def trace(self: A => B)(x: A): B = {
    println(x);
    f(self)(x)
  }
  def traced(x: A): B = trace(traced)(x)
  traced
}

def fatRA(self: Int => Int)(x: Int): Int = if (x < 2) 1 else x * self(x-1)

def fat(x: Int): Int = fatRA(fat)(x)

fat(5)

tracer(fatRA)(5)
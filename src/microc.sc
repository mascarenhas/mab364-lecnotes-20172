import microc._

run(parse(
  """
fun troca(_a, _b)
  let tmp = _a in
    _a := _b;
    _b := tmp;
    _a
  end
end  -- vai trocar os valores de x e y!
let x = read, y = read in troca(x, y) + x end
  """), Stream(1, 2))


run(parse(
  """
try
let x = 1, i = 5 in
    while 0 < i do
      print(x, i);
      x := x * 2;
      i := i - 1
    end;
    --*500;
    print(x)
end
catch
print(50);
*500
end;
print(10)
  """), Stream())


/*
bind(escreve(0, 2), _ => le(0))
  k => escreve(0, 2)(v =>
    (_ => le(0))(v)(k))
  k => escreve(0, 2)(v => le(0)(k))
  k => (k1 => (in, sp, mem) => k1(2)(in, sp, mem + (0 -> 2)))(v => le(0)(k))
  k => (in, sp, mem) => (v =>
    le(0)(k))(2)(in,
            sp, mem + (0 -> 2))
  k => (in, sp, mem) =>
    le(0)(k)(in,
      sp, mem + (0 -> 2))
  k => (in, sp, mem) =>
    (k1 => (in1, sp1, mem1) =>
      k1(mem1(0))(in1, sp1, mem1))(k)
           (in, sp, mem + (0 -> 2))
  k => (in, sp, mem) =>
    ((in1, sp1, mem1) =>
      k(mem1(0))(in1, sp1, mem1))
          (in, sp, mem + (0 -> 2))
  k => (in, sp, mem) =>
      k((mem + (0 -> 2))(0))
        (in, sp, mem + (0 -> 2))
  k => (in, sp, mem) =>
    k(2)(in, sp, mem + (0 -> 2))
*/

run(parse(
  """
 fun gen(i)
  let n = i in
    while 0 < n do
      yield(n);
      n := n - 1
     end
  end
end

try
 let c = coro gen(read) in
  print(resume(c));
  print(resume(c));
  print(resume(c));
  print(resume(c))
end
catch
5
end

  """), Stream(10, 2))



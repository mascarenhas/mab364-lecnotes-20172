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
let x = 1, y = 2 in troca(x, y) + x end
  """), Stream())


run(parse(
  """
let x = 1, i = 5 in
    while 0 < i do
      print(x, i);
      x := x * 2;
      i := i - 1
    end;
    x
end
  """), Stream())


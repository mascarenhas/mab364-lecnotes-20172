import microc._

run(parse(
  """
fun troca(a, b)
  let tmp = a in
    a := b;
    b := tmp;
    a
  end
end  -- não vai trocar os valores de x e y!
let x = 1, y = 2 in troca(x, y) + x end
  """))

run(parse(
  """
fun troca(a, b)  -- a e b são ponteiros
  let tmp = *a in
    *a := *b;
    *b := tmp
  end
end
-- troca os valores x e y!
let x = 1, y = 2 in troca(&x, &y); x end
  """))

run(parse(
"""
fun arr5(x1, x2, x3, x4, x5)
  let arr = *0 in
    *0 := *0 + 5;
    *arr := x1;
    *(arr+1) := x2;
    *(arr+2) := x3;
    *(arr+3) := x4;
    *(arr+4) := x5;
    arr
  end
end

fun sumarr(a, n)
  if 0 < n then
    *a + sumarr(a+1, n-1)
  else
    *5000
  end
end

try
let arr = arr5(1, 3, 5, 7, 9) in
  sumarr(arr, 5)
end
catch 0
end
"""
))

run(parse(
  """
fun troca(_a, _b)
  let tmp = _a in
    _a := _b;
    _b := tmp;
    _a
  end
end  -- vai trocar os valores de x e y!
let x = 1, y = 2 in troca(*1000, y) + x end
  """))

import fun._

val pcont = parse(
  """
let cont =
  let n = ref 0 in
    fun () n := !n + 1; !n end
  end
in
  cont();
  cont();
  cont()
end
  """)

run(pcont)

run(parse(
  """
let c = ref fun () x end in
  let x = 2 in
    (!c)()
  end + 3
end
  """
))

run(parse(
  """
let r = ref 2 in r := 5 end
  """))

run(parse(
  """
let c = ref 0 in
  c := !c + 1;
  c := !c + 1;
  !c
end
  """))

run(parse(
  """
let c = ref 0 in
  (c := !c + 1; !c) + (c := !c + 1; !c)
end
  """))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  s((c := !c + 1; !c),
    (c := !c + 1; !c),
    (c := !c + 1; !c))
end
  """
))

run(parse(
  """
let c = ref 0 in
  (c := !c + 1; c) := !c + 1
end
  """
))

run(parse(
"""  let c = ref 0 in
  !(c := 1; c)
end
"""
))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  s((c := !c + 1; !c),
    (c := c + 1; !c),
    (c := !c + 1; !c))
end
  """
))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = 0 in
  s((c := !c + 1; !c),
    (c := !c + 1; !c),
    (c := !c + 1; !c))
end
  """
))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  s((c := !c + 1; !c),
    (c := !c + 1; throw "erro aqui"; !c),
    (c := !c + 1; !c))
end
  """
))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  try s((c := !c + 1; !c),
    (c := !c + 1; throw "erro aqui"; !c),
    (c := !c + 1; !c))
  catch
    c := !c + 1; throw "erro no catch"
  end
end
  """
))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  try s((c := !c + 1; !c),
    (c := !c + 1; throw "erro aqui"; !c),
    (c := !c + 1; !c))
  catch
    c := !c + 1; throw "erro no catch"
   finally
    c := !c + 1
  end
end
  """
))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  try s((c := !c + 1; !c),
    (c := !c + 1; throw "erro aqui"; !c),
    (c := !c + 1; !c))
  catch
    c := !c + 1; !c
  finally
    c := !c + 1
  end
end
  """
))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  try s((c := !c + 1; !c),
    (c := !c + 1; throw "erro aqui"; !c),
    (c := !c + 1; !c))
  catch
    c := !c + 1; throw "erro no catch"
   finally
    c := !c + 1; throw "erro no finally"
  end
end
  """
))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  try s((c := !c + 1; !c),
    (c := !c + 2; !c),
    (c := !c + 3; !c))
  catch
    c := !c + 1; throw "erro no catch"
   finally
    c := !c + 1; throw "erro no finally"
  end
end
  """
))

run(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  try s((c := !c + 1; !c),
    (c := !c + 2; !c),
    (c := !c + 3; !c))
  catch
    c := !c + 1; throw "erro no catch"
   finally
    c := !c + 1
  end
end
  """
))

run(parse(
  """
fun loop()
  loop()
end

let _x = loop() in
   2
end
  """
))

run(parse(
  """
let _x = 2 + 3 in
   _x
end
  """
))

run(parse(
  """
let _y = 2 + 3 in
let _x = _y in
   _x
end
end
  """
))

run(parse(
  """
letrec fat = fun (x, acc)
  if x < 2 then
     acc
  else
     fat(x-1, acc*x)
  end
end in
fat(5, 1)
end
  """
))

run(parse(
  """
letrec parimpar = fun (sel, x)
  if sel then
    if x < 1 then true
    else parimpar(false, x-1) end
  else
    if x < 1 then false
    else parimpar(true, x-1) end
  end
end
in parimpar(true, 4) end
  """))

run(parse(
  """
fun cons(a, b)
  fun (sel)
    if sel then a else b end
  end
end

fun fst(p)
  p(true)
end

fun snd(p)
  p(false)
end

letrec parimpar = cons(
  fun (x)
    if x < 1 then true
    else (snd(parimpar))(x-1) end
  end,
  fun (x)
    if x < 1 then false
    else (fst(parimpar))(x-1) end
  end
) in
let par = fst(parimpar),
    impar = snd(parimpar) in
      par(4)
end
end
  """))

run(parse(
  """
fun fix(f)
  let F2 = fun (F1)
             f(F1(F1))
           end
  in F2(F2) end
end

let fat = fix(fun (_fat)
                fun (x)
                  if x < 2 then 1
                  else x * _fat(x-1) end
                end
              end) in fat(5) end

  """))
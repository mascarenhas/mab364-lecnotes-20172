import fun._

val pmrec = parse(
  """
letrec par = fun (x)
              if x < 1 then true
              else impar(x-1) end
             end
in
 letrec impar = fun(x)
              if x < 1 then false
              else par(x-1) end
              end
in par(4) end
end
  """)

runs(pmrec)

val pmrec1 = parse(
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
  """)

val res = runs(pmrec1)

res

val pmcons = parse(
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
      impar(4)
end
end
  """)

val rescons = runs(pmcons)
rescons

val cl = parse(
  """
fun t(a)
  fun (b)
    a()
  end
end

fun f(a)
  fun (b)
    b()
  end
end

fun cond(b, th, el)
  (b(th))(el)
end

fun zero(x)
  x
end

fun um(x)
  x(x)
end

fun dois(x)
  x(x(x))
end

fun s(n)
  fun (x)
    x(n(x))
  end
end

fun soma(a, b)
  fun (x)
    a(b(x))
  end
end

soma(dois, dois)

  """)

val plista = parse(
  """
fun Cons(h, t)
  fun (v, c)
    c(h, t)
  end
end

fun Vazia()
  fun (v, c)
    v()
  end
end

fun soma(l)
  l(fun () 0 end,
    fun (h, t) h + soma(t) end)
end

let l1 = Cons(1, Cons(3, Cons(5, Vazia()))) in soma(l1) end
  """)

run(plista)

val pfatL2 = parse(
  """
let fat =
 let F = fun (F)
           fun (x)
             if x < 2 then 1
             else x * (F(F))(x-1) end
           end
         end
in F(F) end in
  fat(5)
end
  """)

run(pfatL2)

val pfatfix = parse(
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

  """)

run(pfatfix)


val step = stepper(Prog(Map(), Num("0")))

step(Rec("loop", Var("loop")))

val ploop = parse(
  """
(fun (x) x(x) end)(fun (x) x(x) end)
  """)

ploop.corpo
step(ploop.corpo)

ploop.corpo == step(ploop.corpo)
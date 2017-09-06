import fun._

val pfat = parse(
  """
fun fat(x)
  if x < 2 then
    1
  else
     x * fat(x-1)
  end
end
fat(5)
  """
)

run(pfat)

val p1 = parse(
  """
let fat = fun (x)
  if x < 2 then
     1
  else
     x * fat(x-1)
  end
end in
fat(5)
end
  """.stripMargin
)

run(p1)
runs(p1)

val p2 = parse(
  """
let soma = fun (x)
  fun (y)
    x + y
  end
end in
(soma(2))(3)
end
  """
)

run(p2)
runs(p2)

val p3 = parse(
  """
fun soma()
  fun (y)
    x + y
  end
end
let f = soma(),
    x = 2 in f(3) end
  """
)

run(p3)
runs(p3)

val lm = parse(
  """
    let _x = 2,
        y = 5, z = 20 + 30,
        w = z + y
    in
        _x + y + z + w
    end

  """)

runs(lm)

val p4 = parse(
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
  """.stripMargin
)

run(p4)
runs(p4)

val p5 = parse(
  """
let f = fun (x)
  if x < 2 then
    1
  else
    x * (fun (x)
      if x < 2 then
        1
      else
        x * (fun (x)
          if x < 2 then
            1
          else
            x * (fun (x)
              if x < 2 then
                1
              else
                x * (fun (x)
                  if x < 2 then
                    1
                  else
                    x * fat(x - 1)
                  end
                end)(x - 1)
              end
            end)(x - 1)
          end
        end)(x - 1)
      end
    end)(x - 1)
  end
end in f(4) end
  """)

run(p5)

val step = stepper(p4)

step(Rec("loop", Var("loop")))

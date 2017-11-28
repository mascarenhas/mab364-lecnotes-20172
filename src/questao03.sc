
fun.run(fun.parse(
  """
    let fat = fun (f, n)
                if n < 2 then
                  1
                else
                  n * (f)(f, n-1)
                end
              end
    in
      fat(fat, 5)
    end
  """))

proto.run(proto.parse(
  """
    2 + 2
  """), Stream())
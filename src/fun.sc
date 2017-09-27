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
  """
let c = ref fun () x end in
  let x = 2 in
    (!c)()
  end
end
  """
))

run(parse(
"""  let c = ref 0 in
  !(c := 1; c)
end
"""
))
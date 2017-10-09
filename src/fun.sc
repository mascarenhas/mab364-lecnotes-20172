import java.lang.Exception

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
  end + (ref 3)
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

runs(parse(
  """
fun s(a, b, c)
    a - b - c
end
let c = ref 0 in
  try s((c := !c + 1; !c),
    (c := !c + 1; throw "erro aqui"; !c),
    (c := !c + 1; !c))
  catch
    c := !c + 10; throw "erro no catch"
  end
end
  """
))

runs(parse(
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

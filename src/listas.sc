import fun._

val plista = fun.parse(
"""
  fun Cons(h, t)
    fun (v, c)
      (c)(h, t)
    end
  end

  fun Vazia()
    fun (v, c)
      (v)()
    end
  end

  fun tamanho(l)
    (l)(fun () 0 end, fun (h, t) 1 + tamanho(t) end)
  end

  fun map(f, l)
    -- implementação de map
    Vazia()
  end

  fun filter(p, l)
    -- implementação de filter
    Vazia()
  end

  fun foldLeft(z, op, l)
    -- implementação de foldLeft
    0
  end

  fun foldRight(z, op, l)
    -- implementação de foldRight
    0
  end

  fun flatMap(f, l)
    -- implementação de flatMap
    0
  end

  tamanho(Cons(1, Cons(2, Cons(3, Vazia()))))
"""
)
run(plista)

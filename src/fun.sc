import fun._

run(parse(
  """
fun Handler(hk, next)
  fun (h, e)
    h(hk, next)
  end
end

fun EmptyH()
  fun (h, e)
    e()
  end
end

let handlers = ref EmptyH(),
    erro = fun (codigo, k)
      (!handlers)(fun (hk, hrest)
          handlers := hrest;
          hk(codigo)
      end,
      fun ()
        codigo
      end)
    end,
    trycatch = fun (ftry, fcatch, k)
       handlers := Handler(fun (v)
          fcatch(v, k)
       end, !handlers);
       ftry(fun (v)
         handlers := (!handlers)(
           fun (hk, hrest)
             hrest
           end, fun() 0 end);
         k(v)
       end)
    end,
    kid = fun (v)
      v
    end,
    seq = fun (e, k)
      k(e)
    end,
    deref = fun (r, k)
      k(!r)
    end,
    plus = fun (a, b, k)
      k(a+b)
    end,
    set = fun (r, v, k)
      r := v;
      k(v)
    end,
    s = fun (a, b, c, k)
      k(a + b + c)
    end,
    cref = fun (v, k)
      k(ref v)
    end in
cref(0, fun (c)
  trycatch(fun (k)
     deref(c, fun (vc)
       plus(vc, 1, fun (vp)
         set(c, vp, fun (vs)
           seq(vs, fun (vss)
             deref(c, fun (a)
               deref(c, fun (vc)
                 plus(vc, 1, fun(vp)
                   set(c, vp, fun (vs)
                     seq(vs, fun (vss)
                       erro(-1, fun (ve)
                         deref(c, fun (b)
                           s(a, b, 3, k)
                         end)
                       end)
                     end)
                   end)
                 end)
               end)
             end)
           end)
         end)
       end)
     end)
  end, fun (v, k)
    deref(c, fun (vc)
      plus(vc, 1, fun (vp)
        set(c, vp, fun (vs)
          seq(vs, fun (vss)
            erro(v * 2, fun (ve)
              k(2)
            end)
          end)
        end)
      end)
    end)
  end, fun (vtc)
    kid(vtc)
  end)
end)
end
  """
))


(* algoritmo di farey *)
fun farey n1 d1 n2 d2 n 0 = (n1+n2,  d1+d2)
  | farey n1 d1 n2 d2 n i =
  let
    val a = (real n1) / (real d1)
    val b = (real (n1+n2)) / (real (d1+d2))
    val c = (real n2) / (real d2)
  in
    if n < b then farey n1 d1 (n1+n2) (d1+d2) n (i-1)
    else farey (n1+n2) (d1+d2) n2 d2 n (i-1)
  end;

farey 0 1 1 1 0.336944434029 306;

(* euclide *)
fun divide n m =
  let
    val n1 = n - m
  in
    if n1 > m then
      let
        val (q, r) = divide n1 m;
      in
        (q + 1, r)
      end
    else (1, n1)
  end;

fun mcd n 0 = n
  | mcd n m =
  let
    val (q, r) = divide n m
  in
    mcd m r
  end;

mcd 54 39;

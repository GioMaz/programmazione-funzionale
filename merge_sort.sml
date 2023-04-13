fun split nil = (nil, nil)
  | split [x] = ([x], nil)
  | split (x::y::ys) =
  let
    val (m, n) = split (ys)
  in
    (x::m, y::n)
  end;

fun merge (nil, L) = L
  | merge (N, nil) = N
  | merge (L as x::xs, M as y::ys) = if x < y then x::merge(xs, M)
                                     else y::merge(L, ys)

fun merge_sort nil = nil
  | merge_sort [a] = [a]
  | merge_sort L =
  let
    val (M, N) = split L
    val M = merge_sort M
    val N = merge_sort N
  in
    merge (M, N)
  end;

merge_sort [5,32,6,31,7,9,44,16,3]

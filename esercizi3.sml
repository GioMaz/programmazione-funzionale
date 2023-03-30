(* factorial using patterns *)
fun fact 0 = 1
  | fact n = n * fact(n-1)

(* fibonacci using patterns *)
fun fibonacci 0 = 0
  | fibonacci 1 = 1
  | fibonacci n = fibonacci(n-1) + fibonacci(n-2)

(* cycle i times using patterns *)
fun cycle_for (nil, i) = nil
  | cycle_for (x, 0) = x
  | cycle_for (x::xs, i) = cycle_for(xs @ [x], i-1)

(* largest string using patterns *)
fun largest nil = ""
  | largest (x::nil) = x
  | largest (x::y::xs) = if x > y then largest (x::xs)
                         else largest (y::xs)

(* reverse list using patterns *)
fun reverse nil = nil
  | reverse (x::xs) = reverse (xs) @ [x]

(* merge two sorted arrays using patterns *)
fun merge (x, nil) = x
  | merge (nil, y) = y
  | merge (x::xs, y::ys) = if x < y then x::merge (xs, y::ys)
                           else y::merge (x::xs, ys)

(* flip every two elements of list *)
fun flip nil = nil
  | flip (x::nil) = [x]
  | flip (x::y::xs) = y::x::flip(xs)

(* remove i element of list remove i element of list  *)
fun remove (nil, i) = nil
  | remove (x::xs, 1) = xs
  | remove (x::xs, i) = x::remove(xs, i-1);

(* order tuples inside of list *)
fun order_couples nil = nil
  | order_couples ((a,b)::xs) = if a < b then (a,b)::order_couples xs
                               else (b,a)::order_couples xs;

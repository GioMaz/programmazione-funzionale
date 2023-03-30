(* factorial with patterns *)
fun fact 0 = 1
  | fact n = n * fact(n-1);

(* fibonacci with patterns *)
fun fibonacci 0 = 0
  | fibonacci 1 = 1
  | fibonacci n = fibonacci(n-1) + fibonacci(n-2);

(* largest string with patterns *)
fun largest (nil: string list) = ""
  | largest (x::nil) = x
  | largest (x::xs) = if x > hd xs then largest (x :: tl xs)
                              else largest (xs)

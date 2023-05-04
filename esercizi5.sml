print("hello\n");

(* print all the element of list *)
fun print_list nil = ()
  | print_list (x::xs) = (
      print(Int.toString(x));
      print("\n");
      print_list(xs)
    );

(* print 2 to the nth times "X" *)
fun print_2_to_the_n 0 = print("X\n")
  | print_2_to_the_n n = (
      print_2_to_the_n(n-1);
      print_2_to_the_n(n-1)
    );

(* get content of file "merge_sort.sml" *)
val infile = TextIO.openIn("file.txt");
val content = TextIO.input(infile);

(* combination of n of class m *)
fun fact 0 = 1
  | fact n = n * fact (n-1);

fun comb (n, m) = real(fact(n)) / real((fact(m) * fact(n-m)));

(* exceptions *)
exception ExceptionName of int;

fun fun1 n = raise ExceptionName (n);

fun fun2 n = fun1 n handle
  ExceptionName 0 => 0
  | ExceptionName n => (
        print(Int.toString(n));
        print(" caused an error\n");
        1
    );

(* integral of F between a and b with approximation of n *)
fun trap (a, b, n, f) =
  if n <= 0 orelse b-a <= 0.0 then 0.0
  else
    let
      val delta = (b-a)/real(n)
      fun trap1(x, 0) = 0.0
        | trap1(x, i) = delta * (f(x) + f(x + delta)) / 2.0
        + trap1(x + delta, i-1)
    in
      trap1(a, n)
    end;

trap (0.0, 1.0, 200, fn x => x * x);

(* tabulate function *)
fun tabulate (a, delta, 0, f) = ()
  | tabulate (a, delta, n, f) = (
      print(Real.toString(a));
      print("\t");
      print(Real.toString(f(a)));
      print("\n");
      tabulate(a+delta, delta, n-1, f)
    );

tabulate (0.0, 0.1, 9, fn x => x * x);

(* funzioni utili: implementation of simpleMap, reduce and filter *)
fun simpleMap (f, nil) = nil
  | simpleMap (f, x::xs) = f(x) :: simpleMap(f, xs);

simpleMap (fn x => x*x, [1,2,3,4]);

exception EmptyList;

fun reduce (f, nil) = raise EmptyList
  | reduce (f, [x]) = x
  | reduce (f, x::xs) = f(x, reduce(f, xs));

reduce (op +, [1,2,3,4]);

fun filter (f, nil) = nil
  | filter (f, x::xs) = if f(x) then x :: filter(f, xs) else filter(f, xs);

filter (fn x => x mod 2 = 0, [1,2,3,4]);

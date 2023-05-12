(* add curried function *)
fun add x = fn y => x + y;

(* add3 *)
fun add3 x = add x 3;

(* applyList non curried function *)
(*
fun applyList (x, nil) = nil
  | applyList (x, y::ys) = y(x) :: applyList(x, ys);
*)

(* applyList curried function *)
fun applyList x nil = nil
  | applyList x (y::ys) = y(x) :: applyList x ys;

applyList 3 [fn x => x+1, fn x => x*2, fn x => x*x];

(* curry function F *)
fun F (a, b, c) = a + b + c;
fun G a b c = F (a, b, c);

(* function composition *)
fun f1 x = x+1;
fun f2 x = x*2;
(* fun f3 x = f1(f2(x)); *)
val f3 = f1 o f2;

(* built-ins are curried *)
map (fn x => x+1) [1,2,3];
foldr (fn (x, y) => x - y) 0 [1,2,4,8]; (* 4-8 -> 2--4 -> 1-6 *)
foldl (fn (x, y) => x - y) 0 [1,2,4,8]; (* 2-1 -> 4-1 -> 8-3 *)

(* peano *)
datatype naturale = zero | succ of naturale;

fun somma x zero = x
  | somma x (succ y) = somma (succ x) y;

fun prod x zero = zero
  | prod x (succ zero) = x
  | prod x (succ y) = somma x (prod x y);

(* binary tree *)
datatype 'label btree =
  Empty |
  Node of 'label * 'label btree * 'label btree;

fun lookup Empty R _ = false
  | lookup (Node(y, left, right)) R x = if R x y then lookup left R x
                                        else if R y x then lookup right R x
                                        else true;

fun insert Empty R x = Node(x, Empty, Empty)
  | insert (T as Node(y, left, right)) R x =
      if R x y then Node (y, insert left R x, right)
      else if R y x then Node (y, left, insert right R x)
      else T;

(* switch *)
fun day1 n = case n of
                    1 => "Monday"
                  | 2 => "Tuesday"
                  | 3 => "Wednesday"
                  | 4 => "Thursday"
                  | 5 => "Friday"
                  | 6 => "Saturday"
                  | 7 => "Sunday"
                  | _ => "Other";

val day2 = fn 1 => "Monday"
  | 2 => "Tuesday"
  | 3 => "Wednesday"
  | 4 => "Thursday"
  | 5 => "Friday"
  | 6 => "Saturday"
  | 7 => "Sunday"
  | _ => "Other";

val isone = fn 1 => "One"
  | _ => "Other";

fun islowerthan5 x = case x < 5 of
                          true => "Yes"
                          | false => "No";

val infile = TextIO.openIn("temp.txt");
fun readfile x =
let
  val c = TextIO.inputN(infile, 1)
in
  if TextIO.endOfStream(infile) then x
  else if c = "*" then x
  else readfile (x @ [c])
end;

structure IntLT = struct
  type t = int
  val lt = op <
  val eq = op =
end;

(* STACK *)
signature STACK =
sig
  val empty: 'a list
  val pop: 'a list -> 'a option * 'a list
  val push: 'a * 'a list -> 'a list
  eqtype 'a stack
end;

structure Stack = struct
  type 'a stack = 'a list
  val empty = []
  fun pop [] = (NONE, [])
    | pop (x::xs) = (SOME x, xs)
  val push = op ::
end:> STACK;

val a = [1,2,3,4];
val a = Stack.push(0, a);
val (n, a) = Stack.pop(a);

(* SET *)
signature SET =
sig
  val empty: 'a list
  val contains: ''a * ''a list -> bool
  val add: ''a * ''a list -> ''a list
  val remove: ''a * ''a list -> ''a list
  eqtype 'a set
end;

structure Set = struct
  type 'a set = 'a list
  val empty = []
  fun contains (_, []) = false
    | contains (x, y::ys) = if x = y then true
       else contains(x, ys)
  fun add (x, y) = if contains(x, y) then y else x::y
  fun remove (_, nil) = nil
    | remove (x, y::ys) = if x = y then ys
                          else y::remove(x, ys)
end:> SET

val a = [1,2,3,4];
Set.remove(3, a);

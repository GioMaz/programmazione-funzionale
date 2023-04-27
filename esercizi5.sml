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
val content = TextIO.input(infile)

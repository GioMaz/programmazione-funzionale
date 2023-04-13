(* true if first char of list is vowel *)
fun is_first_vowel (#"a"::_) = true
  | is_first_vowel (#"e"::_) = true
  | is_first_vowel (#"i"::_) = true
  | is_first_vowel (#"o"::_) = true
  | is_first_vowel (#"u"::_) = true
  | is_first_vowel (_) = false

(* check if element is in list *)
fun contains (_, nil) = false
  | contains (x, y::ys) = if x = y then true
                          else contains (x, ys)

(* insert element in set *)
fun insert (x, nil) = [x]
  | insert (x, y::ys) = if x = y then y::ys
                        else y::insert (x, ys)

(* insert element in all lists of list *)
fun insert_all (_, nil) = nil
  | insert_all (x, y::ys) = (x::y)::insert_all (x, ys)

(* powerset (insieme delle parti) *)
fun powerset nil = [nil]
  | powerset (x::xs) = (insert_all (x, powerset(xs))) @ powerset(xs)

(* product of differences *)
fun prod_diff_1 (x, nil) = 1.0
  | prod_diff_1 (x, y::ys) = (x - y) * prod_diff_1 (x, ys)

fun prod_diff_2 nil = 1.0
  | prod_diff_2 (x::xs) = prod_diff_1(x, xs) * prod_diff_2(xs);

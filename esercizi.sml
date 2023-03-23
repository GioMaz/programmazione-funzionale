(* get the minimum of a tuple of 3 *)
fun min (a, b, c) = if a < b then if a < c then a else c else 
  if b < c then b else c

(* get nth element of list *)
fun get (l, n) = if n <= 0 then hd l else get(tl l, n-1)

(* reverse tuple of 3 *)
fun reverse (a, b, c) = (c, b, a)

(* get third element of string *)
fun get_third s = hd(tl(tl(explode s)))

(* cycle list once *)
fun cycle l = tl l @ [hd l]

(* get the maximum of a tuple of 3 *)
fun max (a, b, c) = if a > b then if a > c then a else c else 
  if b > c then b else c

(* get the minimum and the maximum of a tuple of 3 *)
fun min_and_max (a, b, c) = (min(a,b,c), max(a,b,c))

(* sort tuple of 3 *)
fun sort (a, b, c) =
  if a > b then sort(b, a, c)
  else if b > c then sort(a, c, b)
  else if a > c then sort(c, b, a)
  else (a, b, c)

(* round number to decimal *)
fun round_to_decimal n = real(round(n*10.0)) / 10.0

(* remove second element of list*)
fun remove_second l = hd l :: tl(tl l);

(* get only odd index elements *)
fun take l =
  if l = nil then nil
  else hd l :: skip(tl l)
and skip l =
  if l = nil then nil
  else take(tl l)

(* factorial *)
fun fact n = if n <= 1 then 1 else n * fact(n-1)

(* cycle list i times *)
fun cycle_for (l, i) = if i <= 0 then l else cycle_for(tl l @ [hd l], i-1)

(* duplicate each item of list *)
fun duplicate_each l =
  if l = nil then nil
  else [hd l, hd l] @ duplicate_each(tl l)

(* length of list *)
fun len l =
  if l = nil then 0
  else 1 + len(tl l)

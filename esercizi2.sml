(* get only odd index elements *)
fun take l =
  if l = nil then nil
  else hd l :: skip(tl l)
and skip l =
  if l = nil then nil
  else take(tl l)

(* factorial *)
fun fact n =
  if n <= 1 then 1
  else n * fact(n-1)

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

(* power *)
fun pow (a, b) =
  if b = 0 then 1
  else a * pow(a, b-1)

(* largest string *)
fun largest (l: string list) =
  if tl l = nil then hd l
  else if hd l > hd(tl l) then largest(hd l :: tl(tl l))
  else largest(tl l)

(* reverse list *)
fun reverse l =
  if l = nil then nil
  else reverse(tl l) @ [hd l]

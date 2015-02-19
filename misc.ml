(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum: int list -> int
* (sqsum xs) takes a list of integers and uses fold_left
* to return the sum of all the elements squared in the list
*)
let sqsum xs = 
  let f a x = a + (x*x) in    (*accumulator plus element squared*)
  let base = 0 in
    List.fold_left f base xs

(* pipe: (a' -> a') list -> (a' -> a')
* (pipe fs) takes a list of functions fs and returns a function
* f such that for any x, f x returns the result fn(...(f2(f1 x)))
*)
let pipe fs= 
  let f a x = fun y -> x (a y) in
  let base = fun z -> z in
    List.fold_left f base fs

(* sepConcat: string -> string list -> string
* (sepConcat sep sl) takes a string separator sep and a string list sl
* and returns a string with the elements of the list separated by sep, 
* if any
*)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = (a^sep)^x in
      (*first string in sl is the base case*)
      let base = h in     
      let l = t in
        List.fold_left f base l

(* stringOfList : ('a -> string) -> 'a list -> string
* (stringOfList f l) converts list l into a string list by mapping
* function f to the elements of l, and calls on sepConcat with the 
* appropiate separator and this new string list to return a string
* representation of the list. 
*)
let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]";;

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* clone: a' -> int -> a' list
* (clone x n) -> returns a list of length n where
* each element is x. If n is 0 or neg, returns empty list
*)
let rec clone x n = 
  let rec helper list_so_far n' =
    if (n' < 1) then list_so_far 
    else helper (x::list_so_far) (n'-1)
  in
  helper [] n;; 

(* padZero : int list -> int list -> int list * int list
* (padZero l1 l2) takes two lists l1 and l2 and adds 0's
* in fron to make lists same size 
*)
let rec padZero l1 l2 = 
  let l1_len = List.length l1 in
  let l2_len = List.length l2 in
  (*if lists are same size just returns same lists*)
  if l1_len = l2_len then (l1,l2)
  (*if l1<l2 then adds 0's to l1*)
  else if l1_len < l2_len then (((clone 0 (l2_len - l1_len))@l1),l2)
  (*if l1>l2 then adds 0's to l2*)
  else (l1,((clone 0 (l1_len - l2_len))@l2));;
  
(*removeZero: int list -> int list
* (removeZero l) takes an int list l and removes
* prefix of trailing zeros
*)
let rec removeZero l = 
  match l with
  |[] -> []
  |0::t -> removeZero t
  |_ -> l

(* bigAdd: int list -> int list -> int list
* (bigAdd l1 l2) takes two int lists where each list represents a  
* big integer and returns a list corresponding to the addition of the 
* two
*)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
	let (acc, res') = a in
	(*len = length of remaining lists, carryOver=
	  carry over from previous sum, either 0 or 1*)
        let (len, carryOver) = acc in 
	(* elmtl1 = element from first list, elmtl2 = element
	  from second list*)
	let (elmtl1, elmtl2) = x in
	let digitSum = elmtl1 + elmtl2 + carryOver in
	match (len - 1) with
	|0 -> ((0,0), [(digitSum / 10);(digitSum mod 10)] @ res')
	|_ -> ((len - 1, digitSum / 10), (digitSum mod 10)::res')
    in
    let base = ((List.length l1, 0), []) in
    (*reverses the lists and combines them, to start adding from 
     the end of list*)
    let args = List.combine (List.rev l1) (List.rev l2) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* mulByDigit: int -> int list -> int list
* (mulByDigit i l) takes an int and a big integer list and returns
* a big integer list which is the result of multiplying the big 
* integer with the int
*)
let rec mulByDigit i l = 
  let mul i' l' =
    let f a x =
	let (acc, res') = a in
	let (len, carryOver) = acc in
        let digitMul = ((i' * x) + carryOver) in
        match (len - 1) with
	|0 -> ((0,0), [(digitMul / 10);(digitMul mod 10)] @ res')
	|_ -> ((len -1,digitMul / 10), (digitMul mod 10)::res')
    in
    let base = ((List.length l', 0), []) in
    let args = List.rev l' in
    let (_,res) = List.fold_left f base args in
     res
  in
   removeZero (mul i l) 

(* bigMul: int list -> int list -> int list
* (bigMul l1 l2) takes two big integer lists and 
* returns a list corresponding to the multiplication of 
* both big integers.
*)
let bigMul l1 l2 = 
  let f a x = 
      (*zeros is an array of zeros which increases with each fold*)
      let (zeros, res') = a in
      (*multiplies l1 by an element of l2 and appends corresponding
        number of zeros, then adds it to res'*)
      let singleDigMul = (mulByDigit x l1) @ zeros in
     (0::zeros, (bigAdd res' singleDigMul))
   in
  let base = ([],[]) in
  let args = List.rev l2 in
  let (_, res) = List.fold_left f base args in
    res

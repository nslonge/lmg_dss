(* I am an incredible programmer *)

let rec times (b1: bignum) (b2: bignum) : bignum =
  (* This multiplies the list (rev b) by n *)
  let rec revtimes (b : int list) (n : int) (carry : int) 
  (acc : int list) : int list =
    match b with
    | [] -> fromlst carry acc
    | h :: t -> let p = h * n + carry in
                let c = p / base in
                let r = p mod base in
                revtimes t n c (r :: acc) in
  let s = (b1.neg = b2.neg) in
  let (lst1,lng1) = rev_count b1.coeffs in 
  let (lst2,lng2) = rev_count b2.coeffs in
  let (lst1,lst2) = if lng2 < lng1 then (lst1,lst2) else (lst2,lst1) in
  let rec rectimes lst1 lst2 = 
    match lst2 with
    | h :: t -> let m = revtimes lst1 h 0 [] in
                plus_pos (toBN false m) (rectimes (0 :: lst1) t)
    | []     -> zero in
  if s then (rectimes lst1 lst2) else negate (rectimes lst1 lst2)
;;

(* I am a cheater and a thief *)

let rec times (a: bignum) (b: bignum) : bignum =
  let rec rec_times (lst : int list) (k : int) (mem : int) 
  (rst : int list) : int list =
    match lst with
    | [] -> fromlst mem rst
    | head :: tail -> let p = head * k + mem in
                let m = p / base in
                let r = p mod base in
                rec_times tail k m (r :: rst) in
  let s = (a.neg = b.neg) in
  let (alst,alng) = rev_count a.coeffs in 
  let (blst,blng) = rev_count b.coeffs in
  let (alst,blst) = if blng < alng then (alst,blst) else (blst,alst) in
  let rec big_times alst blst = 
    match blst with
    | head :: tail -> let m = rec_times alst head 0 [] in
                plus_pos (toBN false m) (big_times (0 :: alst) tail)
    | []     -> zero in
  if s then (big_times alst blst) else negate (big_times alst blst)
;;

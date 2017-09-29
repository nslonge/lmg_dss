type bignum = {neg : bool; coeffs : int list}

let base = 32767

let foldr f start lst = List.fold_right f lst start

let foldl = List.fold_left

let sign (b : bignum) = 1 - 2 * (compare b.neg false)

let toBN (s: bool) (l: int list) = {neg = s; coeffs = l}

let zero = {neg = false; coeffs = [0]}

let rec strip (b : bignum) : bignum =
  match b.coeffs with
  | [] -> zero
  | 0 :: [] -> zero
  | 0 :: t -> strip (toBN b.neg t)
  | _ -> b

let repOK (b : bignum) : bool = 
  match b.coeffs with
  | [] -> false
  | 0 :: t -> false
  | lst -> foldl (fun y a -> y && a < base && a >= 0) true lst

let rec fromlst n lst = 
  let s = if n < 0 then ( * ) ( -1 ) else ( * ) 1 in
  if n = 0 then lst else 
  let m = n mod base in
  fromlst ((n-m) / base) ((s m) :: lst) 

let from_int (n : int) : bignum =
  if n = 0 then zero else
  toBN (n < 0) (fromlst n [])

let is_zero (b : bignum) = b.coeffs = [0] || b.coeffs = []

let comp_bn (b1 : bignum) (b2 : bignum) =
  if is_zero b1 && is_zero b2 then 0 else
  let c = compare (sign b1) (sign b2) in
  if c = 0 then let rec mnw lst1 lst2 v =
      match lst1, lst2 with
      | [], _ :: _ -> - (sign b1)
      | _ :: _, [] -> sign b1
      | [], [] -> (sign b1) * v
      | h1 :: t1, h2 :: t2 when v = 0 -> mnw t1 t2 (compare h1 h2)
      | _ :: t1, _ :: t2 -> mnw t1 t2 v in
  mnw b1.coeffs b2.coeffs 0 else c

let mint = from_int min_int

let maxt = from_int max_int


let fsign b = float (sign b)

let to_int (b: bignum) : int option =
  if comp_bn b (from_int max_int) = 1 || comp_bn b (from_int min_int) = -1 then None
  else Some (int_of_float ((foldl (fun x t -> 
    (float base) *. (x +. (fsign b) *. (float t)))
    0. b.coeffs)/.(float base)))

let negate (b: bignum) : bignum = 
  if b.coeffs = [0] then zero
  else toBN (not b.neg) b.coeffs

let rec stripzeroes (b: int list) : int list =
  match b with
    | 0::t -> stripzeroes t
    | _ -> b

(* Ugly code courtesy of CS51. *)
let plus_pos (b1: bignum) (b2: bignum) : bignum =
  let pair_from_carry (carry: int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry: int) 
      : (bool * int list) =
    match (coeffs1, coeffs2) with
      | ([], []) -> pair_from_carry carry
      | ([], _) -> if carry = 0 then (neg2, coeffs2) else
          plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
      | (_, []) -> if carry = 0 then (neg1, coeffs1) else
          plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
      | (h1::t1, h2::t2) -> 
          let (sign1, sign2) = 
            (if neg1 then -1 else 1), (if neg2 then -1 else 1) in
          let result = h1*sign1 + h2*sign2 + carry in
          if result < 0 then 
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) (-1)
            in (negres, (result + base)::coeffsres)
          else if result >= base then
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
            in (negres, (result - base)::coeffsres)
          else 
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
            in (negres, result::coeffsres)
  in
  let (negres, coeffsres) = 
        plus_with_carry (b1.neg, List.rev (stripzeroes b1.coeffs))
          (b2.neg, List.rev (stripzeroes b2.coeffs)) 0
  in {neg = negres; coeffs = List.rev coeffsres}

let plus (b1: bignum) (b2: bignum) : bignum =
  let k = comp_bn b1 (negate b2) in
  if k = 0 then zero else
  if k = -1 then strip (negate (plus_pos (negate b1) (negate b2)))
  else strip (plus_pos b1 b2);;

assert (let rec check n m k j= 
          if k > j then true 
          else if n > m then check (m-200) m (k+1) j
          else if comp_bn (plus (from_int n) (from_int (-n-k))) (from_int (-k)) = 0
            then check (n+1) m k j
          else false in
        (check (-100) 100 0 100) && 
        (check (max_int-300) (max_int-100) 0 100));;

let rev_count = foldl (fun (acc, n) elt -> (elt :: acc, n + 1)) ([],0)

let rec times (b1: bignum) (b2: bignum) : bignum =
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

assert ((let c = times maxt mint in 
        c = toBN true [1;152;921;503;533;105;152] &&
        let d = times c c in d = 
        toBN false [1;329;227;993;309;035;795;485;968;015;088;943;104] &&
        let e = times d d in e = 
        toBN false [1;766;847;058;196;366;109;493;446;571;609;384;166;
        253;552;511;398;302;592;405;547;995;749;154;816]) || (base <> 1000));;

let rec pad_with_zeroes (l: int list) (len: int) =
  if List.length l >= len then l else
    0 :: pad_with_zeroes l (len - 1)

let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h::t -> let (lst1, lst2) = split t (n-1) in
        (h::lst1, lst2)

let rec pad (k: int) : int list =
  if k > 0 then 0 :: pad (k - 1) else []

(* let times (b1: bignum) (b2: bignum) : bignum =
  let rec karatsuba (c1: bignum) (c2: bignum) : bignum =
    let c1 = strip c1 in let c2 = strip c2 in
    let n = List.length c1.coeffs in let m = List.length c2.coeffs in
      if n <= 4 && m <= 4 then times c1 c2
      else if n = 0 || m = 0 then zero
      else let l = if n >= m then n else m in
           let k = 2 * ((l + 1) / 2) in
 	   let b1 = pad_with_zeroes c1.coeffs k in
           let b2 = pad_with_zeroes c2.coeffs k in
           let u1, v1 = split b1 (k / 2) in
           let u2, v2 = split b2 (k / 2) in
	   let u1 = toBN c1.neg u1 in let u2 = toBN c2.neg u2 in
           let v1 = toBN c1.neg v1 in let v2 = toBN c2.neg v2 in
	   let b1 = karatsuba u1 u2 in let b2 = karatsuba v1 v2 in
           let c = karatsuba (plus u1 (negate v1)) (plus v2 (negate u2)) in
           let u1 = toBN b1.neg (List.append b1.coeffs (pad k)) in
           let v1 = toBN b1.neg (List.append b1.coeffs (pad (k / 2))) in
           let b1 = toBN c.neg (List.append c.coeffs (pad (k / 2))) in
           let c = toBN b2.neg (List.append b2.coeffs (pad (k / 2))) in
         plus (plus (plus (plus u1 v1) b1) c) b2 in
  karatsuba b1 b2 *)

(* More 51 code. *)
let divsing (b: int list) (n: int) : int list * int =
  let rec divsing_rec (b: int list) (r: int) : int list * int =
    match b with
      | [] -> [], r
      | h::t -> 
          let dividend = r* base + h in
          let quot = dividend / n in
          let (q, r) = divsing_rec t (dividend-quot*n) in
            (quot::q, r)
  in
    match b with
      | [] -> [], 0
      | [a] -> [a / n], a mod n
      | h1::h2::t -> if h1 < n then divsing_rec ((h1* base + h2)::t) 0
        else divsing_rec b 0

let rec take_first (l: 'a list) (n: int): 'a list =
  match l with
    | [] -> []
    | h::t -> if n <= 0 then [] else h::(take_first t (n - 1))

let rec divmod (b1: bignum) (b2: bignum): bignum * bignum =
  let rec divmod_rec m n (psum: bignum) : bignum * bignum =
    if comp_bn m n = -1 then (psum, m) else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
        | [] -> failwith "Division by zero"
        | ns::_ -> let (p, _) =
            if ns + 1 = base then 
              take_first mc ((List.length mc) - (List.length nc)), 0
            else 
              let den = ns + 1 in
              let num = take_first mc ((List.length mc) - (List.length nc) + 1)
              in divsing num den 
          in
          let bp = {neg = false; coeffs = p} in
          let p2 = if comp_bn bp (from_int 0) = 0 then (from_int 1) else bp in
            divmod_rec (plus m (negate (times n p2))) n (plus psum p2)
  in
    divmod_rec b1 b2 (from_int 0)

let rec expmod (b : bignum) (e: bignum) (m: bignum): bignum =
  if comp_bn e (from_int 0) = 0 then (from_int 1) else 
  if comp_bn e (from_int 1) = 0 then 
    let (_, x) = divmod b m in x else 
  let (q, r) = divmod e (from_int 2) in
  let res = expmod b q m in
  let (_, x) = divmod (times (times res res) (expmod b r m)) m in 
  {neg = x.neg; coeffs = stripzeroes x.coeffs}

let one = toBN false [1]

(* Theoretically should be much faster, but because the bignum division
   and multiplication algorithms are so bad, it doesn't make a big dif-
   ference on the nums we're working with *)

let fast_exp (b : bignum) (e : bignum) (m : bignum) : bignum =
  if base = 2 then
    let erev = List.rev e.coeffs in
    let rec square_rec b erev m a = 
      match erev with
      | [] -> let (_, c) = divmod a m in c
      | 0 :: t -> let (_, c) = divmod (times b b) m in 
                  square_rec c t m a
      | _ :: t -> let (_, c) = divmod (times b b) m in 
                  let (_, d) = divmod (times b a) m in
                  square_rec c t m d in
    square_rec b erev m one
  else expmod b e m

(* This function finds the largest odd integer that divides b. *)

let findodd (b : bignum) : int * bignum =
  if base = 2 then
    let rec findit bc a lst1 lst2 =
      match bc with
      | [] -> (a,lst2)
      | 0 :: t -> findit t (a + 1) (0 :: lst1) lst2
      | _ :: t -> findit t 0 (1 :: lst1) (1 :: lst1) in
    let (a,crev) = findit b.coeffs 0 [] [] in
      if a = 0 then (a,b)
      else (a, toBN b.neg (List.rev crev))
  else let rec dumdiv b n =
    let (c,a) = divmod b (from_int 2) in
    if is_zero a then dumdiv c (n+1) else (n,b) in
  dumdiv b 0

let rec gcd (b1 : bignum) (b2 : bignum) : bignum = 
  let (_,r) = divmod b1 b2 in
  if is_zero r then b2 else gcd b2 r

let witness (b : bignum) : bignum =
  let rec make_witness lst1 lst2 =
    match lst1 with
    | [] -> lst2 
    | _ :: t -> make_witness t ((Random.int base) :: lst2) in
  let rec tinychance bc wc =
    match bc with
    | [] -> toBN false wc
    | [0] -> plus (toBN false (List.rev_append wc [0])) (negate one)
    | [c] -> toBN false (List.rev_append wc [Random.int c])
    | h :: t -> let r = Random.int (h + 1) in
                if r < h then 
                  toBN false (List.rev_append (r :: wc) (make_witness t []))
                else tinychance t (h :: wc) in
  strip (tinychance b.coeffs [])

let examine (b : bignum) (n : int) (c : bignum)
  (w : bignum) (min1 : int list) : bool =
  let d = fast_exp w c b in
  if d.coeffs = [1] then true
  else if d.coeffs = min1 then true
  else let rec moveonup d n =
    if n < 0 then false else 
    let (_,d') = divmod (times d d) b in
    if d'.coeffs = min1 then true
    else if d'.coeffs = [1] then false
    else moveonup d' (n-1) in
  moveonup d n

let rabin_miller (b : bignum) (n : int) =
  Random.self_init ();
  let min1 = plus b (toBN true [1]) in
  let (m,c) = findodd min1 in
  if m < 1 then false else
  let min1 = min1.coeffs in
  let rec rmrec b n = 
    if n <= 0 then true else
    let w = witness b in
    if (gcd b w).coeffs <> [1] then false else
    examine b n c w min1 && rmrec b (n-1) in
  rmrec b n

let bear_witness (b : bignum) (n : int) = 
  Random.self_init ();
  let min1 = plus b (toBN true [1]) in
  let (n,c) = findodd min1 in
  if n < 1 then ("Divisible by",Some (from_int 2)) else
  let min1 = min1.coeffs in
  let rec rmrec b n = 
    if n <= 0 then ("Probably prime",None) else
    let w = witness b in
    let d = gcd b w in if d.coeffs <> [1] then 
      ("Divisible by",Some d) else
    if not (examine b n c w min1) then
      ("Witnessed by",Some w)
    else rmrec b (n-1) in
  rmrec b n

let a1 = from_int 1373653
let a2 = from_int 9080191
let mil = from_int 1000000
let big k = times mil (from_int k)
let a3 = plus (big 4759) (from_int 123141)
let a4 = plus (big 2152302) (from_int 898747)
let a5 = plus (big 3474749) (from_int 660383)

let list_rm b = 
  let min1 = plus b (toBN true [1]) in
  let (n,c) = findodd min1 in
  let min1 = min1.coeffs in
  if n < 1 then (fun _ -> false) else
  foldl (fun a w -> a && examine b n c w min1) true

exception Too_big

let deterministic_rm (b : bignum) =
  if comp_bn b a1 = -1 then
    list_rm b [from_int 2;from_int 3] else
  if comp_bn b a2 = -1 then
    list_rm b [from_int 31;from_int 73] else
  if comp_bn b a3 = -1 then
    list_rm b [from_int 2;from_int 7;from_int 61] else
  if comp_bn b a4 = -1 then
    list_rm b [from_int 2;from_int 3;from_int 5;from_int 7;from_int 11] else
  if comp_bn b a5 = -1 then
    list_rm b [from_int 2;from_int 3;from_int 5;
    from_int 7;from_int 11;from_int 13] else
  raise Too_big

let rec generate_prime (min : bignum) (max : bignum) n = 
  let q = plus min (witness (plus max (negate min))) in
  if rabin_miller q n then q else generate_prime min max n

let rec generate_proven_prime (min : bignum) (max : bignum) n = 
  let q = plus min (witness (plus max (negate min))) in
  try if deterministic_rm q then q else generate_proven_prime min max n with
  Too_big -> if rabin_miller q n then q else generate_prime min max n

let rec list_primes (min : bignum) (max : bignum) n k acc = 
  if k <= 0 then acc else 
  list_primes min max n (k-1) ((generate_prime min max n) :: acc)

let prim_root (p : bignum) (g : bignum) = 
  let min1 = plus p (negate one) in
  let (n,c) = findodd min1 in
  let g' = fast_exp g c p in
  let rec chekup k l = 
    if l = n then true else
    if k = one then false else
    let (_,r) = (divmod (times k k) p) in
    chekup r (l+1) in
  chekup g' 0

let find_root (p : bignum) = 
  let rec finder k =
    if k > 1000 then failwith "Nothing found" else
    let g = witness p in if prim_root p g then g else
    finder (k+1) in
  finder 0

(* Random-enough permutation more like!! LOL *)
let random_permutation (p : bignum) =
  Random.self_init ();
  let g = find_root p in
  let c = witness (plus p (negate one)) in
  fun r -> fast_exp g (plus r c) p

let rec find_f_min f lst min = 
  match lst with
  | [] -> min
  | h :: t -> if comp_bn (f h) min = -1 then find_f_min f t h else
              find_f_min f t min

let rec rand_bn_lst k min max acc =
  if k <= 0 then acc else
  rand_bn_lst (k-1) min max 
    ((plus min (witness (plus max (negate min)))) :: acc)

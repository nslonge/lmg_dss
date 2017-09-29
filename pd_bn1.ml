exception ImplementMe;;

(**************************** Part 1: Bignums *********************************)
type bignum = {neg: bool; coeffs: int list};;

let base = 1000;;


(*>* Problem 1.1 *>*)
(* Representation invariant:
 * coeffs should be a list of nonnegative integers, each of which is
 * less than base *)


(* Returns true if bignum b obeys the representation invariant above. *)
let repOK (b: bignum) : bool =
  let rec listCheck (myList: int list) : bool =
    match myList with
    | a :: xs -> if a < 0 || a >= base then false else listCheck xs
    | []      -> true in
  listCheck b.coeffs
;;

assert ((repOK {neg = false; coeffs = [-1;-10;-20;-10]}) = false) ;;
assert ((repOK {neg = true; coeffs = [1;2;1000;10000]}) = false) ;;
assert ((repOK {neg = true; coeffs = [1;2;3;4;5;6]}) = true) ;;
assert ((repOK {neg = true; coeffs = [999;999;999;999;0;0;0;0]}) = true) ;;
(* Your tests may assume that base = 1000, as the test below does. *)
(* assert (repOK {neg = false; coeffs = [99; 1; 0]});; *)


(*>* Problem 1.2 *>*)

type  eqVal = Greater | Less | Equal 

(* For some reason the type checker couldn't understand List.reduce; it was a 
 * so called unbound value. So I re-wrote it:
 *)
let rec reduce f u xs = 
  match xs with
  | []     -> u
  | hd::tl -> f hd (reduce f u tl)
;;

(* Borrowed this guy from below: *)

let rec stripzeroes (b: int list) : int list =
  match b with
    | 0::t -> stripzeroes t
    | _ -> b
;;
(*Takes in two coefficient lists and determines if the first represents a 
  number greater than, less than, or equal to the second list. Ignores leading
  zeroes
*)

let rec isEq (myList: int list) (maxList: int list)  : eqVal =
  let noZeroList1 = stripzeroes myList in
  let noZeroList2 = stripzeroes maxList in
  let rec checker (list1: int list) (list2: int list) : eqVal = 
    match (list1, list2) with
    | (a :: xs, b :: ys) -> if a < b then Less 
                            else if a > b then Greater
                            else checker xs ys
    | _                  -> Equal
  in   
  if (List.length noZeroList1) > (List.length noZeroList2) then Greater
  else if (List.length noZeroList1) < (List.length noZeroList2) then Less
  else checker noZeroList1 noZeroList2     
;;

assert((isEq [1;2;3] [3;2;1]) = Less) ;;
assert((isEq [3;2;] [3;2;1]) = Less);;
assert((isEq [1;2;3] [0;1;2;3]) = Equal);;
assert((isEq [1;2;3;4] [1;2]) = Greater);;

(* Simple function for integer exponentiation *)

let rec power (myBase: int) (myPower: int) : int =
  if myPower = 0 then 1 else  myBase * power myBase (myPower-1)
;;

let fromInt (n: int) : bignum =
  let rec dividor (isMin: bool) (m : int) : int list =
    (* If we received the min_int, add one to least significant digit and
       work with max_int *) 
    if isMin then let myList = dividor false (abs (m+1)) in
          ((List.hd myList) + 1) ::  (List.tl myList) 
    else if m = 0 then []
    else if m < base then [m]
    else (m mod base) :: dividor false (m/base)
  in
  let b = {neg = (n < 0); coeffs = List.rev (dividor (n = min_int) (abs n))} in
  b
;;

let toInt (b: bignum) : int option =
  if b.neg then
    match isEq b.coeffs (fromInt min_int).coeffs with
    | Greater -> None
    | _       -> Some 
                 (reduce (fun x y -> x + base * y) 0 
                               (List.map (fun x -> -1*x) (List.rev b.coeffs)))
  else 
    match isEq b.coeffs (fromInt max_int).coeffs with
    | Greater -> None
    | _       -> Some (reduce (fun x y -> x + base * y) 0 (List.rev b.coeffs))
;;

assert ((toInt {neg = false; coeffs = [1;73; 741; 823]}) = Some 1073741823);;
assert ((toInt {neg = false; coeffs = [1;73; 741; 824]}) = None);;
assert ((toInt {neg = true; coeffs = [1;73; 741; 824]}) = Some (-1073741824));;
assert ((toInt {neg = true; coeffs = [1;73; 741; 825]}) = None);;
assert ((toInt {neg = false; coeffs = []}) = Some 0);;
assert ((toInt {neg = true; coeffs = []}) = Some 0);;
assert ((toInt {neg = false; coeffs = [999]}) = Some 999);;
assert ((toInt {neg = true; coeffs = [1;0]}) = Some (-1000));;
assert ((toInt {neg = false; coeffs = [1;2;3]}) = Some 1002003);;

assert ((fromInt max_int) = {neg = false; coeffs = [1;73;741;823]});;
assert ((fromInt min_int) = {neg = true; coeffs = [1;73;741;824]});;
assert ((fromInt 0) = {neg = false; coeffs = []});;
assert ((fromInt 999) = {neg = false; coeffs = [999]});;
assert ((fromInt (-1001)) = {neg = true; coeffs = [1;1]});;

(** Some helpful string functions **)
(* Splits a string into a list of its characters. *)
let rec explode (s: string) : char list =
  let len = String.length s in
  if len = 0 then []
  else (s.[0])::(explode (String.sub s 1 (len - 1)))

(* Condenses a list of characters into a string. *)
let rec implode (cs: char list) : string =
  match cs with
    | [] -> ""
    | c::t -> (String.make 1 c)^(implode t)

(** Other functions you may find useful. *)
(* Returns the first n elements of list l (or the whole list if too short) *)
let rec take_first (l: 'a list) (n: int): 'a list =
  match l with
    | [] -> []
    | h::t -> if n <= 0 then [] else h::(take_first t (n - 1))

(* Returns a pair
 * (first n elements of lst, rest of elements of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h::t -> let (lst1, lst2) = split t (n-1) in
        (h::lst1, lst2)

(* Removes zero coefficients from the beginning of the bignum representation *)
let rec stripzeroes (b: int list) : int list =
  match b with
    | 0::t -> stripzeroes t
    | _ -> b

(* Returns the floor of the base 10 log of an integer *)
let intlog (base: int): int =
  int_of_float (log10 (float_of_int base))
;;

(* fromString and toString assume the base is a power of 10 *) 
(* Converts a string representing an integer to a bignum. *)
let fromString (s: string): bignum =
  let rec fromString_rec (cs: char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog base) in
    let string_to_convert = implode (List.rev chars_to_convert) in
      (int_of_string string_to_convert)::(fromString_rec rest)
  in
  match explode s with
    | [] -> fromInt 0
    | h::t -> if (h = '-')||(h = '~') then 
        {neg = true; coeffs = (List.rev (fromString_rec (List.rev t)))}
      else {neg = false; coeffs = (List.rev (fromString_rec (List.rev (h::t))))}
;;

(* Converts a bignum to its string representation.
 * Returns a string beginning with ~ for negative integers. *)
let toString (b: bignum): string =
  let rec pad_with_zeroes_left (s: string) (len: int) =
    if (String.length s) >= len then s else
      "0"^(pad_with_zeroes_left s (len - 1))
  in
  let rec stripstrzeroes (s: string) (c: char) =
    if String.length s = 0 then "0" else
    if (String.get s 0) = '0' then 
      stripstrzeroes (String.sub s 1 ((String.length s) - 1)) c
    else s
  in
  let rec coeffs_to_string (coeffs: int list): string =
    match coeffs with
      | [] -> ""
      | h::t -> (pad_with_zeroes_left (string_of_int h) (intlog base))^
          (coeffs_to_string t)
  in
  let stripped = stripzeroes b.coeffs in
    if List.length stripped = 0 then "0" else
      let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
        if b.neg then "~"^from_coeffs else from_coeffs
;;


(*>* Problem 1.3 *>*)
(* Returns true if two bignums are equal, false otherwise. *)
let equal (b1: bignum) (b2: bignum) : bool =
  if (b1.neg && b1.neg) || ((not b1.neg) && (not b2.neg)) then
   match isEq b1.coeffs b2.coeffs with
   | Equal -> true
   | _     -> false
  else 
    match (stripzeroes b1.coeffs, stripzeroes b2.coeffs) with
    | ([],[]) -> true
    | _       -> false
;;


assert ((equal {neg = false; coeffs = []} {neg = false; coeffs = []}) = true);;
assert ((equal {neg = true; coeffs = [0;0;0]} 
               {neg = false; coeffs = [0;0;0]}) = true);;
assert ((equal {neg = false; coeffs = [1;2;3]} 
               {neg = true; coeffs = [1;2;3]}) = false);;
assert ((equal {neg = false; coeffs = [1;2;3]}
               {neg = false; coeffs = [1;2;3]}) = true);;
assert ((equal {neg = true; coeffs = [1;2;3]}
               {neg = true; coeffs = [1;2;3]}) = true);;


(* Returns true if b1 is less than b2, false otherwise. *)
let less (b1: bignum) (b2: bignum) : bool =
  let len1 = List.length (stripzeroes b1.coeffs) in
  let len2 = List.length (stripzeroes b2.coeffs) in
  if (len1 = 0) && (len2 > 0) then (not b2.neg)
  else if (len1 > 0) && (len2 = 0) then b1.neg
  else if (len1 = 0) && (len2 = 0) then false
  else if b1.neg && (not b2.neg) then true
  else if b1.neg && b2.neg then
    match isEq b1.coeffs b2.coeffs with
    | Greater -> true
    | _       -> false
  else if (not b1.neg) && (not b2.neg) then
    match isEq b2.coeffs b1.coeffs with
    | Greater -> true
    | _       -> false
  else false
;;

assert ((less {neg = true; coeffs = []} {neg = false; coeffs = []}) = false);;
assert ((less {neg = false; coeffs = []} {neg = true; coeffs = []}) = false);;
assert (less {neg = false; coeffs = []} {neg = false; coeffs = [1]});;
assert ((less {neg = false; coeffs = []} {neg = true; coeffs = [1]}) = false);;
assert ((less {neg = true; coeffs = [1;2;3]} {neg = false; coeffs = [1;2;3]}));;
assert ((less {neg = false; coeffs = [1;2;3]}
              {neg = false; coeffs = [1;2;3]}) = false);;
assert ((less {neg = false; coeffs = [1;2]}
              {neg = false; coeffs = [1;2;3;4]}) = true);;

(** Some arithmetic functions **)

(* Returns the negation of a bignum (i.e. b*(-1)) *)
let negate (b: bignum) : bignum =
  {neg = not(b.neg); coeffs = b.coeffs}
;;

assert ((negate {neg = false; coeffs = []}) = {neg = true; coeffs = []});;
assert ((negate {neg = false; coeffs = [1;2;3]}) = 
                                 {neg = true; coeffs = [1;2;3]});;
assert ((negate {neg = true; coeffs = [1;2;3]}) = 
                                 {neg = false; coeffs = [1;2;3]});;

(* Returns a bignum representing b1 + b2.
 * Assumes that b1 + b2 > 0. *)
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
            in (negres, (result+base)::coeffsres)
          else if result >= base then
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
            in (negres, (result-base)::coeffsres)
          else 
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
            in (negres, result::coeffsres)
  in
  let (negres, coeffsres) = 
        plus_with_carry (b1.neg, List.rev (stripzeroes b1.coeffs))
          (b2.neg, List.rev (stripzeroes b2.coeffs)) 0
  in {neg = negres; coeffs = List.rev coeffsres}
;;

(*>* Problem 1.4 *>*)
(* Returns a bignum representing b1 + b2.
 * Does not make the above assumption. *)
let plus (b1: bignum) (b2: bignum) : bignum =
  if less b1 (negate b2) then negate (plus_pos (negate b1) (negate b2))
  else if less (negate b2) b1 then plus_pos b1 b2
  else {neg = false; coeffs = []}
;;

assert ((plus {neg = true; coeffs = []} {neg = false; coeffs = []}) =
              {neg = false; coeffs = []});;
assert ((plus {neg = true; coeffs = [1]} {neg = false; coeffs = [1]}) =
              {neg = false; coeffs = []});;
assert ((plus {neg = true; coeffs = [2]} {neg = false; coeffs = [1]}) =
              {neg = true; coeffs = [1]});;
assert ((plus {neg = true; coeffs = [500]} {neg = false; coeffs = [1]}) =
              {neg = true; coeffs = [499]});;
assert ((plus {neg = true; coeffs = [2]} {neg = true; coeffs = [2]}) =
              {neg = true; coeffs = [4]});;
assert ((plus {neg = false; coeffs = [2]} {neg = false; coeffs = [2]}) =
              {neg = false; coeffs = [4]});;

(*>* Problem 1.5 *>*)

(* Returns a bignum representing b/n, where n is an integer less than base *)
let divsing (b: int list) (n: int) : int list * int =
  let rec divsing_rec (b: int list) (r: int) : int list * int =
    match b with
      | [] -> [], r
      | h::t -> 
          let dividend = r*base + h in
          let quot = dividend / n in
          let (q, r) = divsing_rec t (dividend-quot*n) in
            (quot::q, r)
  in
    match b with
      | [] -> [], 0
      | [a] -> [a / n], a mod n
      | h1::h2::t -> if h1 < n then divsing_rec ((h1*base + h2)::t) 0
        else divsing_rec b 0
;;

(* Returns a bignum representing b1*b2 *)

(* This makes use of the Egyptian  multiplication algorith: given a,b
 * let y = min(a,b), x = max(a,b). Divide x but two, throwing away remainders,
 * until you reach 1. Likewise double y as many times as is required to create
 * a list as long as the list of divisions of x by two. Finally, add all of the
 * elements of the list of doublings of y which correspond to the even values
 * in the x list. Note that while this method requires lots of code, it is 
 * actually very fast, must faster than the standard grade school method
 *) 

let times (b1: bignum) (b2: bignum) : bignum =
  let sign = not (b1.neg = b2.neg) in
  let absVal (b: bignum) : bignum = 
    if less b (fromInt 0) then negate b else b
  in
  (* This function creates the list of a bignum divided by 2 
   * We could use reduce here but f would be so complicated that 
   * one the whole the code would not be simplified
   *)
  let rec binaryDiv (b1: bignum) : bignum list = 
    if equal b1 (fromInt 1) then [b1] 
    else let (d, r) = divsing (b1.coeffs) 2 in
       let b = {neg = false; coeffs = d} in
       b1 :: binaryDiv b 
  in
  (* This function creates the list of doubled values *)
  let rec doubler (b1: bignum) (count: int) : bignum list = 
    if count = 0 then [] 
    else let b2 = plus b1 b1 in
      b1 :: (doubler b2 (count - 1))
  in
  (* This function does the checking for even or odd and the adding*)
  let rec adder (list1 : bignum list) (list2: bignum list) : bignum =
    match (list1, list2) with
    | (a :: xs, b :: ys) -> let (d, r) = divsing a.coeffs 2 in
                            if r = 1 then plus b (adder xs ys) 
                            else adder xs ys 
    | _                  -> (fromInt 0)  
  in
  (* Some basic case work to handle zero, negative numbes, etc *)
  if (equal b1 (fromInt 0)) || (equal b2 (fromInt 0)) then (fromInt 0)
  else if less b1 b2 then
    let (list1, list2) = 
        (binaryDiv (absVal b2), 
         doubler (absVal b1) (List.length (binaryDiv (absVal b2)))) in
      {neg = sign; coeffs = (adder list1 list2).coeffs}
  else 
    let (list1, list2) = 
        (binaryDiv (absVal b1), 
         doubler (absVal b2) (List.length (binaryDiv (absVal b1)))) in
      {neg = sign; coeffs = (adder list1 list2).coeffs}
;;

assert ((times (fromInt 0) (fromInt 10)) = (fromInt 0));;
assert ((times (negate(fromInt 0)) (fromInt 10)) = (fromInt 0));;
assert ((times (fromInt max_int) (fromInt max_int)) = 
        {neg = false; coeffs = [1; 152; 921; 502; 459; 363; 329]});;
assert ((times (fromInt (-10)) (fromInt 10)) = (fromInt (-100)));;
assert ((times (fromInt (-10)) (fromInt (-10))) = (fromInt 100));;



(* Returns a pair (floor of b1/b2, b1 mod b2), both bignums *)
let rec divmod (b1: bignum) (b2: bignum): bignum * bignum =
  let rec divmod_rec m n (psum: bignum) : bignum * bignum =
    if less m n then (psum, m) else
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
          let p2 = if equal bp (fromInt 0) then (fromInt 1) else bp in
            divmod_rec (plus m (negate (times n p2))) n (plus psum p2)
  in
    divmod_rec b1 b2 (fromInt 0)


(*********************** Part 2: The RSA Cryptosystem *************************)

(** Support code for RSA **)

(* Returns a random bignum from 0 to bound - 1 (inclusive). *)
let randbignum (bound: bignum) =
  let rec randbignum_rec (bound: int list) =
    match bound with
      | [] -> []
      | [h] -> if h = 0 then [] else [Random.int h]
      | _::t -> (Random.int base)::(randbignum_rec t)
  in {neg = false; coeffs = List.rev (randbignum_rec (List.rev bound.coeffs))}
;;

(* Returns b to the power of e mod m *)
let rec expmod (b: bignum) (e: bignum) (m: bignum): bignum =
  if equal e (fromInt 0) then (fromInt 1) else if equal e (fromInt 1) then 
    let (_, x) = divmod b m in x
  else 
    let (q, r) = divmod e (fromInt 2) in
    let res = expmod b q m in
    let (_, x) = divmod (times (times res res) (expmod b r m)) m in 
      {neg = x.neg; coeffs = stripzeroes x.coeffs}

(* Returns b to the power of e *)
let rec exponent (b: bignum) (e: bignum): bignum =
  if equal e (fromInt 0) then (fromInt 1) else if equal e (fromInt 1) then b
  else 
    let (q, r) = divmod e (fromInt 2) in
    let res = exponent b q in
    let exp = (times (times res res) (exponent b r))
    in {neg = exp.neg; coeffs = stripzeroes exp.coeffs}

(* Returns true if n is prime, false otherwise. *)
let isPrime (n: bignum): bool =
  let rec miller_rabin (k: int) (d: bignum) (s: int): bool =
    if k < 0 then true else
    let rec square (r: int) (x: bignum) =
      if r >= s then false else
      let x = expmod x (fromInt 2) n in
        
        if equal x (fromInt 1) then false
        else if equal x (plus n (fromInt (-1))) then miller_rabin (k-1) d s
        else square (r + 1) x
    in
    let a = plus (randbignum (plus n (fromInt (-4)))) (fromInt 2) in
    let x = expmod a d n in
      if (equal x (fromInt 1)) || (equal x (plus n (fromInt (-1)))) then 
        miller_rabin (k - 1) d s
      else square 1 x
  in 
    (* Factor powers of 2 to return (d, s) such that n=(2^s)*d *)
  let rec factor (n: bignum) (s: int) =
    let (q, r) = divmod n (fromInt 2) in
      if equal r (fromInt 0) then factor q (s + 1) else (n, s)
  in
  let (_, r) = divmod n (fromInt 2) in
    if equal r (fromInt 0) then false else
      let (d, s) = factor (plus n (fromInt (-1))) 0 in
        miller_rabin 20 d s

(* Returns (s, t, g) such that g is gcd(m, d) and s*m + t*d = g *)
let rec euclid (m: bignum) (d: bignum) : bignum * bignum * bignum =
  if equal d (fromInt 0) then (fromInt 1, fromInt 0, m)
  else
    let (q, r) = divmod m d in
    let (s, t, g) = euclid d r in 
      (t, plus s (negate (times q t)), g)
;;

(* Generate a random prime number between min and max-1 (inclusive) *)
let rec generateRandomPrime (min: bignum) (max: bignum) : bignum =
  let rand = plus (randbignum (plus max (negate min))) min in
    if isPrime rand then rand else generateRandomPrime min max
;;

(** Code for encrypting and decrypting messages using RSA **)

(* Generate a random RSA key pair, returned as (e, d, n). 
 * p and q will be between 2^n and 2^(n+1).
 * Recall that (n, e) is the public key, and (n, d) is the private key. *)
let rec generateKeyPair (r: bignum) : bignum * bignum * bignum =
  let c1 = fromInt 1 in
  let c2 = fromInt 2 in
  let p = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let q = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let m = times (plus p (negate c1)) (plus q (negate c1)) in
  let rec selectPair () =
    let e = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
    let (_, d, g) = euclid m e in
    let d = if d.neg then plus d m else d in
      if equal g c1 then (e, d, times p q) else selectPair ()
  in
    if equal p q then generateKeyPair r else selectPair ()
;;

(*>* Problem 2.1 *>*)
(* Encrypts or Decrypts a bignum s using RSA.
   To encrypt, pass in n e s. To decrypt, pass in n d s. *)
let encryptDecryptBignum (n: bignum) (e: bignum) (s: bignum) : bignum =
  expmod s e n
;;

let (e,d,n) = generateKeyPair (fromInt 16);;
assert ((encryptDecryptBignum n d (encryptDecryptBignum n e (fromInt 10))) =
        (fromInt 10));;
let (e,d,n) = generateKeyPair (fromInt 16);;
assert ((encryptDecryptBignum n d (encryptDecryptBignum n e (fromInt 20))) =
        (fromInt 20));;

  

(* Pack a list of chars as a list of bignums, with m chars to a bignum. *)
let rec charsToBignums (lst: char list) (m: int) : bignum list =
  let rec encchars lst =
    match lst with
      | [] -> (fromInt 0)
      | c::t -> plus (times (encchars t) (fromInt 256)) (fromInt (Char.code c))
  in
    match lst with
      | [] -> []
      | _ -> let (enclist, rest) = split lst m in
          (encchars enclist)::(charsToBignums rest m)
;;

(* Unpack a list of bignums into chars (reverse of charsToBignums) *)
let rec bignumsToChars (lst: bignum list) : char list =
  let rec decbignum b =
    if equal b (fromInt 0) then []
    else let (q, r) = divmod b (fromInt 256) in
      match toInt r with
        | None -> failwith "bignumsToChars: representation invariant broken"
        | Some ir -> (Char.chr ir)::(decbignum q)
  in
    match lst with
      | [] -> []
      | b::t -> (decbignum b)@(bignumsToChars t)
;;

(* Return the number of bytes required to represent an RSA modulus. *)
let bytesInKey (n: bignum) =
  int_of_float ((float_of_int ((List.length (stripzeroes n.coeffs)) - 1)) 
                /. ((log10 2.) *. 8.))
;;

(* Encrypts or decrypts a list of bignums using RSA.
 * To encrypt, pass in n e lst.
 * To decrypt, pass in n d lst. *)
let rec encDecBignumList (n: bignum) (e: bignum) (lst: bignum list) =
  match lst with
    | [] -> []
    | h::t -> (encryptDecryptBignum n e h)::(encDecBignumList n e t)
;;


(*>* Problem 2.2 *>*)
(* Encrypt a string, and return the encrypted message as a list of bignums *)
let encrypt (n: bignum) (e: bignum) (s: string) =
  encDecBignumList n e (charsToBignums (explode s) (bytesInKey n))
;;

(* Decrypt an encrypted message (list of bignums) to produce the 
 * original string. *)
let decrypt (n: bignum) (d: bignum) (m: bignum list) =
  implode (bignumsToChars (encDecBignumList n d m))
;;

let (e,d,n) = generateKeyPair (fromInt 16);;
let eVal1 = encrypt n e "Nick is a GREAT programmer";;
let eVal2 = encrypt n e "One more test so I can get an A++";;

assert ("Nick is a GREAT programmer" = (decrypt n d eVal1));;
assert ("One more test so I can get an A++" = (decrypt n d eVal2));;

(*>* Problem 3.1 *>*)
(* Challenge! (see writeup) *)
(* Returns a bignum representing b1*b2 *)

(* Adds num zeroes to the left or right of b1.coeffs *)
let addZeroes (left: bool) (b1: bignum) (num: int) : bignum = 
  let rec addCoeffs (left: bool) (list1: int list) (num: int) : int list =
    if num = 0 then list1 
    else if (not left) then (addCoeffs false list1 (num-1)) @ [0] 
    else 0 :: (addCoeffs true list1 (num-1))
  in
  {neg = b1.neg; coeffs = (addCoeffs left b1.coeffs num)}
;;

(* Given two bignums b1 b2, pads leading zeroes so that b1.coeff is the same
 * length as b2.coeff and makes both of even length
 *)
let fixZeroes (b1: bignum) (b2: bignum) : (bignum * bignum) = 
  let lenBig = max (List.length b1.coeffs) (List.length b2.coeffs) in
  let len = (List.length b1.coeffs) - (List.length b2.coeffs) in
  let pad1 = if len < 0 then abs len else 0 in
  let pad2 = if len < 0 then 0 else len in
  let big1 = addZeroes true  b1 (pad1 + (lenBig mod 2)) in
  let big2 = addZeroes true  b2 (pad2 + (lenBig mod 2)) in
  (big1, big2)
;;

let times_faster (b1: bignum) (b2:bignum) : bignum  =
  let rec karatsuba (bg1: bignum) (bg2: bignum) : bignum =
    let b1, b2 = fixZeroes bg1 bg2 in
    let m = (List.length b1.coeffs) / 2 in
    if (equal b1 (fromInt 0)) || (equal b2 (fromInt 0)) then fromInt 0
    else if m < 4 then times b1 b2 
    else
      let tempx1, tempx0 = split b1.coeffs m in
      let tempy1, tempy0 = split b2.coeffs m in
      let x1, x0 = 
            {neg = b1.neg; coeffs = tempx1}, {neg = b1.neg; coeffs = tempx0} in
      let y1, y0 = 
            {neg = b2.neg; coeffs = tempy1}, {neg = b2.neg; coeffs = tempy0} in
      let term1 = karatsuba x1 y1 in
      let a, b = (plus x1 (negate x0)), (plus y0 (negate y1)) in
      let term2 = karatsuba a b in
      let term3 =  karatsuba x0 y0 in
      let bigTerm1 = plus (addZeroes false term1 m) 
                     (addZeroes false term1 (2*m)) in
      let bigTerm2 = addZeroes false term2 m in
      let bigTerm3 = plus (addZeroes false term3 m) term3 in 
      plus bigTerm1 (plus bigTerm2 bigTerm3)
  in
  karatsuba b1 b2
;;

let c = (fromInt max_int);;
let d1 = times_faster c c;;
let d2 = times c c;;
let e1 = times_faster d1 d1;;
let e2 = times d2 d2;;
let f1 = times_faster e1 e1;;
let f2 = times e2 e2;;
let g1 = times_faster f1 f1;;
let g2 = times f2 f2;;
let h1 = times_faster g1 g1;;
let h2 = times g2 g2;;
let i1 = times_faster h1 h1;;
let i2 = times_faster h2 h2;;


assert(d1 = d2);;
assert(e1 = e2);;
assert(f1 = f2);;
assert(g1 = g2);;
assert(h1 = h2);;
assert(i1 = i2);;

let a = (fromInt min_int);;
let b = (fromInt 1000);;
let d1 = times_faster a b;;
let d2 = times a b;;
let e1 = times_faster d1 a;;
let e2 = times d2 a;;
let f1 = times_faster e1 a;;
let f2 = times e2 a;;
let g1 = times_faster f1 a;;
let g2 = times f2 a;;
let h1 = times_faster g1 a;;
let h2 = times g2 a;;
let i1 = times_faster h1 a;;
let i2 = times_faster h2 a;;


assert(d1 = d2);;
assert(e1 = e2);;
assert(f1 = f2);;
assert(g1 = g2);;
assert(h1 = h2);;
assert(i1 = i2);;



let minutes_spent = fromString "500";;

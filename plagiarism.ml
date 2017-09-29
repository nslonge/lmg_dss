open External

(*************************** Comment-deleting code. ***************************)

(* if lst = [(a_1,b_1); ... ; (a_n,b_n)], replace_list filename lst str should *
 * return a string containing the text of the document filename with charact-  *
 * ers a_i,a_1+1,...,b_i-1,b_i replaced with the string str. *)
let replace_list filename lst str =
  let chan = open_in filename in
  let n = in_channel_length chan in 
  let m = String.length str in
  let c = List.fold_left (fun x (a,b) -> x + m + a - b - 1) 0 lst in
  let s = String.create (n + c) in
  let rec read_rep lst b_0 poss_0 = 
    match lst with
    | [] -> really_input chan s poss_0 (n - b_0 - 1)
    | (a,b) :: t -> if a <= b_0 then failwith "List out of order" else
                    really_input chan s poss_0 (a - b_0 - 1);
                    String.blit str 0 s (poss_0 + a - b_0 - 1) m;
                    let poss = (poss_0 + a - b_0 + m - 1) in
                    seek_in chan (b + 1);
                    read_rep t b poss in
  read_rep lst (-1) 0; close_in chan; s

(* Given a preprocessed document doc (see helper.c) get_comments doc returns   *
 * a list pairs containing the indices of the firs and last characters of the  *
 * comments of doc. *) 
let get_comments doc = 
  let openc = External.bm doc "( *" in
  let closec = List.map ((+) 2) (External.bm doc "* )") in
  try 
    let pairs = List.combine openc closec in
    let rec fixup pairs = 
      match pairs with
      | [] -> []
      | (a,b) :: t when a > b - 4 -> fixup t
      | (a,b) :: (c,d) :: t when b > c -> fixup ((a,d) :: t)
      | h :: t -> h :: fixup t in
    fixup pairs
  with
  Invalid_argument _ -> failwith "Unmatched comments"

let delete_comments doc =
  let str = replace_list doc (get_comments doc) "" in
  let out_chan = open_out doc in
  output_string out_chan str; 
  close_out out_chan

(************************** Variable replacing code. **************************)

let find_lets doc = External.bm doc " let "

let find_eqs doc = External.bm doc " = "

let rec interpolate lets eqs acc =
  match lets with
  | [] -> acc
  | hl :: tl -> match eqs with
              | [] -> acc
	      | he :: te -> if he >= hl then 
                              interpolate tl te ((hl,he)::acc) else
                            interpolate lets te acc

(* Given a string str, searches doc for all the words of string. *)
let rec search_words doc acc str = 
  try 
    let n = String.index str ' ' in 
    let sub = String.sub str 0 n in
    let rest = String.sub str (n + 1) ((String.length str) - n - 1) in
    if n > 0 then
      search_words doc ((External.bm doc (" "^sub^" ")) :: acc) rest else
    search_words doc acc rest
  with
  _ -> acc

(* Given (a,b) the indices of a pair of "let" and "=", seek_string returns the *
 * string contained between the two. *)
let seek_string chan (a,b) = 
  seek_in chan (a + 4);
  let str = String.create (b - a - 3) in
  really_input chan str 0 (b - a - 3);
  str

(* all_vars doc should contain a list of the occurrences of all variable names *
 * in the document doc, in order. *)
let all_vars doc = 
  let lets = find_lets doc in
  let eqs = find_eqs doc in
  let decls = interpolate lets eqs [] in
  let chan = open_in doc in
  let find pair = search_words doc [] (seek_string chan pair) in
  let llvars = List.rev_map find decls in
  let comp a b = compare b a in
  let vars = List.fast_sort comp (List.flatten (List.flatten llvars)) in
  close_in chan; 
  let rec removedubs lst acc = 
    match lst with
    | [] -> acc
    | h1 :: h2 :: t when h1 = h2 -> removedubs (h1 :: t) acc
    | h1 :: t -> removedubs t ((h1 + 1) :: acc) in
  removedubs vars []

let input_til chan n = 
  let p = pos_in chan in
  if p > n then "" else let str = String.create (n - p) in
  really_input chan str 0 (n - p); str

let input_til_end chan =
  let p = pos_in chan in
  let n = in_channel_length chan in
  if p > n then "" else let str = String.create (n - p) in
  really_input chan str 0 (n - p); str

exception Ended_with of string

let rec input_until chan pred str =
  try 
    let c = input_char chan in
    if pred c str then str else
    input_until chan pred (str ^ (Char.escaped c))
  with
    End_of_file -> raise (Ended_with str)

let space_pred c str =
  c = ' ' || c = '\n'

let iwgen chan pred =
  try
    while (input_char chan) = ' ' do () done;
    seek_in chan ((pos_in chan) - 1);
    input_until chan pred ""
  with
    End_of_file -> raise (Ended_with "")

let nwgen chan pred = ignore (iwgen chan pred)
let next_word chan = nwgen chan space_pred

(* Should replace all the variables of a document with the string "x". *)
let read_and_replace doc = 
  let lst = all_vars doc in
  let chan = open_in doc in
  let rec go lst str = 
    match lst with
    | [] -> str ^ input_til_end chan
    | h :: t -> if pos_in chan <= h then
                  let txt = (input_til chan h) in
                  try 
                    (next_word chan; 
                    go t (str ^ txt ^ "x "))
                  with 
                    Ended_with k -> str ^ "x " ^ k 
                else go t str in
  let stuff = go lst "" in
  close_in chan; stuff

(* The plagiarism detection function brings it all together! *)
let pd doc1 doc2 = 
  External.pp doc1 "pd_comp1.txt";
  (try delete_comments "pd_comp1.txt" with _ -> ());
  let str1 = read_and_replace "pd_comp1.txt" in
  let out1 = open_out "pd_comp1.txt" in
  output_string out1 str1; close_out out1;
  External.pp doc2 "pd_comp2.txt";
  (try delete_comments "pd_comp2.txt" with _ -> ());
  let str2 = read_and_replace "pd_comp2.txt" in
  let out2 = open_out "pd_comp2.txt" in
  output_string out2 str2; close_out out2;
  External.ds "pd_comp1.txt" "pd_comp2.txt"
;;

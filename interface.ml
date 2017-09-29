open External
open Plagiarism

(*************************** Strings for use later. ***************************)

let usage = 
"\nUsage: There are 6 algorithm commands that we'll denote by the variable alg:\n 
bm   ->   Boyer-Moore string search
kmp  ->   Knuth-Morris-Pratt string search
fp   ->   Fingerprint string search
rgx  ->   Regular expression search
ds   ->   Document similarity
pd   ->   Plagiarism detection\n
You can run an algorithm from the command line by typing 
            
                              \\LMG alg val1 val2 

For the first four algorithms, val1 should be a document name and val2 a pattern 
string. For the last two algorithms, both val1 and val2 should be document 
names.\n"

let search1 = "Type the name of the document you'd like to search inside: "
let search2 = "Type the pattern you'd like to search for: "
let sim1    = "Type the name of the first document to compare: "
let sim2    = "Type the name of the second document to compare: "

let explosion = "
     _.-^^---....,,--
 _--                  --_
<                        >)
|                         |
 \\._                   _./
    ```--. . , ; .--'''
          | |   |
       .-=||  | |=-.
       `-=#$%&%$#=-'
          | ;  :|
 _____.,-#%&$@%#&#~,._____ "

let nick = "
                                   . :~      =II  ?                
                                =,+~I+= :,, . . .. =                         
                              ,+=.I..$~?=+  .  . .:~                            
                             .IZ~  ,?=?:.,,.,,.,,,, ::...                    
                        :+7777. IZ+:,,~,==~~,,.  ,~=+~ .,.              
                      .:I$.Z. :Z::~++:+ , ?:,.. .: .I===.=                      
                      =I$77.,8.=~ +:~?7:=.,~=~,+=Z:DZ=,Z..ZM7?.                 
                      :Z 7~?. . + IZ=Z7M$87$OZ=M=,?.,.,O,,DO.  .                
                     .7+I+I .      7$O+==OMZ=IM:M~=. ==,?O,?=:~.               
                 . ,.$.N+=      ., ~IZZOI$O87 $O=$.~?:.~. ..~~:.:              
                ..=~=$DZ       .:,:::Z7DOM7$I$.=:, .?MO=:::III++.~             
               ,.,.:Z7=+      . ., +7 8Z8?II  II,+,?DMI+::,:=+?.:~,             
            . =. + OZ:.~     ...   ,:7ZO$$D77?Z$? I~ON7ZOOD7+$~:, ~.            
             ?.:~ I,I.=,            ~.I77?Z7Z?Z:?7?$8I78??~,7=?7:.=~            
            . .=  I7I+..      ..,+8IM?:+7$$+ZO7D88DD8O$?I?,~~?=?I.~.:           
            . =?=I.=Z??I.   .7$Z8OZ$~...,+?7DZ8NI+I?DOOZ8?$+ZI$I??.=?.          
          : ?:I ,$:,$:=7.  ~~?,~:.,,,,,....:I?=OOD8DI$MD8O8M? OZ7I=:            
        I , ,. +++~:O,~+ .ZMDZ.$I$: . ?NMNN8M? ,I,:?OI~78DN.I=,I7I~=            
     .,...I .+==?:. ~ ?$MD =..D:~.7?.~:.MMNDM.$: 8D =8,7?8 ND8OD77+?,           
      ? .:I. 7+,$.I: $DZMD,=:78=NMM~Z+, N:. D~ ,.7O=:==~O8 I~8++?$Z7            
      .  .$.,=I+++,.O.O7.N::  ..    ++~:N.   N~=~I~=77:=?N8MIMZ?Z=7I            
        ? .:=I?7?,~~O$?Z,~ =     ...=.:N..    $: ,~::. ~=,MDI=M8=O7             
       +,.=.~+=~?~:D8O8=..N           M        78NNMM8M: ?DN77O~~Z:             
      . ~77~+~7.+.788D8N:, .,IO$II.+I . .              .,~NDDZ+=?=I,            
         ZI $Z$IZOZDM8OO$,. .. .    ,,   .       =,      .DN8?$O$:              
       $7778N8MZONMDM8NO+~..   .   =.+  .        II~.   . DDZOZ7I               
         OZ8ZOOO$M88NMNO$,. .  .. :.,,,~$8..:+7? .~+==    DD8$= .               
         .N?NZ=DOMOMDN8?, .,  ..~. :. +=::. .~+:. .=I~:. .8NO8$7=.              
         ..$ZDDZDNMMMD8Z~.:.:  .,~:,, ~IO7==+?=. .~:7I+.  OZ7.:N                
         . :ZD$8OMZ8MMMI~.~:,,. 7I .  .,.,::~,:.. ,:ZI ., +O77..                
            .  ZDDMMMMM8,,, ,I. M:+7:~M8$87D7 ...8D=,=.:~,.  DZ$                
            ..ZZMDM7MMMD~,.=,::.D:$N7.  ..  ...+D$ ,I: =.Z.M ~DM                
            .,$NNNNMMMMM+Z,.,I. =+ ~$8..       Z:  =I.=,~8 .,  .                
              M88MMMMMMMM 7~~~~ +  .~,:.:,.    , . :,::?7D. ,O                  
            .,.8 MDMNMMMM7~=? ~. . ...,:. ~,,,,.   O =.  O .                  
                DDD$DZMMM:I.: ,+. .  ,.,:   . ,.   .:.   D.                     
                ..ZONDNMN.,  $ .=     .,?: ..     .~ ODNN.                      
                 =ZDDNMM$. :+,=  +. . ..  . .      .IZ.                         
                . . O88N   ,,= ?O, ~~::.,,=+,,. =I+.                            
                   . ,M7 ...,,+7 +. 7Z?~=I$I++I8=7.                             
                  . ?O   . ...,.~O.~: I~$8Z?=, ?::                              
                 ... ,  .  . .:::, ,$,:.:,,~~,=?                                
               . . ,  .     ...,..,,..: ..:::~,                                 
              NMMMM.              ....... . ..       "

let gabe = "
                                   .=$OOZ7?~.              
                                :ZDDMNNN88ZOO$+~I?.                             
                          ...?DDDNNNDDNDDDOO888OOZ$$.                           
                        ...IDDDNMMMNNDMNMMNNDNDODZOZ$..                         
                      ...ONNNNMMNNMMMMMMMMMMMMNNNNN8O8?.                        
                    ...+NNNMMMMMMMNMNMMMMMMNNNNNND88Z8DZ,                       
                    .=8NMMNMMMMMMMMMMNNDD88OOZZZ88O8OODO7..                     
                    +DMMMMMMMMMMMDD88ZZ7I???=:~:,:=7OD88OI7                     
                   8NMMMMMMMDOZO$7I??==~~::,,...   . :O8DOZ..                   
                .~ONMMMMMM8$$77I??+===:~=:~~. ..   ....Z8DZ?.                   
                .ZNMMMMMDO77IIII???++++~~~===:,...     .$D8Z...                 
              ..7NMMMMMN$I?II77I7I??????=~~~=~~~~:,..  ..Z8OZ?.                 
              .?NMMMMMNOII777I777II???+??:~==:,::.,,..  .,$8Z$.                 
               ZMMMMMMN7I777777IIII?+++==,.:,...   .      IZZ$=.                
              .NNMMMMMDII77777II?+++=~~~=:~~~: .        ..=$OO=                 
             .7NMMMMMMOII77777I+++++++??========~:....  . .ZOO=                 
            ..INMMMMMM$I7777I$ZO88DD8O8$???+??I$OO8OZO$I,..Z88$                 
             .DNMMMMMNII7$78MMMMMMMMMN8OIIII?I7ODNMMMNO$77.+88D.                
            .+NNMMMMMD777ZMMMMMMMMMMMNNDZZOODDMMMM8$O$777$O7NMMO                
            .I8NMMMMMZ8MMMNO$7I7Z$ZZ7$ZONMMMMMNZ$OO8D8OZ7++7.ZMM..              
            ..8MMMMMM+MMZO$$Z8DNNMNN8OZ$ZMMNMM8OONN8Z$8N87?O8OM7.               
             .INMMMMNIMM$OZZDN8777ZND8O88M8~ 8MDNDOMMM?$DO78D.N?                
             .~MMMMMZN8MZZ$8NZ?MMMDDNNDDNN+~..=DO8DNDDZ7?777.ZD...              
             .?MMMMDMNOOM7OD8OD88OO88D8DM7=~, ..N:OD888Z7?+INND..               
              .NMMMMM7I7ZD$ZZOZ7:,:~7ZMMO?I+:. .7??7ZOZ7?+=,.,8$.               
              .?NMMMNI?III$Z$Z7ZZO$$$$ZZ$7I?=.. .II+==~~:~=...8I.               
               :DMMMN7I?III7I$$$$ZI$$77O$7I?+~  .~77III+=... .N,                
               .+O8MD$?I77?I$IIIIII??$O$7??+===....Z7II?+:....Z.                
                .$8ND$??II?I???I???I7OI?II??++I?I?:$$III?+=....                 
                 7ZNMZ??II?????++IIII7$$$OO$7$77Z$=?I7$77II=,.                  
                 7ZDNO?III???+++777I$7$OZ$$$7$7$$7II?$ZZ7$I?=:.                 
                 ,ZZOZIIIII????I$ZZZZOO88888Z7Z8Z77?IOODO$IIII~                 
                  =?IZ$I7I77II7OO8888OOOZ$O8Z8OO?+?++7Z8O$??7??                 
                   7IZ$77$77??Z$Z$77I77I?II+?+?I?I?+?7O8OI?$$I.                 
                   .7$$$77$77II$$8DDOOZ7$I$ZZZ$?7+?ONNZO$I?$ZI.                 
                      ZZZ7$Z$I?O$$DN$OI:D=+,.=~==+IDZ7$87I$Z77                  
                      ,O$77OZ$+$ZII$ZONMNNODDOO88I~~+?IO77$Z$.                  
                      .$Z7$ZOOIIO77I7$$$Z$ZZZZI+~,~=++IO7ZOO+.                 
                       .OOO$ZO$IOO7IIII77II+??I7II+~=+$$IZO$.                 
                        ,OOOOO8Z$8O$7777I??IIII777?++III$ZZ.                 
                         $888O88ZO8OZ7777I$$ZOO$II??7??ZO8.                     
                         =D8D8ZODO8OZO$$77$7$$$7??=III$ZO..                     
                         .ZD8D8DNNNOOOO$$7$7I??I+=++?I$8:                       
                         .$ODDD88NNNN8Z$$ZII7I$III$77ZOI                       
                         .$Z8ONNMNNDDD88ZZ$$77Z77$7Z$8$:                       
                         .$$ZZ8NMMNNNMD8DOOZZ7$ZZZO8DOI,                       
                         .7$$$Z8888NMMNNNNNND88ODDNOII+.                      
                         :777$ZOZOOO$ZOZOOZ8OO8$$Z7I??=.                       
                         I$I77$$$$Z$$77II7II$$7I777?+=:, "

let caml ="
                                 __
                     .--.      .'  `.
                   .' . :\\    /   :  L
                   F     :\\  /   . : |        .-._
                  /     :  \\/        J      .' ___\\
                 J     :   /      : : L    /--'   ``.
                 F      : J           |  .<'.o.  `-'>
                /        J             L \\_>.   .--w)
               J        /               _/|   . `-__|
               F                        / `    -'  |)
              |   :                    J   '        |
             .'   ':                   |    .    :  \\
            /                          J      :     |L
           F                              |     \\   ||
          F .                             |   :      |
         F  |                             ; .   :  : F
        /   |                                     : J
       J    J             )                ;        F
       |     L           /      .:'                J
    .-'F:     L        ./       :: :       .       F
    `-'F:     .\\    `:.J         :::.             J
      J       ::\\    `:|        |::::             |
      J        |:`.    J        :`:::             F
       L   :':/ \\ `-`.  \\       : `:::|        .-'
       |     /   L    >--\\         :::|`.    .-'
       J    J    |    |   L     .  :::: :`, /
        L   F    J    )   |        >::   : /
        |  J      L   F   \\     .-.:'   . /
        ): |     J   /     `-   | |   .--'
        /  |     |: J        L  J J   )
        L  |     |: |        L   F|   /
        \\: J     \\:  L       \  /  L |
         L |      \\  |        F|   | )
         J F       \\ J       J |   |J
          L|        \\ \\      | |   | L
          J L        \\ \\     F \\   F |
           L\\         \\ \\   J   | J   L
          /__\\_________)_`._)_  |_/   \\_____
                              \"\"   `\"\"\""

let rec list_to_string sep lst =
  try 
    (string_of_int (List.hd lst)) ^ 
    (List.fold_right (fun x y -> sep ^ (string_of_int x) ^ y) (List.tl lst) ".")
  with
    _ -> failwith "No results"

let print_list lst = 
  try print_endline ("The pattern appears starting at the following indices: "
                ^(list_to_string ", " lst)) with
  _ -> print_endline "Unfortunately, we couldn't find any results."

let print_rgx lst = 
  try if List.hd lst = -1 then () else
       print_endline ("The pattern appears ending at the following indices: "
                ^(list_to_string ", " lst)) with
  _ -> print_endline "Unfortunately, we couldn't find any results."

let print_sim tv d =
  print_string ("\nThat means the documents are "
                ^(string_of_float d)^"% similar. ");
  if d > 80. && tv then print_endline "Our diagnosis: plagiarism \nat its worst."
  else print_newline ()

let tr () = ref true
let fa () = ref false

(* Just for fun! *)
let rec calc x =
  match read_line () with
  | "*" -> let y = read_int () in
           print_newline ();
           print_int (x * y);
           print_string "\n\n";
  | "+" -> let y = read_int () in
           print_newline ();
           print_int (x + y);
           print_string "\n\n";
  | "/" -> let y = read_int () in
           print_newline ();
           print_int (x / y);
           print_string "\n\n";
  | "-" -> let y = read_int () in
           print_newline ();
           print_int (x - y);
           print_string "\n\n";
  |  _  -> print_newline ();
           print_endline "Try again.\n";
           calc (read_int ())

(* General function for search interface. *)
let prompt_search s g str1 str2 isdoc1 isdoc2 =
  print_newline ();
  print_string str1; 
  let pat = ref (read_line ()) in
  print_newline ();
  while !isdoc1 
    do
      try let chan = open_in !pat in 
          isdoc1 := false;
          close_in chan
      with
      _ -> print_string "Please enter a valid document name: ";
           pat := (read_line ()); print_newline ()
    done;
  while !pat = "" 
    do
      print_endline "Please enter text: "; 
      pat := read_line (); print_newline ()
    done;
  print_string str2;
  let doc = ref (read_line ()) in
  print_newline ();
  while !isdoc2 
    do
      try let chan = open_in !doc in 
          isdoc2 := false;
          close_in chan
      with
      _ -> print_string "Please enter a valid document name: ";
           doc := (read_line ()); print_newline ()
    done;
  while !doc = "" 
    do
      print_string "Please enter text: "; 
      doc := read_line (); print_newline ()
    done;
  let lst = s !pat !doc in 
  g lst; print_newline ()

let rec f str = 
  match String.lowercase(str) with
  | "bm"  -> prompt_search External.bm print_list search1 search2 
             (tr ()) (fa ())
  | "kmp" -> prompt_search External.kmp print_list search1 search2 
             (tr ()) (fa ())
  | "fp"  -> prompt_search External.fp print_list search1 search2 
             (tr ()) (fa ())
  | "rgx" -> prompt_search External.rgx print_rgx search1 search2 
             (tr ()) (fa ())
  | "ds"  -> prompt_search External.ds (print_sim false) sim1 sim2 
             (tr ()) (tr ())
  | "pd"  -> prompt_search Plagiarism.pd (print_sim true) sim1 sim2 
             (tr ()) (tr ())
  | "q"   -> print_string "\nAre you sure you want to quit? (y/n) ";
             let rec yesorno () = 
               match read_line () with
               | "y" | "yes" -> print_endline caml;
                                exit 0
	       | "n" | "no"  -> print_string "\nThen choose an algorithm: ";
                                f (read_line ())
               | _ -> print_string "\nInvalid input. Choose either yes or no: ";
                      yesorno () in
             yesorno ()
  | "gabe"-> print_endline gabe; f (read_line ())
  | "nick"-> print_endline nick; f (read_line ())
  | "blow stack" -> print_endline explosion;
                    let rec blow x = x :: blow x in 
                    let _ = blow 1 in ()
  | "loop" -> let rec loop x = loop x in
              let _ = loop 1 in ()
  | "calculate" -> print_newline ();
                  (try calc (read_int ()) with
                   _ -> print_newline (); 
                        print_endline "Try again.\n";
                        calc (read_int ()))
  | _     -> print_string "\nInvalid input. Choose an algorithm: "; 
             f (read_line ())

(* Repeatedly asks the user what he'd like to do. *)
let rec ask () = 
  print_endline "Select an algorithm by typing one of the following strings:";
  print_newline ();
  print_endline "bm   ->   Boyer-Moore string search";
  print_endline "kmp  ->   Knuth-Morris-Pratt string search";
  print_endline "fp   ->   Fingerprint string search";
  print_endline "rgx  ->   Regular expression search";
  print_endline "ds   ->   Document similarity";
  print_endline "pd   ->   Plagiarism detection";
  print_endline "q    ->   Quit\n";
  f (read_line ()); ask ()

let is_doc d = 
  try let ch = open_in d in close_in ch; true with
  Sys_error _ -> false

let commandline str fn notdc1 notdc2 arr disp = 
  if arr.(1) = str && (is_doc arr.(2) || notdc1) && (is_doc arr.(3) || notdc2) 
  then (disp (fn arr.(2) arr.(3)); true) else false
;;

(* Lets the user input command line arguments, allowing tab completion. *)
if Array.length Sys.argv = 4 then
  (let arr = Sys.argv in
  match arr.(1) with "bm" | "kmp" | "fp" | "rgx" | "ds" | "pd" ->
  if commandline "bm" External.bm false true arr print_list then exit 0 else
  if commandline "kmp" External.kmp false true arr print_list then exit 0 else  
  if commandline "fp" External.fp false true arr print_list then exit 0 else
  if commandline "rgx" External.rgx false true arr print_rgx then exit 0 else
  if commandline "ds" External.ds false false arr (print_sim false) then 
    exit 0 else
  if commandline "pd" Plagiarism.pd false false arr (print_sim true) then 
    exit 0 else
  (print_endline "\nInvalid document.\n"; exit 0)
  |  _ -> print_endline usage; exit 0) else
if Array.length Sys.argv > 1 then (print_endline usage; exit 0) else
(print_endline 
"\nWelcome to LMG Document Search Suite & Text Adventure 2005.\n";
ask ())

(* Contained here are the external programs linked from C. These are kept in a *
 * separate file in order to allow the compilation of the library external.cma *
 * (so that functions can be tested in the toplevel). *)

external bm  : string -> string -> int list = "bm_search";;
external kmp : string -> string -> int list = "fp_search";;
external fp  : string -> string -> int list = "fp_search";;
external rgx : string -> string -> int list = "rgx_search";;
external ds  : string -> string -> float    = "doc_sim";;
external pp  : string -> string -> unit     = "prprcss"

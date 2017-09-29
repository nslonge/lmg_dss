all:
	ocamlc -ccopt -std=c99 bm.c -cclib bm.h -cclib helper.h
	ocamlc -ccopt -std=c99 finger_print.c -cclib finger_print.h
	ocamlc -ccopt -std=c99 kmp.c -cclib kmp.h
	ocamlc -ccopt -std=c99 nfa.c -cclib nfa.h
	ocamlc -ccopt -std=c99 similar.c -cclib similar.h
	ocamlc -ccopt -std=c99 postfix.c -cclib postfix.h
	ocamlc -ccopt -std=c99 helper.c
	ocamlc -ccopt -std=c99 ocamlc.c
	ocamlmklib -o _wrap_stubs ocamlc.o bm.o kmp.o finger_print.o nfa.o postfix.o similar.o helper.o
	ocamlc -i external.ml
	ocamlc -c external.ml
	ocamlc -a -o external.cma external.ml -dllib dll_wrap_stubs.so
	ocamlc -i plagiarism.ml
	ocamlc -c plagiarism.ml
	ocamlc -a -o plagiarism.cma plagiarism.ml
	ocamlc -i interface.ml
	ocamlc -c interface.ml
	ocamlc -o LMG -dllpath ../LMG_Suite plagiarism.cma external.cma interface.ml

clean:
	rm LMG *.[oa] *.so *.cm[ioa] 

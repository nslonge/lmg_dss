#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "helper.h"
#include "bm.h"
#include "kmp.h"
#include "finger_print.h"
#include "similar.h"
#include "postfix.h"
#include "nfa.h"
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value bm_search(value dl, value pt);
CAMLprim value fp_search(value dl, value pt);
CAMLprim value kmp_search(value dl, value pt);
CAMLprim value rgx_search(value dl, value pt);
CAMLprim value doc_sim(value dl1, value dl2);
CAMLprim value prprcss(value dl1, value dl2);

CAMLprim value bm_search(value dl, value pt)
{
    CAMLparam2(dl,pt);
    CAMLlocal2(list, cons);
    char *pattxt = String_val(pt);
    char *doc_loc = String_val(dl);
    result *r = malloc(sizeof(result));
    bm(doc_loc,pattxt,r);
    int len = r->found;
    list = Val_emptylist;
    for (int i = len; i > 0; i--)
    {
        cons = caml_alloc(2, 0);
        Store_field(cons, 0, Val_int((r->location)[i-1]));
        Store_field(cons, 1, list);
        list = cons;
    }
    free(r->location);
    free(r);
    CAMLreturn(list);
}

CAMLprim value fp_search(value dl, value pt)
{
    CAMLparam2(dl,pt);
    CAMLlocal2(list, cons);
    char *pattxt = String_val(pt);
    char *doc_loc = String_val(dl);
    result *r = malloc(sizeof(result));
    finger_print(doc_loc,pattxt,r);
    int len = r->found;
    list = Val_emptylist;
    for (int i = len; i > 0; i--)
    {
        cons = caml_alloc(2, 0);
        Store_field(cons, 0, Val_int((r->location)[i-1]));
        Store_field(cons, 1, list);
        list = cons;
    }
    free(r->location);
    free(r);
    CAMLreturn(list);
}

CAMLprim value kmp_search(value dl, value pt)
{
    CAMLparam2(dl,pt);
    CAMLlocal2(list, cons);
    char *pattxt = String_val(pt);
    char *doc_loc = String_val(dl);
    result *r = malloc(sizeof(result));
    kmp(doc_loc,pattxt,r);
    int len = r->found;
    list = Val_emptylist;
    for (int i = len; i > 0; i--)
    {
        cons = caml_alloc(2, 0);
        Store_field(cons, 0, Val_int((r->location)[i-1]));
        Store_field(cons, 1, list);
        list = cons;
    }
    free(r->location);
    free(r);
    CAMLreturn(list);
}

CAMLprim value rgx_search(value dl, value pt)
{
    CAMLparam2(dl,pt);
    CAMLlocal2(list, cons);
    char *pattxt = String_val(pt);
    char *doc_loc = String_val(dl);
    result *r = malloc(sizeof(result));
    regex(doc_loc,pattxt,r);
    int len = r->found;
    list = Val_emptylist;
    if(r->found == -1)
    {
        list = caml_alloc(2, 0);
        Store_field(list, 0, Val_int(-1));
        Store_field(list, 1, Val_emptylist);
    }
    else
    {
       for (int i = len; i > 0; i--)
       {
           cons = caml_alloc(2, 0);
           Store_field(cons, 0, Val_int((r->location)[i-1]));
           Store_field(cons, 1, list);
           list = cons;
       }
    }
    free(r->location);
    free(r);
    CAMLreturn(list);
}

CAMLprim value doc_sim(value dl1, value dl2)
{
    CAMLparam2(dl1,dl2);
    char *doc1_loc = String_val(dl1);
    char *doc2_loc = String_val(dl2);
    double d = similar(doc1_loc,doc2_loc);
    CAMLreturn(caml_copy_double(d));
}

CAMLprim value prprcss(value dl1, value dl2)
{
    CAMLparam2(dl1,dl2);
    char *doc1_loc = String_val(dl1);
    char *doc2_loc = String_val(dl2);
    pre_process(doc1_loc,doc2_loc);
    CAMLreturn(Val_unit);
}

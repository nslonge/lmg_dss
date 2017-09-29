#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include "helper.h"
#include "postfix.h"
#include "nfa.h"

void regex(char *filename, char *exp, result *ans)
{  

   int len = strlen(exp);

   // create memory for answer
   ans->found = 0;
   ans->location = malloc(sizeof(int) * 100);

   // build a result
   text2 res;
   res.strophe = malloc(sizeof(char) * (len * 2));
   res.length = 0;

   // get postfix
   postfix(exp, &res);

   // invalid expression
   if(res.length == -1)
   {
      ans->found = - 1;
      free(res.strophe);
      return;
   }     

   // allocate state_stack
   state_stack *s = malloc(sizeof(state_stack));
   s->elms = malloc(sizeof(state*) * (res.length * 100));
   s->size = 0;

   // build the nfa
   state *r = build_nfa(&res, s);

   // search a pattern or a document 
   int p_or_d = 1;
   text2 doc;
   read2(&doc, p_or_d, filename);   

   // initialize lists for NFA simulation
   state_stack *lst = malloc(sizeof(state_stack));
   state_stack *lst_next = malloc(sizeof(state_stack));
   lst->elms = malloc(sizeof(state*) * 1000);
   lst_next->elms = malloc(sizeof(state*) * 1000);

   // perform simulation
   lst->elms[0] = r;
   lst->size = 1;
   lst_next->size = 0;
   //int finished = 0;
   for(int i = 0; i < doc.length; i++)
   {
      push2(lst, r);
      for(int j = 0; j < lst->size; j++)
      {  
           reset_visited(lst->elms[j]);
           check(lst->elms[j], doc.strophe[i], lst_next, lst);
      }
      // copy next states, erase current states
      copy_erase_list(lst_next, lst);

      // case of matching a single string
      /*if(p_or_d == 0 && lst->size == 0)
      {
         finished = 1;
         break;
      }*/
      
      // check for final state
      if(p_or_d == 1 && check_final(lst) == 1)
      {
         // found a match, so remember index
         ans->found++;
         // check if we need to allocate more memory
         if(ans->found % 100 == 0)
         {
            int *tmp = realloc(ans->location, sizeof(int) * (ans->found + 100));
            if(tmp != NULL)
               ans->location = tmp;
            else
               return;
         }
         ans->location[ans->found-1] = i;
      }
   }

   free_state_stack(lst);
   free_state_stack(lst_next);

   free_nfa(r, s);

   free(res.strophe);
   free(doc.strophe);

   return;
}

/*
 *    Functions for using the NFA
 */

/*
 *    Check if the state r can accept c, if so add new state to state list
 */
void check(state *r, char c, state_stack *lst, state_stack *lst2)
{
   if(r == NULL)
      return;
   // found arrow, check val
   if(r->is_arrow == 1)
   {
      // found match, add state to list
      if(r->c == c || r->c == '?')
      {
         // ensure that we don't need more memory
         if(lst->size != 0 && lst->size % 1000 == 0)
         {
            int len = lst->size;
            state **newlst = 
               realloc(lst->elms, sizeof(state*) * (len + 1000));
            if(newlst != NULL)
               lst->elms = newlst;
            else
               return;

            // change size of second list
            state **newlst2 = 
               realloc(lst2->elms, sizeof(state*) * (len + 1000));
            if(newlst2 != NULL)
               lst2->elms = newlst2;
            else
               return;
         }
         push2(lst, r->out1);
      }
      return;
   }
   if(r->visited == 1)
      return;
   r->visited = 1;
   check(r->out1, c, lst, lst2);
   check(r->out2, c, lst, lst2);
}

/*
 *    Checkl state list for final states
 */
int check_final(state_stack *lst)
{
   for(int i = 0; i < lst->size; i++)
   {
      if(checker(lst->elms[i]) == 1)
         return 1;
   }
   return 0;
}

int checker(state *r)
{
   // base case
   if(r == NULL)
      return 0;
   if(r->is_final == 1)
      return 1;
   else
      return checker(r->out2);
}

/*
 *    Copy lst1 into lst2 and erase lst1
 */
void copy_erase_list(state_stack *lst1, state_stack *lst2)
{
   for(int i = 0; i < lst1->size; i++)
   {
      lst2->elms[i] = lst1->elms[i];
   }  
   lst2->size = lst1->size;
   lst1->size = 0;
}

/*
 *    Given a postfix regular expression, build corresponding NFA
 */
state *build_nfa(text2 *res, state_stack *s)
{
   // allocate a special "next" state
   state *next = malloc(sizeof(state));
   next->is_arrow = 0;
   next->is_final = 0;
   next->out1 = NULL;
   next->out2 = NULL;
   next->visited = 0;

   for(int i = 0; i < res->length; i++)
   {
      // grab character
      char c = tolower(res->strophe[i]);

      // we have a character, so build o->c->
      if(((int)c > 47 && (int)c < 58) || isalpha(c) != 0 || (int)c == 63 || (int)c == 32)
      {
         // first make state
         state *new_state = malloc(sizeof(state));
         new_state->is_arrow = 0;
         new_state->is_final = 0;
         new_state->out2 = NULL;
         new_state->visited = 0;

         // now make arrow
         state *a = malloc(sizeof(state));
         a->is_arrow = 1;
         a->c = c;
         a->is_final = 0;
         a->out1 = next;
         a->out2 = NULL;
         a->visited = 0;

         // make state point to arrow
         new_state->out1 = a;

         // push2 new_state onto state_stack
         push2(s, new_state);
      }  
      // else we have an operator
      else if(c == '.')
      {
         // pop2 the state_stack twice
         state *r1 = pop2(s);
         state *r2 = pop2(s);
         // concatenate2 r2 and r1
         concatenate2(r2, r1, next);
         // push2 r2 = r2 o r1 onto state_stack
         push2(s, r2);
      }
      else if(c == '*')
      {
         // pop2 the state_stack
         state *r = pop2(s);
         // star r
         state *r1 = kleene2(r, next);
         // push2 r1 onto state_stack
         push2(s, r1);
      }
      else if(c == '|')
      {
         // pop2 the staack twice
          state *r1 = pop2(s);
          state *r2 = pop2(s);
          // build an or state
          state * r3 = or(r2, r1, next);
          // push2 r3 on the state_stack
          push2(s, r3);
      }
   }
   // insert final state
   state *r = pop2(s);
   make_final(r, next);

   free(next);
    
   return r;
}

/*
 *    Reset the visit count at every node to 1
 *    Due to loops, we need to push every node onto
 *    the stack, then reset the values as we pop them
 *    off. This way we don't ever get stuck, nor do
 *    we upset the pointers
 */
void reset_visited(state *r)
{
   state_stack *s = malloc(sizeof(state_stack));
   s->elms = malloc(sizeof(state*) * 100);
   s->size = 0;
   push2_visited(s, r);
   while(s->size > 0)
   {
      r = pop2(s);
      r->visited = 0;
   }
   free(s->elms);
   free(s);
}

/*
 *    Recursively push the nodes of the NFA onto the stack
 */
void push2_visited(state_stack *s, state *r)
{
   if(r == NULL)
      return;
   if(r->visited == -1)
      return;
   r->visited = -1;
   if(s->size != 0 && s->size % 100 == 0)
   {
      state **tmp = realloc(s->elms, sizeof(state*) * (s->size + 100));
      if(tmp != NULL)
         s->elms = tmp;
      else return;
   }
   push2(s, r);
   push2_visited(s, r->out1);
   push2_visited(s, r->out2);
}

/*
 *    Recursively print the nfa
 */
void print_result(state *r)
{
   if(r == NULL)
      return;
   if(r->visited == 1)
      return;
   if(r->is_final == 1)
   {
      printf(".O. ");
      r->visited = 1;
      return;
   }
   if(r->is_arrow == 1)
   {
      printf("%c->", r->c);
      r->visited = 1;
      print_result(r->out1);
      return;
   }
   if(r->out1 != NULL && r->out2 == NULL)
   {
      printf("O->");
      r->visited = 1;
      print_result(r->out1);
      return;
   }
   else if(r->out1 != NULL && r->out2 != NULL)
   {
      printf("O->");
      printf("split top level: ");
      print_result(r->out1);
      printf("split bottom level: ");
      print_result(r->out2);
      r->visited = 1;
      return;
   }
}

/*
 *    Make the given state point to a final state
 */
void make_final(state *r, state *next)
{
   state *final = malloc(sizeof(state));
   final->is_arrow = 0;
   final->is_final = 1;
   final->out1 = NULL;
   final->out2 = NULL;
   final->visited = 0;
   reset_visited(r);
   set_next(r, final, next);
}

/*
 *      Set all of a nodes next pointers to result
 */
void set_next(state *source, state *result, state *next)
{
   if(source == NULL)
      return;
   if(source->visited == 1)
      return;
   // we're at a state
   if(source->is_arrow == 0)
   {  
      // states will only ever have out2 pointing to next
      if(source->out2 == next)
      {
         source->visited = 1;
         source->out2 = result;
         set_next(source->out1, result, next);
      }
      // otherwise check out1 and out2
      else
      {
         source->visited = 1;
         set_next(source->out1, result, next);
         set_next(source->out2, result, next);
      }
   }
   // we're at an arrow
   else if(source->is_arrow == 1)
   {
      // arrows will only ever have out1 pointing to next
      if(source->out1 == next)
      {
         source->visited = 1;
         source->out1 = result;
      }
      // otherwise check out1
      else
      {
         source->visited = 1;
         set_next(source->out1, result, next);
      }
   }
}

/*
 *    Build an or node 
 */
state *or(state *r1, state *r2, state *next)
{
   // make a new state
   state *new_state = malloc(sizeof(state));
   new_state->is_arrow = 0;
   new_state->is_final = 0;
   // out1 = r1, out2 = r2
   new_state->out1 = r1;
   new_state->out2 = r2;
   new_state->visited = 0;
   return new_state;
}

/*
 *    Build a kleene node
 */
state *kleene2(state *r, state *next)
{
   // if r already has two outs, create a new state
   if(r->out1 != NULL && r->out2 != NULL)
   {
      state *new_state = malloc(sizeof(state));
      new_state->is_arrow = 0;
      new_state->is_final = 0;
      // set all of the next pointers in r to new_state
      reset_visited(r);
      set_next(r, new_state, next);
      //out1 goes to r
      new_state->out1 = r;
      // out2 goes to next
      new_state->out2 = next;
      new_state->visited = 0;
      return new_state;
   }
   // othwerwise we can just use r
   else
   {
      // update all of the next pointers to r
      reset_visited(r);
      set_next(r, r, next);
      // out2 goes to next
      r->out2 = next;
      return r;
   }
   return NULL;
}

/*
 *    Concatenate two nodes
 */
void concatenate2(state *r1, state *r2, state *next)
{
   // set all of the next pointers in r1 to r2
   reset_visited(r1);
   set_next(r1, r2, next);
}


state *pop2(state_stack *s)
{
   if(s->size == 0)
      return NULL;
      
   state *r = s->elms[s->size-1];
   s->size--;
   return r;
}

void push2(state_stack *s, state *v)
{
   s->elms[s->size] = v;
   s->size++;
}  



/*
 *    Function for reading. BORING
 */
void read2(text2 *docum, int is_file, char *filename)
{
    FILE *fr;
    if(is_file == 1)
        fr = fopen (filename, "r"); 
    else
    {
        fr = stdin;
        printf("Enter a string to match against followed by the character / : \n");
        fflush(stdout);  
    }
    
    // read entire file into a giant array, first guess is size 1000
    char *doc = malloc(sizeof(char)*1000);
    
    // start reading
    int index = 0;
    int count = 1;

    //int prevc = -1;
    for (int c = fgetc(fr); c != EOF; c = fgetc(fr))
    {
       
        int tmp = (int)tolower(c);
        // convert all escape sequences to spaces
        if(tmp < 32)
            tmp = 32;
            
        // still room left in allocated memory
        if((index % 1000) != 0 || index == 0)
        {
            doc[index] = (char)tmp;
            index++;
        }
        // else we need to do some re-allocating, give another 1000 slots
        else
        {
            // remember we increased by 1000
            count++;

            // realloc
            char *new_doc;
            new_doc = realloc(doc, sizeof(char) * (count * 1000));
            if(new_doc != NULL)
                doc = new_doc;
            else
                return;
                
            // put in element
            doc[index] = (char)tmp;
            index++;
        }
        //prevc = c;
    }
    // terminate doc
    doc[index-1] = '\0';
    index--;
    
    // now delete extra memory we have allocated
    char *new_doc;
    new_doc = realloc(doc, sizeof(int) * (index+1));
    if(new_doc != NULL)
        doc = new_doc;
    else
        return;
        
    // test that read was succesful
    /*
    for(int i = 0; i < index; i++)
    {
        printf("%c", doc[i]);
    }
    printf("\n");*/
    
    fclose(fr);
    docum->length = index;
    docum->strophe = doc;
}

/*
 *    Memory Freeing
 */
void free_state_stack(state_stack *s)
{
   free(s->elms);
   free(s);
}

void push2_free(state_stack *s, state *r)
{
   if(r == NULL)
      return;
   if(r->visited == 1)
      return;
   r->visited = 1;
   push2(s, r);
   push2_free(s, r->out1);
   push2_free(s, r->out2);
}

void free_nfa(state *r, state_stack *s)
{ 
   // reset visit counts
   reset_visited(r);
   // push2 all the states on the state_stack
   push2_free(s, r);
   // free states from top down
   while(s->size > 0)
   {
      state *r = pop2(s);
      free(r);
   }
   free_state_stack(s);
}




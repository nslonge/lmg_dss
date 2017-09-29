#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include "postfix.h"

/* 
 *    Actual Postfix driver fuction
 */
void postfix(char *exp, text2 *r)
{

   // initialize stack
   stack *s = malloc(sizeof(stack));
   s->elms = malloc(sizeof(node2*) * strlen(exp));
   s->size = 0;

   // counters for or and parentheses
   marker or_mark, paren_mark;
   or_mark.count = 0;
   paren_mark.count = 0;
   or_mark.ind = malloc(sizeof(int) * 10);
   paren_mark.ind = malloc(sizeof(int) * 10);
   or_mark.count = 0;
   paren_mark.count = 0;
   
   // parse expression
   for(int i = 0; i < strlen(exp); i++) 
   {
      char c = tolower(exp[i]);
      
      // we have an alphabetical char!
      if(((int)c > 47 && (int)c < 58) || 
         isalpha(c) != 0 || 
         (int)c == 63 || 
         (int)c == 32 )
      {  
         // create a new node to hold charcter
         node2 *new_node = malloc(sizeof(node2));
         new_node->left = NULL;
         new_node->right = NULL;
         new_node->character = c;

         // pop the top
         node2 *e1 = pop(s);
         
         // stack is empty, push new_node
         if(e1 == NULL)
         {
            push(s, new_node);
         }
         // previous character was an or: push it back and push the character
         else if(e1->character == '|')
         {
            push(s, e1);
            push(s, new_node);
         }
         // previous character was a parenthesis: push it back and push the character
         else if(e1->character == '(')
         {
            push(s, e1);
            push(s, new_node);
         }
         // concatenate character with previous char
         else
         {
            // check if next character is *
            // if so we only apply it to previous character
            if(i < strlen(exp) -1 && exp[i+1] == '*')
            {
               push(s, concatenate(e1, kleene(new_node)));
               i++;
            }
            // pop last element and concatenate, then push back on
            else
               push(s, concatenate(e1, new_node));
         }  
      }
      // otherwise we have an operator
      else
      {
         // remember we saw an open parenthesis, push it on stack
         if(c == '(')
         {
            paren_mark.count++;
            
            node2 *paren = malloc(sizeof(node2));
            paren->character = '(';
            paren->left = NULL;
            paren->right = NULL;
            push(s, paren);
            if(s->size % 10 == 0)
            {
               // allocate more memory for index array
               int *tmp = realloc(paren_mark.ind, sizeof(int) * (s->size + 10));
               paren_mark.ind = tmp;
            }
            paren_mark.ind[paren_mark.count-1] = s->size;
         }
         // easy case of kleene star, pop last element, star it, push back
         else if(c == '*')
         {
            node2 *e1 = pop(s);
            if(e1 == NULL)
            {
               printf("Invalid Regular Expression: Too many operators.\n\n");
               r->length = -1;   
               return;
            }
            else if(e1->character == '(')
            {
               printf("Invalid Regular Expression: Extra or misplaced \'(\'.\n\n");
               r->length = -1;   
               return;
            }
            else if(e1->character == '|' && e1->left == NULL)
            {
               printf("Invalid Regular Expression: Extra or misplaced \'|\'.\n\n");
               r->length = -1;   
               return;
            }
            node2 *kleene_op = kleene(e1);
            push(s, kleene_op);
         }
         // another easy case of +: pop last and push it back
         else if(c == '+')
         {
            // build e1*
            node2 *e1 = pop(s);
            if(e1 == NULL)
            {
               printf("Invalid Regular Expression: Too many operators.\n\n");
               r->length = -1;   
               return;
            }
            else if(e1->character == '(')
            {
               printf("Invalid Regular Expression: Extra or misplaced \'(\'.\n\n");
               r->length = -1;   
               return;
            }
            else if(e1->character == '|' && e1->left == NULL)
            {
               printf("Invalid Regular Expression: Extra or misplaced \'|\'.\n\n");
               r->length = -1;   
               return;
            }
            node2 *plus_op = plus(e1);
            push(s, plus_op);
         }
         // push on stack, remember we saw it and deal with later
         else if(c == '|')
         {
            if(s->size == 0)
            {
               printf("Invalid Regular Expression: Too many operators.\n\n");
               r->length = -1;   
               return;
            }
            // remember we have an or
            or_mark.count++;
            // push onto the stack
            node2 *or_node = malloc(sizeof(node2));
            or_node->character='|';
            or_node->left = NULL;
            or_node->right = NULL;
            push(s, or_node);
            if(s->size % 10 == 0)
            {
               // allocate more memory for index array
               int *tmp = realloc(or_mark.ind, sizeof(int) * (s->size + 10));
               or_mark.ind = tmp;
            }
            or_mark.ind[or_mark.count-1] = s->size;
         }
         // close paren: time to go back and deal with |, if we have it, or condense stack
         else if(c == ')')
         {
            if(paren_mark.count == 0)
            {
               printf("Invalid Regular Expression: Extra or misplaced \')\'.\n\n");
               r->length = -1;   
               return;
            }
            // check if we had or
            if(or_mark.count > 0 && 
               or_mark.ind[or_mark.count-1] > paren_mark.ind[paren_mark.count -1])
            {
               while(or_mark.ind[or_mark.count-1] > paren_mark.ind[paren_mark.count -1])
               {  
                  node2 *e1 = pop(s);
                  node2 *or = pop(s);
                  node2 *e2 = pop(s);
                  // pop parenthesis, if there is one
                  node2 *e4 = pop(s);
                  int stop = 0;
                  if(e4 !=NULL)
                  {
                     if(e4->character != '(')
                        push(s, e4);
                     else
                     {
                        // remember we found parenthesis & free memory
                        stop = 1;
                        free_tree2(e4);
                        paren_mark.count--;
                     }
                  }
                  if(or->character != '|')
                  {
                     printf("Invalid Regular Expression: Extra or misplaced \'|\'.\n\n");
                     r->length = -1;
                     return;
                  }
                  or->left = e2;
                  or->right = e1;
                  push(s, or);
                  or_mark.count--;
                  if(or_mark.count == 0 || stop == 1)
                     break;
               }
            }
            // otherwise we need to find last paren, then condense stack
            else
            {
               node2 *e1 = pop(s);
               while(e1 != NULL)
               {
                  if(e1->character != '(')
                  {
                     node2 *e2 = pop(s);
                     if(e2->character == '(')
                     {
                        push(s, e1);
                        paren_mark.count--;
                        free_tree2(e2);
                        break;
                     }
                     else
                     {
                        push(s, concatenate(e2, e1));
                        e1 = pop(s);
                     }
                  }
                  else
                  {
                     paren_mark.count--;
                     free_tree2(e1);
                     break;
                  }
               }
            }
         }
      }
   } 
   if(exp[strlen(exp) -1] == '|')
   {
      printf("Invalid Regular Expression: Extra or misplaced \'|\'.\n\n");
      r->length = -1;   
      return;
   }
   if(s->size == 0)
   {
      printf("Invalid Regular Expression.\n\n");
      r->length = -1;   
      return;
   }

   // clean up remaining stack
   if(or_mark.count > 0)
   { 
      int check_val = 0;
      if(paren_mark.count == 0)
         check_val = 0;
      else
         check_val = paren_mark.ind[paren_mark.count-1];
         
      while(or_mark.ind[or_mark.count-1] > check_val)
      {
         // pop until we get to '|'
         
         node2 *e1 = pop(s);
         node2 *or = pop(s);

         
         while(or->character != '|')
         {
            push(s, concatenate(or, e1));
            e1 = pop(s);
            or = pop(s);
            if(e1 == NULL || or == NULL)
            {
               printf("Invalid Regular Expression: Extra or misplaced \'|\'.\n\n");
               r->length = -1;   
               return;
            }
         }
         node2 *e2 = pop(s);
         // pop parenthesis, if there is one
         node2 *e4 = pop(s);
         if(e4 !=NULL)
         {
            if(e4->character != '(')
               push(s, e4);
            else
            {
               paren_mark.count--;
               free_tree2(e4);
               printf("Invalid Regular Expression: Extra or misplaced \'(\'.\n\n");
               r->length = -1;   
               return;
            }
         }
         if(or->character != '|')
         {
            printf("Invalid Regular Expression: Extra or misplaced \'|\'.\n\n");
            r->length = -1;   
            return;
         }
         or->left = e2;
         or->right = e1;
         push(s, or);
         or_mark.count--;
         if(or_mark.count == 0)
            break;
         if(paren_mark.count == 0)
            check_val = 0;
         else
            check_val = paren_mark.ind[paren_mark.count-1];
      }
   }
   while(s->size > 1)
   {
      node2 *e1 = pop(s);
      node2 *e2 = pop(s);
      if(e1->character == '(' || e2->character == '(')
      {
         printf("Invalid Regular Expression: Extra or misplaced \'(\'.\n\n");
         r->length = -1;   
         return;
      }
      push(s, concatenate(e2, e1));
   }

   // print result
   printf("Result in postfix notation: ");
   node2 *e1 = pop(s);

   // get and print result
   get_result(e1, r);
   printf("\n\n");

   /*for(int i = 0; i < r->length; i++)
      printf("%c", r->strophe[i]);
   printf("\n");*/

   // free memory
   free_stack(s);
   free_tree2(e1);
   free(or_mark.ind);
   free(paren_mark.ind);
}



/*
 *    Copy a node
 */
node2 *copy(node2 *e1)
{
   // base case
   if(e1 == NULL)
      return NULL;

   // recursive case
   node2 *e2 = malloc(sizeof(node2));
   e2->character = e1->character;
   e2->left = copy(e1->left);
   e2->right = copy(e1->right);
   return e2;
}

/*
 *    Deal with +, remember e+ = ee*
 */
node2 *plus(node2 *e1)
{
   // compute e1*
   node2 *kleene_e1 = kleene(e1);
   // copy e1
   node2 *copy_e1 = copy(e1);
   // concat e1, e1*
   return concatenate(copy_e1, kleene_e1);
}

/*
 *    Take Kleene star of input
 */
node2 *kleene(node2 *e1)
{
   node2 *kleene_op = malloc(sizeof(node2));
   kleene_op->character = '*';
   kleene_op->left = e1;
   kleene_op->right = NULL;
   return kleene_op;
}

/*
 *    Concatenate two inputs
 */
node2 *concatenate(node2 *e1, node2 *e2)
{
   node2 *add_op = malloc(sizeof(node2));
   add_op->character = '.';
   add_op->left = e1;
   add_op->right = e2;
   return add_op;
}

/*
 *    Print postfix expression
 */
void get_result(node2 *e1, text2 *r)
{
   // base case
   if(e1->left == NULL && e1->right == NULL)
   {
      printf("%c", e1->character);
      r->strophe[r->length] = e1->character;
      r->length++;
      return;
   }

   // case of **
   if(e1->character == '*' && e1->left->character == '*')
   {
      // print left
      get_result(e1->left, r);
      // print right
      if(e1->right != NULL)
         get_result(e1->right, r);
      // don't print character
   }
   else
   {
      // print left
      get_result(e1->left, r);
      // print right
      if(e1->right != NULL)
         get_result(e1->right, r);
      // print node
      printf("%c", e1->character);
      r->strophe[r->length] = e1->character;
      r->length++;
   }
}


node2 *pop(stack *s)
{
   if(s->size == 0)
      return NULL;
      
   node2 *r = s->elms[s->size-1];
   s->size--;
   return r;
}

void push(stack *s, node2 *v)
{
   s->elms[s->size] = v;
   s->size++;
}  

void free_tree2(node2 *e1)
{
   // base case
   if(e1->left == NULL && e1->right == NULL)
   {
      free(e1);
      return;
   }

   if(e1->left != NULL)
      free_tree2(e1->left);
   if(e1->right != NULL)
      free_tree2(e1->right);
   free(e1);
}

void free_stack(stack *s)
{
   for(int i = 0; i < s->size; i++)
      free_tree2(s->elms[i]);
   free(s->elms);
   free(s);
}

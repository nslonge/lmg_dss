#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include "helper.h"
#include "bm.h"

void bm(char *file1, char *pattern_txt, result *r)
{
    // create memory for file
    text doc;
    text pattern;

    // save pattern
    int len = strlen(pattern_txt);
    pattern.strophe = malloc(sizeof(int) * len);
    for(int i = 0; i < len; i++)
      pattern.strophe[i] = (int)(tolower(pattern_txt[i]));
    pattern.length = len;

    // create memory for result
    r->found = 0;
    r->location = malloc(sizeof(int) * 100);

    // read file, prompt for pattern to find
    readit(&doc, 1, file1);

    // check that file wasn't empty
    if(doc.max_word_size == 0)
    {
      printf("Document is empty!\n");
      return;
    }

    // check that pattern < document
    if(doc.length < pattern.length)
    {
      printf("Pattern longer than document!\n");
      return;
    }

    // perform search
    search_bm(&doc, &pattern, r); 

    // free memory
    free_text(&doc);
    free_text(&pattern);
}

/*
 *  Does the actual searching
 */ 
int search_bm(text *doc, text *pattern, result *r)
{
    int found = 0;

   /*
    * First, we should explain what we'd like to store in Table 1. Let the 
    * length of a string S be denoted |S|, and its i-th character denoted S[i].
    * Given a nonnegative integer i, Let F(S,i) be the set of strings
    * T such that T[0] != S[|S| - (i + 1)] and T[k] = S[|S| + k - (i + 1)] 
    * holds for all k = 1, ..., i (whenever T[0] and T[k] are defined). Then 
    * let L(S,i) be the smallest positive integer l such that the string U(l) 
    * given by U(l)[k] = S[|S| + k - (i + l + 1)] for k = 0, ..., i is an el-
    * ement of F(S,i). This integer is called the i-th left-shift value for S. 
    * If we are searching for the string S, then table1[i] = L(S,i).
    * 
    * (It is possible that you are wondering why we would want to store these 
    * values. This'll be addressed in the explanation of the search function.)
    *
    * Now it is instructive to consider an example: let S = "ICE JUICE MICE". 
    * In this case, F(S,3) is the set of strings T with T[0] != M, and such 
    * that T[k] = S[14 + k - 4] for k = 1, ..., 3. That is, for some variable 
    * character * != M, T is of the form "*ICE". What is L(S,3)? We want to 
    * find the smallest integer l such that the string U(l) with 
    *                   U(l)[k] = S[14 + k - (i + l + 1)] 
    * for k = 0, ..., 3 is an element of F(S,3). The string "UICE" is in 
    * F(S,3), so we have 14 - (3 + l + 1) = 5 which implies l = 5.
    * 
    * We work one more example for the tricky case. F(S,4) is the set of 
    * strings "*MICE" for "*" != " ". It might appear that L(S,4) is 
    * undefined: after all, there is no substring of "ICE JUICE MICE" that has
    * this form. However, we implied subtly that we would consider strings that
    * are only partially defined. We'll denote the undefined character by "-",
    * and notice that by our definition "--ICE" is a member of F(S,4). And
    * the partially defined string U(11) = "--ICE". So L(S,4) = 11. In fact,
    * it's clear that L(S,j) = 11 for all j > 3.
    */

    // Create table1. We have S = pattern->strophe
    int *table1 = malloc(sizeof(int) * pattern->length);
    for(int i = 0; i < pattern->length; i++)
        table1[i] = pattern->length;

    // i is the index for which we are calculating L(S,i)
    for(int i = 0; i < pattern->length; i++)
    {
        // left_shift is to be increased until it reaches L(S,i) 
        for(int left_shift = 1; left_shift < pattern->length + 1; left_shift++)
        {
         // The variable start is the first character of U(left_shift)
            int start = pattern->length - i - left_shift - 1;

            // Ensures that we don't start before the beginning of the pattern, 
            // and deals with the undefined characters case.
            if(start < 0)
               start = 0;

            // We'll set changed to 1 if we find that U(left_shift) defined 
            // as above is in F(S,3); then L(S,i) = left_shift by definition.
            int changed = 0;

            // We check that U(left_shift)[0] != S[|S| - (i + 1)] and 
            // U(left_shift)[k] = S[|S| + k - (i + 1)] holds for all 
            // k = 1, ..., i (whenever U(left_shift)[0] and U(left_shift)[k]
            // are defined) but we make the substitution
            // j = |S| + k - (i + left_shift + 1).
            for(int j = start; j < (pattern->length - left_shift); j++)
            {
                // Matched on the first character, so break 
                if(j == (pattern->length - i - left_shift - 1) && 
                   pattern->strophe[j] == pattern->strophe[j+left_shift])
                {
                    changed = 1;
                    break;
                }   
                // Didn't find a match, so break
                else if(j != (pattern->length - i - left_shift - 1) && 
                        pattern->strophe[j] != pattern->strophe[j+left_shift])
                {
                    changed = 1;
                    break;
                }
            }
            // Catch the break and set the value accordingly
            if(changed == 0)
            {
                table1[i] = left_shift;
                break;
            }
        }
    }
    FILE *fr = fopen("test.txt", "w");
    for(int i = 0; i < pattern->length; i++)
        fprintf(fr,"%d ", table1[i]);
    fclose(fr);
   /*
    * Table 2 is easier. For a string S and an ASCII value x, let t(S,x) be
    * the index of the last occurrence of the character x in S. We make the 
    * slight exception that if x is the last character of S, then t(S,x) is
    * the index of the second to last occurrence of x in S. Finally, if x is
    * not in S, t(S,x) = -1. We then define
    *                      table2[x] = |S| - (t(S,x) + 1).
    */

    // Create table 2
    int *table2 = malloc(sizeof(int) * 128);
    
    // Initialize all table values to pattern length
    for(int i = 0; i < 128; i++)
        table2[i] = pattern->length;

    // Set table values
    for(int i = pattern->length - 2; i > -1; i--)
    {
        // Check if we should update the table value for that character
        if(table2[pattern->strophe[i]] == pattern->length)
        {
            table2[pattern->strophe[i]] = pattern->length - i - 1;
        }
    }

   /*
    * Now comes the task of explaining how we actually use these tables.
    * 
    * To search for a string S in a document D with local variables
    * i,k,m, with k <- |S| - 1 and i <-0, the algorithm is as follows:
    *
    *    (1) Set m <- 0 and i <- i + k. If i > |D| - 1, return FALSE. Otherwise,
    *        proceed to (2).
    *    (2) If m = |S|, return TRUE. Otherwise, continue to (3).
    *    (3) If S[|S| - m - 1] = D[i - m], set m <- m + 1 and return to (2). 
    *        Otherwise, proceed to (4).
    *    (4) Set k = max(table1[m],table2[D[i-m]]), and return to (1).
    * 
    * Note that we have constructed table1 so that after matching m characters
    * of D with the final m characters of S, and mismatching on character m + 1,
    * table1[m] is the farthest we can move the variable i forward without the 
    * possibility of missing a copy of S in D (without regard to the value of
    * the mismatched character). Similarly, if we don't know m, but we know that
    * the first character that erred was x, table2[x] is the farthest we can move
    * i forward without the possibility of missing a copy of S in D.
    */
    int i = pattern->length -1;
    while(i < doc->length)
    {
        int match = 0;
        int c = doc->strophe[i];
        // found a single character match, so keep looking
        while(c == pattern->strophe[pattern->length - match - 1])
        {
            match++;
            c = doc->strophe[i-match];
            if(match == pattern->length)
                break; 
        }
        // found an instance of the pattern
        if(match == pattern->length)
        {
            found++;
            // update result
            r->found++;

            // check if we need more memory
            if(r->found % 100 == 0)
            {
               // allocate more memory
               int *new_r = realloc(r->location, 
                                    sizeof(int) * (r->found + 100));
               if(new_r != NULL)
                  r->location = new_r;
               else
                  return 0;
            }
            // remember index
            r->location[r->found - 1] = i - pattern->length + 1;
            i = i + table1[pattern->length-1];
        }
        // update search index by the maximum of the table values
        else
            i = i + max(table1[match], table2[c] - match);
    }

    // free memory
    free(table1);
    free(table2);
    
    return found; 
}

#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include "helper.h"
#include "finger_print.h"

int base = 128;

void finger_print(char *file1, char *pattern_txt, result *r)
{
    // structures for holding files
    text doc;
    text pattern;

    // save pattern
    int len = strlen(pattern_txt);
    pattern.strophe = malloc(sizeof(int) * len);
    for(int i = 0; i < len; i++)
      pattern.strophe[i] = (int)(tolower(pattern_txt[i]));
    pattern.length = len;

    // read file
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

    // initialize power table
    power_table table;
    table.p_num = 2; 
    table.ps = malloc(sizeof(int) * table.p_num);
    table.p_stps = malloc(sizeof(int) * table.p_num);
    table.ps[0] = 3169;
    table.ps[1] = 3181;

    // set power table vals
    make_power_tables(&table);

    // perform search
    search_finger(&pattern, &doc, &table, r);


    // free memory
    free_text(&doc);
    free_text(&pattern);
    free_table(&table);
}

/*
 *  Redefines mod so that it works for negative numbers in the way we expect
 */
int mod(int a, int n)
{
    if(a >= 0)
        return a % n;
    else
    {
        int tmp = (-a) % n;
        return n - tmp;
    }
}

/*
 *  Checks if two hashes are equal
 */
int match(int *hash1, int *hash2, int length)
{
    for(int i = 0; i < length; i++)
    {
        if(hash1[i] != hash2[i])
            return 0;
    }
    return 1;
}

/*
 *    Performs actual search for pattern within doc
 */
int search_finger(text *pattern, text *doc, power_table *table, result *r)
{
    // initialize r
    r->found = 0;
    r->location = malloc(sizeof(int) * 100);
    
    // return value
    int found = 0;
    
    // compute hashes for the pattern to check
    int *pat_hashes = hash(pattern, table);
    
    text begin;
    begin.strophe = doc->strophe;
    begin.length = pattern->length;
    
    // compute hashes for first pattern->length characters of doc
    int *doc_hashes = hash(&begin, table);
    
    // check if we've found a solution
    found = match(pat_hashes, doc_hashes, table->p_num);
    if(found == 1)
    {
         r->found++;
         r->location[found - 1] = 0;
    }
    
    // calculate 1/base % p_i for all i
    int *base_recips = malloc(sizeof(int) * table->p_num);
    for(int i = 0; i < table->p_num; i++)
    {
        base_recips[i] = get_power(i, table->ps[i] - 2, table);
    }
    
    // calculate base^(pattern->length) % p_i
    int *base_powers = malloc(sizeof(int) * table->p_num);
    for(int i = 0; i < table->p_num; i++)
    {
        base_powers[i] = get_power(i, pattern->length-1, table);
    }
    
    for(int i = pattern->length; i < doc->length; i++)
    {
        for(int j = 0; j < table->p_num; j++)
        {
            // subtract off first element
            doc_hashes[j] = mod(((doc_hashes[j] - doc->strophe[i-pattern->length])
                             * base_recips[j]), table->ps[j]);
                             
            // add new element times base^(pattern->length) % p_i
            doc_hashes[j] = mod(doc_hashes[j] + mod(doc->strophe[i] * base_powers[j],
                              table->ps[j]), table->ps[j]);
        }
        // check hashes
        found = match(pat_hashes, doc_hashes, table->p_num);
        if(found == 1)
        {
            // update result
            r->found++;

            // check if we need more memory
            if(r->found % 100 == 0)
            {
               // allocate more memory
               int *new_r = realloc(r->location, sizeof(int) * (r->found + 100));
               if(new_r != NULL)
                  r->location = new_r;
               else
                  return 0;
            }
            // remember index
            r->location[r->found - 1] = i - pattern->length + 1;
        }
    }
    
    // free memory
    free(pat_hashes);
    free(doc_hashes);
    free(base_powers);
    free(base_recips);
    
    return found;
    
}

/*
 *    Hashes a string of integers down to 16 bits by taking them mod each prime in power_table
 */
int *hash(text *pattern, power_table *table)
{
    int *results = malloc(sizeof(int) * table->p_num);
    // step through for each prime
    for(int i = 0; i < table->p_num; i++)
    {
        // compute current integer * 128^(current index) % p_i
        // and add to running sum
        results[i] = 0;
        for(int j = 0; j < pattern->length; j++)
        {
            int power = get_power(i, j, table);
            results[i] = mod((results[i] + pattern->strophe[j] * power),                       
                           table->ps[i]); 
        }
    }
    return results;
}

/*
 * Frees table
 */
void free_table(power_table *table)
{
    for(int i = 0; i < table->p_num; i++)
        free(table->p_pows[i]);
    free(table->p_pows);
    free(table->p_stps);
    free(table->ps);
}

/* 
 * Returns 128^n % p_i for all p_i in table
 */
int get_power(int index, int n, power_table *table)
{
    return table->p_pows[index][mod(n, (table->p_stps[index] + 1))];
}

/* 
 * This computes a table of values of the form 127^n % p_i for all of the primes
 * p_i that we are using. It allocates memory according to the following paradigm:
 * for each p_i, we allocate slots for max(length, p_i-1) integers
 */
void make_power_tables(power_table *table)
{  
    // first initialize power_table 
    table->p_pows = malloc(sizeof(int *) * table->p_num);
    for(int i = 0; i < table->p_num; i++)
    {
        // this saves the number of slots we need to compute 128^n mod p_i for
        // all n < p_i
        table->p_pows[i] = malloc(sizeof(int) * table->ps[i]-1);
    }  
    
    // actually compute the values
    for(int i = 0; i < table->p_num; i++)
    {
        // first entries for all p_i are 1, i.e., 128^0 % p_i = 1
        table->p_pows[i][0] = 1;
        
        // now, compute powers until we find that n such that 128^n % p = 1
        // or until we have computed powers up to length, i.e., 128^length % p
        table->p_stps[i] = 0;
        for(int  j = 1; j < table->ps[i]-1; j++)
        {
            table->p_pows[i][j] = mod((table->p_pows[i][j-1] * base), 
                                       table->ps[i]);
            table->p_stps[i] = j;
            if(table->p_pows[i][j] == 1)
                break;
        }
        int new_size;
        
        // check if last element is one and if so erase it
        if(table->p_pows[i][table->p_stps[i]] == 1)
        {
            new_size = table->p_stps[i];
            table->p_stps[i]--;
        }
        else
            new_size = table->p_stps[i]+1;
            
        // clean up memory use
        int *new_arr = 
             realloc(table->p_pows[i], sizeof(int) * new_size);
        if(new_arr != NULL)
            table->p_pows[i] = new_arr;
    } 
}

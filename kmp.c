#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include "helper.h"
#include "kmp.h"

void kmp(char *file1, char *pattern_txt, result *r)
{
    text doc;
    text pattern;
    int len;
    len = strlen(pattern_txt);
    // save pattern
    
    pattern.strophe = malloc(sizeof(int) * len);
    for(int i = 0; i < len; i++)
      pattern.strophe[i] = (int)(tolower(pattern_txt[i]));
    pattern.length = len;

    // create memory for result
    r->found = 0;
    r->location = malloc(sizeof(int) * 100);

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
    
    search_kmp(&doc, &pattern, r); 
    // printf("Found: %d\n", found);
    
    free_text(&doc);
    free_text(&pattern);
}

void setupTable(text *pattern, int *table) 
{
   int i, j;
   int m = pattern->length;
   i = 0;
   j = table[0] = -1;
   while (i < m) 
   {
      while (j > -1 && pattern->strophe[i] != pattern->strophe[j])
         j = table[j];
      i++;
      j++;
      if (i < pattern->length && pattern->strophe[i] == pattern->strophe[j])
         table[i] = table[j];
      else
         table[i] = j;
   }
}

/*
 *  Does the actual searching
 */ 
int search_kmp(text *doc, text *pattern, result *r)
{
    int found, table_index, doc_index;
    int doc_length = doc -> length;
	 int pattern_length = pattern -> length;

	 int *table = (int *) malloc(sizeof(int) * 128);

	 setupTable(pattern, table);

	 found = table_index = doc_index = 0;

	 while (doc_index < doc_length) 
	 {
         while (table_index > -1 
                && pattern->strophe[table_index] != doc->strophe[doc_index])
            table_index = table[table_index];
            table_index++;
            doc_index++;
         if(table_index >= pattern_length)
         {
            //printf("%d", (doc_index - table_index));
            table_index = table[table_index];
            found++;
            
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
            r->location[r->found - 1] = doc_index - pattern->length;
		   }		
	}
    free(table);
    return found; 
}




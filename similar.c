#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "helper.h"
#include "similar.h"

double similar(char *file1, char *file2)
{
   // seed random number generator for later use
   srand(time(NULL));
   
   text doc1;
   text doc2;

   readit(&doc1, 1, file1);
   readit(&doc2, 1, file2);

   if(doc1.max_word_size == 0 || doc2.max_word_size == 0)
   {
      printf("Once of the documents was empty!\n");
      return -1;
   }

   int hash_size = 5;

   // create tables to hold shingles
   char **words1 = malloc(sizeof(char*) * ((doc1.num_words - hash_size) + 1));
   for(int i = 0; i < (doc1.num_words - hash_size) + 1; i++)
      words1[i] = malloc(sizeof(char) * (1 + (hash_size * doc1.max_word_size)));

   char **words2 = malloc(sizeof(char*) * ((doc2.num_words - hash_size) + 1));
   for(int i = 0; i < (doc2.num_words - hash_size) + 1; i++)
      words2[i] = malloc(sizeof(char) * (1 + (hash_size * doc2.max_word_size)));

   // generate shingles
   make_shingles(&doc1, hash_size, words1);
   make_shingles(&doc2, hash_size, words2);
    
   // hash shingles
   unsigned long long *hashes1 = malloc(sizeof(unsigned long long) * 
                     ((doc1.num_words - hash_size) + 1));
   unsigned long long *hashes2 = malloc(sizeof(unsigned long long) * 
                     ((doc2.num_words - hash_size) + 1));

   make_hashes(hashes1, words1, (doc1.num_words - hash_size) + 1);
   make_hashes(hashes2, words2, (doc2.num_words - hash_size) + 1); 

   // compare documents
   double score = compare(hashes1, hashes2, (doc1.num_words - hash_size) + 1, 
           (doc2.num_words - hash_size) + 1);
   
   // free memory
   free(doc1.strophe);
   free(doc2.strophe);
   for(int i = 0; i < (doc1.num_words - hash_size) + 1; i++)
      free(words1[i]);
   free(words1);
   for(int i = 0; i < (doc2.num_words - hash_size) + 1; i++)
      free(words2[i]);
   free(words2);
   free(hashes1);
   free(hashes2);

   return score;
}

/*
 *    This functions takes the two hash arrays and loads them into
 *    a bst in order to find similar elements
 */
double compare(unsigned long long *hashes1, unsigned long long *hashes2,
             int len1, int len2)
{
   // create a bst
   node *tree = malloc(sizeof(node));
   tree->set_num = -100;
   tree->value = 0;
   tree->left = NULL;
   tree->right = NULL;

   // pick smaller of two hashes
   unsigned long long *h1, *h2;
   int l1, l2;
   if(len1 < len2)
   {
      l1 = len2;
      h1 = hashes2;
      l2 = len1;
      h2 = hashes1;
   }
   else
   {
      l1 = len1;
      h1 = hashes1;
      l2 = len2;
      h2 = hashes2;
   }
   
   // insert hashes1 into a bst
   for(int i = 0; i < l1; i++)
   {
      insert(tree, h1[i], 0);
   }
   
   // insert hashes2 into tree, check for collisions
   int matches = 0;
   for(int i = 0; i < l2; i++)
   {
      if(insert(tree, h2[i], 1) == 1)
         matches++;
   }

   // print results
   

   double score = 0;
   printf("%d of %d hashes matched\n", matches, l2);
   score = 100 * (matches / (double)l2);
      
   printf("%f percent of the hashes matched: \n", score);

   free_tree(tree);

   return score;
}

/* 
 *    Inserts a hash value into a bst, remember which set the value came from
 *    for later comparisons
 */
int insert(node *tree, unsigned long long value, int set_num)
{
   // emtpy tree
   if(tree->set_num == -100)
   {
      tree->value = value;
      tree->set_num = set_num;
   }
   // found value
   if(value == tree->value)
   {
      // both from same set, do nothing
      if(tree->set_num == set_num)
         return 0;
         
      // from different sets, so found a match
      else
         return 1;
   }
   // value less
   else if(value < tree->value)
   {
      // left sub-branch is null, insert there
      if(tree->left == NULL)
      {
         node *new_node = malloc(sizeof(node));
         new_node->left = NULL;
         new_node->right = NULL;
         new_node->value = value;
         new_node->set_num = set_num;
         tree->left = new_node;
         return 0;
      }
      // else search left branch
      else
         return insert(tree->left, value, set_num);
   }
   // value greater
   else if(value > tree->value)
   {
      // left sub-branch is null, insert there
      if(tree->right == NULL)
      {
         node *new_node = malloc(sizeof(node));
         new_node->left = NULL;
         new_node->right = NULL;
         new_node->value = value;
         new_node->set_num = set_num;
         tree->right = new_node;
         return 0;
      }
      // else search left branch
      else
         return insert(tree->right, value, set_num);
   }
   return 0;
}


/*
 *  Frees a bst
 */
void free_tree(node *tree)
{
   if(tree->left == NULL && tree->right == NULL)
   {
      free(tree);
      return;
   }
   else
   {
      if(tree->left != NULL)
         free_tree(tree->left);
      if(tree->right != NULL)
         free_tree(tree->right);
      free(tree);
   }
}

/*
 *    Takes a table of words and hashes each row down to 64 bits
 */
void make_hashes(unsigned long long *hashes, char **words, int len)
{
   for(int i = 0; i < len; i++)
   {
      int length = 0;
      length = strlen(words[i]);
      hashes[i] = hash2(words[i], length);
   }
}

/*
 *    Takes num_word sized blocks of words from the document and
 *    saves them in shingles. Progresses by one word at each iteration
 *    This process is called shingling
 */
void make_shingles(text *doc, int num_words, char **shingles)
{
   // index in document
   int index = 0;
   // set position in shingles matrix
   int i = 0;
   // reset value for next time through
   int reset = 0;

   // step through document
   while(index != doc->length)
   {
      index = reset;
      int word_count = 0;
      int count = 0;
      while(word_count < num_words)
      { 
         int c = doc->strophe[index];
         // read until space
         while(c != 32)
         {
            shingles[i][count] = c;
            
            // be sure not to leave document
            if(index == doc->length)
               break;
            else
            {
               count++;
               index++;
            }
            c = doc->strophe[index];
            // check if this is the first time through, if so set reset
         }
         // absorb extra spaces
         while(c == 32)
         {  
            // be sure not to leave document
            if(index == doc->length)
               break;
            else
               index++;
            c = doc->strophe[index];
         }
         if(word_count == 0)
            reset = index;
         word_count++;
      }
      shingles[i][count] = '\0';
      i++;
   }
}

/*
 *   This hash function, borrowed from http://www.cse.yorku.ca/~oz/hash.html
 *   with some slight modifications, returns a 64 bit hash of the given 
 *   string
 */
unsigned long long hash2(char *str, int len)
{
   unsigned long long hash = 5183;//rand();

   for(int i = 0; i < len; i++)
   {
      int c = str[i];
      hash = ((hash << 5) + hash) + c;
   }

   return hash;
}

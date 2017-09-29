#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "helper.h"

/*
 * Reads from a file stream. If is_file = 1 then it opens the text file
 * and reads the file's contents, else it reads from stdin. size is assigned
 * to be a pointer to the size (number of characters) of the thing read
 * Also counts the number of words and the longest words
 */
void readit(text *docum, int is_file, char *filename)
{
    // Decide what type of input to ask for, perform actual read 
    FILE *fr;
    if(is_file == 1)
    {
        fr = fopen (filename, "r"); 
        if(fr == NULL)
            return;
     }
    else
    {
        fr = stdin;
        printf("Enter a string to be searched for, followed by the character:\n");
        fflush(stdout);  
    }
    
    // read entire file into a giant array, first guess is size 1000
    int *doc = malloc(sizeof(int)*1000);
    
    // start reading
    int index = 0;
    // how many blocks of size 1000 have we used
    int thousands = 1;
    int word_count = 1;
    int word_size = 0;
    int tmp_word_size = 0;
    int prevc = -1;
    for (int c = fgetc(fr); c != EOF; c = fgetc(fr))
    {
        // break if user inputs quit marker
        /*if((char)prevc == '/' && c == '\n')
            break;*/
        
        int tmp = tolower(c);
        
        // convert all escape sequences to spaces
        if(tmp < 32)
            tmp = 32;

        // we've reached the end of a word
        if(prevc != 32 && tmp == 32)
        {
            // was this the longest word so far?
            if(tmp_word_size > word_size)
               word_size = tmp_word_size;
        }
        // otherwise increase word count
        else if(tmp != 32)
            tmp_word_size++;

        // start of new word, so update word count and reset size of current word
        if(prevc == 32 && tmp != 32)
        {
            tmp_word_size = 1;
            word_count++;
         }
            
        // check if still room left in allocated memory
        if((index % 1000) != 0 || index == 0)
        {
            doc[index] = (int)tmp;
            index++;
        }
        // else we need to do some re-allocating, give another 1000 slots
        else
        {
            // remember we increased by 1000
            thousands++;

            // realloc
            int *new_doc;
            new_doc = realloc(doc, sizeof(int) * (thousands * 1000));
            if(new_doc != NULL)
                doc = new_doc;
            else
                return;
                
            // put in element
            doc[index] = tmp;
            index++;
        }
        // remember previous character
        prevc = tmp;
    }
       // empty document
    if(index == 0)
    {
         fclose(fr);
         docum->length = index;
         docum->strophe = doc;
         docum->num_words = 0;
         docum->max_word_size = 0;
         return;
    }
    // only spaces
    if(word_size == 0)
    {
         fclose(fr);
         docum->length = index;
         docum->strophe = doc;
         docum->num_words = 0;
         docum->max_word_size = 0;
         return;
    }
    
    // terminate doc
    doc[index-1] = '\0';
    index--;
    
    // delete extra memory we have allocated
    int *new_doc;
    new_doc = realloc(doc, sizeof(int) * (index+1));
    if(new_doc != NULL)
        doc = new_doc;
    else
        return;
        
    // test that read was succesful
    /*for(int i = 0; i < index; i++)
    {
        printf("%c", (char)doc[i]);
    }
    printf("\n");*/

    // close file and return result
    fclose(fr);
    docum->length = index;
    docum->strophe = doc;
    docum->num_words = word_count;
    docum->max_word_size = word_size;
}

/*
 *  Spaces operator characters for plagiarism detection
 */
void pre_process(char *infile, char *outfile)
{
    // Decide what type of input to ask for, perform actual read 
    FILE *fr;
    fr = fopen (infile, "r"); 
        if(fr == NULL)
            return;
 
    // read entire file into a giant array, first guess is size 1000
    int *doc = malloc(sizeof(int)*1000);
    doc[0] = 32;
    
    // start reading
    int index = 1;
    // how many blocks of size 1000 have we used
    int thousands = 1;
    char prevc = ' ';
    for (int c = fgetc(fr); c != EOF; c = fgetc(fr))
    {
         // check if still room left in allocated memory
        if((index % 1000) < 2)
        {
            // remember we increased by 1000
            thousands++;

            // realloc
            int *new_doc;
            new_doc = realloc(doc, sizeof(int) * (thousands * 1000));
            if(new_doc != NULL)
                doc = new_doc;
            else
                return;
        }

        if((int)c < 32)
            c = ' ';
            
        if(prevc == '*' || prevc == '+' || prevc == '-' || prevc == '/' || 
           prevc == '(' || prevc == ')' || prevc == '=' || prevc == '<' || 
           prevc == '>' || prevc == '@' || prevc == '^' || prevc == '|' || 
           prevc == '&' || prevc == '~' || prevc == '$' || prevc == '%' || 
           prevc == '=' || prevc == '.' || prevc == '.' || prevc == ';' ||
           prevc == ':' || prevc == '?' || prevc == ',') 
        {
            doc[index] = ' ';
            index++;
            prevc = ' ';   
        }
        
        
        // found operator
        if(c != '*' && c != '+' && c != '-' && c != '/' && c != '(' &&
           c != ')' && c != '=' && c != '<' && c != '>' && c != '@' &&
           c != '^' && c != '|' && c != '&' && c != '~' && c != '$' && 
           c != '%' && c != '!' && c != '.' && c != '.' && c != ';' &&
           c != ':' && c != '?' && c != ',') 
        {
            doc[index] = c;
            index++;
        }
        else
        {
            if(prevc != ' ')
            {
               doc[index] = ' ';
               doc[index + 1] = c;
               index += 2;
            }
            else
            {
               doc[index] = c;
               index++;
            }
        }
        prevc = c;
    }

    // empty document
    if(index == 0)
    {
         fclose(fr);
         printf("The document was empty!\n");
         free(doc);
         return;
    }
    
    // print result
    FILE *fr2 = fopen(outfile, "w");
    for(int i = 0; i < index; i++)
    {
        fprintf(fr2, "%c", doc[i]);
    }
    fprintf(fr2, "%c", ' ');
    fclose(fr2);
    
    // close file and return result
    fclose(fr);
    free(doc);
}


/*
 * Frees text
 */
void free_text(text *doc)
{
    free(doc->strophe);
}


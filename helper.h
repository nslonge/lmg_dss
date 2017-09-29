// used to hold input from file
typedef struct text
{
   int *strophe;
   int length;
   int num_words;
   int max_word_size;
}text;

typedef struct result
{
   int found;
   int *location;
}result;

void readit(text *docum, int is_file, char *filename);
void pre_process(char *infile, char *outfile);
void free_text(text *doc);



typedef struct node
{
   unsigned long long value;
   int set_num;
   struct node *left;
   struct node *right;
}node;

double similar(char *file1, char *file2);
void make_shingles(text *doc, int num_words, char **shingles);
void make_hashes(unsigned long long *hashes, char **words, int len);
unsigned long long hash2(char *str, int len);
int insert(node *tree, unsigned long long value, int set_num);
double compare(unsigned long long *hashes1, unsigned long long *hashes2,
             int len1, int len2);
void free_tree(node *tree);


#ifndef max
	#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif

#ifndef min
	#define min( a, b ) ( ((a) < (b)) ? (a) : (b) )
#endif

typedef struct power_table
{
    // a two dimensional array of powers of the form a_ij = 128^j mod p_i
    int **p_pows;
    // a one dimenisonal array of the index when we stop computing the powers
    int *p_stps;
    // a one dimensional array of primes
    int *ps;
    // the number of primes
    int p_num;
}power_table;

void finger_print(char *filename, char *pattern_txt, result *r);
void make_power_tables(power_table *table);
int get_power(int index, int n, power_table *table);
void free_table(power_table *table);
int *hash(text *pattern, power_table *table);
int search_finger(text *pattern, text *doc, power_table *table, result *r);
int mod(int a, int n);

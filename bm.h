
#ifndef max
	#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif

// BM functions
void bm(char *file1, char* pattern_txt, result *r);
int search_bm(text *doc, text *pattern, result *r);



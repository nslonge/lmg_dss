typedef struct state_stack
{
   struct state **elms;
   int size;
}state_stack;

// structure for parse tree
typedef struct state
{
   // first outbound arrow
   struct state *out1;
   // second outbound arrow
   struct state *out2;
   // is this an arrow or a state?
   int is_arrow;
   // character to check
   char c;
   // final state
   int is_final;
   // visited and index
   int visited;
   int ind;
}state;

/* Driver */
void regex(char *filename, char *exp, result *ans);

/* Functions for running the NFA */
void check(state *r, char c, state_stack *lst, state_stack *lst2);
int checker(state *r);
int check_final(state_stack *lst);
void copy_erase_list(state_stack *lst1, state_stack *lst2);

void read2(text2 *docum, int is_file, char *filename);

/* NFA construction executor and helpers */
state *build_nfa(text2 *r, state_stack *s);
void make_final(state *r, state *next);
void reset_visited(state *r);
void push2_visited(state_stack *s, state *r);

/* Basic Functions for NFA */
state *or(state *r1, state *r2, state *next);
state *kleene2(state *r, state *next);
void concatenate2(state *r1, state *r2, state *next);
void set_next(state *source, state *result, state *next);

/* Stack mainupulations */
state *pop2(state_stack *s);
void push2(state_stack *s, state *v);

/* Memory Freeing */
void free_nfa(state *r, state_stack *s);
void free_state_stack(state_stack *s);
void push2_free(state_stack *s, state *r);

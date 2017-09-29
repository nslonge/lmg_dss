typedef struct node2
{
   struct node2 *left;
   struct node2 *right;
   char character;
}node2;

typedef struct stack
{
   struct node2 **elms;
   int size;
}stack;

typedef struct text2
{
    char *strophe;
    int length;
}text2;

typedef struct marker
{
   int count;
   int *ind;
}marker;


/* Postfix Executor */
void postfix(char *exp, text2 *r);

/* Basic Functions for postfix operations */
node2 *copy(node2 *e1);
node2 *plus(node2 *e1);
node2 *kleene(node2 *e1);
node2 *concatenate(node2 *e1, node2 *e2);
void get_result(node2 *e1, text2 *r);

/* Stack operations */
void push(stack *s, node2 *v);
node2 *pop(stack *s);

/* Memory management */
void free_stack(stack *s);
void free_tree2(node2 *e1);



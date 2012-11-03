/*
+----
| Word Ladders
| Christopher Bowron
| A26086081
| CSE 841
+----
*/


#include <stdio.h>
#include <stdlib.h>


double branching_factor = 0.0;
int nodes = 0;

typedef struct statestruct
{
    char *word;
    int cost;
    int estimate;
    struct statestruct *parent;
} state;

typedef struct liststruct
{
    state *data;
    struct liststruct *next;
} list;


/* how many characters are different between s and g */
int difference(char *s, char *g)
{
    int i, diff = 0;
    
    for (i=0;i<strlen(s);i++)
    {
	if (s[i]!=g[i])
	    diff++;
    }
    return diff;
}


state *make_state(char *word, int cost, int estimate, state *p)
{
    state *foo = malloc(sizeof(state));
    foo->word = word;
    foo->cost = cost;
    foo->estimate = estimate;
    foo->parent = p;
    return foo;
}

void free_state(state *s)
{
    free(s->word);
    free(s);
}

/* is s a goal state? */
int goalp(state *s, state *g)
{
    return (strcmp(s->word,g->word)==0);
}

/* get the name of the dictionary based in the number of letters */
char *dictionary(int n)
{
    static char base[] = "words ";

    if ((n<3)||(n>7))
    {
	printf("invalid word length\n");
	exit(EXIT_FAILURE);
    }
    
    base[5] = "01234567"[n];

    return base;
}


list *alloclist()
{
    return malloc(sizeof(list));
}

void freelistitem(list *p)
{
    free(p);
}

void push (list **l, state *s)
{
    list *new = alloclist();
    new->data = s;
    new->next = *l;
    *l=new;
} 

state * pop (list **l)
{
    state *m;
    list *temp=*l;
    
    m = temp->data;
    
    *l = temp->next;
    freelistitem(temp);
    return m;
}

int length(list *l)
{
    if (l == NULL) return 0;
    return length(l->next)+1;
}

void printlist(list *l)
{
    while (l)
    {
	printf("%2d %2d %s\n", l->data->cost,
	       l->data->estimate, l->data->word);
	l=l->next;
    }
}

/* return a list of states that are accessible from s */
list *successors(state *s, state *g, FILE *f)
{
    char temp[80];
    list *succ = NULL;
    char *word = s->word;
    int cost = s->cost+1;
    
    fseek(f,0,SEEK_SET);

    while (fscanf(f, "%s",temp)!=EOF)
    {
	if ((difference(temp, word))==1)
	{
	    char *w = malloc(strlen(temp)+1);
	    state *next;

	    strcpy(w, temp);
	    next = make_state(w, cost, difference(w, g->word), s);
	    
	    push(&succ, next);
	}
    }
    return succ;
}

/* insert bar between foo->next and foo->next->next */
void wedge(list *foo, list *bar)
{
    list *t;

    t = foo->next;
    foo->next = bar;
    bar->next = t;
}


void enqueue(list **q, state *s)
{
    list *t, *last = NULL;
    list *mid = NULL;

    push (&mid, s);

    if (!*q)
    {
	*q = mid;
	return;
    }
    
    for (t=*q; t; t=t->next)
    {
	state *c = t->data;
	
	if ((s->estimate+s->cost)<(c->cost+c->estimate))
	{
	    if (last)
	    {
		wedge(last, mid);
	    }
	    else
	    {
		list *temp;

		temp = (*q)->next;
		*q = mid;
		mid -> next = temp;
	    }
	    return;
	}
	last = t;
    }

    last -> next = mid;
}

void printstate(state *s)
{
    printf("%2d %2d %s\n", s->cost, s->estimate, s->word);
}


int find(state *needle, list *haystack)
{
    if (!haystack)
	return 0;
    if (strcmp(needle->word,haystack->data->word)==0)
	return 1;
    return find(needle,haystack->next);
}


state *search(state *start, state *goal)
{
    FILE *dict;
    char *filename = dictionary(strlen(start->word));
    list *q = NULL;
    list *states_reached = NULL;
    int branches;
    
    if (!(dict = fopen(filename, "r")))
    {
	perror("search");
	return NULL;
    }
    
    push(&q, start);
    
    while(q)
    {
	state *current;
	
	current = pop(&q);

	if (find(current,states_reached))
	{
	    
	}
	else
	{
	    list *succ;
	    push(&states_reached, current);
	    
	    if (goalp(current, goal))
	    {
		printf("Goal: "); printstate(current);
		return current;
	    }
	    
	    succ = successors(current, goal, dict);
	
	    branches = 0;
	    while (succ)
	    {
		state *n = pop(&succ);
		branches++;
		enqueue(&q, n);
	    }
	    
	    branching_factor = ((branching_factor*nodes)+(branches))/(nodes+1);
	    nodes++;
	}
    }
    fclose(dict);
    return NULL;
}


/* return a random word of length n */
char *randomword(int n)
{
    char *filename = dictionary(n);
    FILE *dict;
    int branches;
    int lengths[] = {671, 2920, 6747, 11142,16352, 37832};
    int max = lengths[n-3];
    char *result = NULL;
    
    if (!(dict = fopen(filename, "r")))
    {
	perror("randomword");
	exit(EXIT_FAILURE);
    }
    else
    {
	char temp[80];	
	int c;
	int index = rand() % max;

	fseek(dict,0,SEEK_SET);

	for(c=0;c<index;c++)
	    fscanf(dict, "%s",temp);

	result = malloc(strlen(temp)+1);
	strcpy(result, temp);
    }
    fclose(dict);
    return result;
}

int main(int argc, char **argv)
{
    char *startword;
    char *goalword;
    state *reached;
    int record = 1;
    int cost;
    int i;
    int randoms = 0;
    
    srand(time(0));
    
    if (argc >= 3)
    {
	startword = argv[1];
	goalword = argv[2];
    }
    else
    {
	randoms = 1;
	record = 1;
    }

    if (argc>=4)
    {
	if (strcmp(argv[3],"record")==0)
	    record = 1;
    }
    
    
    setbuf(stdout, NULL);

    for(i=0;i<50;i++)
    {
	if (randoms)
	{
	    int n = rand() % 5 + 3;
	    startword = randomword(n);
	    goalword = randomword(n);
	}
	
	printf("start: %s\n", startword);
	printf("goal: %s\n", goalword);
	reached = search(make_state(goalword, 0, 0, NULL),
			 make_state(startword, 0, 0, NULL));

	if (!reached)
	{
	    printf("path not found\n");
	}
	else
	{
	    cost = reached->cost;
	    printf("branching factor: %f\n", branching_factor);
	    printf("goal found at cost %d\n", cost);
	    
	    /* we have the path now print it */
	    for(;reached;reached=reached->parent)
	    {
		printf("-->%s\n", reached->word);
	    }
	    
	    if (record)
	    {
		FILE *history=fopen("results.word_ladder", "a+");
		if (!history)
		{
		    perror("main");
		    exit(EXIT_FAILURE);
		}
		fprintf(history, "%d\n", cost);
		fclose(history);
	    }
	}

	if (!randoms)
	    break;
    }
}

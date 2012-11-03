#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

int main(int argc, char **argv)
{
    char *word;
    char dictword[80];
    int i;
    
    if (argc!=2)
    {
	printf("this program generates words that can be found by rearranging letters\n");
	printf("of a given word.  A word must be supplied as an argument.\n");
	exit(EXIT_FAILURE);
    }

    for (word=argv[1],i=0;i<strlen(word);i++)
	word[i] = tolower(word[i]);
    
    for (;!feof(stdin);fgets(dictword,80,stdin))
    {
	int i;
	for(i=0;i<strlen(dictword);i++)
	    if (dictword[i]=='\n')
		dictword[i]=0;
	
	if (subword(dictword, word))
	    if (valid(dictword))
		printf("%s\n", dictword);
    }
}

int subword(char *needle, char *haystack)
{
    static char local[80];
    int i;
    
    strcpy(local, haystack);

    for (i=0;i<strlen(needle);i++)
    {
	int j;
	char c = needle[i];

	for (j=0;;j++)
	{
	    /*
	      if we reach the end of the word, the letter is not
	      found, and the word is not a subword
	    */
	    if (local[j] == 0)
		return 0;
	    
	    if (local[j] == c)
	    {
		local[j] = ' ';
		break;
	    }
	}
    }
    return 1;
}

int valid(char *s)
{
    int j=strlen(s);

    return (j>=4);

    //&&!(s[j-1]=='s'&&s[j-2]!='s'));
}

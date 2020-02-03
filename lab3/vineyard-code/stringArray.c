//Subtle problems with dealing with arrays of strings
#include <stdio.h>
#include <string.h>

int main()
{
  char str[10][5];//10 strings, 5 chars each
  int pos=0;
  int rdflag;
  rdflag=1;
  printf("\nEnter 5 strings. press ctrl+D to stop input\n\n");
  while (rdflag) {
    rdflag = scanf("%s",str[pos]);
    if (rdflag != EOF) pos++;
    else rdflag = 0;
  }
  printf("\n\nYour strings are\n\n");
  for (rdflag = 0; rdflag <= pos; rdflag++)
    printf("%s\n",str[rdflag]);
}

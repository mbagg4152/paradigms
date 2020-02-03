/*
 * working with strings.
 * scanf is not appropriate for input, use fgets
 */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define space ' '
#define OUT 0
#define IN 1

void mkNull(char *s);
unsigned alphaCount(char *str);
void reverseOrder(char *str);
unsigned wordCount(char *str);

int main(){
	char input[40];
	printf("enter string: \n");
	fgets(input, 20, stdin);
	printf("Word count: %u\n", wordCount(input));
	printf("Letter count: %u\n", alphaCount(input));
	reverseOrder(input);
	return 0;
}

// initialize a string to the empty string
void mkNull(char *s){ *s = '\0'; }


unsigned wordCount(char *str){
	int state = OUT;
	unsigned wc = 0;
	unsigned aCount = 0;
	while (*str) {
		if ((*str == ' ')|| (*str == '\n')|| (*str == '\t')) // If next character is a separator, set the state as OUT
			state = OUT;
		else if (state == OUT) { // If next character is not a word separator and state is OUT, then set the state as IN and increment word count
			state = IN;
			++wc;
		}
		++str;
	}
	return wc;
}

unsigned alphaCount(char *str){
	int state = OUT;
	unsigned aCount = 0;
	while (*str) {
		if (isalpha(*str)) ++aCount;
		++str;
	}
	return aCount;

}

void reverseOrder(char *str){
	//printf("fun reverseOrder\n");
}

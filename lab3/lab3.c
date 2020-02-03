/*
 * working with strings.
 * scanf is not appropriate for input, use fgets
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define space ' '
#define OUT 0
#define IN 1

void wordCount(char* inputStr);
void mkNull(char *s);
void alphaCount();
void reverseOrder();
unsigned countWords(char *str);

int main(){
	char input[40];

	printf("enter string: \n");
	fgets(input, 20, stdin);
	printf("input: %s\n", input);
	printf("Word count: %u\n", countWords(input));
	alphaCount();

	reverseOrder();
	return 0;
}

// initialize a string to the empty string
void mkNull(char *s){
	*s = '\0';
}

void wordCount(char* inputStr){
	int wordCount = 1;

	for (int i = 0; strlen(inputStr); i++)
		//if (strncmp(' ', (char)inputStr[i]) == 0) wordCount++;

		printf("word count: %d\n", wordCount);


}


unsigned countWords(char *str){
	int state = OUT;
	unsigned wc = 0;                      // word count

	// Scan all characters one by one
	while (*str) {
		// If next character is a separator, set the
		// state as OUT
		if (*str == ' ' || *str == '\n' || *str == '\t')
			state = OUT;

		// If next character is not a word separator and
		// state is OUT, then set the state as IN and
		// increment word count
		else if (state == OUT) {
			state = IN;
			++wc;
		}

		// Move to next character
		++str;
	}

	return wc;
}

void alphaCount(){
	//printf("fun alphaCount\n");
}

void reverseOrder(){
	//printf("fun reverseOrder\n");
}

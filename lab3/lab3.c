/*
 * working with strings.
 * scanf is not appropriate for input, use fgets
 */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define REG_CHAR 1 // flag used for word count for non separator char
#define SP_CHAR 0 // flag used for word count for separator char

unsigned alphaCount(char *inLine);
unsigned wordCount(char *inLine);
void mkNull(char *s);
void output(unsigned words, unsigned alphas);
void reverseOrder(char *inLine);
unsigned charCheck(char *toCheck);

int main(){
	char input[40];
	printf("enter string: \n");
	fgets(input, 20, stdin);
	output(wordCount(input),alphaCount(input));
	reverseOrder(input);
	return 0;
}


void output(unsigned words, unsigned alphas){
	printf("Word count: %u\n", words);
	printf("Letter count: %u\n", alphas);
}

unsigned wordCount(char *inLine){
	int charType = SP_CHAR;
	unsigned wordCount = 0;
	while (*inLine) {
		if(charCheck(inLine)) charType = SP_CHAR; // set separator flag
		else if (charType == SP_CHAR) { // only counts words after seeing separator
			charType = REG_CHAR;
			++wordCount;
		}
		++inLine;
	}
	return wordCount;
}

unsigned charCheck(char *toCheck){
	return (*toCheck == ' ') || (*toCheck == '\n') || (*toCheck == '\t');
}

unsigned alphaCount(char *inLine){
	unsigned alphaCnt = 0;
	while (*inLine) {
		if (isalpha(*inLine)) ++alphaCnt;
		++inLine;
	}
	return alphaCnt;

}

void reverseOrder(char *inLine){
	//printf("fun reverseOrder\n");
}

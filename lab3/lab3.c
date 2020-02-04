/* Maggie Horton
 * CS-231 - Lab 3
 * lab3.c - working with strings
 * ------------------------------
 * ALGORITHM
 * ------------------------------
 * [EXTERN. DATA TABLE]
 * REG_CHAR		flag used for word count for non separator char
 * SP_CHAR 		flag used for word count for separator char
 * STR_LEN		initializer for array size, string lenth
 * STR_NUM		initializer for array size, string count
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define REG_CHAR 1
#define SP_CHAR 0
#define STR_LEN 40
#define STR_NUM 40

unsigned alphaCount(char *inLine);
unsigned charCheck(char *check);
unsigned wordCount(char *inLine);
void output(unsigned words, unsigned alphas);
void reverseOrder(char *inLine);

int main(){
	char input[40];
	printf("enter string: \n");
	fgets(input, 20, stdin);
	output(wordCount(input), alphaCount(input));
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

unsigned charCheck(char *check){
	return (*check == ' ') || (*check == '\n') || (*check == '\t') || (*check == '\r');
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
	char words[STR_NUM][STR_LEN];
	char *token;
	token = strtok(inLine, " ");
	int c = 0;
	while(token != NULL) {
		printf("adding: %s\n", token);
		//strcpy(token, words[c]);
		strcpy(words[c], token);
		token = strtok(NULL, " ");
		++c;
	}

	int i = 0;
	while(words[c]) {
		printf("in arr: %s\n", words[i]);
		++i;
	}





}

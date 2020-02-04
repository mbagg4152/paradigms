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

#define A_LOW 'a'
#define A_UP 'A'
#define ALPHA_LEN 26
#define NAME_LEN 25
#define REG_CHAR 1
#define SP_CHAR 0
#define STR_LEN 100
#define STR_NUM 100
#define Z_LOW 'z'
#define Z_UP 'Z'




unsigned alphaCount(char *inLine);
unsigned charCheck(char *check);
unsigned wordCount(char *inLine);
void output(unsigned words, unsigned alphas);
void reverseOrder(char *inLine);

int main(){
	char input[STR_LEN];
	printf("enter string: \n");
	fgets(input, STR_LEN, stdin);
	output(wordCount(input), alphaCount(input));
	reverseOrder(input);
	return 0;
}

void output(unsigned words, unsigned alphas){
	printf("Word count: %u\n", words);
	printf("Number of letters seen: %u\n", alphas);
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
unsigned charCheck(char *check) {
	return (*check == ' ') || (*check == '\n') || (*check == '\t') || (*check == '\r');
}

unsigned alphaCount(char *inLine){
	unsigned alphaCnt = 0;
	int pos = 0;
	int alphaTally[ALPHA_LEN] = {0}, tallyTotal[ALPHA_LEN] = {0};

	while (inLine[pos] != '\0') {
		if (isalpha(*inLine)) {
			++alphaCnt;
			char t = inLine[pos];
			if (((*t).tolower() >= A_LOW) && ((*t).tolower() <=Z_LOW)) {
				int tmp = inLine[pos] - A_LOW;
				//printf("lower if-else - alphaTally & tallyTotal before ++: %d, %d\n", alphaTally[tmp], tallyTotal[tmp]);
				++alphaTally[tmp];
				++tallyTotal[tmp];
			}
			// else if((inLine[pos] >= A_UP) && (inLine[pos]<=Z_UP)) {
			// 	int tmp = inLine[pos] - A_UP;
			// 	//printf("upper if-else - alphaTally & tallyTotal before ++: %d, %d\n", alphaTally[tmp], tallyTotal[tmp]);
			// 	++alphaTally[tmp];
			// 	++tallyTotal[tmp];
			// }
		}

		++inLine;
	}

	printf("Outputting letters and appearance rate as (letter, count)\n" );

	for (int i = 0; i< ALPHA_LEN; i++) {
		if(tallyTotal[i]!=0)
			printf("(%c, %d)  ", i + A_LOW, tallyTotal[i]);
	}
	printf("\n" );




	return alphaCnt;
}

void reverseOrder(char *inLine){
	char words[STR_NUM][STR_LEN];
	char *token;
	token = strtok(inLine, " ");
	int c = 0;
	while(token != NULL) {
		//	printf("adding: %s\n", token);
		//strcpy(token, words[c]);
		strcpy(words[c], token);
		token = strtok(NULL, " ");
		++c;
	}





}

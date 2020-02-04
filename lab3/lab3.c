/* Maggie Horton
 * CS-231 - Lab 3
 * lab3.c - working with strings
 * ------------------------------
 * ALGORITHM
 * ------------------------------
 * [EXTERN. DATA TABLE]
 * SP_CHAR		flag used for word count for non separator char
 * SAW_SP_CHAR 		flag used for word count for separator char
 * STR_LEN		initializer for array size, string lenth
 * STR_NUM		initializer for array size, string count
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define A_LOW 'a'
#define ALPHA_LEN 26
#define NAME_LEN 25
#define REG_CHAR 0
#define SAW_SP_CHAR 0
#define SP_CHAR 1
#define STR_LEN 100
#define STR_NUM 100
#define Z_LOW 'z'

void alphaCount(char *inLine);
unsigned isSpace(char *check);
unsigned wordCount(char *inLine);
void output(unsigned words);
void reverseOrder(char *inLine);

int main(){
	char input[STR_LEN];
	printf("enter string: \n");
	fgets(input, STR_LEN, stdin);
	output(wordCount(input));
	alphaCount(input);
	reverseOrder(input);
	printf("\n");
	return 0;
}

void output(unsigned words){
	printf("Word count: %u\n", words);
}




/* PARAM inLine: string recieved from file/input
 * VAR charTypeFlag: used to keep track of whether a character
 * is a space or separator
 */
unsigned wordCount(char *inLine){
	int charTypeFlag = REG_CHAR;
	char words[STR_NUM][STR_LEN];
	unsigned wordCount = 0;
	while (*inLine) {
		// set flag after seeing separator
		if(isSpace(inLine) == SP_CHAR) charTypeFlag = SAW_SP_CHAR;
		// only increment word counter after seeing separator
		else if (charTypeFlag == SAW_SP_CHAR) {
			charTypeFlag = SP_CHAR;
			++wordCount;
		}
		++inLine;
	}
	return wordCount;
}

unsigned isSpace(char *check) {
	return (*check == ' ') || (*check == '\n') || (*check == '\t') || (*check == '\r');
}

void alphaCount(char *inLine){
	int pos = 0;
	int alphaTally[ALPHA_LEN] = {0};
	while (inLine[pos] != '\0') {
		if (isalpha(*inLine)) {
			if ((tolower(inLine[pos]) >= A_LOW) && (tolower(inLine[pos]) <= Z_LOW)) {
				int tmp = tolower(inLine[pos]) - A_LOW;
				++alphaTally[tmp];
			}
		}
		pos++;
	}

	printf("Outputting letters and appearance rate as [letter, count]\n" );
	for (int i = 0; i < ALPHA_LEN; i++) {
		if(alphaTally[i] != 0)
			printf("[%c,%d] ", i + A_LOW, alphaTally[i]);
	}
	printf("\n" );
}

void reverseOrder(char *inLine){
	char words[STR_NUM][STR_LEN];
	char *token;
	token = strtok(inLine, " ");
	int c = 0;
	while(token != NULL) {
		//	printf(" %s ", token);
		strcpy(words[c], token);

		token = strtok(NULL, " ");
		c++;
	}
//	printf("\n");
	for(int i = c-1; i >= 0; i--)
		if(isSpace(words[i]) != SP_CHAR) printf("\nval: %s, index: %d ", words[i], i);




}

/* Maggie Horton
 * CS-231 - Lab 3
 * lab3.c - working with strings
 * ------------------------------
 * ALGORITHM
 * ------------------------------
 * [EXTERN. DATA TABLE]
 * A, '\0', '\n', '\r', ' ', '\t', 'z'
 * IS_SP		flag used for word count for non separator char
 * SP_FLAG          flag used for word count for separator char
 * STR_LEN		initializer for array size, string lenth
 * STR_NUM		initializer for array size, string count
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define AB_LEN 26
#define IS_SP 1
#define NOT_SP 0
#define SP_FLAG 0
#define STR_LEN 100
#define STR_NUM 100

unsigned isSpace(char *check);
unsigned wordCount(char *inLine);
void alphaCount(char *inLine);
void reverseOrder(char *inLine);
void strip(char *str);

int main() {
	char input[STR_LEN];
	char in[STR_NUM][STR_LEN];
	int pos = 0;
	int rdflag;
	rdflag = 1;
	printf("\nEnter 5 strings. press ctrl+D to stop input\n\n");
	printf("enter string: \n");
	while (rdflag) {
		//fgets(input, STR_LEN, stdin);
		rdflag = fgets(in[pos], STR_LEN, stdin);
		//rdflag = scanf("%s", str[pos]);
		if (rdflag != EOF) {pos++;} else {rdflag = 0;}
	}

	int tmp  = 0;
	for (tmp = 0; tmp < pos; tmp++) {
		if (isSpace(in[tmp])==IS_SP) {break;}
		printf("Word count: %u\n", wordCount(in[tmp]));
		alphaCount(in[tmp]);
		reverseOrder(in[tmp]);
		printf("\n");
	}
	//printf("%s\n", str[rdflag]);








	return 0;
}

/* PARAM inLine: string recieved from file/input
 * VAR charTypeFlag: used to keep track of whether a character
 * is a space or separator
 */
unsigned wordCount(char *inLine) {
	int charTypeFlag = NOT_SP;
	char words[STR_NUM][STR_LEN];
	unsigned wordCount = 0;
	while (*inLine) {
		if(isSpace(inLine) == IS_SP) { // set flag after seeing separator
			charTypeFlag = SP_FLAG;
		} else if (charTypeFlag == SP_FLAG) { // only increment word counter after seeing separator
			charTypeFlag = IS_SP;
			++wordCount;
		}
		++inLine;
	}
	return wordCount;
}

unsigned isSpace(char *check) {
	return (*check == ' ') || (*check == '\n') || (*check == '\t') || (*check == '\r');
}

void alphaCount(char *inLine) {
	int pos = 0;
	int alphaTally[AB_LEN] = {0};
	while (inLine[pos] != '\0') {
		if (isalpha(*inLine)) {
			if ((tolower(inLine[pos]) >= 'a') && (tolower(inLine[pos]) <= 'z')) {
				int tmp = tolower(inLine[pos]) - 'a';
				++alphaTally[tmp];
			}
		}
		pos++;
	}

	printf("Outputting letters & occurance as [letter, count]\n" );
	for (int pos = 0; pos < AB_LEN; pos++) {
		if(alphaTally[pos] != 0) {
			printf("[%c,%d] ", (pos + 'a'), alphaTally[pos]);
		}
	}
	printf("\n" );
}

void strip(char *str) {
	if (str == NULL) { return; }
	int len = strlen(str);
	if ((str[len-1] == '\n') || (str[len-1] == '\r') || (str[len-1] == '\t')) {
		str[len-1]  = '\0';
	}
}

void reverseOrder(char *inLine) {
	char words[STR_NUM][STR_LEN];
	char *token;
	token = strtok(inLine, " ");
	int c = 0;
	while(token != NULL) {
		strip(token);
		strcpy(words[c], token);
		token = strtok(NULL, " ");
		c++;
	}
	printf("Reversed order: ");
	for(int i = c-1; i >= 0; i--) {
		printf("%s ", words[i]);
	}
	printf("\n");
}

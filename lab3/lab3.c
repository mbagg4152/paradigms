/* Maggie Horton
 * CS-231 - Lab 3
 * lab3.c - working with strings
 * ------------------------------
 * ALGORITHM
 * get input from keyboard (or file)
 * if no input errors -->
 * from last line to first:
 *              if lines have value -->
 *                              display line number
 *                              find & display word count
 *                              find & display letter occurance
 *                              display original string & reversed string
 *
 * ------------------------------
 * [EXTERNAL DATA TABLE]
 * NAME				VAL
 * AB_LEN			size of the alphabet
 * TRUE	                        value in C is true if nonzero
 * FALSE                value in C is false if zero
 * STR_LEN		initializer for array size, string lenth
 * STR_NUM		initializer for array size, string count
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define AB_LEN 26
#define TRUE 1
#define FALSE 0
#define STR_LEN 100
#define STR_NUM 100

unsigned isSpace(char *check);
unsigned wordCount(char *inLine);
void alphaInventory(char *inLine);
void reverseOrder(char *inLine);
void strip(char *inLine);

/* The main function that drives the program
 * RETURNS: 0 when done executing
 * VAR inputStr: used to hold the input strings
 * VAR flag: keeps track of string parsing
 */
int main() {
	char inputStr[STR_NUM][STR_LEN];
	int pos = 0, flag = 1;
	printf("\n\n#########################################\n\n");
	printf("This is the CS-231 Lab 3 Assignment!\nBrought to you by Maggie Horton\n\n");
	printf("#########################################\n");
	printf("\nEnter strings. Press ctrl+D to stop input\n");
	while (flag) {
		flag = fgets(inputStr[pos], STR_LEN, stdin);
		if (flag != EOF) {
			pos++;
		} else {
			flag = 0;
		}
	}
	flag = 0;
	for (int count = pos - 1; count >= 0; count--) {
		if (wordCount(inputStr[count]) == 0) {
			printf("\n"); // dont want to run functions on empty input
		} else {
			if (isSpace(inputStr[count]) == TRUE) {
				printf("\n"); // dont want to run functions on empty input
			}
			strip(inputStr[count]);
			printf("\n##########################################################\n");
			printf("Line %d's word count: %u\n", count + 1, wordCount(inputStr[count]) + 1);
			alphaInventory(inputStr[count]);
			reverseOrder(inputStr[count]);
			printf("\n");
		}
	}
	return 0;
}

/* Counts the number of words within a given line
 * PARAM inLine: string recieved from file/input
 * RETURNS: unsigned, no. of words in a line
 * VAR wordCount: value to return after counting no. of words
 * VAR counter: used to keep track while looping through array
 */
unsigned wordCount(char *inLine) {
	unsigned wordCount = 0;
	int counter = 0;
	char tmpChunk[STR_LEN];
	for (int i = 0; inLine[counter] != '\0'; i++) {
		if ((inLine[counter] == ' ') && (inLine[counter + 1] != ' ')) {
			wordCount++;
		}
		counter++;
	}
	return wordCount;
}

/* Counts the occurance of each letter present within a given Line
 * PARAM inLine: the string to be parsed in order to find letter count
 * RETURNS: void
 * VAR alphaTally: array of counters used to keep track of letter occurance
 * VAR addToIndex: difference between value & 'a', used to increment values
 * at a specific index
 */
void alphaInventory(char *inLine) {
	int pos = 0;
	int alphaTally[AB_LEN] = {0};
	while (inLine[pos] != '\0') {
		if (isalpha(*inLine)) {
			if ((tolower(inLine[pos]) >= 'a') && (tolower(inLine[pos]) <= 'z')) {
				int addToIndex = tolower(inLine[pos]) - 'a';
				++alphaTally[addToIndex];
			}
		}
		pos++;
	}

	printf("(Letter, occurance) in line:\n");
	for (int pos = 0; pos < AB_LEN; pos++) {
		if (alphaTally[pos] != 0) {
			printf("(%c,%d) ", (pos + 'a'), alphaTally[pos]);
		}
	}
	printf("\n");
}

/* Strips any trailing newline character from a string.
 * PARAM inLine: string to be checked to strip
 * RETURNS: void
 */
void strip(char *inLine) {
	for (int x = 0; x <= STR_LEN; x++) {
		if (inLine[x] == '\n') {
			inLine[x] = '\0';
			break;
		}
	}
}

/* Reverses the order of which word appears and outputs the new values\
 * PARAM inLine: string to have its words reordered
 * RETURNS: void
 * VAR words: holds strings after tokenization
 * VAR reversed: will have strings stored in reverse order
 * VAR strChunk: tokenized chunks based on delimiter " "
 */
void reverseOrder(char *inLine) {
	char words[STR_NUM][STR_LEN], reversed[STR_NUM][STR_LEN];
	char *strChunk;
	strChunk = strtok(inLine, " ");
	int c = 0;
	while (strChunk != NULL) {
		int len = strlen(strChunk);
		strcpy(words[c], strChunk);
		c++;
		strChunk = strtok(NULL, " ");
	}

	printf("\nOriginal input: ");
	for (int i = 0; i < c; i++) {
		printf(" %s", words[i]);
		strcpy(reversed[c - 1 - i], words[i]);
	}

	printf("\nReversed order: ");
	for (int i = 0; i < c; i++) {
		if (i == 0) {
			printf("%s", reversed[i]);
		} else {
			printf(" %s", reversed[i]);
		}
	}
	printf("\n");
}

/* Checks if character is a space, newline, tab or carriage return, used to
 * stop output if input is not valuable
 * PARAM check: char to be checked
 * RETURNS: 0 if character is not a space, newline, tab or return
 */
unsigned isSpace(char *check) {
	return (*check == ' ') || (*check == '\n') || (*check == '\t') || (*check == '\r');
}

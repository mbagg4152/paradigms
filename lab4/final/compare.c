#include<stdlib.h>
#include<stdio.h>
#include<unistd.h>
#include<string.h>
#include<ctype.h>

/* Maggie Horton
 * CS-231
 * Lab 4 - compare.c
 * The purpose of this program is to compare each word from stdin against each word in the
 * dictionary file in order to determine whether or not it is spelled correctly.
 * ----------------------------------------------------------------------------------------------
 * ALGORITHM
 * if there are enough args & the dictionary file is valid
 *      while looping is allowed
 *          if dictionary word > input string, move to next word in dictionary
 *          if dictionary word < input string, move to next word in stdin
 *      if no match is found or if words from stdin remain, tell user they're
 *      spelled incorrectly
 * ----------------------------------------------------------------------------------------------
 * EXTERNAL DATA TABLE
 * TRUE     non-zero values are true in c
 * FALSE    zero values are false in c
 * LEN      arbitrary max length of any word */

#define TRUE 1
#define FALSE 0
#define LEN 50

void checkWords(char *dictFName);
int getDifference(char *first, char *second);
void quit(char *message, int code);

/* start of the program, calls checkWords()
* returns:     0 when done executing
* argc:        param - no. of args supplied
* argv:        param - args supplied */
int main(int argc, char *argv[]) {
    if ((argc > 2) || (argc < 2)) { quit("incorrect arg count", 0); }
    checkWords(argv[1]);
    return 0;
}

/* function that looks at the contents of the dictionary and stdin to see
 * if any words from stdin match. displays appropriate message based on the
 * difference between two strings
 * returns:     nothing
 * dictFName:   param - name of the dictionary file
 * haltLoop:    var - flag for stopping while loop */
void checkWords(char *dictFName) {
    char input[LEN], dContent[LEN];
    int difference, haltLoop = FALSE;
    FILE *dFile = fopen(dictFName, "r"); // open dictionary
    if (dFile != NULL) {
        // if one or both of the input sources is empty, no need to loop
        if (fgets(input, LEN, stdin) == NULL || fgets(dContent, LEN, dFile) == NULL) haltLoop = TRUE;
        while (!haltLoop) {
            difference = getDifference(input, dContent); // get difference of strings
            if (difference > 0) { // dictionary word > input string, move to next dictionary word
                if (fgets(dContent, LEN, dFile) == NULL) haltLoop = TRUE;  // got to EOF
            } else if (difference < 0) { // go to next string from stdin
                if (fgets(input, LEN, stdin) == NULL) haltLoop = TRUE;  // no more input to compare
            } else {
                // if dictionary or stdin have no more content, stop looking
                if (fgets(input, LEN, stdin) == NULL || fgets(dContent, LEN, dFile) == NULL) haltLoop = TRUE;
            }
        }
        // any remaining words from stdin are spelled wrong
        while (fgets(input, LEN, stdin) != NULL) { printf("Wrong: %s", input); }

    } else { quit("Couldn't open dictionary", 0); }

}

/* helper function that returns the difference between two strings
 * returns:     int - the difference between strings
 * first:       param - first word to be compared
 * second:      param - second word to be compared */
int getDifference(char *first, char *second) {
    int compareRes = strcmp(first, second);
    if (compareRes == 0) { printf("Correct: %s", first); } // no difference between words
    else if (compareRes < 0) { printf("Wrong: %s", first); } // difference between words
    return compareRes;
}

/* nice little helper function for quitting
 * returns:     nothing
 * message:     param - message to display before exit()
 * code:        exit code */
void quit(char *message, int code) {
    printf("%s\n", message);
    exit(code);
}


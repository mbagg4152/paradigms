#include<stdlib.h>
#include<stdio.h>
#include<unistd.h>
#include<string.h>
#include<ctype.h>

#define TRUE 1
#define FALSE 0
#define LEN 50

void checkWords(char *dictFName);
int getDifference(char *word1, char *word2);
void quit(char *message, int code);

int main(int argc, char *argv[]) {
    checkWords(argv[1]);
}

void checkWords(char *dictFName) {
    char input[LEN], dContent[LEN];
    int difference, haltLoop = FALSE;
    FILE *dFile = fopen(dictFName, "r"); // open dictionary
    if (dFile != NULL) {
        // if one or both of the input sources is empty, no need to loop
        if (fgets(input, LEN, stdin) == NULL || fgets(dContent, LEN, dFile) == NULL) haltLoop = TRUE;
        while (!haltLoop) {
            difference = getDifference(input, dContent); // get value from comparison
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

int getDifference(char *word1, char *word2) {
    int compareRes = strcmp(word1, word2);
    if (compareRes == 0) { printf("Correct: %s", word1); } // no difference between words
    else if (compareRes < 0) { printf("Wrong: %s", word1); } // difference between words
    return compareRes;
}

void quit(char *message, int code) {
    printf("%s\n", message);
    exit(code);
}


#include<stdlib.h>
#include<stdio.h>
#include<unistd.h>
#include<string.h>
#include<ctype.h>

#define TRUE 1
#define FALSE 0
void checkWords(char *dictFName);
int getDifference(char *word1, char *word2);
void quit(char *message, int code);

int main(int argc, char *argv[]) {
    checkWords(argv[1]);
}

void checkWords(char *dictFName) {
    char inWord[50], dictWord[50];
    int compareRes, haltLoop = FALSE;
    FILE *dict = fopen(dictFName, "r"); // open dictionary
    if (dict != NULL) {
        // if one of the input sources is empty, no need to loop
        if (fgets(inWord, 50, stdin) == NULL || fgets(dictWord, 50, dict) == NULL) { haltLoop = TRUE; }
        while (!haltLoop) {
            compareRes = getDifference(inWord, dictWord); // get value from comparison
            if (compareRes > 0) { // dictionary word > input string, move to next dictionary word
                if (fgets(dictWord, 50, dict) == NULL) { haltLoop = TRUE; } // reached EOF
            } else if (compareRes < 0) { //if the value is less than 0, take the next string from stdin
                //if stdin has reached EOF, set the stop flag
                if (fgets(inWord, 50, stdin) == NULL) {
                    haltLoop = TRUE;
                }
            }
                //if it is 0, take the next string and next word from stdin and the dictionary
                //respectively
            else {
                //if either have reached EOF set the stop flag
                if (fgets(inWord, 50, stdin) == NULL || fgets(dictWord, 50, dict) == NULL) { haltLoop = TRUE; }
            }
        }
        //if there are any remaining words after the previous loop, they are spelled wrong
        while (fgets(inWord, 50, stdin) != NULL) {
            printf("Wrong: %s", inWord);
        }
    } else { quit("error in opening dictionary", 0); }
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


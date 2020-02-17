#include<stdlib.h>
#include<stdio.h>
#include<unistd.h>
#include<string.h>
#include<ctype.h>

/*
lowerCase will go through a string and make every letter in a string lowercase.
DATA TABLE
NAME			DESCRIPTION
word			parameter - the word which will be made lower case
spot			counter used to increment the pointer
*/
void lowerCase(char *word) {
    int spot = 0;
    //while the string isn't over make each letter into lower case
    while (*(word + spot) != '\0') {
        *(word + spot) = tolower(*(word + spot));
        spot++;
    }
}

/*
wordCompare takes two words and returns their comparison value, if it is 0 it prints that the first
word is spelled correctly, it is less than 0 it prints that the first word is spelled wrong
DATA TABLE
NAME			DESCRIPTION
word1			parameter - first word for comparison
word2			parameter - second word for comparison
compare			the result of the comparison
*/
int wordCompare(char *word1, char *word2) {
    //convert the first word to lower case to match the second one
    lowerCase(word1);
    //compare the two words
    int compare = strcmp(word1, word2);
    //if comparison is 0, the word is spelled right
    if (compare == 0) {
        printf("This word is spelled correctly: %s", word1);
    }
        //if it is less than 0, the word is spelled wrong
    else if (compare < 0) {
        printf("This word is spelled incorrectly: %s", word1);
    }
    return compare;

}

/*
main is the controlling function of the program. While stdin and the dictionary aren't at EOF,
it will read in a line from both and compare them using the wordCompare function. It will then
either attempt to take the next line from stdin, the dictionary or both depending on the return
value from wordCompare. If either stdin or the dictionary reach EOF, the loop terminates. If
the dictionary runs out of words first, the remaining words in stdin will be labeled as incorrect
DATA TABLE
NAME			DESCRIPTION
argc			parameter - length of argv
argv 			parameter - array of parameters passed via the command line
checkWord		the current word from stdin
dictWord		the current word from the dictionary
compare			the comparison value between the two words
stop			boolean for whether or not a condition has been met to stop the loop
dict			the dictionary file
*/
int main(int argc, char *argv[]) {
    char checkWord[50];
    char dictWord[50];
    int compare;
    int stop = 0;
    FILE *dict;
    //attempt to open the dictionary
    if ((dict = fopen(argv[1], "r"))) {
        //if either stdin or the dictionary is empty, cancel the loop
        if (fgets(checkWord, 50, stdin) == NULL || fgets(dictWord, 50, dict) == NULL) {
            stop = 1;
        }
        while (!stop) {
            //compare the values of the two words
            compare = wordCompare(checkWord, dictWord);
            //if the value is greater than 0, implying that the dictionary
            //word is less than the string, take the next dictionary word
            if (compare > 0) {
                //if the dictionary has reached EOF, set the stop flag
                if (fgets(dictWord, 50, dict) == NULL) {
                    stop = 1;
                }
            }
                //if the value is less than 0, take the next string from stdin
            else if (compare < 0) {
                //if stdin has reached EOF, set the stop flag
                if (fgets(checkWord, 50, stdin) == NULL) {
                    stop = 1;
                }
            }
                //if it is 0, take the next string and next word from stdin and the dictionary
                //respectively
            else {
                //if either have reached EOF set the stop flag
                if (fgets(checkWord, 50, stdin) == NULL || fgets(dictWord, 50, dict) == NULL) {
                    stop = 1;
                }
            }
        }
        //if there are any remaining words after the previous loop, they are spelled wrong
        while (fgets(checkWord, 50, stdin) != NULL) {
            printf("This word is spelled incorrectly: %s", checkWord);
        }
    }
        //if the dictionary failed to open, print it
    else {
        printf("An error occurred opening the dictionary\n");
    }
}
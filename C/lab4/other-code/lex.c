#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<unistd.h>
#include<ctype.h>

/*
charReplace will read through a string character by character until it reaches a null character.
If it encounters a character that is not a non letter, it replaces it with a space.
DATA TABLE
NAME			DESCRIPTION
string			the string to read through
spot			a variable used to move the pointer
*/
void charReplace(char *string) {
    int spot = 0;
    while (*(string + spot) != '\0') {
        //if the character isn't a letter, replace it with a space
        if (!isalpha(*(string + spot))) {
            *(string + spot) = ' ';
        }
        spot++;
    }
}

/*
main is the controlling function of lex.c, main will read a file line by line,
remove the newline character, tokenize each line by the delimiter string,
and then print each token.
DATA TABLE
NAME			DESCRIPTION
argc			parameter - length of argv
argv			parameter - list of arguments passed
fp			file pointer for the file name provided by the arguments list
line			current line of the file
newLine			pointer for location of newline character
strptr			pointer for location of each token
*/
int main(int argc, char *argv[]) {
    FILE *fp;
    char line[256];
    //char * newLine;
    char *strptr;
    //attempt to open the file
    if ((fp = fopen(argv[1], "r"))) {
        //if the file exists, read until the end
        while (fgets(line, 256, fp) != NULL) {
            charReplace(line);
            //tokenize the string based off the delimiter
            //continue until there are no more tokens
            strptr = strtok(line, " ");
            while (strptr != NULL) {
                printf("%s\n", strptr);
                strptr = strtok(NULL, " ");
            }
        }
        fclose(fp);
    }
    return 0;
}
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Maggie Horton
 * CS-231
 * Lab 4 - lex.c
 * The purpose of this program is to take in a file name as an argument, convert each word to
 * all lowercase and then output one word per line, to be processed by sort.
 * ----------------------------------------------------------------------------------------------
 * ALGORITHM
 * if there are enough args & the file is valid
 *      read in the contents of the file
 *      split file into array of strings
 *      convert any uppercase character to lowercase
 *      output each word one line at a time
 * ---------------------------------------------------------------------------------------------- */

void processFile(char *fName);
void quit(char *message, int code);

/* main function that fetches arguments and runs the rest of the program
 * returns:     0 when done
 * argc:        param - no. of args supplied
 * argv:        param - args supplied */
int main(int argc, char *argv[]) {
    if ((argc > 2) || (argc < 2)) { quit("incorrect arg count. lex.out requires arg <file_name>.txt", 0); }
    char *fileName = argv[1];
    processFile(fileName);
    return 0;
}

/* splits the input file into an array of strings then outputs one
 * word per line.
 * returns:     nothing
 * fName:       param - name of file to be read
 * fSize:       var - size of file, used to properly allocate mem */
void processFile(char *fName) {
    FILE *file = fopen(fName, "r");
    if (file == NULL) { quit("error opening file(s)", 0); }
    fseek(file, 0, SEEK_END);
    long fSize = ftell(file);
    rewind(file);
    char *lines = (char *) malloc(fSize);
    int lineCount = 0, counter = 0;
    for (int ch = getc(file); ch != EOF; ch = getc(file)) {
        if (isupper(ch)) ch = tolower(ch);
        if (ch == '\n') { lineCount++; }
        lines[counter] = ch;
        counter++;
    }

    char *delim = "\n", *tokenized = strtok(lines, delim);
    char *lineArr[lineCount];
    int pos = 0;
    while (tokenized != NULL) {
        lineArr[pos++] = tokenized;
        tokenized = strtok(NULL, delim);
    }
    for (pos = 0; pos < lineCount; pos++) printf("%s\n", lineArr[pos]);
    fclose(file);
}

/* nice little helper function for quitting
 * returns:     nothing
 * message:     param - message to display before exit()
 * code:        exit code */
void quit(char *message, int code) {
    printf("%s\n", message);
    exit(code);
}



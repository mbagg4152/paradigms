#include <unistd.h>
#include <stdio.h>
#include <wait.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

void fileTest(char *fName);
void quit(char *message, int code);

int main(int argc, char *argv[]) {
    if ((argc > 3) || (argc < 3)) { quit("incorrect arg count. usage: ./s <file>.txt <dict>.txt", 0); }
    char *inFName = argv[1], *dFName = argv[2];
    printf("input file: %s, dictionary: %s\n", inFName, dFName);
    //fileTest(inFName);
    //fileTest(dFName);
}

void fileTest(char *fName) {
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

void quit(char *message, int code) {
    printf("%s\n", message);
    exit(code);
}


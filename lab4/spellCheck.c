#include <unistd.h>
#include <stdio.h>
#include <wait.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#define DICT_LEN 1280
#define WORD_LEN 40
#define L 400

void fileTest(char *inName, char *dName);

int main(int argc, char *argv[]) {
    if ((argc > 3) || (argc < 3)) {
        printf("incorrect arg count. usage: ./s <file>.txt <dict>.txt\n");
        exit(0);
    }
    char *inFName = argv[1], *dFName = argv[2];
    printf("input file: %s, dictionary: %s\n", inFName, dFName);
    fileTest(inFName, dFName);

}

void fileTest(char *inName, char *dName) {
    char inLines[L][L];
    int index = 0;

    FILE *dFile = fopen(dName, "r");

    if (dFile == NULL) {
        printf("error opening file(s)\n");
        exit(0);
    }

    fseek(dFile, 0, SEEK_END);
    long fSize = ftell(dFile);
    rewind(dFile);
    char *dLines = (char *) malloc(fSize);
    int lineCount = 0;
    int n = 0;
    for (int c = getc(dFile); c != EOF; c = getc(dFile)) {
        dLines[n] = c;
        n++;
        if (c == '\n') lineCount++;
    }


    //printf("%s\n", dLines);
    printf("%d lines in file\n", lineCount);
    printf("file size %ld \n", fSize);
    int avg = (int) fSize / lineCount;
    printf("avg word length: %i\n", avg);
    char *p = strtok(dLines, "\n");
    char *arr[lineCount];
    int i = 0;
    while (p != NULL) {
        arr[i++] = p;
        p = strtok(NULL, "\n");
    }
    for (i = 0; i < lineCount; i++) {
        printf("%s\n", arr[i]);
    }
    printf("\n");
    fclose(dFile);
}


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 200
#define LINES 200

int main(int argc, char *argv[]) {
    if ((argc > 2) || (argc < 2)) {
        printf("incorrect arg count. usage: ./l <file_name>.txt\n");
        exit(0);
    }

    char *fileName = argv[1];
    FILE *inputFile = fopen(fileName, "r");
    if (inputFile == NULL) {
        printf("cant open");
        exit(0);
    }

    char fileLines[LEN][LINES];
    int index = 0;
    while (fgets(fileLines[index], LINES, inputFile)) {
        char *tmpStr = fileLines[index];
        for (int c = 0; c < strlen((const char *) tmpStr); c++) {
            if (isupper(tmpStr[c])) { tmpStr[c] = tolower(tmpStr[c]); }
        }
        fileLines[index][strlen(fileLines[index]) - 1] = '\0';
        index++;
    }

    for (int pos = 0; pos < index; pos++) {
        printf("%s\n", fileLines[pos]);
    }
    return 0;
}


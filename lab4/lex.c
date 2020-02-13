#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define LEN 400

int main(int argc, char *argv[]) {
    FILE *fptr;
    char *fname = argv[1];
    char contents[LEN];
    system("pwd");
    char* path[LEN];
    getcwd(path, LEN);
    printf("Current Directory = %s\n", path);
    printf("%s\n", fname);
    fptr = fopen("test.txt", "r");
    //char *text = {0};
    if (fptr == NULL) {
        perror("err opening file. ");
        exit(0);
    }
    //while (fgets(contents, LEN, fptr) != NULL) printf("%s", contents);
    fclose(fptr);


    return 0;
}
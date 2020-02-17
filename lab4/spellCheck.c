#include <unistd.h>
#include <stdio.h>
#include <wait.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

char *lexCmd = "./l";

void quit(char *message, int code);
void lex(int pipe[2], char *file);
void sortIt(int in[2], int out[2]);
void checker(char *inFile, char *dictFile);

int main(int argc, char *argv[]) {
    if ((argc > 3) || (argc < 3)) { quit("incorrect arg count. usage: ./s <file>.txt <dict>.txt", 0); }
    char *inFName = argv[1], *dFName = argv[2];
    printf("input file: %s, dictionary: %s\n", inFName, dFName);
    checker(inFName, dFName);
}

void checker(char *inFile, char *dictFile) {
    char lexOutput[4096], sortOutput[4096], compareOutput[4096];
    int lexPid = -1, sortPid = -1, comparePid = -1;
    int lexSortPipe[2], sortComparePipe[2];

    pipe(lexSortPipe);
    lexPid = fork();
    printf("lex pid: %d\n", lexPid);
    if (lexPid == 0) {
        lex(lexSortPipe, inFile);
        return;
    } else {
        close(lexSortPipe[1]);
        int nBytes = read(lexSortPipe[0], lexOutput, sizeof(lexOutput));
        printf("Output:\n%.*s\n", nBytes, lexOutput);
        wait(NULL);
    }
    close(lexSortPipe[1]);
    waitpid(lexPid, NULL, 0);
    pipe(sortComparePipe);
    sortPid = fork();
    printf("sort pid: %d\n", sortPid);
    if (sortPid == 0) {
        sortIt(lexSortPipe, sortComparePipe);
        return;
    } else {
        close(sortComparePipe[1]);
        int data = read(sortComparePipe[0], sortOutput, sizeof(sortOutput));
        printf("Sort output:\n%.*s\n", data, sortOutput);
    }

}

void lex(int pipe[2], char *file) {
    dup2(pipe[1], STDOUT_FILENO);
    char *args[3] = {lexCmd, file, NULL};
    execv(args[0], args);
    close(pipe[1]);
    close(pipe[0]); // Read end of pipe
}

void sortIt(int in[2], int out[2]) {
    dup2(in[0], STDIN_FILENO);
    dup2(out[1], STDOUT_FILENO);
    char *args[3] = {"sort", "-f", NULL};
    execvp(args[0], args);
    close(in[0]);
    close(out[1]);
    //close(out[0]);
}

void quit(char *message, int code) {
    printf("%s\n", message);
    exit(code);
}


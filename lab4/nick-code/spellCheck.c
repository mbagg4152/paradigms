#include <unistd.h>
#include <stdio.h>
#include <wait.h>
#include <stdlib.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmain-return-type"
#define lex "./l"
#define comp "./c"

void spellCheck(char *file, char *dictionary);
void compareOut(int *pipe, char *dictionary);
void uniq(int *in, int *out);
void sort(int *in, int *out);
void lexOut(int *pipe, char *file);
void makeLog(int lexPid, int sortPid, int uniqPid, int compPid);

void main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Call spellCheck.c like this >>> ./spellCheck (input file).txt dict.txt\n");
        exit(1);
    }
    spellCheck(argv[1], argv[2]);
}

void spellCheck(char *file, char *dictionary) {
    pid_t lexPid = -1, sortPid = -1, uniqPid = -1, compPid = -1;
    int lexSortPipe[2], sortUniqPipe[2], uniqCompPipe[2];

    pipe(lexSortPipe);
    lexPid = fork();
    if (lexPid == 0) {
        lexOut(lexSortPipe, file);
        return;
    }
    close(lexSortPipe[1]); // NEEDS TO CLOSE HERE
    waitpid(lexPid, NULL, 0);
    pipe(sortUniqPipe);
    sortPid = fork();
    if (sortPid == 0) {
        sort(lexSortPipe, sortUniqPipe);
        return;
    }
    close(sortUniqPipe[1]);
    waitpid(sortPid, NULL, 0);
    pipe(uniqCompPipe);
    uniqPid = fork();
    if (uniqPid == 0) {
        uniq(sortUniqPipe, uniqCompPipe);
        return;
    }
    close(uniqCompPipe[1]);
    waitpid(uniqPid, NULL, 0);
    compPid = fork();
    if (compPid == 0) {
        compareOut(uniqCompPipe, dictionary);
        return;
    }
    waitpid(compPid, NULL, 0);
    close(lexSortPipe[0]);
    close(sortUniqPipe[0]);
    close(uniqCompPipe[0]);
    makeLog(lexPid, sortPid, uniqPid, compPid);
}

void compareOut(int *pipe, char *dictionary) {
    dup2(pipe[0], STDIN_FILENO);
    char *args[3] = {comp, dictionary, NULL};
    execv(args[0], args);
    close(pipe[0]);
}

void uniq(int *in, int *out) {
    dup2(in[0], STDIN_FILENO);
    dup2(out[1], STDOUT_FILENO);
    char *args[3] = {"uniq", "-i", NULL};
    execvp(args[0], args);
    close(in[0]);
    close(out[1]);
    close(out[0]); // Read end of pipe
}

void sort(int *in, int *out) {
    dup2(in[0], STDIN_FILENO);
    dup2(out[1], STDOUT_FILENO);
    char *args[3] = {"sort", "-f", NULL};
    execvp(args[0], args);
    close(in[0]);
    close(out[1]);
    close(out[0]); // Read end of pipe
}

void lexOut(int *pipe, char *file) {
    dup2(pipe[1], STDOUT_FILENO);
    char *args[3] = {lex, file, NULL};
    execv(args[0], args);
    close(pipe[1]);
    close(pipe[0]); // Read end of pipe
}

void makeLog(int lexPid, int sortPid, int uniqPid, int compPid) {
    FILE *file = fopen("spellCheck.log", "w");  //open log in write
    fprintf(file, "lex.out pid: %d\n", lexPid); //mode
    fprintf(file, "sort pid: %d\n", sortPid);
    fprintf(file, "uniq pid: %d\n", uniqPid);
    fprintf(file, "compare.out pid: %d\n", compPid);
    fclose(file);
}

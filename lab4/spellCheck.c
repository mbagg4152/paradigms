#include <unistd.h>
#include <stdio.h>
#include <wait.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <fcntl.h>
#include <signal.h>

char *lexCmd = "./l", *compCmd = "./c", *sortCmd = "sort", *sortFlag = "-u";
char *lexArgs[3], *sortArgs[3], *compArgs[3], *procs[3];
int pids[3];
int deadCount;
pid_t lexPid = -1, sortPid = -1, compPid = -1;
FILE *logFile;

void driver(char *inputName, char *dictName);
void initValues(char *inputName, char *dictName);
void initSigAction();
void sigHandler(int sigID, siginfo_t *sigInfo, void *context);
void runLex(int pipe[]);
void runSort(int *in, int *out);
void runCompare(int pipe[]);
void quit(char *message, int code);

int main(int argc, char *argv[]) {
    if ((argc > 3) || (argc < 3)) { quit("incorrect arg count. usage: ./s <file>.txt <dict>.txt", 0); }
    char *txtFName = argv[1], *dictFName = argv[2];
    printf("[DEBUG]input file: %s, dictionary: %s\n\n", txtFName, dictFName);
    driver(txtFName, dictFName);
}

void driver(char *inputName, char *dictName) {
    initValues(inputName, dictName);
    int ltsPipe[2]; // store ends of pipe from lex --> sort
    int stcPipe[2]; // store ends of pipe from sort --> compare
    initSigAction();

    pipe(ltsPipe);
    lexPid = fork(); // create child process for lex
    pids[0] = lexPid;
    if (lexPid == 0) runLex(ltsPipe); // pid of 0 ==> child process

    pipe(stcPipe);
    sortPid = fork(); // create child for sort
    pids[1] = sortPid;
    if (sortPid == 0) runSort(ltsPipe, stcPipe); // pid of 0 ==> child process

    // lex --> sort no longer needed
    close(ltsPipe[0]);
    close(ltsPipe[1]);

    compPid = fork(); // create child for compare
    pids[2] = compPid;
    if (compPid == 0) runCompare(stcPipe); // pid of 0 ==> child process

    close(stcPipe[0]);
    close(stcPipe[1]);

    while (deadCount < 3); // run until the 3 child processes are dead
    fclose(logFile);
}

void initValues(char *inputName, char *dictName) {
    // fill with process names, to be used in log file
    procs[0] = lexCmd;
    procs[1] = sortCmd;
    procs[2] = compCmd;
    // args needed to properly execute lex.out
    lexArgs[0] = lexCmd;
    lexArgs[1] = inputName;
    lexArgs[2] = NULL;
    // args needed to properly execute sort so that it removes duplicates
    sortArgs[0] = sortCmd;
    sortArgs[1] = sortFlag;
    sortArgs[2] = NULL;
    // args needed to properly execute compare.out
    compArgs[0] = compCmd;
    compArgs[1] = dictName;
    compArgs[2] = NULL;
    deadCount = 0; // init counter for dead children
    logFile = fopen("spellCheck.log", "w"); // open log file for writing
}

void initSigAction() {
    struct sigaction action;
    action.sa_sigaction = sigHandler; // sa_sigaction requires (int, siginfo_t*, void*)
    action.sa_flags = SA_SIGINFO; // int, this flag is only meaningful when establishing a signal handler
    sigfillset(&action.sa_mask); // init & fill signal set
    sigdelset(&action.sa_mask, SIGCHLD); // delete SIGCHLD from signal set
    sigaction(SIGCHLD, &action, NULL); // examine & change signal action
}

// needs to have the same signature as sa_sigaction
void sigHandler(int sigID, siginfo_t *sigInfo, void *context) {
    pid_t current;
    int status, pos = 0;
    char logContent[400];
    if (sigID == SIGCHLD) { // do something only if signal is SIGCHLD
        // while there are dead children, do something
        // WNOHANG ensures waitpid returns status info immediately
        while ((current = waitpid(-1, &status, WNOHANG)) > 0) {
            // set pos based on value of current in order to have correct vals written to log
            if (current == pids[0]) { pos = 0; }
            else if (current == pids[1]) { pos = 1; }
            else if (current == pids[2]) { pos = 2; }
            // write process name & id to spellCheck.log
            sprintf(logContent, "Process %s with id: %d has been put down\n", procs[pos], pids[pos]);
            fputs(logContent, logFile);
            deadCount++;
        }
    }
}

void runLex(int pipe[]) {
    dup2(pipe[1], STDOUT_FILENO);
    close(pipe[0]);
    close(pipe[1]);
    execvp(lexArgs[0], lexArgs);
}

void runSort(int *in, int *out) {
    close(in[1]);
    dup2(in[0], STDIN_FILENO);
    close(in[0]);
    close(out[0]);
    dup2(out[1], STDOUT_FILENO);
    close(out[1]);
    execvp(sortArgs[0], sortArgs);
}

void runCompare(int pipe[]) {
    close(pipe[1]);
    dup2(pipe[0], STDIN_FILENO);
    close(pipe[0]);
    execvp(compArgs[0], compArgs);
}

void quit(char *message, int code) {
    printf("%s\n", message);
    exit(code);
}


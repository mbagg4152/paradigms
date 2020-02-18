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
int counter;
pid_t lexPid = -1, sortPid = -1, compPid = -1;
FILE *logFile;

void quit(char *message, int code);
void lex(int pipe[2], char *file);
void sortIt(int in[2], int out[2]);
void checker(char *inFile, char *dictFile);
void driver(char *inputName, char *dictName);
void makeLog();
void initValues();
void handler(int sigID, siginfo_t *sigInfo, void *context);
void initSigAction();

int main(int argc, char *argv[]) {
    if ((argc > 3) || (argc < 3)) { quit("incorrect arg count. usage: ./s <file>.txt <dict>.txt", 0); }
    printf("Note: The program keeps running, that's something I'm trying to work out. For now just use ctrl+C to exit.\n");
    char *inFName = argv[1], *dFName = argv[2];
    printf("[DEBUG]input file: %s, dictionary: %s\n\n", inFName, dFName);
    driver(inFName, dFName);
}

void initValues(char *inputName, char *dictName) {
    procs[0] = lexCmd;
    procs[1] = sortCmd;
    procs[2] = compCmd;
    lexArgs[0] = lexCmd;
    lexArgs[1] = inputName;
    lexArgs[2] = NULL;
    sortArgs[0] = sortCmd;
    sortArgs[1] = sortFlag;
    sortArgs[2] = NULL;
    compArgs[0] = compCmd;
    compArgs[1] = dictName;
    compArgs[2] = NULL;
    pids[0] = lexPid;
    pids[1] = sortPid;
    pids[2] = compPid;

}

void driver(char *inputName, char *dictName) {
    initValues(inputName, dictName);
    int ltsPipe[2]; // store ends of pipe from lex --> sort
    int stcPipe[2]; // store ends of pipe from sort --> compare


    initSigAction();
    counter = 0;
    logFile = fopen("spellCheck.log", "w");
    pipe(ltsPipe);
    pids[0] = fork(); // create child process for lex
    if (pids[0] == 0) { // pid of 0 ==> child process
        dup2(ltsPipe[1], STDOUT_FILENO);
        close(ltsPipe[0]);
        close(ltsPipe[1]);
        execvp(lexArgs[0], lexArgs);
    }

    pipe(stcPipe);
    pids[1] = fork();
    if (pids[1] == 0) { // pid of 0 ==> child process
        close(ltsPipe[1]);
        dup2(ltsPipe[0], STDIN_FILENO);
        close(ltsPipe[0]);
        close(stcPipe[0]);
        dup2(stcPipe[1], STDOUT_FILENO);
        close(stcPipe[1]);
        execvp(sortArgs[0], sortArgs);
    }

    // close pipe for non child process
    close(ltsPipe[0]);
    close(ltsPipe[1]);

    pids[2] = fork();
    if (pids[2] == 0) { // pid of 0 ==> child process
        close(stcPipe[1]);
        dup2(stcPipe[0], STDIN_FILENO);
        close(stcPipe[0]);
        execvp(compArgs[0], compArgs);
    }

    close(ltsPipe[0]);
    close(stcPipe[0]);
    kill(compPid, 0);

    while (counter < 3);
    fclose(logFile);
}

void initSigAction() {
    struct sigaction action;
    action.sa_sigaction = handler;
    action.sa_flags = SA_SIGINFO;
    sigfillset(&action.sa_mask);
    sigdelset(&action.sa_mask, SIGCHLD);
    sigaction(SIGCHLD, &action, NULL);
}

//https://linux.die.net/man/2/signal
void handler(int sigID, siginfo_t *sigInfo, void *context) {
    pid_t current;
    int status, selector = 0;
    char output[256];
    if (sigID == SIGCHLD) { //if the signal is that of SIGCHLD, evaluate
        while ((current = waitpid(-1, &status, WNOHANG)) > 0) { //while there are still dead children
            //compare the current of the dead child to the ones in pids
            //set selector to that position
            if (current == pids[0]) { selector = 0; }
            else if (current == pids[1]) { selector = 1; }
            else if (current == pids[2]) { selector = 2; }
            //write the name and process ID into output, print it to file
            sprintf(output, "The process id:%d name:%s has died\n", pids[selector], procs[selector]);
            //fputs(output, sLog);
            //increment the number of dead children
            counter++;
        }
    }
}

void quit(char *message, int code) {
    printf("%s\n", message);
    exit(code);
}


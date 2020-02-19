#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <wait.h>

/* Maggie Horton
 * CS-231
 * Lab 4 - spellCheck.c
 * The purpose of this program is to use system calls and pipe to redirect output. This
 * file is the driver program. It utilizes lex.c, the linux sort command with a remove duplicate
 * flag and compare.c in order to determine if the words contained in a file are spelled correctly.
 * ----------------------------------------------------------------------------------------------
 * ALGORITHM
 * if 3 args have been supplied, save values of argv1 (input file) & argv2 (dictionary file).
 *     as long as there haven't been 3 terminated child processes, keep track of said processes
 *          make child process & pass the input file name to lex.c
 *          redirect output via pipe from lex.c to the linux sort command
 *              after running sort close pipe from lex --> sort
 *          redirect output via pipe from the sort command & pass the name of the dictionary file
 *          to compare.c and display the output from compare.c in stdout
 *              close pipe from sort --> compare
 *    if a child process is killed, log the name & process id to spellCheck.log
 * ----------------------------------------------------------------------------------------------
 * EXTERNAL DATA TABLE
 * lexCmd       command for running lex
 * compCmd      command for running compare
 * sortCmd      command for running sort
 * sortFlag     flag used in sort command to remove duplicate words
 * lexArgs      holds args for running lex
 * sortArgs     holds args for running sort
 * compArgs     holds args for running compare
 * pids         holds process ids, used in writing log file
 * deadCount    counter to keep track of killed child processes
 * lexPid       hold pid of lex
 * sortPid      hold pid of sort
 * compPid      hold pid of compare
 * logFile      spellCheck.log file */

char *lexCmd = "./lex.out", *compCmd = "./compare.out", *sortCmd = "sort", *sortFlag = "-u";
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

/* main function that drives the rest of the program
 * returns:     0 when done executing
 * argc:        param - count of args supplied in command line
 * argv:        param - arguments supplied in command line
 * txtFName:    var - name of the input file
 * dictFName:   var - name of the dictionary file */
int main(int argc, char *argv[]) {
    printf("------------------------------------------------------------\n");
    printf("Lab Assignment 4 for CS-231, brought to you by Maggie Horton\n");
    printf("------------------------------------------------------------\n");
    if ((argc > 3) || (argc < 3)) {
        quit("incorrect arg count. usage: ./spellCheck.out <file>.txt <dict>.txt", 0);
    }
    char *txtFName = argv[1], *dictFName = argv[2];
    printf("Input file: %s, Dictionary file: %s\n\n", txtFName, dictFName);
    driver(txtFName, dictFName);
    return 0;
}

/* the main functional portion of the code. creates child processes & pipes output
 * between different programs.
 * returns:     nothing
 * inputName:   param - name of the input file
 * dictName:    param - name of the dictionary file
 * ltsPipe:     var - store ends of pipe from lex --> sort
 * stcPipe:     var - store ends of pipe from sort --> compare */
void driver(char *inputName, char *dictName) {
    initValues(inputName, dictName);
    int ltsPipe[2];
    int stcPipe[2];
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

/* helper function used to initialize/assign values & remove clutter
 * from the other functions
 * returns:     nothing
 * inputName:   param - name of input file
 * dictName:    param - name of dictionary file */
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

/* function to initialize sigaction to keep track of processes &
 * assign signal handler
 * returns:     nothing
 * action:      var - used to look at & kill children */
void initSigAction() {
    struct sigaction action;
    action.sa_sigaction = sigHandler; // sa_sigaction requires (int, siginfo_t*, void*)
    action.sa_flags = SA_SIGINFO; // int, this flag is only meaningful when establishing a signal handler
    sigfillset(&action.sa_mask); // init & fill signal set
    sigdelset(&action.sa_mask, SIGCHLD); // delete SIGCHLD from signal set
    sigaction(SIGCHLD, &action, NULL); // examine & change signal action
}

/* signal handler function to be used to keep track of children, also used to write spellCheck.log.
 * must have the same signature as sa_sigaction.
 * returns:     nothing
 * sigID:       param - pid to examine & handle
 * sigInfo:     param - information about the signal
 * context:     param - flag that is required in the signature it has no use but the compiler
 *              gets mad when it is not there
 * current:     var - pid of dead child
 * pos:         var - contains index values
 * logContent:  var - hold content to be written to log
 * */
void sigHandler(int sigID, siginfo_t *sigInfo, void *context) {
    pid_t current;
    int state, pos = 0;
    char logContent[400];
    if (sigID == SIGCHLD) { // do something only if signal is SIGCHLD
        // while there are dead children, do something
        // WNOHANG ensures waitpid returns signal state info immediately
        while ((current = waitpid(-1, &state, WNOHANG)) > 0) {
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

/* takes in the appropriate pipe & executes lex.c
 * returns:     nothing
 * pipe:        param - pipe from lex --> sort */
void runLex(int pipe[]) {
    dup2(pipe[1], STDOUT_FILENO);
    close(pipe[0]);
    close(pipe[1]);
    execvp(lexArgs[0], lexArgs);
}

/* takes in appropriate pipes & passes output from
 * lex into sort.
 * returns: nothing
 * in:      param - pipe from lex --> sort
 * out:     param - pipe from sort --> compare */
void runSort(int in[], int out[]) {
    close(in[1]);
    dup2(in[0], STDIN_FILENO);
    close(in[0]);
    close(out[0]);
    dup2(out[1], STDOUT_FILENO);
    close(out[1]);
    execvp(sortArgs[0], sortArgs);
}

/* takes in the appropriate pipe & executes compare.c
 * returns:     nothing
 * pipe:        param - pipe from sort --> compare */
void runCompare(int pipe[]) {
    close(pipe[1]);
    dup2(pipe[0], STDIN_FILENO);
    close(pipe[0]);
    execvp(compArgs[0], compArgs);
}

/* nice little helper function for quitting
 * returns:     nothing
 * message:     param - message to display before exit()
 * code:        exit code */
void quit(char *message, int code) {
    printf("%s\n", message);
    exit(code);
}


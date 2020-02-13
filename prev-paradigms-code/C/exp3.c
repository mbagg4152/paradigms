#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<unistd.h>
#include<sys/wait.h>
#include<ctype.h>
#include<signal.h>

int stop;
int counter;
int pids[4];
FILE * sLog;
//void handler(int, siginfo_t *, void *);

int main(int argc, char * argv[]) {
	printf("%d\n",STDIN_FILENO);
}
/*
void handler (int signum, siginfo_t * si, void * ucontext){
	pid_t pid;
	int status;
	//FILE * pcs;
	char program[20];
	char line[256];
	if (signum==SIGCHLD) {
		while((pid = waitpid(-1, &status, WNOHANG) >0)){
			counter++;
			stop=1;
		}
		//fclose(pcs);
	}
} */

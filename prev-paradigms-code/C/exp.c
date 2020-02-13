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
void handler(int, siginfo_t *, void *);

void adultProcess(int fd[], int pid) {
	stop=0;
	int status;
	//printf("I'm here!\n");	
	close(fd[1]);
	while(!stop);
	//waitpid(pid,&status,0);
	dup2(fd[0], STDIN_FILENO);
	close(fd[0]);
}

void childProcess(int fd[], char * args[]) {
	close(fd[0]);
	dup2(fd[1], STDOUT_FILENO);
	execvp(args[0],args);
	//return 0;

}

void finalAdult(int fd[], int pid) {
	char string[2048];
	int status;	
	stop=0;
	close(fd[1]);
	while(!stop);
	//waitpid(pid,&status,0);
	read(fd[0],string,2048);
	printf("%s",string);
	close(fd[0]);
}

int main(int argc, char * argv[]) {
	pid_t pid;
	int fdl[2];

	char * lexArgs[3];
	lexArgs[0]="./lex.out";
	lexArgs[1]=argv[1];
	lexArgs[2]=NULL;

	char * sortArgs[2];
	sortArgs[0]="sort";
	sortArgs[1]=NULL;
	
	char * uniqArgs[3];
	uniqArgs[0]="uniq";
	uniqArgs[1]="-i";
	uniqArgs[2]=NULL;
	
	char * compArgs[2];
	compArgs[0]="./temp";
	compArgs[1]=NULL;

	struct sigaction sigact;	

	sigact.sa_sigaction = handler;
  	sigact.sa_flags = SA_SIGINFO;
  	sigfillset(&sigact.sa_mask);
  	sigdelset(&sigact.sa_mask, SIGCHLD);
  	sigaction(SIGCHLD, &sigact, NULL);	

	counter=0;
	sLog = fopen("spellCheck.log","w");	
	while (counter < 4) {
		pipe(fdl);
		pid=fork();
		if (pid > 0) {
			pids[counter]=pid;
			if (counter < 3){
				adultProcess(fdl, pid);
			}
			else {
				finalAdult(fdl, pid);
				fclose(sLog);
				return 0;
			}
		} else {
			switch(counter) {
				case 0:
					childProcess(fdl, lexArgs);
					break;
				case 1:
					childProcess(fdl, sortArgs);
					break;
				case 2:
					childProcess(fdl, uniqArgs);
					break;
				case 3:
					childProcess(fdl, compArgs);
					break;
				default:
					break;
			}
			return 0;
		}
		//counter++;
	}
}
void handler (int signum, siginfo_t * si, void * ucontext){
	pid_t pid;
	int status;
	//FILE * pcs;
	char program[20];
	char line[256];
	if (signum==SIGCHLD) {
		while((pid = waitpid(-1, &status, WNOHANG) >0)){
			
			stop=1;
			//printf("child died, so sad\n");
			sprintf(line,"%d has died\n",pids[counter++]);
			fputs(line, sLog);
		}
		//fclose(pcs);
	}
} 

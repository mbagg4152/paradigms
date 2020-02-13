/*Bradley Gardner
CS-231-02L
Lab4*/
/*
The purpose of this program is to create children that execute the programs lex.out, sort, uniq and compare.out.
The output of each child process will be piped from stdout to the next process where it will be piped into stdin.
When a child process dies, the id and name of that process is then written to file, and a counter is incremented
to denote the number of dead children. Once the parent has made 4 children, it will enter an infinite loop until 
all of the child processes are dead.

if there are less than 3 arguments, immediately end the program and ask for a text and dictionary file
Initialize the arguments for the 4 children, along with the signal action and the file
pipe fdls
Create the first child
if the id of process is 0
	duplicate STDOUT to fdls[1] to the pipe
	run lex.out
pipe fdsu
create the second child
if the id of process is 0
	duplicate STDIN to fdls[0] to feed the output of lex.out to sort
	duplicate STDOUT to fdsu[1] to output to the pipe  
	run sort
close fdls
pipe fduc
create third child
if the id of process is 0
	duplicate STDIN to fdsu[0] to feed the output of sort to uniq
	duplicate STDOUT to fduc[1] to output to the pipe  
	run uniq
close fdls
create the fourth child
if the id of process is 0
	duplicate stdin to fdsu[0] to feed the output of uniq to compare.out
	run compare.out
while the number of child processes is less than 4, run on an infinite loop
close the file	


if a child process dies
	print its id and name to file
	increment the number of dead children

DATA TABLE
NAME		DESCRIPTION
counter		the number of dead children
pids		array containing the pids of each child created
names		the names of the independent processes
sLog		spell check log file pointer
*/


#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<unistd.h>
#include<sys/wait.h>
#include<ctype.h>
#include<signal.h>

int counter;
int pids[4];
char * names[4];
FILE * sLog;

void handler(int, siginfo_t *, void *);
/*
main is the controlling funtion of the program. Main will create a series of children
and pipes to check the spelling in a specified file. The main will create children
that will execute lex.out, sort, uniq and compare.out, piping the output from each to 
the input of the next.

DATA TABLE
NAME			DESCRIPTION
argc			parameter - length of argv
argv			parameter - arguments passed to the process
fdls			pipe from lex.out to sort
fdsu			pipe from sort to uniq
fduc			pipe from uniq to compare.out
lexArgs			the arguments passed to lex.out
sortArgs		the arguments passed to sort
uniqArgs		the arguments passed to uniq
compareArgs		the arguments passed to compare
*/

int main(int argc, char * argv[]) {
	//kill the process if there aren't 3 args
	if (argc<3){
		printf("I need a text file AND a dictionary\n");
		return 0;
	}
	//add the names of the processes
	names[0]="lex.out";
	names[1]="sort";
	names[2]="uniq";
	names[3]="compare.out";

	int fdls[2];
	int fdsu[2];
	int fduc[2];

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
	
	char * compArgs[3];
	compArgs[0]="./compare.out";
	compArgs[1]=argv[2];
	compArgs[2]=NULL;

	//define our signal action
	struct sigaction sigact;	

	sigact.sa_sigaction = handler;
  	sigact.sa_flags = SA_SIGINFO;
  	sigfillset(&sigact.sa_mask);
  	sigdelset(&sigact.sa_mask, SIGCHLD);
  	sigaction(SIGCHLD, &sigact, NULL);	

	counter=0;
	sLog = fopen("spellCheck.log","w");
	//create the first child
	pipe(fdls);
	if ((pids[0]=fork())==0){
		//put stdout into the pipe
		dup2(fdls[1],STDOUT_FILENO);
		//close the pipes
		close(fdls[0]);
		close(fdls[1]);
		execvp(lexArgs[0], lexArgs);
	}

	pipe(fdsu);
	if((pids[1]=fork())==0) {
		close(fdls[1]);
		//put the data from lex.out into stdin
		dup2(fdls[0], STDIN_FILENO);
		close(fdls[0]);
		close(fdsu[0]);
		dup2(fdsu[1],STDOUT_FILENO);
		close(fdsu[1]);
		execvp(sortArgs[0], sortArgs);
	}
	//the pipe is no longer necessary, so close it
	close(fdls[0]);
	close(fdls[1]);
	
	pipe(fduc);
	if((pids[2]=fork())==0) {
		close(fdsu[1]);
		dup2(fdsu[0], STDIN_FILENO);
		close(fdsu[0]);

		close(fduc[0]);		
		dup2(fduc[1], STDOUT_FILENO);
		close(fduc[1]);		
		execvp(uniqArgs[0], uniqArgs);

	}
	close(fdsu[0]);
	close(fdsu[1]);

	if ((pids[3]=fork())==0) {
		close(fduc[1]);
		dup2(fduc[0], STDIN_FILENO);
		close(fduc[0]);
		execvp(compArgs[0],compArgs);
	}
	
	close(fduc[0]);
	close(fduc[1]);	
	//while the number of process that have died is less than 0
	//keep the parent in an infinite loop
	while(counter < 4);

	fclose(sLog);
}
/*
handler deals with signals that occur, specifically SIGCHLD. If a child
dies, write its name and id to a file, and increment the number of dead children

DATA TABLE
NAME			DESCRIPTION
pid			the pid of the dead process
status			the status of the process
selector		a position in both the names and pids arrays
line			the line that will be written to the file
*/
void handler (int signum, siginfo_t * si, void * ucontext){
	pid_t pid;
	int status;
	int selector;
	char line[256];
	//if the signal is that of SIGCHLD, evaluate
	if (signum==SIGCHLD) {
		//while there are still dead children
		while((pid = waitpid(-1, &status, WNOHANG)) >0){
			//compare the pid of the dead child to the ones in pids
			//set selecto to that position
			if (pid == pids[0]) {
				selector=0;
			}
			else if (pid == pids[1]) {
				selector=1;
			}
			else if (pid == pids[2]) {
				selector=2;
			}
			else if (pid == pids[3]) {
				selector=3;
			}
			//write the name and process ID into line, print it to file
			sprintf(line,"The process id:%d name:%s has died\n",pids[selector], names[selector]);
			fputs(line, sLog);
			//increment the number of dead children
			counter++;
		}
	}
} 

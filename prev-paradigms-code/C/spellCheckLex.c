#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<unistd.h>
#include<sys/wait.h>
#include<ctype.h>
int main(int argc, char * argv[]) {
	pid_t pid;
	int status;
	char string[2048];
	int fdl[2];
	pipe(fdl);
	pid=fork();
	if (pid > 0) {
		close(fdl[1]);
		waitpid(pid,&status,0);
		printf("Before or after");
		read(fdl[0],string,2048);
		printf("%s",string);
		printf("\n\n\n");
		return 0;
	} else {
		close(fdl[0]);
		dup2(fdl[1],STDOUT_FILENO);
		close(fdl[1]);
		char * args[3];
		args[0]="./lex.out";
		args[1]=argv[1];
		args[2]=NULL;
		execvp("./lex.out",args);
		
		return 0;
	}
}

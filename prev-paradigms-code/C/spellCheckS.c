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
	int fds[2];
	int fdu[2];
	pipe(fdl);
	pid=fork();
	if (pid > 0) {
		close(fdl[1]);
		waitpid(pid,&status,0);
		dup2(fdl[0],STDIN_FILENO);
		//read(fdl[0],string,2048);
		//printf("%s\n",string);
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
	pipe(fds);
	pid=fork();
	if (pid > 0) {
		waitpid(pid,&status,0);
		read(fds[0],string,2048);
		printf("%s",string);		
		return 0;
	} else {
		close(fds[0]);
		dup2(fds[1],STDOUT_FILENO);
		close(fds[1]);			
		char * args[2];
		args[0]="sort";
		args[1]=NULL;
		execvp("sort",args);
		return 0;
	}/*
	pipe(fdu);
	pid=fork();
	if (pid > 0) {
		//dup2(STDOUT_FILENO, STDIN_FILENO);
		waitpid(pid,&status,0);
		read(fdu[0], string, 2048);
		printf("%s",string);		
		return 0;
	} else {
		close(fdu[0]);
		dup2(STDIN_FILENO,STDOUT_FILENO);
		dup2(STDOUT_FILENO,fdu[1]);
		//close(fdu[1]);			
		char * args[3];
		args[0]="uniq";
		args[1]="-i";
		args[2]=NULL;
		execvp("uniq",args);
		return 0;
	}*/		
}

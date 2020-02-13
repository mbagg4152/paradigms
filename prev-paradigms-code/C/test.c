#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#define THING "Hello There:"
char * newLineRemove(char * word){
	char * newLine = strchr(word,'\n');
	*newLine='\0';
	return word;
}
int main() {
int t;
t=strcmp("rat","bat");
if (t>0) {
	printf("rat is bigger\n");
}
}

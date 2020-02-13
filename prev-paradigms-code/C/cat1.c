/*Bradley Gardner
CS-231-02L
Lab3*/

/*
This program is designed to perform the tasks of cat, reading files/user input and then concatenating them all into a single message. This message can be modified by the inputs b, E and n if given provided in the command line arguments.

The program's algorithm is to:
loop through the command line arguments beginning at argument index 1
	check if the string starts with - and is less than or equal to 4 		characters
		if the string contains a b, raise the b flag
		if the string contains an E, raise the E flag
		if the string contains an n, raise the n flag
Once the modifiers have been found, loop back through the arguments list from index 1
	if the string is equivalent to -, allow the user to write
		while the user doesn't enter then EOF character into stdin
			read the next user line
			if the line is not the EOF character
				print a tab
				if the b flag is raised, check the string length
					if length is greater than 1
						print and increment the line count.
				else if the n flag is set
					print and increment the line count
				if the e flag is set
					locate the the newline character in the string
					replace the newline character with a $
					concatenate the string with a newline character
				print the line that the user entered
		return the current number of lines
	else, read the file
		if the file exists
			while there is still data in the file
				print a tab
				if the b flag is raised, check the string length
					if length is greater than 1
						print and increment the line count.
				else if the n flag is set
					print and increment the line count
				if the e flag is set
					locate the the newline character in the string
					replace the newline character with a $
					concatenate the string with a newline character
				print the line of the file
			close the file
		return the current line count
*/

#include<stdio.h>
#include<string.h>
#include<stdlib.h>

int filePrinter(char file[], int b, int E, int n, int lineCount) {
/*
	filePrinter reads through a file and prints each line, modifying each as 		denoted by the set flags

	DATA TABLE
	NAME			VALUE
	file			parameter - the name of the file
	b				parameter - the b flag value
	E				parameter - the E flag value
	n				parameter - the n flag value
	fp				file pointer to the given file
	line			the current line in the file
	newLine			pointer to the newline character
*/
	FILE *fp;
	char line[256];
	char *newLine;
	//try opening the file, if we get a pointer, continue
	if ((fp = fopen(file, "r")))
	{	
		//fgets will return NULL upon EOF		
		while(fgets(line, 256, fp) != NULL)
		{			
			if (b && strlen(line) > 1) {
				//check if the line is just the newline character
				printf("%d. ",lineCount++);
			}
			else if (n) {
				printf("%d. ",lineCount++);
			}
			if (E) {
				//find the memory address of the newline character in the string
				newLine = strchr(line,'\n');
				//replace the character at that memory address
				*newLine='$';
				strncat(line,"\n",1);
			}
			printf("%s",line);
		}
		fclose(fp);
	}
	return lineCount;
}

int userWriter(int b, int E, int n, int lineCount) {
/*
	userWriter allows for the user to write to stdin and then output the line, 		modified by any and all modifiers

	DATA TABLE
	NAME			VALUE
	b				parameter - the b flag value
	E				parameter - the E flag value
	n				parameter - the n flag value
	line			the current line of user input
	newLine			pointer to the newline character
*/
	char line[256];
	char *newLine;
	while(fgets(line,256,stdin)!=NULL)
	{
		//The user enters the value of stdin after the while loop
		//This makes sure that if the user enters EOF, nothing is 
		//executed, the loop dies on the next iteration			
			if(b && strlen(line) > 1) 
			{
				//checks to make sure that there is more than
				//just the newline character in the string
				printf("%d. ",lineCount++);
			}
			else if (n) 
			{
				printf("%d. ", lineCount++);
			}				
			if (E) 
			{
				//locate the current position of the newline character
				newLine = strchr(line, '\n');
				//replace the character at the memory location with a $
				*newLine = '$';
				strncat(line,"\n",1);
			}
			printf("%s", line);
	}
	return lineCount;
}

int main(int argc, char *argv[]) {

/*
	main is the controlling function of the program. This will read through the
	arguments list to check for any modifiers and set the appropriate flags if 		needed. From there, it will loop through the arguments again looking for a 
	-, which it will then allow the user to write to stdin. If it finds
	anything else, it passes it to the file reader. The main will also keep
	track of the current line number.

	DATA TABLE
	NAME			VALUE
	argc			parameter - length of arguments list
	argv			parameter - arguments list
	b				b modifier flag
	E				E modifier flag
	n				n modifier flag
	counter			counter for for loops
	lineNumber		current line number
	foundInString	pointer to a character in a string, if found
*/
	int b = 0;
	int E = 0;
	int n = 0;
	int counter;
	int lineNumber=1;
	char *foundInString;
	for (counter = 1; counter < argc; counter++) {
		//if we find an argument that starts with a -, try to 
		//find b,E or n. Ignore ones that have more than 4 characters
		//since that could be a file name
		if (argv[counter][0]=='-' && !(foundInString = 										strstr(argv[counter],"."))) {
			//if a character is found, set the appropriate flag			
			foundInString = strstr(argv[counter], "b");
			if (foundInString) {
				b=1;
			}
			foundInString = strstr(argv[counter], "E");
			if (foundInString) {
				E=1;
			}			
			foundInString = strstr(argv[counter], "n");
			if (foundInString) {
				n=1;
			}
		}
	}
	for (counter = 1; counter < argc; counter++) {
		//if the argument is just a dash, read user input
		if (strcmp(argv[counter],"-")==0) {
			lineNumber = userWriter(b,E,n,lineNumber);
		} 
		else { 
			lineNumber = filePrinter(argv[counter], b, E, n, lineNumber);
		}

	}
}

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

FILE *openFile(char *fileName); // Function to open the requested file
void compareWords(FILE *inFile);

// Function that compares the user's inputs to words in the designated file
char *stringToLower(char *s);
// Function that converts a string to lower case

void main(int argc, char *argv[]) {
    if (argc != 2)
        // If there are more or less than two commands when calling the program
    {
        // Close the program and print an error
        printf("Please use this command format >>> ./compare (file).txt\n");
        return;
    }
    FILE *file = openFile(argv[1]);
    // Sets the file to the file the user passed in
    compareWords(file); // Prints the file
    fclose(file);       // Closes the file
}

FILE *openFile(char *fileName) {
    FILE *requestedFile = fopen(fileName, "r");
    // Sets requestedFile to the file that the user defined
    if (requestedFile) // If the file is present
    {
        return requestedFile; // Return the found file
    }
    // If the file was not found
    printf("Cannot find the requested file.\n"); // Print an error
    exit(0);                                     // Exit the program
}

void compareWords(FILE *inFile) {
    char inputWord[45], dictionaryWord[45];
    // Defining the arrays for the words
    while (fgets(inputWord, sizeof(inputWord), stdin) != NULL)
        // While the user input is not null
    {
        int wordFound = 0; // Set the "boolean" to false
        while (fgets(dictionaryWord, sizeof(dictionaryWord), inFile))
            // While there are more lines in the file
        {
            if (!strcmp(stringToLower(inputWord), stringToLower(dictionaryWord)))
                // If the words match
            {
                wordFound = 1; // The word was found!
                break;         // Exit the loop
            }
        }
        rewind(inFile);
        // Sets the file position to the beginning of the file
        if (wordFound == 1) // If the word was found
        {
            printf("Word correct: %s\n", inputWord); // Print the word was found
        } else // If the word wasn't found
        {
            printf("Word incorrect: %s\n", inputWord);
            // Print the word was not found
        }
    }
}

char *stringToLower(char *s) {
    int c = 0; // Set index to 0
    while (s[c] != '\0')
        // While the string still has characters
    {
        if (isupper(s[c]) > 0)
            // If the character is upper case
        {
            s[c] = tolower(s[c]);
            // convert it to lower case
        }
        c++; // Increase the index by one
    }
    return s;
    // Return the now lower case string
}
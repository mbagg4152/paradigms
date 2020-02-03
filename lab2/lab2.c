/* Maggie Horton
 * CS-231
 * Program - Modifying integer input greater than 0.
 * The purpose of this program is to manipulate ints. Rules:
 * If int is between 1-9 --> add 1 to int and make it the leftmost
 * digit. Ex: input = 3, output = 43
 * If int is greater than 9 --> move rightmost digit so that it
 * becomes leftmost digit. Ex: input = 234, output = 423
 * --------------------------------------------------------------------
 * ALGORITHM
 * > Get user input
 *  > if (input < 0 OR input digit count > 6 OR input is not int) -->
 *      > quit program
 *  > else (input satisfies criteria)
 *      > if (int is between 1 & 9 exclusive) -->
 *          > create var from adding 1 to input
 *          > pass (var, input) to function for int "concatenation"
 *          > output: result from "concatenation" compared w/ the input
 *      > else (int is greater than 9) -->
 *          > split int into separate digits in array, digits in array
 *            are now in the reverse order
 *          > copy to another array in a loop to fix digit order
 *          > add last digit of corrected array to index 0
 *            of shifted array, then copy all remaining values
 *            staring at index 1
 *          > send array to return "concatenated" resulting int
 *          > output: result from "concatenation" compared w/ the input
 * --------------------------------------------------------------------
 * [External data table]
 *  NAME         DESCRIPTION
 *  TRUE         1 is TRUE in C
 *  FALSE        0 is FALSE in C
 *  SIZE         initial array size
 *  MAX          largest sized int allowed */

#include <stdlib.h>
#include <stdio.h>

#define TRUE 1
#define FALSE 0
#define SIZE 10
#define MAX 100000

/* Helper function used for exiting the program
 * PARAM errMsg - message for specific exit case
 * RETURNS void */
void quit(char errMsg[]) {
    printf("%s", errMsg);
    exit(0);
}

/* Compares two ints then outputs the relation, <, > or =
 * PARAM oldNum - original input
 * PARAM changedNum - changed value
 * RETURNS void */
void compare(int oldNum, int changedNum) {
    if (oldNum > changedNum) printf("Output: %d > %d\n", oldNum, changedNum);
    else if (oldNum < changedNum) printf("Output: %d < %d\n", oldNum, changedNum);
    else if (oldNum == changedNum) printf("Output: %d = %d\n", oldNum, changedNum);
}

/* Concatenates two ints by getting value of rightMost digit,
 * & using it to find how many times to divide the leftmost digit by 10
 * PARAM leftMost - leftmost int digit
 * PARAM rightMost - rightMost int digit
 * VAR rightCp - copy of rightMost, value cannot be modified for the 'concat' to work
 * RETURNS 'concatenated' int*/
int concatInt(int leftMost, int rightMost) {
    int rightCp = rightMost;
    while (rightCp > 0) {
        leftMost *= 10;
        rightCp /= 10;
    }
    return leftMost + rightMost;
}

/* Utilizes concatInt to 'concat' more than 2 ints
 * PARAM numList - array of an ints digits
 * PARAM numCount - length of array
 * VAR builtNum - result of 'concat'
 * RETURNS 'concatenated' int */
int intBuilder(int numList[], int numCount) {
    int builtNum = 0;
    for (int i = 0; i < numCount - 1; i++) {
        if (i == 0) builtNum = concatInt(numList[i], numList[i + 1]);
        else builtNum = concatInt(builtNum, numList[i + 1]);
    }
    return builtNum;
}

/* Finds the length of an int (in digits)
 * PARAM toMeasure - int to get the length of
 * VAR digitCount - counted length of array
 * RETURNS the length of the int */
int findLength(int toMeasure) {
    int digitCount = 0;
    while (toMeasure > 0) {
        digitCount++;
        toMeasure = toMeasure / 10;
    }
    return digitCount;
}

/* Handles the changes for inputs greater than nine.
 * PARAM val - user input
 * VAR digits[] - initial array to hold separated digits
 * VAR length - length of int
 * VAR corrected[] - used to put the values from digits[] in correct order
 * VAR shifted[] - used in completing the shifting for values > 9
 * RETURNS void */
void greaterThanNine(int val) {
    int input = val, length = findLength(input);
    int digits[SIZE];
    for (int i = 0; i < length; i++) {
        int mod = val % 10; // get int digit last --> first
        digits[i] = mod;
        val = val / 10; // move to next digit of int
    }

    int corrected[length], shifted[length];
    for (int i = 0; i < length; i++) corrected[length - 1 - i] = digits[i]; // flip array
    for (int i = 0; i < length; i++) shifted[i + 1] = corrected[i]; // shift digits
    shifted[0] = corrected[(sizeof(corrected) / sizeof(int)) - 1]; // last/rightmost digit of corrected[]
    compare(input, intBuilder(shifted, length));
}


/* Completes specified modification for values between 1 & 9
 * PARAM val - input int
 * VAR out - result of 'concat' of (val+1) & val
 * RETURNS void */
void oneToNine(int val) {
    int toAdd = val + 1, out = concatInt(toAdd, val);
    compare(val, out);
}

/* Determines if the input is between 1 & 9 or if > 9.
 * PARAM toClassify - input from the user
 * RETURNS void */
void classify(int toClassify) {
    if (toClassify >= 1 && toClassify <= 9) oneToNine(toClassify);
    else greaterThanNine(toClassify);
}

/* The main function & menu of the program
 * VAR usrInput - stores user input from scanf
 * VAR loopAgain - value determines while loop execution
 * RETURNS 0 when program is done */
int main(void) {
    int usrInput, loopAgain = 1;
    printf("Lab Assignment 2, courtesy of Maggie Horton\n");
    while (loopAgain) {
        usrInput = 0;
        printf("Enter an int between 1 & 100k inclusive: ");
        scanf(" %d", &usrInput);
        while (getchar() != '\n');
        if ((usrInput < 1) || (usrInput > MAX)) {
            loopAgain = 0;
            quit("\nIllegal input: out of range or non-numeric\n");
        } else classify(usrInput);
    }
    return 0;
}

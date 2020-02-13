/* Bradley Gardner
CS-231-02L
Lab2*/

/*
The purpose of this program is to determine whether or not a number is a
palindrome.
The program algorithm is:
	Ask the user for a number
	Loop for non-negative inputs: 
		If the input is negative, end
		If the input is less than 10, say it is a palindrome
		If the number is divisible by 10, say it is not a palindrome
		If the number does not meet the previous criteria:
			Create a secondary number, 0
			Loop until input is zero:
				Take the modulous of the input and add it to 					the secondary number.
				Multiply the secondary number by 10.
				Divide the input by 10.
			If the secondary number equals the input, say 				it's palindrome
		Ask user for a number 
*/
#include <stdio.h>

int reverseNumber (int number) {
/*
	reverseNumber will reverse the given number.
DATA TABLE 
NAME		DESCRIPTION
number		parameter - number to reverse
reverse		reversed number
*/
	int reverse = 0;
	while (number != 0) {
		reverse *= 10;		
		//add the last digit of the number to the reverse
		reverse+=number%10;
		//move decimal point
		number/=10;
	}
	return reverse;
}

int main () {
/*main is the controlling function in the program. This will ask 
the user to enter which it will then determine if the number is
negative, less than 10 or ends in 0. If the number does not match
any of the criteria, it will then pass it on to reverseNumber. 
Once the reverse of the input has been retrieved, it will compare 
the two numbers for equality.

main data table
NAME		DESCRIPTION
userNumber	the user entered number
reversedNumber	the user number in reverse order

*/
	int userNumber, reversedNumber;
	//prime the pump for the while loop
	printf("Please enter a number, I'll tell you if it's a palindrome.\n Enter a negative to exit: ");
	scanf("%d", &userNumber);
	while (userNumber >=0) {
		if (userNumber < 10) {
			printf("\nAll single digit numbers are palindromes");
		}		
		else if (userNumber%10==0) {
			printf("\nThis is not a palindrome");
		}
		//if the number has more than one digit, and isn't a multiple of
		//10, reverse it
		else {
			reversedNumber = reverseNumber(userNumber);
			if (userNumber == reversedNumber) {
				printf("\nThis is a palindrome");
			}
			else {
				printf("\nThis is not a palindrome");
			}
		}
		//pump in the next input
		printf("\nPlease enter a number, I'll tell you if it's a palindrome.\n Enter a negative to exit: ");
		scanf("%d", &userNumber);
	
	}
	printf("\nGoodbye\n");
	return 0;
}

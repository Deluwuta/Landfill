#include <stdio.h>

// Binary exponentiation (pretty cool!)
int power(int base, int exp) {
    if (exp == 0)
        return 1;
    else if (exp % 2)
        return base * power(base, exp - 1);
    else {
        int temp = power(base, exp / 2);
        return temp * temp;
    }
}

int isPalindrome(int x) {
    if (x < 0) return 0;

    int digits = 1;
    int aux = 10;

    // First I calculate the number of digits of the number
    while (aux <= x) {
        digits++;
        aux *= 10;
    }

    // Number of digits of the number minus the one to check
    int num_digits = digits - 1;
    
    // 'is_not' tell us if the number is palindrome
    int is_not = 0;

    while (is_not != 1) {
        int aux = power(10, num_digits);
        num_digits -= 2; // We remove the first and last digit

        int first_d = x / aux; // First digit
        int last_d = x % 10; // Last digit

        if (first_d != last_d) is_not = 1;
        else {
            // Convert the number removing the last and first digits
            x = (x % aux) / 10; 
            if (x < 10) break; // Is odd o7
        }
    }

    return !is_not;
}

int main(){
    int x = 1000021;
    int aux = isPalindrome(x);

    if (aux == 0) printf("El número %d no es palíndromo\n", x);
    else printf("El número %d ES palíndromo!\n", x);
}

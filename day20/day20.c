#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define INPUT 33100000

int main() {

    long *houses = malloc(sizeof(long) * INPUT);


    for (long i = 1; i <= INPUT; i++) {
        for (int j = i; j <= INPUT; j += i) {
            houses[j-1] += i * 10;
        }
    }

    for (int i = 0; i < INPUT; i++) {
        if (houses[i] >= INPUT) {
            printf("part a: %d\n", i+1);
            break;
        }
    }

    int delivered = 0;
    memset(houses, 0, sizeof(long) * INPUT);
    for (long i = 1; i <= INPUT; i++) {
        delivered = 0;
        for (int j = i; delivered < 50 && j <= INPUT; j += i) {
            houses[j-1] += i * 11;
            delivered++;
        }
    }

    for (int i = 0; i < INPUT; i++) {
        if (houses[i] >= INPUT) {
            printf("part b: %d\n", i+1);
            break;
        }
    }

    return 0;
}

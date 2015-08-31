/* -*- C -*-
 *
 * $HEADER$
 *
 * A program that just spins - provides mechanism for testing user-driven
 * abnormal program termination
 */

#include <stdio.h>
#include <unistd.h>

int main(int argc, char* argv[])
{

    int i, j=0;
    double pi;
    pid_t pid;

    pid = getpid();

    printf("spin: Pid %ld\n", (long)pid);

    i = 0;
    while (0 == j) {
        i++;
        pi = i / 3.14159256;
        if (i > 100) i = 0;
    }

    return 0;
}

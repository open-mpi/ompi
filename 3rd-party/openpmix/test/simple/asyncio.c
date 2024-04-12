#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define REPS 2

char data[1024];

int main(int argc, char *argv[])
{
    int n, size, start, end, delay;

    /* write a line out that has a newline at the end */
    memset(data, 0, sizeof(data));
    memcpy(data, "BEGIN123456788865432356897654345678END\n", strlen("BEGIN123456788865432356897654345678END\n"));
    write(1, data, strlen(data));

    for (n=0; n < REPS; n++) {
        fprintf(stdout, "REP %d\n", n);
        start = 0;
        delay = random() % 4;
 //       sleep(delay);
        memset(data, 0, sizeof(data));
        memset(data, 'A', 1024);
        end = 100;
        write(1, data, end);
        start += end;
        delay = random() % 4;
   //     sleep(delay);
        write(1, &data[start], end);
        start += end;
        delay = random() % 4;
  //      sleep(delay);
        write(1, &data[start], 1024 - start);
        fprintf(stdout, "\n");
    }
}

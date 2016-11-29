/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main(int argc, char* argv[])
{
    unsigned char fifo_cmd = 1;
    int fd;

    if (1 > argc) {
        fprintf(stderr, "usage: attach <full-path-to-debugger-fifo-file>\n");
        exit(1);
    }

    fd = open(argv[1], O_WRONLY);
    write(fd, &fifo_cmd, sizeof(unsigned char));
    close(fd);

    return 0;
}

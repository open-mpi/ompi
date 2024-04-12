/*
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

int main(int argc, char **argv)
{
    int n, limit, rc;
    char buffer[1024];

    if (argc < 2) {
        fprintf(stderr, "Usage: %s <number of 1024 buffers to write>\n", argv[0]);
        exit(1);
    }
    limit = strtol(argv[1], NULL, 10);
    memset(buffer, 'a', 1024);
    buffer[1023] = '\n';
    for (n=0; n < limit; n++) {
        rc = write(STDOUT_FILENO, buffer, 1024);
        if (0 > rc) {
            fprintf(stderr, "Write failed: %d\n", rc);
            exit(1);
        }
    }
    exit(0);
}

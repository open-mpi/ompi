/*
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <fcntl.h>
#include <getopt.h>
#include <jansson.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv)
{
    int mypipe;
    int len;
    char *grps;
    int rc;

    /* open a named pipe */
    mypipe = open("./myfifo", O_RDONLY | O_CREAT | O_NONBLOCK, S_IRWXG | S_IRWXU);

    while (1) {
        rc = read(mypipe, &len, sizeof(int));
        if (0 == rc) {
            continue;
        }
        if (0 > rc || sizeof(int) != rc) {
            /* hit an error */
            continue;
        }
        if (-1 == len) {
            /* signal to exit */
            break;
        }
        grps = (char *) malloc(len);
        rc = read(mypipe, grps, len);
        if (0 > rc || len != rc) {
            /* hit an error */
            continue;
        }
        fprintf(stderr, "GRPS: %s\n", grps);
    }

    unlink("./myfifo");
    return 0;
}

/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/* here to prevent "has no symbols" warnings from ranlib on OS X */
static int dummy ATTRIBUTE((unused)) MPL_USED = 0;

#ifndef MPL_HAVE_MKSTEMP

#define MAX_TRIES 100   /* max number of filenames we try before giving up */
#define NUM_XS 6        /* number of X's in the template */

/* replaces NUM_XS characters in xs with random characters
   xs must be at least NUM_XS chars long */
static void randchar(char xs[])
{
    char chararray[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890";
    int i;

    for (i = 0; i < NUM_XS; ++i)
        xs[i] = chararray[rand() % (sizeof(chararray) - 1)];
}


int MPL_mkstemp(char *template)
{
    int fd;
    unsigned int seed = (unsigned int) getpid();        /* can probably use something better */
    char *X;                    /* points to the XXXXXX substring in template */
    int i;

    srand(seed);

    /* find the end of the template string */
    X = template;
    while (*X)
        ++X;

    /* back up NUM_XS characters and check to make sure they're all 'X' */
    for (i = 0; i < NUM_XS; ++i) {
        --X;
        if (X < template || *X != 'X') {
            errno = EINVAL;
            return -1;
        }
    }

    for (i = 0; i < MAX_TRIES; ++i) {
        randchar(X);
        fd = open(template, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
        if (fd != -1)
            return fd;
    }

    errno = EEXIST;
    return -1;
}
#endif

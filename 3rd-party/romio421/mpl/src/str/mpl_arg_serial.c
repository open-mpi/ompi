/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"
#include <assert.h>

int MPL_args_serialize(int argc, char **argv, int *len, void **serialized_buf)
{
    int buf_size, offset, tmp;
    char *buf;
    int i;

    /*
     * the serialized format will contain the following:
     *   1. An integer indicating how many arguments there are
     *   2. An array of integers indicating the length of each argument
     *   3. An array of strings with the actual arguments
     */

    buf_size = 0;
    buf_size += sizeof(int);    /* for the number of arguments */
    buf_size += argc * sizeof(int);     /* for the argument lengths */
    for (i = 0; i < argc; i++)
        buf_size += strlen(argv[i]);    /* for the arguments themselves */

    buf = MPL_malloc(buf_size, MPL_MEM_STRINGS);
    assert(buf);

    offset = 0;
    memcpy(buf, &argc, sizeof(int));
    offset += sizeof(int);

    for (i = 0; i < argc; i++) {
        tmp = strlen(argv[i]);
        memcpy(buf + offset, &tmp, sizeof(int));
        offset += sizeof(int);
    }

    for (i = 0; i < argc; i++) {
        memcpy(buf + offset, argv[i], strlen(argv[i]));
        offset += strlen(argv[i]);
    }

    *len = buf_size;
    *serialized_buf = buf;

    return 0;
}

int MPL_args_deserialize(int len, const void *serialized_buf, int *argc, char ***argv)
{
    const char *buf = serialized_buf;
    int nargs;
    int *arg_lengths;
    char **targv;
    int i;

    nargs = *((int *) buf);
    buf += sizeof(int);

    targv = (char **) MPL_malloc(nargs * sizeof(char *), MPL_MEM_STRINGS);
    arg_lengths = (int *) MPL_malloc(nargs * sizeof(int), MPL_MEM_STRINGS);

    assert(targv && arg_lengths);

    for (i = 0; i < nargs; i++) {
        arg_lengths[i] = (*((int *) buf));
        buf += sizeof(int);

        /* allocate an extra end-of-string character for each string */
        targv[i] = (char *) MPL_malloc(arg_lengths[i] + 1, MPL_MEM_STRINGS);
        assert(targv[i]);
    }

    for (i = 0; i < nargs; i++) {
        memcpy(targv[i], buf, arg_lengths[i]);
        targv[i][arg_lengths[i]] = 0;   /* append an end of string character */
        buf += arg_lengths[i];
    }

    *argc = nargs;
    *argv = targv;

    MPL_free(arg_lengths);

    return 0;
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2011 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "mpl.h"

int main(void)
{
    char *orig;
    char *str;
    char *next;

    str = NULL;
    next = MPL_strsep(&str, "|");
    assert(next == NULL);
    assert(str == NULL);

    orig = MPL_strdup("");
    str = orig;
    next = MPL_strsep(&str, "|");
    assert(str == NULL);
    assert(next == orig);
    MPL_free(orig);

    orig = MPL_strdup("a|b|c");
    str = orig;
    next = MPL_strsep(&str, "|");
    assert(next == orig);
    assert(0 == strcmp(next, "a"));
    next = MPL_strsep(&str, "|");
    assert(0 == strcmp(next, "b"));
    next = MPL_strsep(&str, "|");
    assert(0 == strcmp(next, "c"));
    next = MPL_strsep(&str, "|");
    assert(next == NULL);
    assert(str == NULL);
    MPL_free(orig);

    orig = MPL_strdup("a|b:c");
    str = orig;
    next = MPL_strsep(&str, ":|");
    assert(next == orig);
    assert(0 == strcmp(next, "a"));
    next = MPL_strsep(&str, ":|");
    assert(0 == strcmp(next, "b"));
    next = MPL_strsep(&str, ":|");
    assert(0 == strcmp(next, "c"));
    next = MPL_strsep(&str, ":|");
    assert(next == NULL);
    assert(str == NULL);
    MPL_free(orig);

    orig = MPL_strdup("a|:b:c");
    str = orig;
    next = MPL_strsep(&str, ":|");
    assert(next == orig);
    assert(0 == strcmp(next, "a"));
    next = MPL_strsep(&str, ":|");
    assert(0 == strcmp(next, ""));
    next = MPL_strsep(&str, ":|");
    assert(0 == strcmp(next, "b"));
    next = MPL_strsep(&str, ":|");
    assert(0 == strcmp(next, "c"));
    next = MPL_strsep(&str, ":|");
    assert(next == NULL);
    assert(str == NULL);
    MPL_free(orig);

    return 0;
}

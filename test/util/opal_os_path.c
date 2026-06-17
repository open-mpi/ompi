/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif

#include "support.h"
#include "opal/util/os_path.h"
#include "opal/constants.h"

int main(int argc, char *argv[])
{
    char *result;
    char long_elem[OPAL_PATH_MAX + 16];
    int i;

    test_init("opal_os_path");

    /* ================================================================
     * Trivial: no path elements
     * ================================================================ */

    /* Relative, no elements -> "./" */
    result = opal_os_path(1 /*relative*/, NULL);
    test_verify("relative no-args: not NULL", NULL != result);
    if (NULL != result) {
        test_verify("relative no-args: equals './'", 0 == strcmp(result, "./"));
        free(result);
    }

    /* Absolute, no elements -> "/" */
    result = opal_os_path(0 /*absolute*/, NULL);
    test_verify("absolute no-args: not NULL", NULL != result);
    if (NULL != result) {
        test_verify("absolute no-args: equals '/'", 0 == strcmp(result, "/"));
        free(result);
    }

    /* ================================================================
     * Relative paths
     * ================================================================ */

    /* Single element */
    result = opal_os_path(1, "aaa", NULL);
    test_verify("relative 1-elem: not NULL", NULL != result);
    if (NULL != result) {
        test_verify("relative 1-elem: equals './aaa'", 0 == strcmp(result, "./aaa"));
        free(result);
    }

    /* Two elements */
    result = opal_os_path(1, "aaa", "bbb", NULL);
    test_verify("relative 2-elem: not NULL", NULL != result);
    if (NULL != result) {
        test_verify("relative 2-elem: equals './aaa/bbb'",
                    0 == strcmp(result, "./aaa/bbb"));
        free(result);
    }

    /* Three elements */
    result = opal_os_path(1, "aaa", "bbb", "ccc", NULL);
    test_verify("relative 3-elem: not NULL", NULL != result);
    if (NULL != result) {
        test_verify("relative 3-elem: equals './aaa/bbb/ccc'",
                    0 == strcmp(result, "./aaa/bbb/ccc"));
        free(result);
    }

    /* ================================================================
     * Absolute paths
     * ================================================================ */

    /* Single element */
    result = opal_os_path(0, "aaa", NULL);
    test_verify("absolute 1-elem: not NULL", NULL != result);
    if (NULL != result) {
        test_verify("absolute 1-elem: equals '/aaa'", 0 == strcmp(result, "/aaa"));
        free(result);
    }

    /* Two elements */
    result = opal_os_path(0, "aaa", "bbb", NULL);
    test_verify("absolute 2-elem: not NULL", NULL != result);
    if (NULL != result) {
        test_verify("absolute 2-elem: equals '/aaa/bbb'",
                    0 == strcmp(result, "/aaa/bbb"));
        free(result);
    }

    /* Three elements */
    result = opal_os_path(0, "aaa", "bbb", "ccc", NULL);
    test_verify("absolute 3-elem: not NULL", NULL != result);
    if (NULL != result) {
        test_verify("absolute 3-elem: equals '/aaa/bbb/ccc'",
                    0 == strcmp(result, "/aaa/bbb/ccc"));
        free(result);
    }

    /* ================================================================
     * Element that already starts with the path separator:
     * the implementation skips inserting an extra separator.
     * ================================================================ */

    result = opal_os_path(0, "/absolute_elem", NULL);
    test_verify("elem starting with '/': not NULL", NULL != result);
    if (NULL != result) {
        /* The element already has the separator, so the result should
         * be "/absolute_elem" (not "//absolute_elem"). */
        test_verify("elem starting with '/': equals '/absolute_elem'",
                    0 == strcmp(result, "/absolute_elem"));
        free(result);
    }

    /* ================================================================
     * Path length limit: element too long -> NULL
     * ================================================================ */

    for (i = 0; i < OPAL_PATH_MAX + 10; ++i) {
        long_elem[i] = 'a';
    }
    long_elem[OPAL_PATH_MAX + 10] = '\0';

    result = opal_os_path(0, long_elem, NULL);
    test_verify("too-long elem: returns NULL", NULL == result);
    if (NULL != result) {
        free(result);
    }

    /* Same test for relative mode */
    result = opal_os_path(1, long_elem, NULL);
    test_verify("too-long elem (relative): returns NULL", NULL == result);
    if (NULL != result) {
        free(result);
    }

    return test_finalize();
}

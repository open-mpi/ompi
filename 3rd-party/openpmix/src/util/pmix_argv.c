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
 * Copyright (c) 2007      Voltaire. All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC. All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#ifdef HAVE_STDLIB_H
#    include <stdlib.h>
#endif /* HAVE_STDLIB_H */
#ifdef HAVE_STRING_H
#    include <string.h>
#endif /* HAVE_STRING_H */

#include "pmix_common.h"
#include "src/util/pmix_argv.h"

#define ARGSIZE 128

/*
 * Append a string to the end of a new or existing argv array.
 */
pmix_status_t pmix_argv_append(int *argc, char ***argv, const char *arg)
{
    pmix_status_t rc;

    /* add the new element */
    if (PMIX_SUCCESS != (rc = PMIx_Argv_append_nosize(argv, arg))) {
        return rc;
    }

    *argc = PMIx_Argv_count(*argv);

    return PMIX_SUCCESS;
}

pmix_status_t pmix_argv_append_unique_idx(int *idx, char ***argv, const char *arg)
{
    int i;
    pmix_status_t rc;

    /* if the provided array is NULL, then the arg cannot be present,
     * so just go ahead and append
     */
    if (NULL == *argv) {
        goto add;
    }
    /* see if this arg is already present in the array */
    for (i = 0; NULL != (*argv)[i]; i++) {
        if (0 == strcmp(arg, (*argv)[i])) {
            /* already exists */
            *idx = i;
            return PMIX_SUCCESS;
        }
    }
add:
    if (PMIX_SUCCESS != (rc = PMIx_Argv_append_nosize(argv, arg))) {
        return rc;
    }
    *idx = PMIx_Argv_count(*argv) - 1;

    return PMIX_SUCCESS;
}

/*
 * Join all the elements of an argv array from within a
 * specified range into a single newly-allocated string.
 */
char *pmix_argv_join_range(char **argv, size_t start, size_t end, int delimiter)
{
    char **p;
    char *pp;
    char *str;
    size_t str_len = 0;
    size_t i;

    /* Bozo case */

    if (NULL == argv || NULL == argv[0] || (int) start >= PMIx_Argv_count(argv)) {
        return strdup("");
    }

    /* Find the total string length in argv including delimiters.  The
     last delimiter is replaced by the NULL character. */

    for (p = &argv[start], i = start; *p && i < end; ++p, ++i) {
        str_len += strlen(*p) + 1;
    }

    if (0 == str_len) {
        return strdup("");
    }

    /* Allocate the string. */

    if (NULL == (str = (char *) malloc(str_len))) {
        return NULL;
    }

    /* Loop filling in the string. */

    str[--str_len] = '\0';
    p = &argv[start];
    pp = *p;

    for (i = 0; i < str_len; ++i) {
        if ('\0' == *pp) {

            /* End of a string, fill in a delimiter and go to the next
             string. */

            str[i] = (char) delimiter;
            ++p;
            pp = *p;
        } else {
            str[i] = *pp++;
        }
    }

    /* All done */

    return str;
}

/*
 * Return the number of bytes consumed by an argv array.
 */
size_t pmix_argv_len(char **argv)
{
    char **p;
    size_t length;

    if (NULL == argv)
        return (size_t) 0;

    length = sizeof(char *);

    for (p = argv; *p; ++p) {
        length += strlen(*p) + 1 + sizeof(char *);
    }

    return length;
}

/*
 * Copy a NULL-terminated argv array, stripping any leading/trailing
 * quotes from each element
 */
char **pmix_argv_copy_strip(char **argv)
{
    char **dupv = NULL;
    int n;
    char *start;
    bool mod;
    size_t len;

    if (NULL == argv) {
        return NULL;
    }

    /* create an "empty" list, so that we return something valid if we
     were passed a valid list with no contained elements */
    dupv = (char **) malloc(sizeof(char *));
    dupv[0] = NULL;

    for (n=0; NULL != argv[n]; n++) {
        mod = false;
        start = argv[n];
        if ('\"' == argv[n][0]) {
            ++start;
        }
        len = strlen(argv[n]);
        if ('\"' == argv[n][len-1]) {
            argv[n][len-1] = '\0';
            mod = true;
        }
        if (PMIX_SUCCESS != PMIx_Argv_append_nosize(&dupv, start)) {
            PMIx_Argv_free(dupv);
            if (mod) {
                argv[n][len-1] = '\"';
            }
            return NULL;
        }
        if (mod) {
            argv[n][len-1] = '\"';
        }
    }

    /* All done */

    return dupv;
}

pmix_status_t pmix_argv_delete(int *argc, char ***argv, int start, int num_to_delete)
{
    int i;
    int count;
    int suffix_count;
    char **tmp;

    /* Check for the bozo cases */
    if (NULL == argv || NULL == *argv || 0 == num_to_delete) {
        return PMIX_SUCCESS;
    }
    count = PMIx_Argv_count(*argv);
    if (start > count) {
        return PMIX_SUCCESS;
    } else if (start < 0 || num_to_delete < 0) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* Ok, we have some tokens to delete.  Calculate the new length of
       the argv array. */

    suffix_count = count - (start + num_to_delete);
    if (suffix_count < 0) {
        suffix_count = 0;
    }

    /* Free all items that are being deleted */

    for (i = start; i < count && i < start + num_to_delete; ++i) {
        free((*argv)[i]);
    }

    /* Copy the suffix over the deleted items */

    for (i = start; i < start + suffix_count; ++i) {
        (*argv)[i] = (*argv)[i + num_to_delete];
    }

    /* Add the trailing NULL */

    (*argv)[i] = NULL;

    /* adjust the argv array */
    tmp = (char **) realloc(*argv, sizeof(char *) * (i + 1));
    if (NULL != tmp)
        *argv = tmp;

    /* adjust the argc */
    (*argc) -= num_to_delete;

    return PMIX_SUCCESS;
}

pmix_status_t pmix_argv_insert(char ***target, int start, char **source)
{
    int i, source_count, target_count;
    int suffix_count;

    /* Check for the bozo cases */

    if (NULL == target || NULL == *target || start < 0) {
        return PMIX_ERR_BAD_PARAM;
    } else if (NULL == source) {
        return PMIX_SUCCESS;
    }

    /* Easy case: appending to the end */

    target_count = PMIx_Argv_count(*target);
    source_count = PMIx_Argv_count(source);
    if (start > target_count) {
        for (i = 0; i < source_count; ++i) {
            pmix_argv_append(&target_count, target, source[i]);
        }
    }

    /* Harder: insertting into the middle */

    else {

        /* Alloc out new space */

        *target = (char **) realloc(*target, sizeof(char *) * (target_count + source_count + 1));

        /* Move suffix items down to the end */

        suffix_count = target_count - start;
        for (i = suffix_count - 1; i >= 0; --i) {
            (*target)[start + source_count + i] = (*target)[start + i];
        }
        (*target)[start + suffix_count + source_count] = NULL;

        /* Strdup in the source argv */

        for (i = start; i < start + source_count; ++i) {
            (*target)[i] = strdup(source[i - start]);
        }
    }

    /* All done */

    return PMIX_SUCCESS;
}

pmix_status_t pmix_argv_insert_element(char ***target, int location, char *source)
{
    int i, target_count;
    int suffix_count;

    /* Check for the bozo cases */

    if (NULL == target || NULL == *target || location < 0) {
        return PMIX_ERR_BAD_PARAM;
    } else if (NULL == source) {
        return PMIX_SUCCESS;
    }

    /* Easy case: appending to the end */
    target_count = PMIx_Argv_count(*target);
    if (location > target_count) {
        pmix_argv_append(&target_count, target, source);
        return PMIX_SUCCESS;
    }

    /* Alloc out new space */
    *target = (char **) realloc(*target, sizeof(char *) * (target_count + 2));

    /* Move suffix items down to the end */
    suffix_count = target_count - location;
    for (i = suffix_count - 1; i >= 0; --i) {
        (*target)[location + 1 + i] = (*target)[location + i];
    }
    (*target)[location + suffix_count + 1] = NULL;

    /* Strdup in the source */
    (*target)[location] = strdup(source);

    /* All done */
    return PMIX_SUCCESS;
}

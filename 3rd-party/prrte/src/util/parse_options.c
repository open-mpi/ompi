/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif

#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"

#include "src/runtime/prte_globals.h"

#include "src/util/pmix_parse_options.h"

void pmix_util_parse_range_options(char *inp, char ***output)
{
    char **r1 = NULL, **r2 = NULL;
    int i, vint;
    int start, end, n;
    char nstr[32];
    char *input, *bang;
    bool bang_option = false;

    /* protect against null input */
    if (NULL == inp) {
        return;
    }

    /* protect the provided input */
    input = strdup(inp);

    /* check for the special '!' operator */
    if (NULL != (bang = strchr(input, '!'))) {
        bang_option = true;
        *bang = '\0';
    }

    /* split on commas */
    r1 = PMIX_ARGV_SPLIT_COMPAT(input, ',');
    /* for each resulting element, check for range */
    for (i = 0; i < PMIX_ARGV_COUNT_COMPAT(r1); i++) {
        r2 = PMIX_ARGV_SPLIT_COMPAT(r1[i], '-');
        if (1 < PMIX_ARGV_COUNT_COMPAT(r2)) {
            /* given range - get start and end */
            start = strtol(r2[0], NULL, 10);
            end = strtol(r2[1], NULL, 10);
        } else {
            /* check for wildcard - have to do this here because
             * the -1 would have been caught in the split
             */
            vint = strtol(r1[i], NULL, 10);
            if (-1 == vint) {
                PMIX_ARGV_FREE_COMPAT(*output);
                *output = NULL;
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(output, "-1");
                PMIX_ARGV_FREE_COMPAT(r2);
                goto cleanup;
            }
            start = strtol(r2[0], NULL, 10);
            end = start;
        }
        for (n = start; n <= end; n++) {
            snprintf(nstr, 32, "%d", n);
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(output, nstr);
        }
        PMIX_ARGV_FREE_COMPAT(r2);
    }

cleanup:
    if (bang_option) {
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(output, "BANG");
    }
    free(input);
    PMIX_ARGV_FREE_COMPAT(r1);
}

void prte_util_get_ranges(char *inp, char ***startpts, char ***endpts)
{
    char **r1 = NULL, **r2 = NULL;
    int i;
    char *input;

    /* protect against null input */
    if (NULL == inp) {
        return;
    }

    /* protect the provided input */
    input = strdup(inp);

    /* split on commas */
    r1 = PMIX_ARGV_SPLIT_COMPAT(input, ',');
    /* for each resulting element, check for range */
    for (i = 0; i < PMIX_ARGV_COUNT_COMPAT(r1); i++) {
        r2 = PMIX_ARGV_SPLIT_COMPAT(r1[i], '-');
        if (2 == PMIX_ARGV_COUNT_COMPAT(r2)) {
            /* given range - get start and end */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(startpts, r2[0]);
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(endpts, r2[1]);
        } else if (1 == PMIX_ARGV_COUNT_COMPAT(r2)) {
            /* only one value provided, so it is both the start
             * and the end
             */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(startpts, r2[0]);
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(endpts, r2[0]);
        } else {
            /* no idea how to parse this */
            pmix_output(0, "%s Unknown parse error on string: %s(%s)",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), inp, r1[i]);
        }
        PMIX_ARGV_FREE_COMPAT(r2);
    }

    free(input);
    PMIX_ARGV_FREE_COMPAT(r1);
}

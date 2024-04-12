/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "src/util/pmix_output.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

#include "src/util/uri.h"

static const char *uri_reserved_path_chars = "!$&'()*+,;=:@ ";

char *prte_uri_get_scheme(const char *uri)
{
    char *turi = strdup(uri);
    char *ptr;

    if (NULL == (ptr = strchr(turi, ':'))) {
        pmix_show_help("help-prte-util.txt", "malformed-uri", true, uri);
        free(turi);
        return NULL;
    }
    *ptr = '\0';
    return turi;
}

char *prte_filename_to_uri(const char *filename, const char *hostname)
{
    char *uri, *fn;
    size_t i, j, k, n;

    /* filename must be an absolute path */
    if (!pmix_path_is_absolute(filename)) {
        pmix_show_help("help-prte-util.txt", "relative-path", true, filename);
        return NULL;
    }

    /* if hostname is NULL, then this is a local file, so
     * the scheme can either be missing or given as "localhost"
     */
    if (NULL == hostname) {
        pmix_asprintf(&uri, "file://%s", filename);
        return uri;
    }

    /* count the number of characters that require escaping
     * in the filename
     */
    n = 0;
    for (j = 0; j < strlen(uri_reserved_path_chars) - 1; j++) {
        if (NULL != strchr(filename, uri_reserved_path_chars[j])) {
            n++;
        }
    }
    /* escape them if necessary */
    if (0 < n) {
        fn = (char *) malloc(strlen(filename) + n + 1);
        i = 0;
        for (k = 0; k < strlen(filename) - 1; k++) {
            for (j = 0; j < strlen(uri_reserved_path_chars) - 1; j++) {
                if (filename[k] == uri_reserved_path_chars[j]) {
                    fn[i] = '\\';
                    i++;
                    break;
                }
            }
            fn[i] = filename[k];
            i++;
        }
        fn[i] = '\0';
    } else {
        fn = strdup(filename);
    }

    /* construct the uri - the filename was already tested to
     * ensure it was absolute, so the required separator should
     * already be present
     */
    pmix_asprintf(&uri, "file://%s%s", hostname, fn);
    free(fn);
    return uri;
}

char *prte_filename_from_uri(const char *uri, char **hostname)
{
    char *turi;
    char *ptr, *fn, *sp;

    /* protect the incoming string */
    turi = strdup(uri);

    /* set defaults */
    fn = NULL;
    if (NULL != hostname) {
        *hostname = NULL;
    }

    /* extract the scheme */
    if (NULL == (ptr = strchr(turi, ':'))) {
        pmix_show_help("help-prte-util.txt", "malformed-uri", true, uri);
        free(turi);
        return NULL;
    }
    *ptr = '\0';
    ptr++; /* step over the new NULL */

    /* if there are three '/', then there is no
     * hostname and the file is local
     */
    if (0 == strncmp(ptr, "///", 3)) {
        /* step to the filename - as it is required
         * to be an absolute path, leave one slash
         * in the name
         */
        ptr += 2;
        fn = strdup(ptr);
    } else if (0 != strncmp(ptr, "//", 2)) {
        /* error */
        pmix_show_help("help-prte-util.txt", "malformed-uri", true, uri);
    } else {
        ptr += 2; /* step to the hostname */
        /* find the separator to the filename */
        if (NULL == (sp = strchr(ptr, '/'))) {
            pmix_show_help("help-prte-util.txt", "malformed-uri", true, uri);
        } else {
            *sp = '\0';
            if (NULL != hostname) {
                *hostname = strdup(ptr);
            }
            /* the filename is required to be an
             * absolute path, so restore the slash
             */
            *sp = '/';
            fn = strdup(sp);
        }
    }
    free(turi);
    return fn;
}

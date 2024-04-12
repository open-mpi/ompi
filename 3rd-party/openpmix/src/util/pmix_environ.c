/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "pmix_common.h"

#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "src/class/pmix_list.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"

#define PMIX_DEFAULT_TMPDIR   "/tmp"
#define PMIX_MAX_ENVAR_LENGTH 100000

/*
 * Merge two environ-like char arrays, ensuring that there are no
 * duplicate entries
 */
char **pmix_environ_merge(char **minor, char **major)
{
    int i;
    char **ret = NULL;
    char *name, *value;

    /* Check for bozo cases */

    if (NULL == major) {
        if (NULL == minor) {
            return NULL;
        } else {
            return PMIx_Argv_copy(minor);
        }
    }

    /* First, copy major */

    ret = PMIx_Argv_copy(major);

    /* Do we have something in minor? */

    if (NULL == minor) {
        return ret;
    }

    /* Now go through minor and call PMIx_Setenv(), but with overwrite
       as false */

    for (i = 0; NULL != minor[i]; ++i) {
        value = strchr(minor[i], '=');
        if (NULL == value) {
            PMIx_Setenv(minor[i], NULL, false, &ret);
        } else {

            /* strdup minor[i] in case it's a constant string */

            name = strdup(minor[i]);
            value = name + (value - minor[i]);
            *value = '\0';
            PMIx_Setenv(name, value + 1, false, &ret);
            free(name);
        }
    }

    /* All done */

    return ret;
}


pmix_status_t pmix_environ_merge_inplace(char ***orig, char **adders)
{
    pmix_status_t ret;

    assert(*orig != environ);

    for (size_t adders_idx = 0 ; adders[adders_idx] != NULL ; adders_idx++) {
        const char *adder_string = adders[adders_idx];

        /* See if the addr is in the original, because we only add.
         * Don't use setenv, to avoid having to split the string,
         * which is kind of expensive as we can't modify the strings
         * in adders (and setenv is going to do this same search loop
         * anyway).
         */
        if (NULL == pmix_getenv(adder_string, *orig)) {
            /* note that append_nosize strdup()s the added argument,
               so both orig and adders can later be freed */
            ret = PMIx_Argv_append_nosize(orig, adder_string);
            if (PMIX_SUCCESS != ret) {
                return ret;
            }
        }
    }

    return PMIX_SUCCESS;
}


char * pmix_getenv(const char *name, char **env)
{
    /* bozo case */
    if (NULL == env) {
        return NULL;
    }

    for (size_t env_idx = 0 ; env[env_idx] != NULL ; env_idx++) {
        size_t str_idx = 0;

        while (true) {
            if (name[str_idx] == '\0') {
                /* if name is a bare key, then finding an equals sign
                 * in the env string at the same position as the
                 * terminator is a match.
                 */
                if (env[env_idx][str_idx] == '=') {
                    return &(env[env_idx][str_idx + 1]);
                }

                break;
            }

            if (env[env_idx][str_idx] == '\0') {
                break;
            }

            if (name[str_idx] != env[env_idx][str_idx]) {
                break;
            }

            if (name[str_idx] == '=') {
                /* we know that env[env_idx][str_idx] is also '=',
                 * from the inequality check above, so this is a match
                 */
                return &(env[env_idx][str_idx + 1]);
            }

            str_idx++;
        }
    }

    return NULL;
}

/*
 * Portable version of unsetenv(), allowing editing of any
 * environ-like array
 */
pmix_status_t pmix_unsetenv(const char *name, char ***env)
{
    int i;
    char *compare;
    size_t len;
    bool found;

    /* Check for bozo case */

    if (NULL == *env) {
        return PMIX_SUCCESS;
    }

    /* Make something easy to compare to */

    i = asprintf(&compare, "%s=", name);
    if (NULL == compare || 0 > i) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    len = strlen(compare);

    /* Look for a duplicate that's already set in the env.  If we find
       it, free it, and then start shifting all elements down one in
       the array. */

    found = false;
    for (i = 0; (*env)[i] != NULL; ++i) {
        if (0 != strncmp((*env)[i], compare, len))
            continue;
        if (environ != *env) {
            free((*env)[i]);
        }
        for (; (*env)[i] != NULL; ++i)
            (*env)[i] = (*env)[i + 1];
        found = true;
        break;
    }
    free(compare);

    /* All done */

    return (found) ? PMIX_SUCCESS : PMIX_ERR_NOT_FOUND;
}

const char *pmix_tmp_directory(void)
{
    const char *str;

    if (NULL == (str = getenv("TMPDIR")))
        if (NULL == (str = getenv("TEMP")))
            if (NULL == (str = getenv("TMP")))
                str = PMIX_DEFAULT_TMPDIR;
    return str;
}

const char *pmix_home_directory(uid_t uid)
{
    const char *home = NULL;

    if (UINT_MAX == uid || uid == geteuid()) {
        home = getenv("HOME");
    }
    if (NULL == home) {
        struct passwd *pw = getpwuid(uid);
        home = pw->pw_dir;
    }

    return home;
}

pmix_status_t pmix_util_harvest_envars(char **incvars, char **excvars, pmix_list_t *ilist)
{
    int i, j;
    size_t len;
    pmix_kval_t *kv, *next;
    char *cs_env, *string_key;
    bool duplicate;

    /* harvest envars to pass along */
    for (j = 0; NULL != incvars[j]; j++) {
        len = strlen(incvars[j]);
        if ('*' == incvars[j][len - 1]) {
            --len;
        }
        for (i = 0; NULL != environ[i]; ++i) {
            if (0 == strncmp(environ[i], incvars[j], len)) {
                cs_env = strdup(environ[i]);
                string_key = strchr(cs_env, '=');
                if (NULL == string_key) {
                    free(cs_env);
                    return PMIX_ERR_BAD_PARAM;
                }
                *string_key = '\0';
                ++string_key;
                /* see if we already have this envar on the list */
                duplicate = false;
                PMIX_LIST_FOREACH (kv, ilist, pmix_kval_t) {
                    if (PMIX_ENVAR != kv->value->type) {
                        continue;
                    }
                    if (0 == strcmp(kv->value->data.envar.envar, cs_env)) {
                        /* if the value is the same, then ignore it */
                        if (0 != strcmp(kv->value->data.envar.value, string_key)) {
                            /* otherwise, overwrite the value */
                            free(kv->value->data.envar.value);
                            kv->value->data.envar.value = strdup(string_key);
                        }
                        duplicate = true;
                        break;
                    }
                }
                if (duplicate) {
                    free(cs_env);
                    continue;
                }
                PMIX_KVAL_NEW(kv, PMIX_SET_ENVAR);
                if (NULL == kv) {
                    free(cs_env);
                    return PMIX_ERR_NOMEM;
                }
                if (NULL == kv->value) {
                    PMIX_RELEASE(kv);
                    free(cs_env);
                    return PMIX_ERR_NOMEM;
                }
                kv->value->type = PMIX_ENVAR;
                PMIX_ENVAR_LOAD(&kv->value->data.envar, cs_env, string_key, ':');
                pmix_list_append(ilist, &kv->super);
                free(cs_env);
            }
        }
    }

    /* now check the exclusions and remove any that match */
    if (NULL != excvars) {
        for (j = 0; NULL != excvars[j]; j++) {
            len = strlen(excvars[j]);
            if ('*' == excvars[j][len - 1]) {
                --len;
            }
            PMIX_LIST_FOREACH_SAFE (kv, next, ilist, pmix_kval_t) {
                if (0 == strncmp(kv->value->data.envar.envar, excvars[j], len)) {
                    pmix_list_remove_item(ilist, &kv->super);
                    PMIX_RELEASE(kv);
                }
            }
        }
    }
    return PMIX_SUCCESS;
}

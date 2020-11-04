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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "include/pmix_common.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>

#include "src/class/pmix_list.h"
#include "src/util/printf.h"
#include "src/util/error.h"
#include "src/util/argv.h"
#include "src/util/pmix_environ.h"
#include "src/include/pmix_globals.h"

#define PMIX_DEFAULT_TMPDIR "/tmp"
#define PMIX_MAX_ENVAR_LENGTH   100000

/*
 * Merge two environ-like char arrays, ensuring that there are no
 * duplicate entires
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
            return pmix_argv_copy(minor);
        }
    }

    /* First, copy major */

    ret = pmix_argv_copy(major);

    /* Do we have something in minor? */

    if (NULL == minor) {
        return ret;
    }

    /* Now go through minor and call pmix_setenv(), but with overwrite
       as false */

    for (i = 0; NULL != minor[i]; ++i) {
        value = strchr(minor[i], '=');
        if (NULL == value) {
            pmix_setenv(minor[i], NULL, false, &ret);
        } else {

            /* strdup minor[i] in case it's a constant string */

            name = strdup(minor[i]);
            value = name + (value - minor[i]);
            *value = '\0';
            pmix_setenv(name, value + 1, false, &ret);
            free(name);
        }
    }

    /* All done */

    return ret;
}

/*
 * Portable version of setenv(), allowing editing of any environ-like
 * array
 */
 pmix_status_t pmix_setenv(const char *name, const char *value, bool overwrite,
                char ***env)
{
    int i;
    char *newvalue, *compare;
    size_t len;
    bool valid;

    /* Check the bozo case */
    if( NULL == env ) {
        return PMIX_ERR_BAD_PARAM;
    }

    if (NULL != value) {
        /* check the string for unacceptable length - i.e., ensure
         * it is NULL-terminated */
        valid = false;
        for (i=0; i < PMIX_MAX_ENVAR_LENGTH; i++) {
            if ('\0' == value[i]) {
                valid = true;
                break;
            }
        }
        if (!valid) {
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            return PMIX_ERR_BAD_PARAM;
        }
    }

    /* If this is the "environ" array, use putenv or setenv */
    if (*env == environ) {
        /* THIS IS POTENTIALLY A MEMORY LEAK!  But I am doing it
           because so that we don't violate the law of least
           astonishmet for PMIX developers (i.e., those that don't
           check the return code of pmix_setenv() and notice that we
           returned an error if you passed in the real environ) */
#if defined (HAVE_SETENV)
        if (NULL == value) {
            /* this is actually an unsetenv request */
            unsetenv(name);
        } else {
            setenv(name, value, overwrite);
        }
#else
        /* Make the new value */
        if (NULL == value) {
            i = asprintf(&newvalue, "%s=", name);
        } else {
            i = asprintf(&newvalue, "%s=%s", name, value);
        }
        if (NULL == newvalue || 0 > i) {
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        putenv(newvalue);
        /* cannot free it as putenv doesn't copy the value */
#endif
        return PMIX_SUCCESS;
    }

    /* Make the new value */
    if (NULL == value) {
        i = asprintf(&newvalue, "%s=", name);
    } else {
        i = asprintf(&newvalue, "%s=%s", name, value);
    }
    if (NULL == newvalue || 0 > i) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    if (NULL == *env) {
        i = 0;
        pmix_argv_append(&i, env, newvalue);
        free(newvalue);
        return PMIX_SUCCESS;
    }

    /* Make something easy to compare to */

    i = asprintf(&compare, "%s=", name);
    if (NULL == compare || 0 > i) {
        free(newvalue);
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    len = strlen(compare);

    /* Look for a duplicate that's already set in the env */

    for (i = 0; (*env)[i] != NULL; ++i) {
        if (0 == strncmp((*env)[i], compare, len)) {
            if (overwrite) {
                free((*env)[i]);
                (*env)[i] = newvalue;
                free(compare);
                return PMIX_SUCCESS;
            } else {
                free(compare);
                free(newvalue);
                return PMIX_EXISTS;
            }
        }
    }

    /* If we found no match, append this value */

    i = pmix_argv_count(*env);
    pmix_argv_append(&i, env, newvalue);

    /* All done */

    free(compare);
    free(newvalue);
    return PMIX_SUCCESS;
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

const char* pmix_tmp_directory(void)
{
    const char* str;

    if (NULL == (str = getenv("TMPDIR")))
        if (NULL == (str = getenv("TEMP")))
            if (NULL == (str = getenv("TMP")))
                str = PMIX_DEFAULT_TMPDIR;
    return str;
}

const char* pmix_home_directory(uid_t uid)
{
    const char *home = NULL;

    if (uid == geteuid()) {
        home = getenv("HOME");
    }
    if (NULL == home) {
        struct passwd *pw = getpwuid(uid);
        home = pw->pw_dir;
    }

    return home;
}

pmix_status_t pmix_util_harvest_envars(char **incvars, char **excvars,
                                       pmix_list_t *ilist)
{
    int i, j;
    size_t len;
    pmix_kval_t *kv, *next;
    char *cs_env, *string_key;
    bool duplicate;

    /* harvest envars to pass along */
    for (j=0; NULL != incvars[j]; j++) {
        len = strlen(incvars[j]);
        if ('*' == incvars[j][len-1]) {
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
                PMIX_LIST_FOREACH(kv, ilist, pmix_kval_t) {
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
                kv = PMIX_NEW(pmix_kval_t);
                if (NULL == kv) {
                    free(cs_env);
                    return PMIX_ERR_OUT_OF_RESOURCE;
                }
                kv->key = strdup(PMIX_SET_ENVAR);
                kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
                if (NULL == kv->value) {
                    PMIX_RELEASE(kv);
                    free(cs_env);
                    return PMIX_ERR_OUT_OF_RESOURCE;
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
        for (j=0; NULL != excvars[j]; j++) {
            len = strlen(excvars[j]);
            if ('*' == excvars[j][len-1]) {
                --len;
            }
            PMIX_LIST_FOREACH_SAFE(kv, next, ilist, pmix_kval_t) {
                if (0 == strncmp(kv->value->data.envar.envar, excvars[j], len)) {
                    pmix_list_remove_item(ilist, &kv->super);
                    PMIX_RELEASE(kv);
                }
            }
        }
    }
    return PMIX_SUCCESS;
}

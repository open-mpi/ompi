/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "support.h"
#include "opal/util/opal_environ.h"
#include "opal/util/argv.h"
#include "opal/constants.h"
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/* helpers                                                              */
/* ------------------------------------------------------------------ */

/*
 * Return the value portion of a "NAME=value" entry in an env array,
 * or NULL if not found.
 */
static const char *env_lookup(char **env, const char *name)
{
    size_t nlen;
    int i;

    if (NULL == env || NULL == name) {
        return NULL;
    }
    nlen = strlen(name);
    for (i = 0; NULL != env[i]; ++i) {
        if (0 == strncmp(env[i], name, nlen) && '=' == env[i][nlen]) {
            return env[i] + nlen + 1;
        }
    }
    return NULL;
}

/* ------------------------------------------------------------------ */
/* opal_setenv                                                          */
/* ------------------------------------------------------------------ */

static void test_setenv(void)
{
    char **env = NULL;
    int rc;
    const char *val;

    /* Add to a NULL *env (env pointer itself is non-NULL, *env is NULL) */
    rc = opal_setenv("FOO", "bar", true, &env);
    test_verify("setenv into *env==NULL: OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("setenv into *env==NULL: env non-NULL", NULL != env);
    val = env_lookup(env, "FOO");
    test_verify("setenv into *env==NULL: value stored", NULL != val);
    test_verify("setenv into *env==NULL: value is 'bar'",
                NULL != val && 0 == strcmp("bar", val));

    /* Add a second distinct variable */
    rc = opal_setenv("BAR", "qux", true, &env);
    test_verify("setenv second var: OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("setenv second var: 'qux' stored",
                0 == strcmp("qux", env_lookup(env, "BAR")));
    test_verify("setenv second var: FOO still present",
                0 == strcmp("bar", env_lookup(env, "FOO")));

    /* Overwrite existing variable (overwrite=true) */
    rc = opal_setenv("FOO", "newval", true, &env);
    test_verify("setenv overwrite=true: OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("setenv overwrite=true: value updated",
                0 == strcmp("newval", env_lookup(env, "FOO")));

    /* Attempt overwrite with overwrite=false => OPAL_EXISTS, value unchanged */
    rc = opal_setenv("FOO", "ignored", false, &env);
    test_verify("setenv overwrite=false: OPAL_EXISTS", OPAL_EXISTS == rc);
    test_verify("setenv overwrite=false: value unchanged",
                0 == strcmp("newval", env_lookup(env, "FOO")));

    /* New variable with overwrite=false -- should still append */
    rc = opal_setenv("NEWVAR", "hello", false, &env);
    test_verify("setenv new var overwrite=false: OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("setenv new var overwrite=false: value stored",
                0 == strcmp("hello", env_lookup(env, "NEWVAR")));

    /* value==NULL: stores "NAME=" */
    rc = opal_setenv("NULLVAL", NULL, true, &env);
    test_verify("setenv NULL value: OPAL_SUCCESS", OPAL_SUCCESS == rc);
    val = env_lookup(env, "NULLVAL");
    test_verify("setenv NULL value: stored", NULL != val);
    test_verify("setenv NULL value: empty string",
                NULL != val && 0 == strcmp("", val));

    /* bozo: NULL env pointer => OPAL_ERR_BAD_PARAM */
    rc = opal_setenv("X", "y", true, NULL);
    test_verify("setenv NULL env: OPAL_ERR_BAD_PARAM",
                OPAL_ERR_BAD_PARAM == rc);

    opal_argv_free(env);
}

/* ------------------------------------------------------------------ */
/* opal_unsetenv                                                        */
/* ------------------------------------------------------------------ */

static void test_unsetenv(void)
{
    char **env = NULL;
    int rc;

    /* Build a small env */
    opal_setenv("A", "1", true, &env);
    opal_setenv("B", "2", true, &env);
    opal_setenv("C", "3", true, &env);

    test_verify("unsetenv initial count == 3", 3 == opal_argv_count(env));

    /* Remove a variable that exists */
    rc = opal_unsetenv("B", &env);
    test_verify("unsetenv existing: OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("unsetenv existing: count decremented", 2 == opal_argv_count(env));
    test_verify("unsetenv existing: B gone", NULL == env_lookup(env, "B"));
    test_verify("unsetenv existing: A still there",
                0 == strcmp("1", env_lookup(env, "A")));
    test_verify("unsetenv existing: C still there",
                0 == strcmp("3", env_lookup(env, "C")));

    /* Remove a variable that does NOT exist => OPAL_ERR_NOT_FOUND */
    rc = opal_unsetenv("NOTPRESENT", &env);
    test_verify("unsetenv not found: OPAL_ERR_NOT_FOUND",
                OPAL_ERR_NOT_FOUND == rc);
    test_verify("unsetenv not found: count unchanged", 2 == opal_argv_count(env));

    /* Remove first element */
    rc = opal_unsetenv("A", &env);
    test_verify("unsetenv first: OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("unsetenv first: count == 1", 1 == opal_argv_count(env));
    test_verify("unsetenv first: A gone", NULL == env_lookup(env, "A"));

    /* Remove last element */
    rc = opal_unsetenv("C", &env);
    test_verify("unsetenv last: OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("unsetenv last: count == 0", 0 == opal_argv_count(env));

    /* unsetenv on empty env (NULL *env) => OPAL_SUCCESS */
    {
        char **empty = NULL;
        rc = opal_unsetenv("X", &empty);
        test_verify("unsetenv on NULL env: OPAL_SUCCESS", OPAL_SUCCESS == rc);
    }

    opal_argv_free(env);
}

/* ------------------------------------------------------------------ */
/* opal_environ_merge                                                   */
/* ------------------------------------------------------------------ */

static void test_environ_merge(void)
{
    char **minor;
    char **major;
    char **merged;

    /* both NULL => NULL */
    merged = opal_environ_merge(NULL, NULL);
    test_verify("merge(NULL,NULL): NULL", NULL == merged);

    /* minor NULL: result is copy of major */
    major = NULL;
    opal_setenv("X", "1", true, &major);
    opal_setenv("Y", "2", true, &major);
    merged = opal_environ_merge(NULL, major);
    test_verify("merge(NULL,major): non-NULL", NULL != merged);
    test_verify("merge(NULL,major): count == 2", 2 == opal_argv_count(merged));
    test_verify("merge(NULL,major): X=1",
                0 == strcmp("1", env_lookup(merged, "X")));
    test_verify("merge(NULL,major): Y=2",
                0 == strcmp("2", env_lookup(merged, "Y")));
    opal_argv_free(merged);
    opal_argv_free(major);

    /* major NULL: result is copy of minor */
    minor = NULL;
    opal_setenv("A", "10", true, &minor);
    opal_setenv("B", "20", true, &minor);
    merged = opal_environ_merge(minor, NULL);
    test_verify("merge(minor,NULL): non-NULL", NULL != merged);
    test_verify("merge(minor,NULL): count == 2", 2 == opal_argv_count(merged));
    test_verify("merge(minor,NULL): A=10",
                0 == strcmp("10", env_lookup(merged, "A")));
    opal_argv_free(merged);
    opal_argv_free(minor);

    /* conflict: major wins */
    minor = NULL;
    major = NULL;
    opal_setenv("VAR", "minor_val", true, &minor);
    opal_setenv("MINOR_ONLY", "only", true, &minor);
    opal_setenv("VAR", "major_val", true, &major);
    opal_setenv("MAJOR_ONLY", "monly", true, &major);

    merged = opal_environ_merge(minor, major);
    test_verify("merge conflict: non-NULL", NULL != merged);

    /* Major wins on VAR */
    test_verify("merge conflict: VAR == major_val",
                0 == strcmp("major_val", env_lookup(merged, "VAR")));

    /* Minor-only key should be present in result */
    test_verify("merge conflict: MINOR_ONLY present",
                0 == strcmp("only", env_lookup(merged, "MINOR_ONLY")));

    /* Major-only key present */
    test_verify("merge conflict: MAJOR_ONLY present",
                0 == strcmp("monly", env_lookup(merged, "MAJOR_ONLY")));

    /* Total count: VAR(1) + MINOR_ONLY(1) + MAJOR_ONLY(1) = 3 */
    test_verify("merge conflict: count == 3", 3 == opal_argv_count(merged));

    opal_argv_free(merged);
    opal_argv_free(minor);
    opal_argv_free(major);

    /* merge disjoint sets */
    minor = NULL;
    major = NULL;
    opal_setenv("M1", "a", true, &minor);
    opal_setenv("M2", "b", true, &minor);
    opal_setenv("J1", "c", true, &major);
    opal_setenv("J2", "d", true, &major);

    merged = opal_environ_merge(minor, major);
    test_verify("merge disjoint: count == 4", 4 == opal_argv_count(merged));
    test_verify("merge disjoint: M1=a",
                0 == strcmp("a", env_lookup(merged, "M1")));
    test_verify("merge disjoint: M2=b",
                0 == strcmp("b", env_lookup(merged, "M2")));
    test_verify("merge disjoint: J1=c",
                0 == strcmp("c", env_lookup(merged, "J1")));
    test_verify("merge disjoint: J2=d",
                0 == strcmp("d", env_lookup(merged, "J2")));

    opal_argv_free(merged);
    opal_argv_free(minor);
    opal_argv_free(major);
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    test_init("opal_environ");

    test_setenv();
    test_unsetenv();
    test_environ_merge();

    return test_finalize();
}

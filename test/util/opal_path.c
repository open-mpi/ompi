/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
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

/*
 * Unit tests for opal/util/path.c.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op.
 * All verification must go through test_verify().
 *
 * opal_path_nfs is covered elsewhere (test/util/opal_path_nfs.c) and
 * is intentionally omitted here.
 *
 * opal_path_access, opal_path_find, and opal_path_findv take char*
 * (non-const) for fname/path arguments.  To avoid -Wwrite-strings
 * warnings, all file-name arguments are passed as mutable char arrays.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/path.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* ------------------------------------------------------------------ */
/* helpers                                                             */
/* ------------------------------------------------------------------ */

/*
 * Create a temporary regular file (mode 0600) and return its path via
 * buf (which must be at least 64 bytes).  Returns 0 on success.
 */
static int make_temp_file(char *buf, size_t bufsz)
{
    int fd;

    snprintf(buf, bufsz, "/tmp/opal_path_test_XXXXXX");
    fd = mkstemp(buf);
    if (-1 == fd) {
        return -1;
    }
    close(fd);
    return 0;
}

/* ------------------------------------------------------------------ */
/* opal_path_is_absolute                                               */
/* ------------------------------------------------------------------ */

static void test_is_absolute(void)
{
    /* An absolute path starts with '/' on UNIX. */
    test_verify("is_absolute('/x') == true",
                true == opal_path_is_absolute("/x"));

    test_verify("is_absolute('/') == true",
                true == opal_path_is_absolute("/"));

    test_verify("is_absolute('/usr/bin/sh') == true",
                true == opal_path_is_absolute("/usr/bin/sh"));

    /* A relative path must return false. */
    test_verify("is_absolute('x') == false",
                false == opal_path_is_absolute("x"));

    test_verify("is_absolute('foo/bar') == false",
                false == opal_path_is_absolute("foo/bar"));

    test_verify("is_absolute('./foo') == false",
                false == opal_path_is_absolute("./foo"));

    /* An empty string has no leading '/'. */
    test_verify("is_absolute('') == false",
                false == opal_path_is_absolute(""));
}

/* ------------------------------------------------------------------ */
/* opal_path_access                                                    */
/* ------------------------------------------------------------------ */

static void test_path_access(void)
{
    char tmppath[64];
    char *result;

    /* Create a regular temporary file (mode 0600 -- user r/w, no x). */
    if (0 != make_temp_file(tmppath, sizeof(tmppath))) {
        test_failure("opal_path_access: could not create temp file");
        return;
    }

    /* Access an existing regular file with R_OK -- should succeed. */
    {
        char fname[64];
        snprintf(fname, sizeof(fname), "%s", tmppath);
        result = opal_path_access(fname, NULL, R_OK);
        test_verify("access existing file R_OK: non-NULL", NULL != result);
        if (NULL != result) {
            free(result);
        }
    }

    /* Access with mode 0 (existence check) -- should also succeed. */
    {
        char fname[64];
        snprintf(fname, sizeof(fname), "%s", tmppath);
        result = opal_path_access(fname, NULL, 0);
        test_verify("access existing file mode=0: non-NULL", NULL != result);
        if (NULL != result) {
            free(result);
        }
    }

    /* A non-existent path should return NULL. */
    {
        char fname[] = "/tmp/opal_path_test_does_not_exist_xyzzy_42";
        result = opal_path_access(fname, NULL, R_OK);
        test_verify("access nonexistent file: NULL", NULL == result);
    }

    /* The temp file has no execute permission (mode 0600), so X_OK
     * should return NULL.  The stat-based check in opal_path_access
     * looks at S_IXUSR, not the access(2) syscall, so it behaves
     * consistently regardless of effective UID. */
    {
        char fname[64];
        snprintf(fname, sizeof(fname), "%s", tmppath);
        result = opal_path_access(fname, NULL, X_OK);
        test_verify("access file without X_OK bit: NULL", NULL == result);
    }

    /* opal_path_access on a directory should return NULL (not a
     * regular file or symlink). */
    {
        char fname[] = "/tmp";
        result = opal_path_access(fname, NULL, R_OK);
        test_verify("access directory: NULL", NULL == result);
    }

    unlink(tmppath);
}

/* ------------------------------------------------------------------ */
/* opal_path_find                                                      */
/* ------------------------------------------------------------------ */

static void test_path_find(void)
{
    char *result;

    /* Build a pathv containing common binary directories.  We look for
     * "sh" which is universally available.  envv is NULL so environment
     * substitution is skipped. */
    {
        char fname[] = "sh";
        char *pathv[] = {"/bin", "/usr/bin", "/usr/local/bin", NULL};
        result = opal_path_find(fname, pathv, X_OK, NULL);
        test_verify("find 'sh' in /bin:/usr/bin: non-NULL", NULL != result);
        if (NULL != result) {
            /* Result must be an absolute path. */
            test_verify("find 'sh': result is absolute",
                        true == opal_path_is_absolute(result));
            free(result);
        }
    }

    /* Searching for a file that definitely does not exist must return
     * NULL. */
    {
        char fname[] = "opal_test_binary_that_does_not_exist_xyzzy";
        char *pathv[] = {"/bin", "/usr/bin", NULL};
        result = opal_path_find(fname, pathv, X_OK, NULL);
        test_verify("find nonexistent binary: NULL", NULL == result);
    }

    /* Single-element pathv with a known directory and file. */
    {
        char tmppath[64];
        if (0 == make_temp_file(tmppath, sizeof(tmppath))) {
            /* Extract directory and basename from tmppath. */
            char dir[64];
            char base[64];
            char *slash;

            snprintf(dir, sizeof(dir), "%s", tmppath);
            slash = strrchr(dir, '/');
            if (NULL != slash) {
                *slash = '\0';
                snprintf(base, sizeof(base), "%s", slash + 1);

                char *pathv_single[] = {dir, NULL};
                result = opal_path_find(base, pathv_single, R_OK, NULL);
                test_verify("find temp file in its dir: non-NULL",
                            NULL != result);
                if (NULL != result) {
                    free(result);
                }
            }
            unlink(tmppath);
        }
    }
}

/* ------------------------------------------------------------------ */
/* opal_path_findv                                                     */
/* ------------------------------------------------------------------ */

static void test_path_findv(void)
{
    char *result;

    /* Build an envv that contains PATH=/bin:/usr/bin.  opal_path_findv
     * extracts PATH from envv.  We search for "sh" with X_OK. */
    {
        char fname[] = "sh";
        char pathenv[] = "PATH=/bin:/usr/bin:/usr/local/bin";
        char *envv[] = {pathenv, NULL};
        result = opal_path_findv(fname, X_OK, envv, NULL);
        test_verify("findv 'sh' via PATH envv: non-NULL", NULL != result);
        if (NULL != result) {
            test_verify("findv 'sh': result is absolute",
                        true == opal_path_is_absolute(result));
            free(result);
        }
    }

    /* Non-existent binary must return NULL. */
    {
        char fname[] = "opal_test_binary_that_does_not_exist_xyzzy";
        char pathenv[] = "PATH=/bin:/usr/bin";
        char *envv[] = {pathenv, NULL};
        result = opal_path_findv(fname, X_OK, envv, NULL);
        test_verify("findv nonexistent binary: NULL", NULL == result);
    }

    /* envv=NULL and no system PATH: no binaries should be found in
     * an environment where PATH truly contains nothing.  We pass a
     * custom envv with an empty PATH to force a NULL result. */
    {
        char fname[] = "sh";
        char pathenv[] = "PATH=";
        char *envv[] = {pathenv, NULL};
        result = opal_path_findv(fname, X_OK, envv, NULL);
        /* With an empty PATH, "sh" cannot be found. */
        test_verify("findv 'sh' with empty PATH: NULL", NULL == result);
    }
}

/* ------------------------------------------------------------------ */
/* opal_find_absolute_path                                             */
/* ------------------------------------------------------------------ */

static void test_find_absolute_path(void)
{
    char *result;

    /* An already-absolute path to a real binary should return the
     * canonical path (via realpath).  The result must be non-NULL and
     * absolute. */
    {
        char input[] = "/bin/sh";
        result = opal_find_absolute_path(input);
        test_verify("find_absolute_path('/bin/sh'): non-NULL",
                    NULL != result);
        if (NULL != result) {
            test_verify("find_absolute_path('/bin/sh'): result is absolute",
                        true == opal_path_is_absolute(result));
            free(result);
        }
    }

    /* A path that does not exist should return NULL. */
    {
        char input[] = "/bin/opal_test_binary_does_not_exist_xyzzy";
        result = opal_find_absolute_path(input);
        test_verify("find_absolute_path nonexistent: NULL", NULL == result);
    }

    /* A relative name that is present in $PATH (using the real environment)
     * should be found.  We ask for "sh" which is always in PATH. */
    {
        char input[] = "sh";
        result = opal_find_absolute_path(input);
        test_verify("find_absolute_path('sh') via PATH: non-NULL",
                    NULL != result);
        if (NULL != result) {
            test_verify("find_absolute_path('sh'): result is absolute",
                        true == opal_path_is_absolute(result));
            free(result);
        }
    }
}

/* ------------------------------------------------------------------ */
/* opal_path_df                                                        */
/* ------------------------------------------------------------------ */

static void test_path_df(void)
{
    uint64_t avail;
    int rc;

    /* Querying '/' must succeed. */
    avail = 0;
    rc = opal_path_df("/", &avail);
    test_verify("path_df('/') returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);
    /* avail can legitimately be 0 on full filesystems; we just verify
     * the call succeeded without requiring a particular value. */

    /* NULL path must return OPAL_ERROR. */
    avail = 0;
    rc = opal_path_df(NULL, &avail);
    test_verify("path_df(NULL, &avail) returns OPAL_ERROR",
                OPAL_ERROR == rc);

    /* NULL out_avail must return OPAL_ERROR. */
    rc = opal_path_df("/", NULL);
    test_verify("path_df('/', NULL) returns OPAL_ERROR",
                OPAL_ERROR == rc);

    /* Query /tmp -- must succeed on any POSIX system. */
    avail = 0;
    rc = opal_path_df("/tmp", &avail);
    test_verify("path_df('/tmp') returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);

    /* A path that does not exist should return OPAL_ERROR. */
    avail = 0;
    rc = opal_path_df("/opal_path_test_nonexistent_dir_xyzzy", &avail);
    test_verify("path_df(nonexistent) returns OPAL_ERROR",
                OPAL_ERROR == rc);
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int r;

    opal_init_util(&argc, &argv);
    test_init("opal_path");

    test_is_absolute();
    test_path_access();
    test_path_find();
    test_path_findv();
    test_find_absolute_path();
    test_path_df();

    r = test_finalize();
    opal_finalize_util();
    return r;
}

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

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif

#include "support.h"
#include "opal/util/opal_getcwd.h"
#include "opal/constants.h"

int main(int argc, char *argv[])
{
    char buf[OPAL_PATH_MAX];
    char libc_cwd[OPAL_PATH_MAX];
    int rc;

    test_init("opal_getcwd");

    /* ================================================================
     * Get the ground-truth cwd from libc for comparison.
     * Unset $PWD first so opal_getcwd() always falls back to getcwd()
     * on the success path, giving us a deterministic string to compare.
     * ================================================================ */
    unsetenv("PWD");

    if (NULL == getcwd(libc_cwd, sizeof(libc_cwd))) {
        /* Cannot determine cwd; skip the comparison tests. */
        test_verify("libc getcwd() baseline succeeded", 0 /* always fail: no cwd */);
        return test_finalize();
    }

    /* ================================================================
     * NULL buf -> OPAL_ERR_BAD_PARAM
     * ================================================================ */
    rc = opal_getcwd(NULL, sizeof(buf));
    test_verify("NULL buf: returns OPAL_ERR_BAD_PARAM", OPAL_ERR_BAD_PARAM == rc);

    /* ================================================================
     * size > INT_MAX -> OPAL_ERR_BAD_PARAM
     * ================================================================ */
    rc = opal_getcwd(buf, (size_t) INT_MAX + 1);
    test_verify("size>INT_MAX: returns OPAL_ERR_BAD_PARAM", OPAL_ERR_BAD_PARAM == rc);

    /* ================================================================
     * Normal success: $PWD is unset so opal_getcwd falls back to
     * getcwd() result.
     * ================================================================ */
    memset(buf, 0, sizeof(buf));
    rc = opal_getcwd(buf, sizeof(buf));
    test_verify("normal success: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("normal success: buf matches getcwd()",
                0 == strcmp(buf, libc_cwd));

    /* ================================================================
     * $PWD is set to the same value as getcwd() -> should succeed and
     * return that same value.
     * ================================================================ */
    setenv("PWD", libc_cwd, 1 /*overwrite*/);
    memset(buf, 0, sizeof(buf));
    rc = opal_getcwd(buf, sizeof(buf));
    test_verify("PWD==cwd: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("PWD==cwd: buf matches libc cwd", 0 == strcmp(buf, libc_cwd));
    unsetenv("PWD");

    /* ================================================================
     * Buffer too small -> OPAL_ERR_TEMP_OUT_OF_RESOURCE.
     * Provide a 1-byte buffer.  strlen(cwd) > 1 for any real directory.
     * The function falls back to writing (part of) the basename.
     * ================================================================ */
    {
        char tiny[2];
        memset(tiny, 0xFF, sizeof(tiny));
        rc = opal_getcwd(tiny, sizeof(tiny));
        test_verify("tiny buf: returns OPAL_ERR_TEMP_OUT_OF_RESOURCE",
                    OPAL_ERR_TEMP_OUT_OF_RESOURCE == rc);
        /* buf must still be NUL-terminated within the provided size */
        test_verify("tiny buf: NUL-terminated", '\0' == tiny[sizeof(tiny) - 1]);
    }

    /* ================================================================
     * $PWD set to a stale / non-existent path -> opal_getcwd must fall
     * back to getcwd() and still return OPAL_SUCCESS.
     * ================================================================ */
    setenv("PWD", "/this/path/does/not/exist/at/all/9z8y7x6w", 1);
    memset(buf, 0, sizeof(buf));
    rc = opal_getcwd(buf, sizeof(buf));
    test_verify("stale PWD: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("stale PWD: buf matches getcwd()", 0 == strcmp(buf, libc_cwd));
    unsetenv("PWD");

    /*
     * Boundary: a buffer of exactly strlen(cwd) bytes cannot hold the
     * cwd plus its NUL terminator.  opal_getcwd() must report
     * OPAL_ERR_TEMP_OUT_OF_RESOURCE rather than silently truncating and
     * (incorrectly) returning OPAL_SUCCESS.
     */
    {
        size_t exact = strlen(libc_cwd);
        char small[OPAL_PATH_MAX];
        memset(small, 0, sizeof(small));
        rc = opal_getcwd(small, exact);
        test_verify("buffer == strlen(cwd): reports OUT_OF_RESOURCE",
                    OPAL_ERR_TEMP_OUT_OF_RESOURCE == rc);
    }

    return test_finalize();
}

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
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
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

#include "support.h"
#include "opal/util/basename.h"
#include "opal/constants.h"

/* Helper that tests opal_basename and frees the result. */
static void check_basename(const char *input, const char *expected)
{
    char *result = opal_basename(input);
    char msg[256];

    if (NULL == expected) {
        snprintf(msg, sizeof(msg),
                 "basename(NULL): expected NULL, got '%s'",
                 result ? result : "(null)");
        test_verify(msg, NULL == result);
        /* result should already be NULL here, but free defensively */
        if (NULL != result) {
            free(result);
        }
        return;
    }

    snprintf(msg, sizeof(msg),
             "basename(\"%s\"): expected \"%s\", got \"%s\"",
             input ? input : "(null)", expected,
             result ? result : "(null)");
    test_verify(msg, NULL != result && 0 == strcmp(result, expected));
    if (NULL != result) {
        free(result);
    }
}

/* Helper that tests opal_dirname and frees the result. */
static void check_dirname(const char *input, const char *expected)
{
    char *result = opal_dirname(input);
    char msg[256];

    snprintf(msg, sizeof(msg),
             "dirname(\"%s\"): expected \"%s\", got \"%s\"",
             input ? input : "(null)", expected,
             result ? result : "(null)");
    test_verify(msg, NULL != result && 0 == strcmp(result, expected));
    if (NULL != result) {
        free(result);
    }
}

int main(int argc, char *argv[])
{
    test_init("opal_basename_dirname");

    /* ================================================================
     * opal_basename() tests
     * ================================================================ */

    /* NULL input -> NULL return */
    check_basename(NULL, NULL);

    /* Empty string -> empty string */
    check_basename("", "");

    /* No slash -> entire string */
    check_basename("foo.txt", "foo.txt");
    check_basename("foo", "foo");

    /* Absolute paths */
    check_basename("/foo/bar/baz", "baz");
    check_basename("/yow.c", "yow.c");

    /* Root: "/" -> "/" */
    check_basename("/", "/");

    /* Trailing slash is stripped */
    check_basename("foo.txt/", "foo.txt");
    check_basename("/foo/bar/baz/", "baz");
    check_basename("/yow.c/", "yow.c");

    /* Double trailing slash */
    check_basename("//", "/");

    /* Multiple trailing slashes on a path */
    check_basename("/foo/bar///", "bar");

    /* Single component with trailing slash */
    check_basename("onlyone/", "onlyone");

    /* Dot and dot-dot */
    check_basename(".", ".");
    check_basename("..", "..");

    /* ================================================================
     * opal_dirname() tests
     *
     * NOTE: The header's documentation examples for opal_dirname() are
     * incorrect -- they list "foo.txt" -> "foo.txt" and "/" -> "", which
     * are basename examples, not dirname. We assert POSIX dirname(3)
     * behavior, which is the authoritative reference and matches the
     * libc-delegating implementation used on systems that have dirname().
     * ================================================================ */

    /* No slash -> "." (POSIX dirname of a bare filename) */
    check_dirname("foo.txt", ".");
    check_dirname("foo", ".");

    /* Absolute paths */
    check_dirname("/foo/bar/baz", "/foo/bar");
    check_dirname("/yow.c", "/");

    /* Root: "/" -> "/" (POSIX) */
    check_dirname("/", "/");

    /* Trailing slash: POSIX dirname treats trailing slashes as if stripped */
    check_dirname("/foo/bar/baz/", "/foo/bar");
    check_dirname("/usr/", "/");

    /* Two-level */
    check_dirname("/a/b", "/a");

    /* Single component absolute */
    check_dirname("/single", "/");

    /* Dot */
    check_dirname(".", ".");

    /* opal_dirname(NULL) is guarded (mirroring opal_basename) and
       returns NULL rather than dereferencing the NULL pointer. */
    {
        char *d = opal_dirname(NULL);
        test_verify("dirname(NULL) returns NULL", NULL == d);
        if (NULL != d) {
            free(d);
        }
    }

    return test_finalize();
}

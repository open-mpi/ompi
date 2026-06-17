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
 * Unit tests for opal/util/error.c.
 *
 * Tests: opal_strerror, opal_strerror_r, opal_perror,
 *        opal_error_register.
 *
 * Note: the library is compiled with -DNDEBUG, so assert() is a no-op.
 * All verification must go through test_verify().
 *
 * Implementation note on opal_strerror(OPAL_SUCCESS):
 *   The OPAL error table is registered with err_base = OPAL_ERR_BASE (0)
 *   and err_max = OPAL_ERR_MAX (-100).  opal_strerror_int() only invokes a
 *   converter when (errnum < err_base && err_max < errnum), i.e. strictly
 *   between -100 and 0.  OPAL_SUCCESS == 0 fails that range (0 < 0 is
 *   false), so no converter runs.  opal_strerror_int() therefore defaults
 *   to OPAL_ERROR for such codes (this is the fix in opal/util/error.c;
 *   it previously defaulted to OPAL_SUCCESS and left *str == NULL).  Since
 *   the result is now != OPAL_SUCCESS, opal_strerror() takes its
 *   unknown-error path and returns a non-NULL "Unknown error: 0" string
 *   rather than NULL.  The tests below assert that non-NULL result.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/error.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/* helpers                                                             */
/* ------------------------------------------------------------------ */

/* Converter used by test_error_register(). */
#define MY_ERR_BASE (-2000)
#define MY_ERR_FOO  (-2001)
#define MY_ERR_BAR  (-2002)
#define MY_ERR_MAX  (-2010)

static int my_converter(int errnum, const char **str)
{
    switch (errnum) {
    case MY_ERR_FOO:
        *str = "custom foo error";
        return OPAL_SUCCESS;
    case MY_ERR_BAR:
        *str = "custom bar error";
        return OPAL_SUCCESS;
    default:
        *str = NULL;
        return OPAL_ERROR;
    }
}

/* ------------------------------------------------------------------ */
/* opal_strerror                                                       */
/* ------------------------------------------------------------------ */

static void test_strerror(void)
{
    const char *s;

    /*
     * OPAL_SUCCESS (0) is matched by no registered converter (the OPAL
     * converter covers the strictly-between (-100, 0) range), so it is
     * treated as an unknown code and opal_strerror() returns a non-NULL
     * "Unknown error: 0" string.  (It does not map to "Success": the
     * opal_err2str converter is never reached for 0.)  The key contract
     * is that the result is non-NULL so that callers can safely %s it.
     */
    s = opal_strerror(OPAL_SUCCESS);
    test_verify("strerror(OPAL_SUCCESS): non-NULL", NULL != s);

    /* OPAL_ERROR = -1, within range (-100,-1]: should return a non-NULL,
     * non-empty string. */
    s = opal_strerror(OPAL_ERROR);
    test_verify("strerror(OPAL_ERROR): non-NULL", NULL != s);
    if (NULL != s) {
        test_verify("strerror(OPAL_ERROR): non-empty", '\0' != s[0]);
        test_verify("strerror(OPAL_ERROR): equals 'Error'",
                    0 == strcmp("Error", s));
    }

    /* OPAL_ERR_OUT_OF_RESOURCE = -2 */
    s = opal_strerror(OPAL_ERR_OUT_OF_RESOURCE);
    test_verify("strerror(OPAL_ERR_OUT_OF_RESOURCE): non-NULL", NULL != s);
    if (NULL != s) {
        test_verify("strerror(OPAL_ERR_OUT_OF_RESOURCE): equals 'Out of resource'",
                    0 == strcmp("Out of resource", s));
    }

    /* OPAL_ERR_NOT_FOUND = -13 */
    s = opal_strerror(OPAL_ERR_NOT_FOUND);
    test_verify("strerror(OPAL_ERR_NOT_FOUND): non-NULL", NULL != s);
    if (NULL != s) {
        test_verify("strerror(OPAL_ERR_NOT_FOUND): equals 'Not found'",
                    0 == strcmp("Not found", s));
    }

    /* OPAL_ERR_BAD_PARAM = -5 */
    s = opal_strerror(OPAL_ERR_BAD_PARAM);
    test_verify("strerror(OPAL_ERR_BAD_PARAM): non-NULL", NULL != s);
    if (NULL != s) {
        test_verify("strerror(OPAL_ERR_BAD_PARAM): equals 'Bad parameter'",
                    0 == strcmp("Bad parameter", s));
    }

    /* Different codes must produce different strings. */
    {
        const char *s_error      = opal_strerror(OPAL_ERROR);
        const char *s_not_found  = opal_strerror(OPAL_ERR_NOT_FOUND);
        const char *s_bad_param  = opal_strerror(OPAL_ERR_BAD_PARAM);
        test_verify("strerror: ERROR != NOT_FOUND",
                    0 != strcmp(s_error, s_not_found));
        test_verify("strerror: ERROR != BAD_PARAM",
                    0 != strcmp(s_error, s_bad_param));
    }

    /* OPAL_ERR_IN_ERRNO: must return strerror(errno) for current errno.
     * The exact string is system-dependent; just verify non-NULL. */
    errno = EINVAL;
    s = opal_strerror(OPAL_ERR_IN_ERRNO);
    test_verify("strerror(OPAL_ERR_IN_ERRNO): non-NULL", NULL != s);

    /* A completely unknown code (well outside any registered range)
     * should return some non-NULL "Unknown error" string. */
    s = opal_strerror(OPAL_ERR_MAX - 500);
    test_verify("strerror(unknown code): non-NULL", NULL != s);
}

/* ------------------------------------------------------------------ */
/* opal_strerror_r                                                     */
/* ------------------------------------------------------------------ */

static void test_strerror_r(void)
{
    char buf[256];
    int rc;

    /* OPAL_ERROR must fill the buffer with a non-empty string and
     * return OPAL_SUCCESS. */
    buf[0] = '\0';
    rc = opal_strerror_r(OPAL_ERROR, buf, sizeof(buf));
    test_verify("strerror_r(OPAL_ERROR): returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);
    test_verify("strerror_r(OPAL_ERROR): buffer non-empty",
                '\0' != buf[0]);
    test_verify("strerror_r(OPAL_ERROR): buffer equals 'Error'",
                0 == strcmp("Error", buf));

    /* OPAL_ERR_NOT_FOUND */
    buf[0] = '\0';
    rc = opal_strerror_r(OPAL_ERR_NOT_FOUND, buf, sizeof(buf));
    test_verify("strerror_r(OPAL_ERR_NOT_FOUND): returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);
    test_verify("strerror_r(OPAL_ERR_NOT_FOUND): equals 'Not found'",
                0 == strcmp("Not found", buf));

    /* OPAL_ERR_BAD_PARAM */
    buf[0] = '\0';
    rc = opal_strerror_r(OPAL_ERR_BAD_PARAM, buf, sizeof(buf));
    test_verify("strerror_r(OPAL_ERR_BAD_PARAM): returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);
    test_verify("strerror_r(OPAL_ERR_BAD_PARAM): equals 'Bad parameter'",
                0 == strcmp("Bad parameter", buf));

    /* OPAL_ERR_IN_ERRNO: should copy strerror(errno) into buf. */
    errno = EINVAL;
    buf[0] = '\0';
    rc = opal_strerror_r(OPAL_ERR_IN_ERRNO, buf, sizeof(buf));
    test_verify("strerror_r(OPAL_ERR_IN_ERRNO): returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);
    test_verify("strerror_r(OPAL_ERR_IN_ERRNO): buffer non-empty",
                '\0' != buf[0]);

    /* Unknown code: should still fill the buffer and return OPAL_SUCCESS. */
    buf[0] = '\0';
    rc = opal_strerror_r(OPAL_ERR_MAX - 500, buf, sizeof(buf));
    test_verify("strerror_r(unknown): returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);
    test_verify("strerror_r(unknown): buffer non-empty",
                '\0' != buf[0]);

    /* Multiple calls must not overwrite each other's result (since each
     * call writes into a caller-supplied buffer). */
    {
        char buf1[256];
        char buf2[256];
        opal_strerror_r(OPAL_ERROR,         buf1, sizeof(buf1));
        opal_strerror_r(OPAL_ERR_NOT_FOUND, buf2, sizeof(buf2));
        test_verify("strerror_r: concurrent buffers independent",
                    0 != strcmp(buf1, buf2));
    }
}

/* ------------------------------------------------------------------ */
/* opal_perror                                                         */
/* ------------------------------------------------------------------ */

static void test_perror(void)
{
    /*
     * opal_perror() writes to stderr; we cannot easily capture that
     * output here, so we just verify it does not crash for representative
     * codes.  A visual inspection of stderr (when running the test) can
     * be used as a secondary check.
     */
    opal_perror(OPAL_SUCCESS,        "perror OPAL_SUCCESS");
    opal_perror(OPAL_ERROR,          "perror OPAL_ERROR");
    opal_perror(OPAL_ERR_NOT_FOUND,  "perror OPAL_ERR_NOT_FOUND");
    opal_perror(OPAL_ERR_BAD_PARAM,  NULL);   /* NULL msg: no prefix */
    errno = EINVAL;
    opal_perror(OPAL_ERR_IN_ERRNO,   "perror OPAL_ERR_IN_ERRNO");
    opal_perror(OPAL_ERR_MAX - 500,  "perror unknown code");
    test_verify("perror: did not crash", 1);
}

/* ------------------------------------------------------------------ */
/* opal_error_register                                                 */
/* ------------------------------------------------------------------ */

static void test_error_register(void)
{
    int rc;
    const char *s;
    char buf[256];

    /*
     * Register a custom error-code range.  The range must be strictly
     * wider than any code we intend to look up:
     *   err_base = MY_ERR_BASE (-2000) -- exclusive upper bound
     *   err_max  = MY_ERR_MAX  (-2010) -- exclusive lower bound
     *
     * opal_strerror_int fires for codes where:
     *   errnum < err_base  AND  err_max < errnum
     * i.e. -2010 < errnum < -2000, so MY_ERR_FOO (-2001) and
     * MY_ERR_BAR (-2002) are both in range.
     */
    rc = opal_error_register("MYTEST", MY_ERR_BASE, MY_ERR_MAX, my_converter);
    test_verify("error_register: returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    /* opal_strerror for MY_ERR_FOO must return the custom string. */
    s = opal_strerror(MY_ERR_FOO);
    test_verify("strerror(MY_ERR_FOO): non-NULL", NULL != s);
    if (NULL != s) {
        test_verify("strerror(MY_ERR_FOO): equals 'custom foo error'",
                    0 == strcmp("custom foo error", s));
    }

    /* opal_strerror for MY_ERR_BAR must return the custom string. */
    s = opal_strerror(MY_ERR_BAR);
    test_verify("strerror(MY_ERR_BAR): non-NULL", NULL != s);
    if (NULL != s) {
        test_verify("strerror(MY_ERR_BAR): equals 'custom bar error'",
                    0 == strcmp("custom bar error", s));
    }

    /* opal_strerror_r for MY_ERR_FOO. */
    buf[0] = '\0';
    rc = opal_strerror_r(MY_ERR_FOO, buf, sizeof(buf));
    test_verify("strerror_r(MY_ERR_FOO): OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("strerror_r(MY_ERR_FOO): equals 'custom foo error'",
                0 == strcmp("custom foo error", buf));

    /* opal_perror for a custom code must not crash. */
    opal_perror(MY_ERR_FOO, "perror custom MY_ERR_FOO");
    test_verify("perror custom code: did not crash", 1);

    /* Re-registering the same range+project must succeed (update the
     * converter) and return OPAL_SUCCESS. */
    rc = opal_error_register("MYTEST", MY_ERR_BASE, MY_ERR_MAX, my_converter);
    test_verify("error_register re-register: OPAL_SUCCESS", OPAL_SUCCESS == rc);
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int r;

    opal_init_util(&argc, &argv);
    test_init("opal_error");

    test_strerror();
    test_strerror_r();
    test_perror();
    test_error_register();

    r = test_finalize();
    opal_finalize_util();
    return r;
}

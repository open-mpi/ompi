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

/*
 * Unit tests for opal_getpagesize() and opal_util_init_sys_limits().
 *
 * opal_getpagesize() wraps getpagesize() / sysconf(_SC_PAGESIZE) and caches
 * the result.  A valid page size must be:
 *   - positive
 *   - a power of two (every real system page size is a power of two)
 *   - equal to the value returned by sysconf(_SC_PAGESIZE)
 *
 * opal_util_init_sys_limits() reads opal_set_max_sys_limits (a module-level
 * string from opal_params).  With the default configuration from
 * opal_init_util(), opal_set_max_sys_limits is NULL, so the function returns
 * OPAL_SUCCESS immediately without touching *errmsg.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/sys_limits.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <unistd.h>
#include <stdlib.h>

/* ------------------------------------------------------------------ */
/* helpers                                                             */
/* ------------------------------------------------------------------ */

/* Returns 1 if n is a positive power of two, 0 otherwise. */
static int is_power_of_two(int n)
{
    if (n <= 0) {
        return 0;
    }
    return 0 == (n & (n - 1));
}

static void test_getpagesize(void)
{
    int ps;
    long sc_ps;

    ps = opal_getpagesize();
    test_verify("opal_getpagesize() returns positive value", ps > 0);
    test_verify("opal_getpagesize() returns a power of two", is_power_of_two(ps));

    /* Compare to sysconf(_SC_PAGESIZE) which is the authoritative source
     * on POSIX systems.  sysconf() may legally return -1 (unsupported or
     * error), in which case there is nothing to compare against and we
     * simply skip this check rather than failing a correct
     * opal_getpagesize(). */
    sc_ps = sysconf(_SC_PAGESIZE);
    if (sc_ps > 0) {
        test_verify("opal_getpagesize() matches sysconf(_SC_PAGESIZE)",
                    (long) ps == sc_ps);
    } else {
        test_comment("sysconf(_SC_PAGESIZE) unavailable; skipping comparison");
    }

    /* Second call must return the same (cached) value. */
    test_verify("opal_getpagesize() is idempotent", ps == opal_getpagesize());
}

static void test_init_sys_limits_default(void)
{
    /*
     * With opal_set_max_sys_limits == NULL (the default set by opal_init_util),
     * opal_util_init_sys_limits() returns OPAL_SUCCESS immediately.
     * The errmsg pointer must not be written in this case; we pass NULL to
     * confirm the function doesn't dereference it when there is nothing to do.
     */
    int rc = opal_util_init_sys_limits(NULL);
    test_verify("opal_util_init_sys_limits(NULL limits) returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);
}

static void test_init_sys_limits_errmsg_untouched(void)
{
    /*
     * Pass a valid errmsg pointer initialised to NULL; confirm it stays NULL
     * when opal_set_max_sys_limits is NULL (nothing to process).
     */
    char *errmsg = NULL;
    int rc = opal_util_init_sys_limits(&errmsg);
    test_verify("opal_util_init_sys_limits: rc == OPAL_SUCCESS", OPAL_SUCCESS == rc);
    test_verify("opal_util_init_sys_limits: errmsg untouched when no limits set",
                NULL == errmsg);
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int r;

    test_init("opal_sys_limits");

    /* test_init_sys_limits_default() passes a NULL errmsg and relies on
       opal_set_max_sys_limits being NULL (its default) so the call is a
       no-op.  An inherited OMPI_MCA_opal_set_max_sys_limits would make it
       non-NULL, taking the processing path (which could dereference the NULL
       errmsg).  Clear it so the default-path assumption holds. */
    unsetenv("OMPI_MCA_opal_set_max_sys_limits");

    opal_init_util(&argc, &argv);

    test_getpagesize();
    test_init_sys_limits_default();
    test_init_sys_limits_errmsg_untouched();

    r = test_finalize();
    opal_finalize_util();
    return r;
}

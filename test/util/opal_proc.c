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
 * Unit tests for the opal proc / local-proc identity API:
 *   opal_proc_local_get()  -- return the current local proc pointer
 *   opal_proc_local_set()  -- replace the local proc pointer
 *   opal_proc_set_name()   -- write a process name into the default local proc
 *
 * Behaviour from proc.c:
 *   - A static opal_local_proc is always present; opal_proc_local_get()
 *     returns &opal_local_proc by default (never NULL).
 *   - opal_proc_local_set(p) retains p and releases the old pointer (unless
 *     the old pointer is the built-in &opal_local_proc).
 *   - opal_proc_local_set(NULL) restores the built-in default.
 *   - opal_proc_set_name(name) copies name into the default local proc's
 *     proc_name field using memcpy.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/proc.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <string.h>

/* ------------------------------------------------------------------ */
/* test_local_get_not_null                                            */
/*                                                                     */
/* opal_proc_local_get() must return non-NULL both before and after   */
/* opal_init_util.  (We call it here, after init_util.)               */
/* ------------------------------------------------------------------ */
static void test_local_get_not_null(void)
{
    opal_proc_t *p = opal_proc_local_get();
    test_verify("opal_proc_local_get() is non-NULL after init_util", NULL != p);
}

/* ------------------------------------------------------------------ */
/* test_local_set_get_roundtrip                                       */
/*                                                                     */
/* Create a new opal_proc_t, install it as the local proc, verify     */
/* get returns the new pointer, then restore the default.             */
/*                                                                     */
/* Refcount protocol (from proc.c):                                   */
/*   1. OBJ_NEW(proc_t)  -- refcount = 1 (our ref)                   */
/*   2. local_set(p)     -- retains p   (refcount = 2)               */
/*   3. local_set(NULL)  -- releases p  (refcount = 1)               */
/*   4. OBJ_RELEASE(p)   -- refcount = 0 -> freed                    */
/* ------------------------------------------------------------------ */
static void test_local_set_get_roundtrip(void)
{
    opal_proc_t *original;
    opal_proc_t *p;
    opal_proc_t *got;
    int rc;

    original = opal_proc_local_get();

    p = OBJ_NEW(opal_proc_t);
    test_verify("OBJ_NEW(opal_proc_t) returns non-NULL", NULL != p);
    if (NULL == p) {
        return;
    }

    rc = opal_proc_local_set(p);
    test_verify("opal_proc_local_set(p) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    got = opal_proc_local_get();
    test_verify("opal_proc_local_get() returns the newly set proc", p == got);

    /* Restore default: set(NULL) => reverts to &opal_local_proc */
    rc = opal_proc_local_set(NULL);
    test_verify("opal_proc_local_set(NULL) returns OPAL_SUCCESS", OPAL_SUCCESS == rc);

    got = opal_proc_local_get();
    test_verify("after set(NULL), get() returns original default",
                original == got);

    /* Release our own reference (the set/unset already handled the lib's ref). */
    OBJ_RELEASE(p);
}

/* ------------------------------------------------------------------ */
/* test_proc_set_name                                                  */
/*                                                                     */
/* opal_proc_set_name() copies a process name struct into the default */
/* local proc.  We write a recognisable jobid/vpid pair and read it   */
/* back through opal_proc_local_get()->proc_name.                     */
/* ------------------------------------------------------------------ */
static void test_proc_set_name(void)
{
    opal_process_name_t name;
    opal_proc_t *lp;

    /* Use values well within OPAL_JOBID_MAX / OPAL_VPID_MAX. */
    name.jobid = 42;
    name.vpid  = 7;

    /* Make sure we are operating on the default local proc (no custom set). */
    opal_proc_local_set(NULL);

    opal_proc_set_name(&name);

    lp = opal_proc_local_get();
    test_verify("opal_proc_set_name: local proc non-NULL", NULL != lp);
    if (NULL != lp) {
        test_verify("opal_proc_set_name: jobid round-trips",
                    (opal_jobid_t) 42 == lp->proc_name.jobid);
        test_verify("opal_proc_set_name: vpid round-trips",
                    (opal_vpid_t) 7 == lp->proc_name.vpid);
    }
}

/* ------------------------------------------------------------------ */
/* test_set_same_pointer_is_noop                                      */
/*                                                                     */
/* Setting the local proc to the current value (same pointer) must    */
/* not double-retain or crash; it must still return OPAL_SUCCESS.     */
/* ------------------------------------------------------------------ */
static void test_set_same_pointer_is_noop(void)
{
    opal_proc_t *cur = opal_proc_local_get();
    int rc = opal_proc_local_set(cur);
    test_verify("opal_proc_local_set(same pointer) returns OPAL_SUCCESS",
                OPAL_SUCCESS == rc);
    test_verify("opal_proc_local_get() unchanged after same-pointer set",
                cur == opal_proc_local_get());
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int r;

    test_init("opal_proc");

    opal_init_util(&argc, &argv);

    test_local_get_not_null();
    test_local_set_get_roundtrip();
    test_proc_set_name();
    test_set_same_pointer_is_noop();

    r = test_finalize();
    opal_finalize_util();
    return r;
}

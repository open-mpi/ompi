/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * White-box unit test for performance variable handle allocation in the
 * OPAL mca_base_pvar code.  Links libopen-pal directly, with no MPI/MPI_T
 * layer.  Runs as a 'make check' program.
 *
 * Allocating a handle for a continuous counter reads the counter's
 * starting value.  When that read fails, the allocation must fail cleanly:
 * release the handle (which is not yet on the variable's list of bound
 * handles or on the session's list), and not hand the freed handle back to
 * the caller.
 */

#include "opal_config.h"

#include <stdio.h>

#include "opal/mca/base/mca_base_pvar.h"
#include "opal/runtime/opal.h"

static int failures = 0;

static void check(const char *what, int cond)
{
    if (cond) {
        printf("PASS: %s\n", what);
    } else {
        ++failures;
        printf("FAIL: %s\n", what);
    }
}

/* Whether the counter's data is available; get_value fails while it is
   not, as a provider's callback does when its data has gone away. */
static int data_available = 0;
static unsigned long counter_value = 42;

static int get_value(const mca_base_pvar_t *pvar __opal_attribute_unused__, void *value,
                     void *obj __opal_attribute_unused__)
{
    if (!data_available) {
        return OPAL_ERR_NOT_AVAILABLE;
    }
    *(unsigned long *) value = counter_value;
    return OPAL_SUCCESS;
}

int main(int argc, char *argv[])
{
    mca_base_pvar_session_t *session;
    mca_base_pvar_handle_t *handle;
    mca_base_pvar_t *pvar;
    int index, count, rc;

    opal_init(&argc, &argv);

    index = mca_base_pvar_register("opal", "test", "pvar", "counter", "a test counter",
                                   OPAL_INFO_LVL_4, MCA_BASE_PVAR_CLASS_COUNTER,
                                   MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL,
                                   MCA_BASE_VAR_BIND_NO_OBJECT,
                                   MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                   get_value, NULL, NULL, NULL);
    check("pvar_register succeeds", index >= 0);
    rc = mca_base_pvar_get(index, (const mca_base_pvar_t **) &pvar);
    check("pvar_get succeeds", OPAL_SUCCESS == rc);
    if (0 > index || OPAL_SUCCESS != rc) {
        opal_finalize();
        return 1;
    }

    session = OBJ_NEW(mca_base_pvar_session_t);

    /* The starting value cannot be read: the allocation fails, and the
       caller does not get the released handle back.  Releasing it must not
       touch the lists it was never put on. */
    handle = NULL;
    rc = mca_base_pvar_handle_alloc(session, index, NULL, &handle, &count);
    check("handle_alloc fails when the starting value cannot be read", OPAL_SUCCESS != rc);
    check("a failed handle_alloc does not return a handle", NULL == handle);
    check("a failed handle_alloc leaves no handle bound to the variable",
          0 == opal_list_get_size(&pvar->bound_handles));
    check("a failed handle_alloc leaves no handle in the session",
          0 == opal_list_get_size(&session->handles));

    /* Once the data is available, allocating and freeing a handle works and
       leaves both lists empty again. */
    data_available = 1;
    handle = NULL;
    rc = mca_base_pvar_handle_alloc(session, index, NULL, &handle, &count);
    check("handle_alloc succeeds when the starting value can be read",
          OPAL_SUCCESS == rc && NULL != handle);
    if (OPAL_SUCCESS == rc && NULL != handle) {
        check("the handle is bound to the variable", 1 == opal_list_get_size(&pvar->bound_handles));
        check("the handle is in the session", 1 == opal_list_get_size(&session->handles));
        (void) mca_base_pvar_handle_free(handle);
        check("a freed handle is no longer bound to the variable",
              0 == opal_list_get_size(&pvar->bound_handles));
        check("a freed handle is no longer in the session",
              0 == opal_list_get_size(&session->handles));
    }

    OBJ_RELEASE(session);
    opal_finalize();

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}

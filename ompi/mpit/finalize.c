/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi/mpit/mpit-internal.h"

#include "opal/include/opal/sys/atomic.h"
#include "opal/runtime/opal.h"

static const char FUNC_NAME[] = "MPI_T_finalize";

int MPI_T_finalize (void)
{
    mpit_lock ();

    if (!mpit_is_initialized ()) {
        mpit_unlock ();
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    if (0 == --mpit_init_count) {
        (void) opal_finalize_util ();
    }

    mpit_unlock ();

    return MPI_SUCCESS;
}

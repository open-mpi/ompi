/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2025-2026 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef MPIT_INTERNAL_H
#define MPIT_INTERNAL_H

#include "opal/util/string_copy.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_pvar.h"
#include "opal/mca/base/mca_base_event.h"

#include "ompi/include/ompi_config.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/file/file.h"
#include "ompi/op/op.h"
#include "ompi/message/message.h"

#include "mpi.h"

#include <string.h>

typedef struct ompi_mpit_cvar_handle_t {
    const mca_base_var_t *var;
    /* XXX -- TODO -- allow binding objects */
    void           *bound_object;
} ompi_mpit_cvar_handle_t;

OMPI_HIDDEN void ompi_mpit_lock (void);
OMPI_HIDDEN void ompi_mpit_unlock (void);

OMPI_DECLSPEC extern volatile uint32_t ompi_mpit_init_count;

/* The thread level of the MPI tool information interface itself, pinned
   for one init epoch: established by the first MPI_T_init_thread() of the
   epoch, reported unchanged by nested init calls (MPI 5.0 sec. 15.3.4:
   they have "no effect beyond increasing the reference count"), and reset
   when the last MPI_T_finalize() ends the epoch.  This is NOT the same
   thing as the World Model's thread level (see MPI_T_init_thread() for
   why the two must never be conflated). */
OMPI_DECLSPEC extern int ompi_mpit_thread_level;

OMPI_HIDDEN extern opal_mutex_t ompi_mpit_big_lock;

/* Set when a first MPI_T_init_thread() failed partway through framework
   registration, which is not unwindable; MPI_T can never be brought up
   in this process again, and MPI_T_finalize() must not run the component
   closes (see both files for the details). */
OMPI_HIDDEN extern bool ompi_mpit_init_failed;

OMPI_HIDDEN int ompit_var_type_to_datatype (mca_base_var_type_t type, MPI_Datatype *datatype);
OMPI_HIDDEN int ompit_opal_to_mpit_error (int rc);
OMPI_HIDDEN bool ompit_obj_invalid(void *obj_handle);

/* --- MPI_T events support (see specs/mpi-t-events/spec.md sec. 6) --------- */

/* The opaque MPI_T event handles are the OPAL pointers, cast.  These helpers
   round-trip them so the casts live in one place. */
static inline mca_base_event_registration_t *ompit_event_reg(MPI_T_event_registration h)
{
    return (mca_base_event_registration_t *) (void *) h;
}
static inline MPI_T_event_registration ompit_event_handle(mca_base_event_registration_t *reg)
{
    return (MPI_T_event_registration) (void *) reg;
}
static inline mca_base_event_instance_t *ompit_event_inst(MPI_T_event_instance h)
{
    return (mca_base_event_instance_t *) (void *) h;
}

/* OMPI-owned context for an MPI_T event/dropped/free callback: the user's
   callback function pointer + user pointer.  Allocated per registration and
   freed (via ompit_event_ctx_release) only after the OPAL slot's refcount hits
   0 -- OPAL never frees it. */
typedef void (*ompit_generic_fn_t)(void);
typedef struct ompi_mpit_event_cb_ctx_t {
    ompit_generic_fn_t fn;
    void              *user_data;
} ompi_mpit_event_cb_ctx_t;

OMPI_HIDDEN ompi_mpit_event_cb_ctx_t *ompit_event_cb_ctx_new(ompit_generic_fn_t fn, void *user_data);
OMPI_HIDDEN void ompit_event_ctx_release(void *user_data);

/* Trampolines with the OPAL callback signatures; each forwards to the user's
   MPI_T callback after casting the opaque handles.  Calling an MPI_T callback
   through an OPAL function-pointer type would be undefined behaviour, so these
   bridge the (function-pointer) type mismatch. */
OMPI_HIDDEN void ompit_event_cb_trampoline(mca_base_event_instance_t *inst,
                               mca_base_event_registration_t *reg,
                               mca_base_event_cb_safety_t cb_safety, void *user_data);
OMPI_HIDDEN void ompit_event_dropped_trampoline(opal_count_t count, mca_base_event_registration_t *reg,
                                    int source_index, mca_base_event_cb_safety_t cb_safety,
                                    void *user_data);
OMPI_HIDDEN void ompit_event_free_trampoline(mca_base_event_registration_t *reg,
                                 mca_base_event_cb_safety_t cb_safety, void *user_data);

/* Install the OPAL debug raise-check hook (a no-op unless OPAL_ENABLE_DEBUG). */
OMPI_HIDDEN void ompit_install_event_debug_hook(void);

static inline int mpit_is_initialized (void)
{
    return !!ompi_mpit_init_count;
}

static inline void mpit_copy_string (char *dest, int *len, const char *source)
{
    if (NULL == len)
        return;

    if (NULL == source) {
        *len = 0;
        if (NULL != dest) {
            dest[0] = '\0';
        }

        return;
    }

    if (0 != *len && NULL != dest) {
        if ((int) strlen (source) < *len) {
            *len = strlen (source) + 1;
        }

        opal_string_copy (dest, source, *len);
    } else {
        *len = strlen (source) + 1;
    }
}

#endif /* MPIT_INTERNAL_H */

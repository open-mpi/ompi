/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MPIT_INTERNAL_H
#define MPIT_INTERNAL_H

#include "opal/util/string_copy.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_pvar.h"

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

void ompi_mpit_lock (void);
void ompi_mpit_unlock (void);

extern volatile uint32_t ompi_mpit_init_count;

/* The thread level of the MPI tool information interface itself, pinned
   for one init epoch: established by the first MPI_T_init_thread() of the
   epoch, reported unchanged by nested init calls (MPI 5.0 sec. 15.3.4:
   they have "no effect beyond increasing the reference count"), and reset
   when the last MPI_T_finalize() ends the epoch.  This is NOT the same
   thing as the World Model's thread level (see MPI_T_init_thread() for
   why the two must never be conflated). */
OMPI_DECLSPEC extern int ompi_mpit_thread_level;

/* Set when a first MPI_T_init_thread() failed partway through framework
   registration, which is not unwindable; MPI_T can never be brought up
   in this process again, and MPI_T_finalize() must not run the component
   closes (see both files for the details). */
extern bool ompi_mpit_init_failed;

int ompit_var_type_to_datatype (mca_base_var_type_t type, MPI_Datatype *datatype);
int ompit_opal_to_mpit_error (int rc);
bool ompit_obj_invalid(void *obj_handle);

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

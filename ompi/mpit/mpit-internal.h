/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MPIT_INTERNAL_H)
#define MPIT_INTERNAL_H

#include "ompi/include/ompi_config.h"
#include "opal/mca/base/mca_base_var.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/constants.h"

#include "mpi.h"

#include <string.h>

#define MPI_T_ENUM_NULL NULL

typedef struct ompi_mpit_cvar_handle_t {
    const mca_base_var_t *var;
    /* XXX -- TODO -- allow binding objects */
    void           *bound_object;
} ompi_mpit_cvar_handle_t;

typedef struct ompi_mpi_pvar_handle_t {
    int dummy;
} ompi_mpi_pvar_handle_t;

typedef struct ompi_mpi_pvar_session_t {
    int dummy;
} ompi_mpi_pvar_session_t;

typedef struct ompi_mpit_enum_t {
    mca_base_var_enum_t *enumerator;
} ompi_mpit_enum_t;

void mpit_lock (void);
void mpit_unlock (void);

extern volatile uint32_t mpit_init_count;

static inline int mpit_is_initialized (void)
{
    return !!mpit_init_count;
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

    if ((int) strlen (source) < *len) {
        *len = strlen (source) + 1;
    }

    strncpy (dest, source, *len);
    dest[*len - 1] = '\0';
}

#endif /* !defined(MPIT_INTERNAL_H) */

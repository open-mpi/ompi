/**
 * Copyright (c) 2021      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpiext/mpiext.h"
#include "ompi/mpiext/continue/c/continuation.h"

/*
 * Similar to Open MPI components, a well-known struct provides
 * function pointers to the extension's init/fini hooks.  The struct
 * must be a global symbol of the form ompi_mpiext_<ext_name> and be
 * of type ompi_mpiext_component_t.
 */
ompi_mpiext_component_t ompi_mpiext_continue = {
    ompi_continuation_init,
    ompi_continuation_fini
};

/*
 * Copyright (c) 2026      Jeff Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Shared offset/sentinel translation for the hand-written
 * MPI_<Handle>_fromint / MPI_<Handle>_toint standard-ABI converters
 * (comm, errhandler, file, group, info, message, op, request, session,
 * type, win).
 *
 * Those converters are maintained by hand (one file per handle type and
 * direction) rather than generated, and they legitimately differ in their
 * per-type validation, handle-table symbol, f2c index field, and any lazy
 * table insertion.  What they all share -- and what previously drifted
 * because it was copy-pasted into every file -- is the translation between
 * an ABI integer handle and an OMPI Fortran handle-table index around
 * OMPI_ABI_HANDLE_BASE_OFFSET (see MPI-5.0 section 20.3).  Centralize that
 * arithmetic here so it cannot diverge.
 */

#ifndef OMPI_MPI_C_ABI_HANDLE_CONVERT_H
#define OMPI_MPI_C_ABI_HANDLE_CONVERT_H

#include "ompi_config.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

#include "ompi/mpi/c/abi_converters.h"

/*
 * ABI integer handle values below OMPI_ABI_HANDLE_BASE_OFFSET are
 * predefined/sentinel constants that the converters pass through
 * unchanged; values at or above it encode a Fortran handle-table index.
 */
static inline bool ompi_abi_handle_int_is_predefined(intptr_t handle_int)
{
    return OMPI_ABI_HANDLE_BASE_OFFSET > handle_int;
}

/*
 * Convert a (non-predefined) ABI integer handle to its Fortran
 * handle-table index.  The caller must have already excluded predefined
 * values with ompi_abi_handle_int_is_predefined().
 */
static inline int ompi_abi_handle_int_to_index(intptr_t handle_int)
{
    int o_index = (int)(handle_int - OMPI_ABI_HANDLE_BASE_OFFSET);
    assert(o_index >= 0);
    return o_index;
}

/*
 * Convert a Fortran handle-table index to its ABI integer handle value.
 */
static inline int ompi_abi_index_to_handle_int(int f_to_c_index)
{
    return f_to_c_index + OMPI_ABI_HANDLE_BASE_OFFSET;
}

#endif /* OMPI_MPI_C_ABI_HANDLE_CONVERT_H */

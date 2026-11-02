/*
 * Copyright (c) 2026      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */
/**
 * @file
 *
 * Internal types shared by the "extended callback" (MPIX_*_x) APIs
 * implemented by the xcallbacks MPI extension.
 *
 * Each of the MPIX_*_x entry points (MPIX_Op_create_x,
 * MPIX_Comm_create_errhandler_x, MPIX_Comm_create_keyval_x, and the
 * like) accepts an MPIX_Destructor_function that Open MPI invokes when
 * the owning object (op, errhandler, or keyval) is destroyed.  The
 * back-end implementations of those objects live in libopen_mpi, below
 * the ompi/mpi/ bindings layer, so the internal equivalent of that
 * destructor type is defined here rather than in any single object's
 * header.
 *
 * This mirrors the public MPIX_Destructor_function typedef in
 * ompi/mpiext/xcallbacks/c/mpiext_xcallbacks_c.h, but is kept separate
 * so that libopen_mpi back-end code does not have to depend on the
 * extension's public header (which pulls in <mpi.h>).
 */

#ifndef OMPI_XCALLBACKS_H
#define OMPI_XCALLBACKS_H

/*
 * User destructor callback invoked, with the user's extra_state, when
 * an object created by one of the MPIX_*_x extended-callback APIs is
 * destroyed.
 */
typedef void (ompi_user_destructor_fn_t)(void *extra_state);

#endif /* OMPI_XCALLBACKS_H */

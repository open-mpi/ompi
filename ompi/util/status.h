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
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_UTIL_STATUS_H
#define OMPI_UTIL_STATUS_H

#include "ompi/mpi/fortran/base/fint_2_int.h"
#include "ompi/mpi/fortran/base/constants.h"

static inline int ompi_status_c2f(const MPI_Status *c_status, MPI_Fint *f_status)
{
    const int *c_ints;
    int i;

    /* Note that MPI-2.2 16.3.5 states that even the hidden data in a
       status must be converted (!).  This is somewhat problematic
       because the Fortran data is all INTEGERS while the C MPI_Status
       contains a size_t.  That being said, note 2 things:

       1. The _ucount and _canceled members are never accessed from
          Fortran.
       2. configure calculated a value of MPI_STATUS_SIZE to ensure
          that the Fortran status is the Right size to hold the C
          MPI_Status (including the size_t member).

       So for the purposes of this function, just copy over all the
       data as if they were int's.  This works because all OMPI
       Fortran MPI API functions that take a status as an IN argument
       first call MPI_Status_f2c on it before using it (in which case
       we'll do the exact opposite copy, thereby rebuilding the size_t
       value properly before it is accessed in C).

       configure requires sizeof(INTEGER) >= sizeof(int) (see
       OMPI_SETUP_MPI_FORTRAN), so the public values in the status are
       never truncated by this copy: with equal sizes the copy is
       exact, and with a wider INTEGER each int is widened into a wider
       slot. */
    c_ints = (const int*)c_status;
    for( i = 0; i < (int)(sizeof(MPI_Status) / sizeof(int)); i++ ) {
        f_status[i] = OMPI_INT_2_FINT(c_ints[i]);
    }

    return MPI_SUCCESS;
}

static inline int ompi_status_f2c(const MPI_Fint *f_status, MPI_Status *c_status)
{
    int i, *c_ints;

    /* ***NOTE*** See huge comment in status_c2f.c (yes, I know
                  there's a size_t member in the C MPI_Status -- go
                  read that comment for an explanation why copying
                  everything as a bunch of int's is ok).

       We can't use OMPI_FINT_2_INT here because of some complications
       with include files.  :-( So just do the casting manually. */
    c_ints = (int*)c_status;
    for( i = 0; i < (int)(sizeof(MPI_Status) / sizeof(int)); i++ ) {
        c_ints[i] = (int)f_status[i];
    }

    return MPI_SUCCESS;
}

#endif /* OMPI_UTIL_STATUS_H */
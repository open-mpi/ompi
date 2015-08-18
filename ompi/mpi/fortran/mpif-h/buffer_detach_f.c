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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_BUFFER_DETACH = ompi_buffer_detach_f
#pragma weak pmpi_buffer_detach = ompi_buffer_detach_f
#pragma weak pmpi_buffer_detach_ = ompi_buffer_detach_f
#pragma weak pmpi_buffer_detach__ = ompi_buffer_detach_f

#pragma weak PMPI_Buffer_detach_f = ompi_buffer_detach_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_BUFFER_DETACH,
                           pmpi_buffer_detach,
                           pmpi_buffer_detach_,
                           pmpi_buffer_detach__,
                           pompi_buffer_detach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BUFFER_DETACH = ompi_buffer_detach_f
#pragma weak mpi_buffer_detach = ompi_buffer_detach_f
#pragma weak mpi_buffer_detach_ = ompi_buffer_detach_f
#pragma weak mpi_buffer_detach__ = ompi_buffer_detach_f

#pragma weak MPI_Buffer_detach_f = ompi_buffer_detach_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_BUFFER_DETACH,
                           mpi_buffer_detach,
                           mpi_buffer_detach_,
                           mpi_buffer_detach__,
                           ompi_buffer_detach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

/* (this comment is repeated in ompi/mpi/fortran/use-mpi-f08/buffer_detach.c)
 *
 * MPI-3.1 section 3.6, page 45, states that the mpif.h and mpi module
 * interfaces for MPI_BUFFER_DETACH ignore the buffer argument.
 * Therefore, for the mpif.h and mpi module interfaces, we use a dummy
 * variable and leave the value handed in alone.
 *
 * The mpi_f08 implementation for MPI_BUFFER_DETACH therefore is a
 * separate routine in the use-mpi-f08 directory (it's not built in
 * the mpif-h directory because of all the different combinations of
 * supporting weak symbols (or not), building the profiling layer (or
 * not), etc.).
 */
void ompi_buffer_detach_f(char *buffer, MPI_Fint *size, MPI_Fint *ierr)
{
    int c_ierr;
    void *dummy;
    OMPI_SINGLE_NAME_DECL(size);

    c_ierr = MPI_Buffer_detach(&dummy, OMPI_SINGLE_NAME_CONVERT(size));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(size);
    }
}

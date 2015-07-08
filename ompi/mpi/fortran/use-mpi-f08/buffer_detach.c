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

#include "mpi.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"

/*
 * This function implemented in this file is only called from Fortran,
 * so we never bothered to put a prototype for it in any C header
 * file.  To avoid compiler warnings about no protoype, we prototype
 * it here.
 */
OMPI_DECLSPEC void ompi_buffer_detach_f08(char *buffer, MPI_Fint *size,
                                          MPI_Fint *ierr);

/* (this comment is repeated in ompi/mpi/fortran/mpif-h/buffer_detach_f.c)
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
 *
 * Note that we only need to build this function once -- the F08
 * interfaces for MPI_BUFFER_ATTACH and PMPI_BUFFER_ATTACH both
 * bind(C) to the name ompi_buffer_detach_f08.
 */
void ompi_buffer_detach_f08(char *buffer, MPI_Fint *size, MPI_Fint *ierr)
{
    int c_ierr;
    void *dummy;
    OMPI_SINGLE_NAME_DECL(size);

    c_ierr = MPI_Buffer_detach(&dummy, OMPI_SINGLE_NAME_CONVERT(size));
    if (NULL != ierr) {
        *ierr = OMPI_INT_2_FINT(c_ierr);
    }

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(size);
        *(void **)buffer = dummy;
    }
}

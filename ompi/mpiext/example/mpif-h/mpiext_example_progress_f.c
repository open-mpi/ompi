/*
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * This file contains the mpif.h implementation of the OMPI_Progress
 * function.  It has no file naming convention, and generally contains
 * whatever the extension needs it to.
 */

#include "ompi_config.h"

#include <stdio.h>

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#include "ompi/mpiext/example/c/mpiext_example_c.h"

/* Rather than doing a whole pile of messy things with weak symbols,
   just define one back-end function (the _f version), and then four
   short functions for the four popular Fortran symbol-mangling
   schemes that call the back-end function.  Make sure that the the
   back-end function is public so that it can be callable from the
   mpi_f08_ext library. */

/* Prototype everything so that the compiler doesn't complain */
OMPI_DECLSPEC void OMPI_Progress_f(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr);
OMPI_DECLSPEC void OMPI_PROGRESS(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr);
OMPI_DECLSPEC void ompi_progress(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr);
OMPI_DECLSPEC void ompi_progress_(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr);
OMPI_DECLSPEC void ompi_progress__(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr);

/* Back-end function */
void OMPI_Progress_f(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_err;
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);

    c_err = OMPI_Progress(OMPI_FINT_2_INT(*count), c_comm);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_err);
}

/* Front-end functions */
void OMPI_PROGRESS(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr) {
    OMPI_Progress_f(count, comm, ierr);
}

void ompi_progress(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr) {
    OMPI_Progress_f(count, comm, ierr);
}

void ompi_progress_(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr) {
    OMPI_Progress_f(count, comm, ierr);
}

void ompi_progress__(MPI_Fint *count, MPI_Fint *comm, MPI_Fint *ierr) {
    OMPI_Progress_f(count, comm, ierr);
}


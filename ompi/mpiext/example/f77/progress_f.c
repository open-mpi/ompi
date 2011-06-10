/*
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/constants.h"

#include "mpiext_example_c.h"

static void OMPI_Progress_f(MPI_Fint *count, MPI_Fint *ierr);

OMPI_DECLSPEC void OMPI_PROGRESS(MPI_Fint *count, MPI_Fint *ierr);
OMPI_DECLSPEC void ompi_progress(MPI_Fint *count, MPI_Fint *ierr);
OMPI_DECLSPEC void ompi_progress_(MPI_Fint *count, MPI_Fint *ierr);
OMPI_DECLSPEC void ompi_progress__(MPI_Fint *count, MPI_Fint *ierr);

void OMPI_PROGRESS(MPI_Fint *count, MPI_Fint *ierr) {
    OMPI_Progress_f(count, ierr);
}
void ompi_progress(MPI_Fint *count, MPI_Fint *ierr) {
    OMPI_Progress_f(count, ierr);
}
void ompi_progress_(MPI_Fint *count, MPI_Fint *ierr) {
    OMPI_Progress_f(count, ierr);
}
void ompi_progress__(MPI_Fint *count, MPI_Fint *ierr) {
    OMPI_Progress_f(count, ierr);
}

static void OMPI_Progress_f(MPI_Fint *count, MPI_Fint *ierr)
{
    *ierr = OMPI_INT_2_FINT(OMPI_Progress(OMPI_FINT_2_INT(*count)));
}

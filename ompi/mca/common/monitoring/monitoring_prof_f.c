/*
 * Copyright (c) 2013-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2017 Inria.  All rights reserved.
 * Copyright (c) 2013-2015 Bull SAS.  All rights reserved.
 * Copyright (c) 2016      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * MPI binding for fortran
 */

#define OMPI_BUILD_MPI_PROFILING 0
#define OMPI_COMPILING_FORTRAN_WRAPPERS 1

#include <stdbool.h>

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"


void monitoring_prof_mpi_init_f2c( MPI_Fint * );
void monitoring_prof_mpi_finalize_f2c( MPI_Fint * );

void monitoring_prof_mpi_init_f2c( MPI_Fint *ierr ) {
    int c_ierr;
    int argc = 0;
    char ** argv = NULL;

    c_ierr = PMPI_Init(&argc, &argv);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

void monitoring_prof_mpi_finalize_f2c( MPI_Fint *ierr ) {
    int c_ierr;

    c_ierr = PMPI_Finalize();
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT = monitoring_prof_mpi_init_f2c
#pragma weak mpi_init = monitoring_prof_mpi_init_f2c
#pragma weak mpi_init_ = monitoring_prof_mpi_init_f2c
#pragma weak mpi_init__ = monitoring_prof_mpi_init_f2c
#pragma weak MPI_Init_f = monitoring_prof_mpi_init_f2c
#pragma weak MPI_Init_f08 = monitoring_prof_mpi_init_f2c

#pragma weak MPI_FINALIZE = monitoring_prof_mpi_finalize_f2c
#pragma weak mpi_finalize = monitoring_prof_mpi_finalize_f2c
#pragma weak mpi_finalize_ = monitoring_prof_mpi_finalize_f2c
#pragma weak mpi_finalize__ = monitoring_prof_mpi_finalize_f2c
#pragma weak MPI_Finalize_f = monitoring_prof_mpi_finalize_f2c
#pragma weak MPI_Finalize_f08 = monitoring_prof_mpi_finalize_f2c
#else

OMPI_GENERATE_F77_BINDINGS (MPI_INIT,
                           mpi_init,
                           mpi_init_,
                           mpi_init__,
                           monitoring_prof_mpi_init_f2c,
                           (MPI_Fint *ierr),
                           (ierr) )

OMPI_GENERATE_F77_BINDINGS (MPI_FINALIZE,
                           mpi_finalize,
                           mpi_finalize_,
                           mpi_finalize__,
                           monitoring_prof_mpi_finalize_f2c,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif

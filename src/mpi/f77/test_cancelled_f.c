/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TEST_CANCELLED = mpi_test_cancelled_f
#pragma weak pmpi_test_cancelled = mpi_test_cancelled_f
#pragma weak pmpi_test_cancelled_ = mpi_test_cancelled_f
#pragma weak pmpi_test_cancelled__ = mpi_test_cancelled_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TEST_CANCELLED,
                           pmpi_test_cancelled,
                           pmpi_test_cancelled_,
                           pmpi_test_cancelled__,
                           pmpi_test_cancelled_f,
                           (MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TEST_CANCELLED = mpi_test_cancelled_f
#pragma weak mpi_test_cancelled = mpi_test_cancelled_f
#pragma weak mpi_test_cancelled_ = mpi_test_cancelled_f
#pragma weak mpi_test_cancelled__ = mpi_test_cancelled_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TEST_CANCELLED,
                           mpi_test_cancelled,
                           mpi_test_cancelled_,
                           mpi_test_cancelled__,
                           mpi_test_cancelled_f,
                           (MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_test_cancelled_f(MPI_Fint *status, MPI_Fint *flag, MPI_Fint *ierr)
{
    MPI_Status c_status;
    OMPI_SINGLE_NAME_DECL(flag);

    MPI_Status_f2c( status, &c_status );

    *ierr = OMPI_INT_2_FINT(MPI_Test_cancelled(&c_status, 
					       OMPI_SINGLE_NAME_CONVERT(flag)
					       ));
    
    OMPI_SINGLE_INT_2_FINT(flag);
}

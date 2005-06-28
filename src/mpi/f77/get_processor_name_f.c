/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GET_PROCESSOR_NAME = mpi_get_processor_name_f
#pragma weak pmpi_get_processor_name = mpi_get_processor_name_f
#pragma weak pmpi_get_processor_name_ = mpi_get_processor_name_f
#pragma weak pmpi_get_processor_name__ = mpi_get_processor_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_PROCESSOR_NAME,
                           pmpi_get_processor_name,
                           pmpi_get_processor_name_,
                           pmpi_get_processor_name__,
                           pmpi_get_processor_name_f,
                           (char *name, MPI_Fint *resultlen, MPI_Fint *ierr),
                           (name, resultlen, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_PROCESSOR_NAME = mpi_get_processor_name_f
#pragma weak mpi_get_processor_name = mpi_get_processor_name_f
#pragma weak mpi_get_processor_name_ = mpi_get_processor_name_f
#pragma weak mpi_get_processor_name__ = mpi_get_processor_name_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET_PROCESSOR_NAME,
                           mpi_get_processor_name,
                           mpi_get_processor_name_,
                           mpi_get_processor_name__,
                           mpi_get_processor_name_f,
                           (char *name, MPI_Fint *resultlen, MPI_Fint *ierr),
                           (name, resultlen, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_get_processor_name_f(char *name, MPI_Fint *resultlen, MPI_Fint *ierr)
{
    OMPI_SINGLE_NAME_DECL(resultlen);

    *ierr = OMPI_INT_2_FINT(MPI_Get_processor_name(name, 
				   OMPI_SINGLE_NAME_CONVERT(resultlen)));
    if (OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(resultlen);
    }
}

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
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_HINDEXED = mpi_type_create_hindexed_f
#pragma weak pmpi_type_create_hindexed = mpi_type_create_hindexed_f
#pragma weak pmpi_type_create_hindexed_ = mpi_type_create_hindexed_f
#pragma weak pmpi_type_create_hindexed__ = mpi_type_create_hindexed_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_HINDEXED,
                           pmpi_type_create_hindexed,
                           pmpi_type_create_hindexed_,
                           pmpi_type_create_hindexed__,
                           pmpi_type_create_hindexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_HINDEXED = mpi_type_create_hindexed_f
#pragma weak mpi_type_create_hindexed = mpi_type_create_hindexed_f
#pragma weak mpi_type_create_hindexed_ = mpi_type_create_hindexed_f
#pragma weak mpi_type_create_hindexed__ = mpi_type_create_hindexed_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_HINDEXED,
                           mpi_type_create_hindexed,
                           mpi_type_create_hindexed_,
                           mpi_type_create_hindexed__,
                           mpi_type_create_hindexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_TYPE_CREATE_HINDEXED";


void mpi_type_create_hindexed_f(MPI_Fint *count,
				MPI_Fint *array_of_blocklengths,
				MPI_Fint *array_of_displacements, 
				MPI_Fint *oldtype, MPI_Fint *newtype,
				MPI_Fint *ierr)
{
    int c_err;
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new = MPI_Type_f2c(*newtype);
    MPI_Aint *c_disp_array;
    int i;
    OMPI_ARRAY_NAME_DECL(array_of_blocklengths);

    c_disp_array = malloc(*count * sizeof(MPI_Aint));
    if (NULL == c_disp_array) {
      c_err = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
				     FUNC_NAME);
      *ierr = OMPI_INT_2_FINT(c_err);
      return;
    }
    for (i = 0; i < *count; i++) {
        c_disp_array[i] = (MPI_Aint) array_of_displacements[i];
    }
    
    OMPI_ARRAY_FINT_2_INT(array_of_blocklengths, *count); 

    *ierr = OMPI_INT_2_FINT(MPI_Type_create_hindexed(OMPI_FINT_2_INT(*count),
			     OMPI_ARRAY_NAME_CONVERT(array_of_blocklengths), 
			     c_disp_array, c_old,
			     &c_new));
    
    if (MPI_SUCCESS == *ierr) {
      *newtype = MPI_Type_c2f(c_new);
    }
    
    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_blocklengths);
    free(c_disp_array);
}

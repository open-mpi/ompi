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
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_HINDEXED = ompi_type_hindexed_f
#pragma weak pmpi_type_hindexed = ompi_type_hindexed_f
#pragma weak pmpi_type_hindexed_ = ompi_type_hindexed_f
#pragma weak pmpi_type_hindexed__ = ompi_type_hindexed_f

#pragma weak PMPI_Type_hindexed_f = ompi_type_hindexed_f
#pragma weak PMPI_Type_hindexed_f08 = ompi_type_hindexed_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_HINDEXED,
                           pmpi_type_hindexed,
                           pmpi_type_hindexed_,
                           pmpi_type_hindexed__,
                           pompi_type_hindexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_HINDEXED = ompi_type_hindexed_f
#pragma weak mpi_type_hindexed = ompi_type_hindexed_f
#pragma weak mpi_type_hindexed_ = ompi_type_hindexed_f
#pragma weak mpi_type_hindexed__ = ompi_type_hindexed_f

#pragma weak MPI_Type_hindexed_f = ompi_type_hindexed_f
#pragma weak MPI_Type_hindexed_f08 = ompi_type_hindexed_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_HINDEXED,
                           mpi_type_hindexed,
                           mpi_type_hindexed_,
                           mpi_type_hindexed__,
                           ompi_type_hindexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_TYPE_HINDEXED";


void ompi_type_hindexed_f(MPI_Fint *count, MPI_Fint *array_of_blocklengths, 
			 MPI_Fint *array_of_displacements, 
			 MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;
    MPI_Aint *c_disp_array;
    int i, c_ierr;
    OMPI_ARRAY_NAME_DECL(array_of_blocklengths);

    c_disp_array = (MPI_Aint *) malloc(*count * sizeof(MPI_Aint));
    if (NULL == c_disp_array) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                        FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }
    for (i = 0; i < *count; i++) {
        c_disp_array[i] = (MPI_Aint) array_of_displacements[i];
    }

    OMPI_ARRAY_FINT_2_INT(array_of_blocklengths, *count);

    c_ierr = MPI_Type_hindexed(OMPI_FINT_2_INT(*count),
                               OMPI_ARRAY_NAME_CONVERT(array_of_blocklengths), 
                               c_disp_array, c_old, &c_new);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    free(c_disp_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_blocklengths);

    if (MPI_SUCCESS == c_ierr) {
        *newtype = MPI_Type_c2f(c_new);
    }
}

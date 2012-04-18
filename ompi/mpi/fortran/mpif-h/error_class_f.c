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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ERROR_CLASS = ompi_error_class_f
#pragma weak pmpi_error_class = ompi_error_class_f
#pragma weak pmpi_error_class_ = ompi_error_class_f
#pragma weak pmpi_error_class__ = ompi_error_class_f

#pragma weak PMPI_Error_class_f = ompi_error_class_f
#pragma weak PMPI_Error_class_f08 = ompi_error_class_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ERROR_CLASS,
                           pmpi_error_class,
                           pmpi_error_class_,
                           pmpi_error_class__,
                           pompi_error_class_f,
                           (MPI_Fint *errorcode, MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorcode, errorclass, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERROR_CLASS = ompi_error_class_f
#pragma weak mpi_error_class = ompi_error_class_f
#pragma weak mpi_error_class_ = ompi_error_class_f
#pragma weak mpi_error_class__ = ompi_error_class_f

#pragma weak MPI_Error_class_f = ompi_error_class_f
#pragma weak MPI_Error_class_f08 = ompi_error_class_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ERROR_CLASS,
                           mpi_error_class,
                           mpi_error_class_,
                           mpi_error_class__,
                           ompi_error_class_f,
                           (MPI_Fint *errorcode, MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorcode, errorclass, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_error_class_f(MPI_Fint *errorcode, MPI_Fint *errorclass, 
		       MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_SINGLE_NAME_DECL(errorclass);

    c_ierr = MPI_Error_class(OMPI_FINT_2_INT(*errorcode),
                             OMPI_SINGLE_NAME_CONVERT(errorclass));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(errorclass);
    }
}

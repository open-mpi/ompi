/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "include/constants.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "mpi/f77/bindings.h"
#include "mpi/f77/strings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_SET_NAME = mpi_type_set_name_f
#pragma weak pmpi_type_set_name = mpi_type_set_name_f
#pragma weak pmpi_type_set_name_ = mpi_type_set_name_f
#pragma weak pmpi_type_set_name__ = mpi_type_set_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_SET_NAME,
                           pmpi_type_set_name,
                           pmpi_type_set_name_,
                           pmpi_type_set_name__,
                           pmpi_type_set_name_f,
                           (MPI_Fint *type, char *type_name, MPI_Fint *ierr, int name_len),
                           (type, type_name, ierr, name_len) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_SET_NAME = mpi_type_set_name_f
#pragma weak mpi_type_set_name = mpi_type_set_name_f
#pragma weak mpi_type_set_name_ = mpi_type_set_name_f
#pragma weak mpi_type_set_name__ = mpi_type_set_name_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_SET_NAME,
                           mpi_type_set_name,
                           mpi_type_set_name_,
                           mpi_type_set_name__,
                           mpi_type_set_name_f,
                           (MPI_Fint *type, char *type_name, MPI_Fint *ierr, int name_len),
                           (type, type_name, ierr, name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_set_name_f(MPI_Fint *type, char *type_name, MPI_Fint *ierr,
			 int name_len)
{
    int ret, c_err;
    char *c_name;
    MPI_Datatype c_type;

    c_type = MPI_Type_f2c(*type);

    /* Convert the fortran string */

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(type_name, name_len,
                                                       &c_name))) {
        c_err = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret,
				       "MPI_TYPE_SET_NAME");
	*ierr = OMPI_INT_2_FINT(c_err);
        return;
    }

    /* Call the C function */

    *ierr = OMPI_INT_2_FINT(MPI_Type_set_name(c_type, c_name));

    /* Free the C name */

    free(c_name);
}

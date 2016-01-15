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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_TYPE_GET_CONTENTS = ompi_type_get_contents_f
#pragma weak pmpi_type_get_contents = ompi_type_get_contents_f
#pragma weak pmpi_type_get_contents_ = ompi_type_get_contents_f
#pragma weak pmpi_type_get_contents__ = ompi_type_get_contents_f

#pragma weak PMPI_Type_get_contents_f = ompi_type_get_contents_f
#pragma weak PMPI_Type_get_contents_f08 = ompi_type_get_contents_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_CONTENTS,
                           pmpi_type_get_contents,
                           pmpi_type_get_contents_,
                           pmpi_type_get_contents__,
                           pompi_type_get_contents_f,
                           (MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Aint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr),
                           (mtype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_CONTENTS = ompi_type_get_contents_f
#pragma weak mpi_type_get_contents = ompi_type_get_contents_f
#pragma weak mpi_type_get_contents_ = ompi_type_get_contents_f
#pragma weak mpi_type_get_contents__ = ompi_type_get_contents_f

#pragma weak MPI_Type_get_contents_f = ompi_type_get_contents_f
#pragma weak MPI_Type_get_contents_f08 = ompi_type_get_contents_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_CONTENTS,
                           mpi_type_get_contents,
                           mpi_type_get_contents_,
                           mpi_type_get_contents__,
                           ompi_type_get_contents_f,
                           (MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Aint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr),
                           (mtype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes, ierr) )
#else
#define ompi_type_get_contents_f pompi_type_get_contents_f
#endif
#endif


static const char FUNC_NAME[] = "MPI_TYPE_GET_CONTENTS";


void ompi_type_get_contents_f(MPI_Fint *mtype, MPI_Fint *max_integers,
			     MPI_Fint *max_addresses, MPI_Fint *max_datatypes,
			     MPI_Fint *array_of_integers,
			     MPI_Aint *array_of_addresses,
			     MPI_Fint *array_of_datatypes, MPI_Fint *ierr)
{
    MPI_Aint *c_address_array = NULL;
    MPI_Datatype *c_datatype_array = NULL;
    MPI_Datatype c_mtype = PMPI_Type_f2c(*mtype);
    int i, c_ierr;
    OMPI_ARRAY_NAME_DECL(array_of_integers);

    if (*max_datatypes) {
        c_datatype_array = (MPI_Datatype *) malloc(*max_datatypes * sizeof(MPI_Datatype));
        if (NULL == c_datatype_array) {
            c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                            FUNC_NAME);
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
    }

    if (*max_addresses) {
        c_address_array = (MPI_Aint *) malloc(*max_addresses * sizeof(MPI_Aint));
        if (NULL == c_address_array) {
            if (NULL != c_datatype_array) {
              free(c_datatype_array);
            }

            c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                            FUNC_NAME);
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
    }

    OMPI_ARRAY_FINT_2_INT(array_of_integers, *max_integers);

    c_ierr = PMPI_Type_get_contents(c_mtype,
                                   OMPI_FINT_2_INT(*max_integers),
                                   OMPI_FINT_2_INT(*max_addresses),
                                   OMPI_FINT_2_INT(*max_datatypes),
                                   OMPI_ARRAY_NAME_CONVERT(array_of_integers),
                                   c_address_array, c_datatype_array);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        for (i = 0; i < *max_addresses; i++) {
            array_of_addresses[i] = c_address_array[i];
        }
        for (i = 0; i < *max_datatypes; i++) {
          array_of_datatypes[i] = PMPI_Type_c2f(c_datatype_array[i]);
        }
    }
    free(c_address_array);
    free(c_datatype_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_integers);
}

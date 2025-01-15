/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/datatype/ompi_datatype.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"

/*
 * big count entry point, only needed by F08 bindings.
 */

static const char FUNC_NAME[] = "MPI_TYPE_GET_CONTENTS_C";

void ompi_type_get_contents_f_c(MPI_Fint *mtype, MPI_Count *max_integers,
                             MPI_Count *max_addresses, MPI_Count *max_large_counts,
			     MPI_Count *max_datatypes,
                             MPI_Fint *array_of_integers,
                             MPI_Aint *array_of_addresses,
                             MPI_Count *array_of_large_counts,
                             MPI_Fint *array_of_datatypes, MPI_Fint *ierr);
void ompi_type_get_contents_f_c(MPI_Fint *mtype, MPI_Count *max_integers,
                             MPI_Count *max_addresses, MPI_Count *max_large_counts,
			     MPI_Count *max_datatypes,
                             MPI_Fint *array_of_integers,
                             MPI_Aint *array_of_addresses,
                             MPI_Count *array_of_large_counts,
                             MPI_Fint *array_of_datatypes, MPI_Fint *ierr)
{
    MPI_Aint *c_address_array = NULL;
    MPI_Count *c_large_counts_array = NULL;
    MPI_Datatype *c_datatype_array = NULL;
    MPI_Datatype c_mtype = PMPI_Type_f2c(*mtype);
    int i, c_ierr;
    OMPI_ARRAY_NAME_DECL(array_of_integers);

    if (*max_datatypes) {
        c_datatype_array = (MPI_Datatype *) malloc(*max_datatypes * sizeof(MPI_Datatype));
        if (NULL == c_datatype_array) {
            c_ierr = OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_NO_MEM,
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

            c_ierr = OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_NO_MEM,
                                            FUNC_NAME);
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
    }

    if (*max_large_counts) {
        c_large_counts_array = (MPI_Count *) malloc(*max_large_counts * sizeof(MPI_Count));
        if (NULL == c_large_counts_array) {
            if (NULL != c_datatype_array) {
              free(c_datatype_array);
            }
            if (NULL != c_address_array) {
              free(c_address_array);
            }

            c_ierr = OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_NO_MEM,
                                            FUNC_NAME);
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
    }

    OMPI_ARRAY_FINT_2_INT(array_of_integers, *max_integers);

    c_ierr = PMPI_Type_get_contents_c(c_mtype,
                                   OMPI_FINT_2_INT(*max_integers),
                                   OMPI_FINT_2_INT(*max_addresses),
                                   OMPI_FINT_2_INT(*max_datatypes),
                                   OMPI_FINT_2_INT(*max_large_counts),
                                   OMPI_ARRAY_NAME_CONVERT(array_of_integers),
                                   c_address_array, c_large_counts_array, 
			           c_datatype_array);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        for (i = 0; i < *max_addresses; i++) {
            array_of_addresses[i] = c_address_array[i];
        }
        for (i = 0; i < *max_large_counts; i++) {
            array_of_large_counts[i] = c_large_counts_array[i];
        }
        for (i = 0; i < *max_datatypes; i++) {
          array_of_datatypes[i] = PMPI_Type_c2f(c_datatype_array[i]);
        }
    }
    free(c_address_array);
    free(c_datatype_array);
    free(c_large_counts_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_integers);
}

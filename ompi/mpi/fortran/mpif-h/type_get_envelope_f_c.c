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

void ompi_type_get_envelope_f_c(MPI_Fint *type, MPI_Count *num_integers,
			     MPI_Count *num_addresses,
			     MPI_Count *num_large_counts,
			     MPI_Count *num_datatypes, MPI_Fint *combiner,
			     MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = PMPI_Type_f2c(*type);
    OMPI_SINGLE_NAME_DECL(num_integers);
    OMPI_SINGLE_NAME_DECL(num_addresses);
    OMPI_SINGLE_NAME_DECL(num_datatypes);
    OMPI_SINGLE_NAME_DECL(combiner);

    c_ierr = PMPI_Type_get_envelope_c(c_type,
                                   OMPI_SINGLE_NAME_CONVERT(num_integers),
                                   OMPI_SINGLE_NAME_CONVERT(num_addresses),
                                   OMPI_SINGLE_NAME_CONVERT(num_large_counts),
                                   OMPI_SINGLE_NAME_CONVERT(num_datatypes),
                                   OMPI_SINGLE_NAME_CONVERT(combiner));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(num_integers);
        OMPI_SINGLE_INT_2_FINT(num_addresses);
        OMPI_SINGLE_INT_2_FINT(num_datatypes);
        OMPI_SINGLE_INT_2_FINT(combiner);
    }
}

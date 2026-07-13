/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

/* This implementation has been removed from the MPI 3.0 standard.
 * Open MPI v4.0.x is keeping the implementation in the library, but
 * removing the prototypes from the headers, unless the user configures
 * with --enable-mpi1-compatibility.
 */

#include "ompi/mpi/c/bindings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_Type_struct = PMPI_Type_struct
#endif
/* undef before defining, to prevent possible redefinition when
 * using _Static_assert to error on usage of removed functions.
 */
#undef MPI_Type_struct
#define MPI_Type_struct PMPI_Type_struct
#else
/*
 * Emit the public MPI_* symbol as a *weak* definition.  Where weak aliases
 * are available the alias above is already weak; where they are not, the
 * bindings are compiled a second time to produce MPI_*, and this is that
 * copy -- so mark it weak here.
 *
 * Weak MPI_* is what lets a profiling library provide a strong MPI_* that
 * overrides ours, and it is what the MPI Forum ABI requires of libmpi_abi.
 */
#pragma weak MPI_Type_struct
#endif

int MPI_Type_struct(int count,
                    int array_of_blocklengths[],
                    MPI_Aint array_of_displacements[],
                    MPI_Datatype array_of_types[],
                    MPI_Datatype *newtype)
{
    /* the param check will be done if necessary on the MPI_Type_create_struct */
    return PMPI_Type_create_struct(count,
                                  array_of_blocklengths,
                                  array_of_displacements,
                                  array_of_types,
                                  newtype);
}

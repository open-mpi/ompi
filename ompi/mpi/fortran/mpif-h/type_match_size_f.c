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
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
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
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"
#include "ompi/runtime/params.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_TYPE_MATCH_SIZE = ompi_type_match_size_f
#pragma weak pmpi_type_match_size = ompi_type_match_size_f
#pragma weak pmpi_type_match_size_ = ompi_type_match_size_f
#pragma weak pmpi_type_match_size__ = ompi_type_match_size_f

#pragma weak PMPI_Type_match_size_f = ompi_type_match_size_f
#pragma weak PMPI_Type_match_size_f08 = ompi_type_match_size_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_MATCH_SIZE,
                           pmpi_type_match_size,
                           pmpi_type_match_size_,
                           pmpi_type_match_size__,
                           pompi_type_match_size_f,
                           (MPI_Fint *typeclass, MPI_Fint *size, MPI_Fint *type, MPI_Fint *ierr),
                           (typeclass, size, type, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_MATCH_SIZE = ompi_type_match_size_f
#pragma weak mpi_type_match_size = ompi_type_match_size_f
#pragma weak mpi_type_match_size_ = ompi_type_match_size_f
#pragma weak mpi_type_match_size__ = ompi_type_match_size_f

#pragma weak MPI_Type_match_size_f = ompi_type_match_size_f
#pragma weak MPI_Type_match_size_f08 = ompi_type_match_size_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_MATCH_SIZE,
                           mpi_type_match_size,
                           mpi_type_match_size_,
                           mpi_type_match_size__,
                           ompi_type_match_size_f,
                           (MPI_Fint *typeclass, MPI_Fint *size, MPI_Fint *type, MPI_Fint *ierr),
                           (typeclass, size, type, ierr) )
#else
#define ompi_type_match_size_f pompi_type_match_size_f
#endif
#endif

static const char FUNC_NAME[] = "MPI_Type_match_size_f";

/*  We cannot use the C function as from Fortran we should check for Fortran types. The only
 * difference is the type of predefined datatypes we are looking for.
 */
void ompi_type_match_size_f(MPI_Fint *typeclass, MPI_Fint *size, MPI_Fint *type, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type;
    int c_size = OMPI_FINT_2_INT( *size );

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    switch( OMPI_FINT_2_INT(*typeclass) ) {
    case MPI_TYPECLASS_REAL:
        c_type = (MPI_Datatype)ompi_datatype_match_size( c_size, OMPI_DATATYPE_FLAG_DATA_FLOAT, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
        break;
    case MPI_TYPECLASS_INTEGER:
        c_type = (MPI_Datatype)ompi_datatype_match_size( c_size, OMPI_DATATYPE_FLAG_DATA_INT, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
        break;
    case MPI_TYPECLASS_COMPLEX:
        c_type = (MPI_Datatype)ompi_datatype_match_size( c_size, OMPI_DATATYPE_FLAG_DATA_COMPLEX, OMPI_DATATYPE_FLAG_DATA_FORTRAN );
        break;
    default:
        c_type = &ompi_mpi_datatype_null.dt;
    }
    *type = PMPI_Type_c2f( c_type );
    if ( c_type != &ompi_mpi_datatype_null.dt ) {
        c_ierr = MPI_SUCCESS;
    } else {
        c_ierr = MPI_ERR_ARG;
        (void)OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}

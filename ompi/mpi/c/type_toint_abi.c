/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
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

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/datatype/ompi_datatype.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Type_toint = PMPI_Type_toint
#endif
#define MPI_Type_toint PMPI_Type_toint
#endif

static const char FUNC_NAME[] = "MPI_Type_toint";

int MPI_Type_toint(MPI_Datatype_ABI_INTERNAL datatype)
{
    int o_index;
    ompi_datatype_t *datatype_ptr;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == datatype) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_TYPE,
                                                   FUNC_NAME );
        }
    }

    if (OMPI_ABI_HANDLE_BASE_OFFSET > (intptr_t)datatype) {
        intptr_t datatype_int = (intptr_t)datatype;
        return (int)datatype_int;
    }

    datatype_ptr = (ompi_datatype_t *)datatype;
    /* If necessary add the datatype to the f2c translation table */
    if( -1 == datatype_ptr->d_f_to_c_index ) {
        datatype_ptr->d_f_to_c_index = opal_pointer_array_add(&ompi_datatype_f_to_c_table, datatype_ptr);
    }

    o_index = datatype_ptr->d_f_to_c_index;
    o_index += OMPI_ABI_HANDLE_BASE_OFFSET;

    return o_index;
}

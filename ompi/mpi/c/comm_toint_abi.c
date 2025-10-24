/* THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT BY HAND. */
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
 * Copyright (c) 2024-2025 Triad National Security, LLC. All rights
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
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Comm_toint = PMPI_Comm_toint
#endif
#define MPI_Comm_toint PMPI_Comm_toint
#endif

static const char FUNC_NAME[] = "MPI_Comm_toint";

int MPI_Comm_toint(MPI_Comm_ABI_INTERNAL comm)
{
    int o_index;
    ompi_communicator_t *comm_ptr;
    MPI_Comm comm_tmp;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        comm_tmp = ompi_convert_abi_comm_intern_comm(comm);
        if (ompi_comm_invalid(comm_tmp)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_COMM, FUNC_NAME);
        }
    }

    if (MPI_COMM_SELF_ABI_INTERNAL >= (MPI_Comm_ABI_INTERNAL)comm) {
        intptr_t comm_int = (intptr_t)comm;
        return (int)(comm_int & (OMPI_ABI_HANDLE_BASE_OFFSET-1));
    }

    comm_ptr = (ompi_communicator_t *)comm;
    o_index = comm_ptr->c_f_to_c_index;
    o_index += OMPI_ABI_HANDLE_BASE_OFFSET;

    return o_index;
}

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
#include "ompi/message/message.h"
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Message_toint = PMPI_Message_toint
#endif
#define MPI_Message_toint PMPI_Message_toint
#endif

static const char FUNC_NAME[] = "MPI_Message_toint";

int MPI_Message_toint(MPI_Message_ABI_INTERNAL message)
{
    int o_index;
    ompi_message_t *message_ptr;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == message) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG, FUNC_NAME);
        }
    }

    if (OMPI_ABI_HANDLE_BASE_OFFSET > (intptr_t)message) {
        intptr_t message_int = (intptr_t)message;
        return (int)message_int;
    }

    message_ptr = (ompi_message_t *)message;

    if (MPI_UNDEFINED == message_ptr->m_f_to_c_index) {
        message_ptr->m_f_to_c_index =
            opal_pointer_array_add(&ompi_message_f_to_c_table, message);
    }

    o_index = message_ptr->m_f_to_c_index;
    o_index += OMPI_ABI_HANDLE_BASE_OFFSET;

    return o_index;
}

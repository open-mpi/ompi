/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
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
#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Keyval_create = PMPI_Keyval_create
#endif
#define MPI_Keyval_create PMPI_Keyval_create
#endif

static const char FUNC_NAME[] = "MPI_Keyval_create";


int MPI_Keyval_create(MPI_Copy_function *copy_attr_fn,
                      MPI_Delete_function *delete_attr_fn,
                      int *keyval, void *extra_state)
{
    int ret;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t del_fn;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == keyval) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_KEYVAL,
                                          FUNC_NAME);
        } else if ((NULL == copy_attr_fn) || (NULL == delete_attr_fn)) {
	    return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                          FUNC_NAME);
        }
    }

    copy_fn.attr_communicator_copy_fn = copy_attr_fn;
    del_fn.attr_communicator_delete_fn = delete_attr_fn;

    ret = ompi_attr_create_keyval(COMM_ATTR, copy_fn,
                                  del_fn, keyval, extra_state, 0, NULL);
    OMPI_ERRHANDLER_NOHANDLE_RETURN(ret, MPI_ERR_OTHER, FUNC_NAME);
}

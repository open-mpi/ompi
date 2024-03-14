/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2021 Triad National Security, LLC. All rights
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
#include "ompi/info/info.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Info_create_env = PMPI_Info_create_env
#endif
#define MPI_Info_create_env PMPI_Info_create_env
#endif

static const char FUNC_NAME[] = "MPI_Info_create_env";

/**
 *  Returns an info object with the same construction as MPI_INFO_ENV as created 
 *  during MPI_INIT or MPI_INIT_THREAD when the same arguments are used.
 *
 * @param argc number or arguments (Integer)
 * @param argv Pointer to array of arguments
 * @param info Pointer to the MPI_Info handle
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_INFO
 * @retval MPI_ERR_NO_MEM
 *
 * When an MPI_Info object is not being used, it should be freed using
 * MPI_Info_free
 */
int MPI_Info_create_env(int argc, char *argv[], MPI_Info *info)
{
    int rc;
    ompi_info_t *the_info;

    if (MPI_PARAM_CHECK) {
        if (NULL == info) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    the_info = ompi_info_allocate ();
    if (NULL == the_info) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_NO_MEM,
                                      FUNC_NAME);
    }

    *info = the_info;

    rc = ompi_mpiinfo_init_env(argc, argv, the_info);
    OMPI_ERRHANDLER_NOHANDLE_CHECK(rc, rc, FUNC_NAME);
    return MPI_SUCCESS;
}

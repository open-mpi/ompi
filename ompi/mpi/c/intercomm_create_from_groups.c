/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 University of Houston.  All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC.  All rights
 *                         reserved.
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
#include "ompi/errhandler/errhandler.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Intercomm_create_from_groups = PMPI_Intercomm_create_from_groups
#endif
#define MPI_Intercomm_create_from_groups PMPI_Intercomm_create_from_groups
#endif

static const char FUNC_NAME[] = "MPI_Intercomm_create_from_groups";


int MPI_Intercomm_create_from_groups (MPI_Group local_group, int local_leader, MPI_Group remote_group,
                                      int remote_leader, const char *tag, MPI_Info info, MPI_Errhandler errhandler,
                                      MPI_Comm *newintercomm)
{
    int rc;

    MEMCHECKER(
        memchecker_comm(local_comm);
        memchecker_comm(bridge_comm);
    );

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (NULL == errhandler) {
            return MPI_ERR_ARG;
        }

        if (NULL == local_group || NULL == remote_group) {
            return ompi_errhandler_invoke (errhandler, MPI_COMM_NULL, errhandler->eh_mpi_object_type,
                                           MPI_ERR_GROUP, FUNC_NAME);
        }
        if (NULL == info || ompi_info_is_freed(info)) {
            return ompi_errhandler_invoke (errhandler, MPI_COMM_NULL, errhandler->eh_mpi_object_type,
                                           MPI_ERR_INFO, FUNC_NAME);
        }
        if (NULL == tag) {
            return ompi_errhandler_invoke (errhandler, MPI_COMM_NULL, errhandler->eh_mpi_object_type,
                                           MPI_ERR_TAG, FUNC_NAME);
        }
        if (NULL == newintercomm) {
            return ompi_errhandler_invoke (errhandler, MPI_COMM_NULL, errhandler->eh_mpi_object_type,
                                           MPI_ERR_ARG, FUNC_NAME);
        }
    }

    rc = ompi_intercomm_create_from_groups (local_group, local_leader, remote_group, remote_leader, tag,
                                            &info->super, errhandler, newintercomm);

    if (MPI_SUCCESS != rc) {
        return ompi_errhandler_invoke (errhandler, MPI_COMM_NULL, errhandler->eh_mpi_object_type,
                                       rc, FUNC_NAME);
    }

    return rc;
}


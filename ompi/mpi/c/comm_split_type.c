/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017-2022 IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/info/info.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Comm_split_type = PMPI_Comm_split_type
#endif
#define MPI_Comm_split_type PMPI_Comm_split_type
#endif

static const char FUNC_NAME[] = "MPI_Comm_split_type";


int MPI_Comm_split_type(MPI_Comm comm, int split_type, int key,
                        MPI_Info info, MPI_Comm *newcomm) {

    int rc;

    MEMCHECKER(
        memchecker_comm(comm);
    );

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( ompi_comm_invalid ( comm )) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_COMM,
                                          FUNC_NAME);
        }

        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_INFO,
                                          FUNC_NAME);
        }

        if ( MPI_COMM_TYPE_SHARED != split_type && // Same as OMPI_COMM_TYPE_NODE
             MPI_COMM_TYPE_HW_UNGUIDED != split_type &&
             MPI_COMM_TYPE_HW_GUIDED != split_type &&
	     OMPI_COMM_TYPE_CLUSTER != split_type &&
	     OMPI_COMM_TYPE_CU != split_type &&
	     OMPI_COMM_TYPE_HOST != split_type &&
	     OMPI_COMM_TYPE_BOARD != split_type &&
	     OMPI_COMM_TYPE_NODE != split_type && // Same as MPI_COMM_TYPE_SHARED
	     OMPI_COMM_TYPE_NUMA != split_type &&
	     OMPI_COMM_TYPE_SOCKET != split_type &&
	     OMPI_COMM_TYPE_L3CACHE != split_type &&
	     OMPI_COMM_TYPE_L2CACHE != split_type &&
	     OMPI_COMM_TYPE_L1CACHE != split_type &&
	     OMPI_COMM_TYPE_CORE != split_type &&
	     OMPI_COMM_TYPE_HWTHREAD != split_type &&
             MPI_UNDEFINED != split_type ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          FUNC_NAME);
        }

        if ( NULL == newcomm ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
    }

#if OPAL_ENABLE_FT_MPI
    /*
     * An early check, so as to return early if we are using a broken
     * communicator. This is not absolutely necessary since we will
     * check for this, and other, error conditions during the operation.
     */
    if( OPAL_UNLIKELY(!ompi_comm_iface_create_check(comm, &rc)) ) {
        OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
    }
#endif

    if ( MPI_COMM_TYPE_HW_GUIDED == split_type ) {
        int flag;
        opal_cstring_t *value = NULL;

        /* MPI_Info is required for this split_type.
         * Not an error condition, per MPI 4.0.
         */
        if ( MPI_INFO_NULL == info ) {
            *newcomm = MPI_COMM_NULL;
            rc = MPI_SUCCESS;
            OMPI_ERRHANDLER_RETURN ( rc, comm, rc, FUNC_NAME);
        }

        /* MPI_Info with key "mpi_hw_resource_type" is required for this split_type.
         * Not an error condition, per MPI 4.0.
         */
        ompi_info_get(info, "mpi_hw_resource_type", &value, &flag);
        if ( !flag ) {
            *newcomm = MPI_COMM_NULL;
            rc = MPI_SUCCESS;
            OMPI_ERRHANDLER_RETURN ( rc, comm, rc, FUNC_NAME);
        }
    }

    if( (MPI_COMM_SELF == comm) && (MPI_UNDEFINED == split_type) ) {
        *newcomm = MPI_COMM_NULL;
        rc = MPI_SUCCESS;
    } else {
        rc = ompi_comm_split_type( (ompi_communicator_t*)comm, split_type, key, &(info->super),
                                   (ompi_communicator_t**)newcomm);
    }
    OMPI_ERRHANDLER_RETURN ( rc, comm, rc, FUNC_NAME);
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
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
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Iallreduce = PMPI_Iallreduce
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Iallreduce";


int MPI_Iallreduce(const void *sendbuf, void *recvbuf, int count,
                   MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request)
{
    int err;

    MEMCHECKER(
        memchecker_datatype(datatype);
        memchecker_comm(comm);

        /* check whether receive buffer is defined. */
        memchecker_call(&opal_memchecker_base_isaddressable, recvbuf, count, datatype);

        /* check whether the actual send buffer is defined. */
        if (MPI_IN_PLACE == sendbuf) {
            memchecker_call(&opal_memchecker_base_isdefined, recvbuf, count, datatype);
        } else {
            memchecker_call(&opal_memchecker_base_isdefined, sendbuf, count, datatype);
        }
    );

    if (MPI_PARAM_CHECK) {
        char *msg;

        /* Unrooted operation -- same checks for all ranks on both
           intracommunicators and intercommunicators */

        err = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM,
                                          FUNC_NAME);
        } else if (MPI_OP_NULL == op) {
            err = MPI_ERR_OP;
        } else if (!ompi_op_is_valid(op, datatype, &msg, FUNC_NAME)) {
            int ret = OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OP, msg);
            free(msg);
            return ret;
        } else if( MPI_IN_PLACE == recvbuf ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_BUFFER,
                                          FUNC_NAME);
        } else if( (sendbuf == recvbuf) &&
                   (MPI_BOTTOM != sendbuf) &&
                   (count > 1) ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_BUFFER,
                                          FUNC_NAME);
        } else {
            OMPI_CHECK_DATATYPE_FOR_SEND(err, datatype, count);
        }
        OMPI_ERRHANDLER_CHECK(err, comm, err, FUNC_NAME);
    }

    OPAL_CR_ENTER_LIBRARY();

    /* Invoke the coll component to perform the back-end operation */

    OBJ_RETAIN(op);
    /* XXX -- CONST -- do not cast away const -- update mca/coll */
    err = comm->c_coll.coll_iallreduce((void *) sendbuf, recvbuf, count, datatype,
                                       op, comm, request, comm->c_coll.coll_iallreduce_module);
    OBJ_RELEASE(op);
    OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}


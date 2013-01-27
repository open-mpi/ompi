/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/info/info.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "orte/mca/snapc/snapc.h"

#include "ompi/mpiext/cr/c/mpiext_cr_c.h"

static const char FUNC_NAME[] = "OMPI_CR_Checkpoint";
#define HANDLE_SIZE_MAX 256

int OMPI_CR_Checkpoint(char **handle, int *seq, MPI_Info *info)
{
    int ret = MPI_SUCCESS;
    MPI_Comm comm = MPI_COMM_WORLD;
    orte_snapc_base_request_op_t *datum = NULL;
    int state = 0;
    int my_rank;

    /* argument checking */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /*
     * Setup the data structure for the operation
     */
    datum = OBJ_NEW(orte_snapc_base_request_op_t);
    datum->event = ORTE_SNAPC_OP_CHECKPOINT;
    datum->is_active = true;

    MPI_Comm_rank(comm, &my_rank);
    if( 0 == my_rank ) {
        datum->leader = OMPI_PROC_MY_NAME->vpid;
    } else {
        datum->leader = -1; /* Unknown from non-root ranks */
    }

    /*
     * All processes must make this call before it can start
     */
    MPI_Barrier(comm);

    /*
     * Leader sends the request
     */
    OPAL_CR_ENTER_LIBRARY();
    ret = orte_snapc.request_op(datum);
    if( OMPI_SUCCESS != ret ) {
        OBJ_RELEASE(datum);
        OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OTHER, 
                               FUNC_NAME);
    }
    OPAL_CR_EXIT_LIBRARY();

    /*
     * Leader then sends out the commit message
     */
    if( datum->leader == (int)OMPI_PROC_MY_NAME->vpid ) {
        *handle = strdup(datum->global_handle);
        *seq = datum->seq_num;
        state = 0;
    } else {
        *handle = (char*)malloc(sizeof(char)*HANDLE_SIZE_MAX);
    }

    MPI_Bcast(&state, 1, MPI_INT, 0, comm);
    MPI_Bcast(seq,    1, MPI_INT, 0, comm);
    MPI_Bcast(*handle, HANDLE_SIZE_MAX, MPI_CHAR, 0, comm);

    datum->is_active = false;
    OBJ_RELEASE(datum);

    return ret;
}

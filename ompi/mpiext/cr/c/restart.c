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

static const char FUNC_NAME[] = "OMPI_CR_Restart";

int OMPI_CR_Restart(char *handle, int seq, MPI_Info *info)
{
    int ret = MPI_SUCCESS;
    MPI_Comm comm = MPI_COMM_WORLD;
    orte_snapc_base_request_op_t *datum = NULL;

    /* argument checking */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /*
     * Setup the data structure for the operation
     */
    datum = OBJ_NEW(orte_snapc_base_request_op_t);
    datum->event = ORTE_SNAPC_OP_RESTART;
    datum->is_active = true;

    /*
     * Restart is not collective, so the caller is the leader
     */
    datum->leader = OMPI_PROC_MY_NAME->vpid;
    datum->seq_num = seq;
    datum->global_handle = strdup(handle);

    /*
     * Leader sends the request
     */
    OPAL_CR_ENTER_LIBRARY();
    ret = orte_snapc.request_op(datum);
    if( OMPI_SUCCESS != ret ) {
        OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OTHER, 
                               FUNC_NAME);
    }
    OPAL_CR_EXIT_LIBRARY();

    datum->is_active = false;
    OBJ_RELEASE(datum);

    /********** If successful, should never reach this point (JJH) ******/

    return ret;
}

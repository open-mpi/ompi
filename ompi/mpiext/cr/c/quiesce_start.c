/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2011      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

static const char FUNC_NAME[] = "OMPI_CR_Quiesce_start";

int OMPI_CR_Quiesce_start(MPI_Comm commP, MPI_Info *info)
{
    int ret = MPI_SUCCESS;
    MPI_Comm comm = MPI_COMM_WORLD; /* Currently ignore provided comm */
    orte_snapc_base_request_op_t *datum = NULL;
    int my_rank;

    /* argument checking */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /*
     * Setup the data structure for the operation
     */
    datum = OBJ_NEW(orte_snapc_base_request_op_t);
    datum->event = ORTE_SNAPC_OP_QUIESCE_START;
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
    /*ret = ompi_crcp_base_quiesce_start(info);*/
    if( OMPI_SUCCESS != ret ) {
        OBJ_RELEASE(datum);
        OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_OTHER, 
                               FUNC_NAME);
    }

    OPAL_CR_EXIT_LIBRARY();

    datum->is_active = false;
    OBJ_RELEASE(datum);

    /*
     * (Old) info logic
     */
    /*ompi_info_set((ompi_info_t*)*info, "target", cur_datum.target_dir);*/

    return ret;
}

/*****************
 * Local Functions
 ******************/
#if 0
/* Info keys:
 *
 * - crs:
 *   none    = (Default) No CRS Service
 *   default = Whatever CRS service MPI chooses
 *   blcr    = BLCR
 *   self    = app level callbacks
 *
 * - cmdline:
 *   Command line to restart the process with.
 *   If empty, the user must manually enter it
 *
 * - target:
 *   Absolute path to the target directory.
 *
 * - handle:
 *   first   = Earliest checkpoint directory available
 *   last    = Most recent checkpoint directory available
 *   [global:local] = handle provided by the MPI library
 *
 * - restarting:
 *   0 = not restarting
 *   1 = restarting
 *
 * - checkpointing:
 *   0 = No need to prepare for checkpointing
 *   1 = MPI should prepare for checkpointing
 *
 * - inflight:
 *   default  = message
 *   message  = Drain inflight messages at the message level
 *   network  = Drain inflight messages at the network level (if possible)
 *
 * - user_space_mem:
 *   0 = Memory does not need to be managed
 *   1 = Memory must be in user space (i.e., not on network card
 *
 */
static int extract_info_into_datum(ompi_info_t *info, orte_snapc_base_quiesce_t *datum)
{
    int info_flag = false;
    int max_crs_len = 32;
    bool info_bool = false;
    char *info_char = NULL;

    info_char = (char *) malloc(sizeof(char) * (OPAL_PATH_MAX+1));

    /*
     * Key: crs
     */
    ompi_info_get(info, "crs", max_crs_len, info_char, &info_flag);
    if( info_flag) {
        datum->crs_name = strdup(info_char);
    }

    /*
     * Key: cmdline
     */
    ompi_info_get(info, "cmdline", OPAL_PATH_MAX, info_char, &info_flag);
    if( info_flag) {
        datum->cmdline = strdup(info_char);
    }

    /*
     * Key: handle
     */
    ompi_info_get(info, "handle", OPAL_PATH_MAX, info_char, &info_flag);
    if( info_flag) {
        datum->handle = strdup(info_char);
    }

    /*
     * Key: target
     */
    ompi_info_get(info, "target", OPAL_PATH_MAX, info_char, &info_flag);
    if( info_flag) {
        datum->target_dir = strdup(info_char);
    }

    /*
     * Key: restarting
     */
    ompi_info_get_bool(info, "restarting", &info_bool, &info_flag);
    if( info_flag ) {
        datum->restarting = info_bool;
    } else {
        datum->restarting = false;
    }

    /*
     * Key: checkpointing
     */
    ompi_info_get_bool(info, "checkpointing", &info_bool, &info_flag);
    if( info_flag ) {
        datum->checkpointing = info_bool;
    } else {
        datum->checkpointing = false;
    }

    /*
     * Display all values
     */
    OPAL_OUTPUT_VERBOSE((3, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s extract_info: Info('crs' = '%s')",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                         (NULL == datum->crs_name ? "Default (none)" : datum->crs_name)));
    OPAL_OUTPUT_VERBOSE((3, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s extract_info: Info('cmdline' = '%s')",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                         (NULL == datum->cmdline ? "Default ()" : datum->cmdline)));
    OPAL_OUTPUT_VERBOSE((3, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s extract_info: Info('checkpointing' = '%c')",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                         (datum->checkpointing ? 'T' : 'F')));
    OPAL_OUTPUT_VERBOSE((3, mca_crcp_bkmrk_component.super.output_handle,
                         "crcp:bkmrk: %s extract_info: Info('restarting' = '%c')",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                         (datum->restarting ? 'T' : 'F')));

    if( NULL != info_char ) {
        free(info_char);
        info_char = NULL;
    }

    return OMPI_SUCCESS;
}
#endif

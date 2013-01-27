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

static const char FUNC_NAME[] = "OMPI_CR_Migrate";

int OMPI_CR_Migrate(MPI_Comm comm, char *hostname, int rank, MPI_Info *info)
{
    int ret = MPI_SUCCESS;
    orte_snapc_base_request_op_t *datum = NULL;
    int my_rank, my_size, i;
    char loc_hostname[MPI_MAX_PROCESSOR_NAME];
    int my_vpid;
    int info_flag;
    char info_value[6];
    int  my_off_node = (int)false;

    /* argument checking */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /*
     * Setup the data structure for the operation
     */
    datum = OBJ_NEW(orte_snapc_base_request_op_t);
    datum->event = ORTE_SNAPC_OP_MIGRATE;
    datum->is_active = true;

    MPI_Comm_rank(comm, &my_rank);
    MPI_Comm_size(comm, &my_size);
    if( 0 == my_rank ) {
        datum->leader = OMPI_PROC_MY_NAME->vpid;
    } else {
        datum->leader = -1; /* Unknown from non-root ranks */
    }

    /*
     * Gather all preferences to the root
     */
    if( NULL == hostname ) {
        loc_hostname[0] = '\0';
    } else {
        strncpy(loc_hostname, hostname, strlen(hostname));
        loc_hostname[strlen(hostname)] = '\0';
    }
    my_vpid = (int) OMPI_PROC_MY_NAME->vpid;

    if( 0 == my_rank ) {
        datum->mig_num = my_size;
        datum->mig_vpids = (int *) malloc(sizeof(int) * my_size);
        datum->mig_host_pref = (char (*)[OPAL_MAX_PROCESSOR_NAME]) malloc(sizeof(char) * my_size * MPI_MAX_PROCESSOR_NAME);
        datum->mig_vpid_pref = (int *) malloc(sizeof(int) * my_size);
        datum->mig_off_node  = (int *) malloc(sizeof(int) * my_size);

        for( i = 0; i < my_size; ++i ) {
            (datum->mig_vpids)[i] = 0;
            (datum->mig_host_pref)[i][0] = '\0';
            (datum->mig_vpid_pref)[i] = 0;
            (datum->mig_off_node)[i] = (int)false;
        }
    }

    my_off_node = (int)false;
    if( NULL != info ) {
        MPI_Info_get(*info, "CR_OFF_NODE", 5, info_value, &info_flag);
        if( info_flag ) {
            if( 0 == strncmp(info_value, "true", strlen("true")) ) {
                my_off_node = (int)true;
            }
        }
    }

    MPI_Gather(&my_vpid, 1, MPI_INT,
               (datum->mig_vpids), 1, MPI_INT, 0, comm);
    MPI_Gather(loc_hostname, MPI_MAX_PROCESSOR_NAME, MPI_CHAR,
               (datum->mig_host_pref), MPI_MAX_PROCESSOR_NAME, MPI_CHAR, 0, comm);
    MPI_Gather(&my_vpid, 1, MPI_INT,
               (datum->mig_vpid_pref), 1, MPI_INT, 0, comm);
    MPI_Gather(&my_off_node, 1, MPI_INT,
               (datum->mig_off_node), 1, MPI_INT, 0, comm);

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

    /*
     * All processes must sync before leaving
     */
    MPI_Barrier(comm);

    return ret;
}

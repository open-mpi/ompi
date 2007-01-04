/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/rds/base/rds_private.h"

static bool recv_issued=false;

int orte_rds_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_RDS,
                                                      ORTE_RML_PERSISTENT,
                                                      orte_rds_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = true;
    
    return rc;
}


int orte_rds_base_comm_stop(void)
{
    int rc;

    if (!recv_issued) {
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_RDS))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = false;

    return rc;
}



/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

void orte_rds_base_recv(int status, orte_process_name_t* sender,
                        orte_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    orte_buffer_t answer;
    orte_rds_cmd_flag_t command;
    orte_jobid_t job;
    orte_std_cntr_t count;
    int rc;

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &command, &count, ORTE_RDS_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    OBJ_CONSTRUCT(&answer, orte_buffer_t);

    switch (command) {
        case ORTE_RDS_QUERY_CMD:
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buffer, &job, &count, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
                
                if (ORTE_SUCCESS != (rc = orte_dss.pack(&answer, &command, 1, ORTE_RDS_CMD))) {
                ORTE_ERROR_LOG(rc);
            }

            if (ORTE_SUCCESS != (rc = orte_rds_base_query(job))) {
                ORTE_ERROR_LOG(rc);
            }

            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            }
            break;
            
       default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }
    
    /* cleanup */
    OBJ_DESTRUCT(&answer);
}


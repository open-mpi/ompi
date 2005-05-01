/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "ompi_config.h"

#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>

#include "util/output.h"

#include "mca/gpr/gpr_types.h"
#include "mca/ns/ns_types.h"
#include "mca/rmgr/rmgr_types.h"

#include "dps/dps_internal.h"

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h> 
#endif

int orte_dps_pack_name(orte_buffer_t *buffer,
                       orte_process_name_t *proc,
                       size_t num_vals)
{
    orte_cellid_t cellid;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    size_t i;
    int rc;
    
    for (i=0; i<num_vals; i++) {
        if (ORTE_SUCCESS != (rc = orte_ns.get_cellid(&cellid, proc))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_dps_pack_cellid(buffer,
                        &cellid, 1))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, proc))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_dps_pack_jobid(buffer,
                        &jobid, 1))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.get_vpid(&vpid, proc))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_dps_pack_vpid(buffer,
                        &vpid, 1))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    return ORTE_SUCCESS;
}

int orte_dps_pack_cellid(orte_buffer_t *buffer,
                         orte_cellid_t *cellid,
                         size_t num_vals)
{
    orte_data_type_t real_type;
    int rc;
    
    real_type = orte_dps_real_type(ORTE_CELLID);
    if (ORTE_SUCCESS != (rc = orte_dps_pack_native_types(buffer,
                (void*)cellid, num_vals, real_type))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_dps_pack_jobid(orte_buffer_t *buffer,
                         orte_jobid_t *jobid,
                         size_t num_vals)
{
    orte_data_type_t real_type;
    int rc;
    
    real_type = orte_dps_real_type(ORTE_JOBID);
    if (ORTE_SUCCESS != (rc = orte_dps_pack_native_types(buffer,
                (void*)jobid, num_vals, real_type))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_dps_pack_vpid(orte_buffer_t *buffer,
                         orte_vpid_t *vpid,
                         size_t num_vals)
{
    orte_data_type_t real_type;
    int rc;
    
    real_type = orte_dps_real_type(ORTE_VPID);
    if (ORTE_SUCCESS != (rc = orte_dps_pack_native_types(buffer,
                (void*)vpid, num_vals, real_type))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_dps_pack_node_state(orte_buffer_t *buffer,
                             orte_node_state_t *state,
                             size_t num_vals)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dps_pack_native_types(buffer,
                (void*)state, num_vals, ORTE_UINT8))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_dps_pack_proc_state(orte_buffer_t *buffer,
                             orte_proc_state_t *state,
                             size_t num_vals)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dps_pack_native_types(buffer,
                (void*)state, num_vals, ORTE_UINT8))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_dps_pack_gpr_addr_mode(orte_buffer_t *buffer,
                                orte_gpr_addr_mode_t *mode,
                                size_t num_vals)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dps_pack_native_types(buffer,
                (void*)mode, num_vals, ORTE_UINT16))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_dps_pack_gpr_cmd(orte_buffer_t *buffer,
                          orte_gpr_cmd_t *cmd,
                          size_t num_vals)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dps_pack_native_types(buffer,
                (void*)cmd, num_vals, ORTE_UINT16))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_dps_pack_notify_action(orte_buffer_t *buffer,
                                orte_notify_action_t *action,
                                size_t num_vals)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dps_pack_native_types(buffer,
                (void*)action, num_vals, ORTE_UINT16))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


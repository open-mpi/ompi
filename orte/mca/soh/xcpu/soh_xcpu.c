/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <pwd.h>
#include <grp.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/soh/base/base.h"
#include "orte/mca/soh/xcpu/soh_xcpu.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/base/rmaps_base_map.h"
#include "opal/util/output.h"

static int orte_soh_xcpu_begin_monitoring_job(orte_jobid_t);
static int orte_soh_xcpu_finalize(void);

int orte_soh_xcpu_module_init(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_ns.get_cellid(&mca_soh_xcpu_component.cellid, orte_process_info.my_name))) {
        fprintf(stderr, "orte_soh_xcpu_module_init error\n");
    	ORTE_ERROR_LOG(rc);
    	return rc;
    }

    return ORTE_SUCCESS;
}   

orte_soh_base_module_t orte_soh_xcpu_module = {
    orte_soh_base_get_proc_soh,
    orte_soh_base_set_proc_soh,
    orte_soh_base_get_node_soh_not_available,
    orte_soh_base_set_node_soh_not_available,
    orte_soh_base_get_job_soh,
    orte_soh_base_set_job_soh,
    orte_soh_xcpu_begin_monitoring_job,
    orte_soh_xcpu_finalize
};

/* @begin_monitoring: right now, its only trying to update registry so
 * that mpirun can exit normally
 * pls_xcpu is waiting for all threads to finish before calling this function
 */
static int orte_soh_xcpu_begin_monitoring_job(orte_jobid_t jobid){
    int rc;
    size_t num_procs, i;
    orte_process_name_t *peers;
    
    if (ORTE_SUCCESS != (rc = orte_ns.get_job_peers(&peers, &num_procs, jobid))) {
        ORTE_ERROR_LOG(rc);
    }else    
    for (i=0; i < num_procs; i++) {
        if (ORTE_SUCCESS != (rc = orte_soh_base_set_proc_soh(&peers[i], ORTE_PROC_STATE_TERMINATED, 0)) ) {
            ORTE_ERROR_LOG(rc);
            break;
        }
    }
    free(peers);
    
    return rc;
}

static int orte_soh_xcpu_finalize(void)
{
    return ORTE_SUCCESS;
}

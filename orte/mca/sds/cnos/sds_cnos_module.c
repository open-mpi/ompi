/*
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
 *
 */

#include "orte_config.h"

#include <catamount/cnos_mpi_os.h>

#include "orte/orte_constants.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/sds/cnos/sds_cnos.h"
#include "orte/util/proc_info.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/errmgr/base/base.h"

orte_sds_base_module_t orte_sds_cnos_module = {
    orte_sds_cnos_contact_universe,
    orte_sds_cnos_set_name,
    orte_sds_cnos_finalize,
};

int
orte_sds_cnos_contact_universe(void)
{
    orte_process_info.seed = false;

    /* since we are seed, ensure that all replica info is NULL'd */
    if (NULL != orte_process_info.ns_replica_uri) {
	free(orte_process_info.ns_replica_uri);
	orte_process_info.ns_replica_uri = NULL;
    }
    if (NULL != orte_process_info.ns_replica) {
	free(orte_process_info.ns_replica);
	orte_process_info.ns_replica = NULL;
    }

    if (NULL != orte_process_info.gpr_replica_uri) {
	free(orte_process_info.gpr_replica_uri);
	orte_process_info.gpr_replica_uri = NULL;
    }
    if (NULL != orte_process_info.gpr_replica) {
	free(orte_process_info.gpr_replica);
	orte_process_info.gpr_replica = NULL;
    }

    return ORTE_SUCCESS;
}

int
orte_sds_cnos_set_name(void)
{
    int rc;
    orte_cellid_t cellid;
    orte_jobid_t jobid;
    orte_vpid_t vpid;

    /* Get our process information
     *
     * we're going to make up the cellid and jobid.  find our vpid,
     * assuming range starts at 0 
     */
    cellid = 0;
    jobid = 1; /* not 0, since it has special meaning */
    vpid = (orte_vpid_t) cnos_get_rank();
    if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&(orte_process_info.my_name),
                                                          cellid,
                                                          jobid,
                                                          vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    orte_process_info.vpid_start = (orte_vpid_t) 0;
    orte_process_info.num_procs = (size_t) cnos_get_size();

    return ORTE_SUCCESS;
}


int 
orte_sds_cnos_finalize(void)
{
    return ORTE_SUCCESS;
}

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
 * Copyright (c) 2008      UT-Battelle, LLC
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
#include "orte/mca/sds/alps/sds_alps.h"
#include "orte/util/proc_info.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/errmgr/base/base.h"

orte_sds_base_module_t orte_sds_alps_module = {
    orte_sds_base_basic_contact_universe,
    orte_sds_alps_set_name,
    orte_sds_alps_finalize,
};

int
orte_sds_alps_set_name(void)
{
    int rc;
    orte_jobid_t jobid;
    orte_vpid_t vpid;

    if(orte_process_info.seed) { 
        if (ORTE_SUCCESS != (rc = orte_ns.create_my_name())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        orte_process_info.num_procs = 1;

        return rc;
    }

    /* Get our process information
     *
     * we're going to make up the jobid.  find our vpid,
     * assuming range starts at 0 
     */
    jobid = 0; /* not 0, since it has special meaning */
    

    vpid = (orte_vpid_t) cnos_get_rank() + 1;
    
    if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&(orte_process_info.my_name),
                                                          jobid,
                                                          vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    orte_process_info.num_procs = (orte_std_cntr_t) cnos_get_size();

    return ORTE_SUCCESS;
}


int 
orte_sds_alps_finalize(void)
{
    return ORTE_SUCCESS;
}

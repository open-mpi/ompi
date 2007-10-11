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

#include "orte/orte_constants.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/sds/singleton/sds_singleton.h"
#include "orte/util/proc_info.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/base/base.h"
#include "opal/util/output.h"


orte_sds_base_module_t orte_sds_singleton_module = {
    orte_sds_base_basic_contact_universe,
    orte_sds_singleton_set_name,
    orte_sds_singleton_finalize,
};


int
orte_sds_singleton_set_name(void)
{
    int rc;

    /*
     * If we do not have a name at this point, then ask for one.
     */
    if( NULL == ORTE_PROC_MY_NAME ) {
        if (ORTE_SUCCESS != (rc = orte_ns.create_my_name())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    orte_process_info.num_procs = 1;
    /* since we are a singleton, then we must have a local_rank of 0
     * and only 1 local process
     */
    orte_process_info.local_rank = 0;
    orte_process_info.num_local_procs = 1;
    
    return ORTE_SUCCESS;
}


int 
orte_sds_singleton_finalize(void)
{
    return ORTE_SUCCESS;
}

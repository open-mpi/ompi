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
    int rc, id, flag;
    orte_vpid_t vpid;

    if (ORTE_SUCCESS != (rc = orte_ns.create_my_name())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    vpid = ORTE_PROC_MY_NAME->vpid;
    
    orte_process_info.num_procs = 1;
    orte_process_info.vpid_start = vpid;
    /* only set the singleton flag is we are NOT infrastructure, 
       and it has not been previously set. */
    id = mca_base_param_find("orte", NULL, "infrastructure");
    mca_base_param_lookup_int(id, &flag);
    if (!flag) {
        orte_process_info.singleton = true;
    }
    
    return ORTE_SUCCESS;
}


int 
orte_sds_singleton_finalize(void)
{
    return ORTE_SUCCESS;
}

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
 */

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <errno.h>

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/orte_constants.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/base/base.h"

int orte_sds_env_get(void)
{
    int vpid_start;
    int num_procs;
    int local_rank;
    int num_local_procs;
    int id;

    id = mca_base_param_register_int("ns", "nds", "vpid_start", NULL, -1);
    mca_base_param_lookup_int(id, &vpid_start);
    if (vpid_start < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    orte_process_info.vpid_start = (orte_vpid_t)vpid_start;
    
    id = mca_base_param_register_int("ns", "nds", "num_procs", NULL, -1);
    mca_base_param_lookup_int(id, &num_procs);
    if (num_procs < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    orte_process_info.num_procs = (orte_std_cntr_t)num_procs;
    
    /* it is okay for this param not to be found - for example, we don't bother
     * to set it for orteds - so just set it to an invalid value which indicates
     * it wasn't found if it isn't there
     */
    id = mca_base_param_register_int("ns", "nds", "local_rank", NULL, ORTE_VPID_INVALID);
    mca_base_param_lookup_int(id, &local_rank);
    orte_process_info.local_rank = (orte_vpid_t)local_rank;
    
    /* it is okay for this param not to be found - for example, we don't bother
     * to set it for orteds - so just set it to a value which indicates
     * it wasn't found if it isn't there
     */
    id = mca_base_param_register_int("ns", "nds", "num_local_procs", NULL, 0);
    mca_base_param_lookup_int(id, &num_local_procs);
    orte_process_info.num_local_procs = (orte_std_cntr_t)num_local_procs;
        
    return ORTE_SUCCESS;
}

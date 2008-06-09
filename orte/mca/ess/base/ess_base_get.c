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
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <errno.h>

#include "opal/util/opal_environ.h"
#include "orte/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"

#include "orte/mca/ess/base/base.h"

int orte_ess_env_get(void)
{
    int num_procs;

    mca_base_param_reg_int_name("orte", "ess_num_procs",
                                "Used to discover the number of procs in the job",
                                true, false, -1, &num_procs);
    
    if (num_procs < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    orte_process_info.num_procs = (orte_std_cntr_t)num_procs;
    
    /* it is okay for this param not to be found - for example, we don't bother
     * to set it for orteds - so just set it to an invalid value which indicates
     * it wasn't found if it isn't there
     */
    mca_base_param_reg_int_name("orte", "ess_local_rank",
                                "Used to discover the local rank of a process on a node",
                                true, false, (int)ORTE_VPID_INVALID, &num_procs);
    orte_process_info.local_rank = (orte_vpid_t)num_procs;
    
    return ORTE_SUCCESS;
}

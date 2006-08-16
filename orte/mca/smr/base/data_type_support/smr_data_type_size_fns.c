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
 
#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/smr/base/smr_private.h"

/*
 * STANDARD SIZE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_smr_base_std_size(size_t *size, void *src, orte_data_type_t type)
{
    switch(type) {
        case ORTE_PROC_STATE:
            *size = sizeof(orte_proc_state_t);
            break;
        
        case ORTE_JOB_STATE:
            *size = sizeof(orte_job_state_t);
            break;
            
        case ORTE_NODE_STATE:
            *size = sizeof(orte_node_state_t);
            break;
            
        case ORTE_EXIT_CODE:
            *size = sizeof(orte_exit_code_t);
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    return ORTE_SUCCESS;
}


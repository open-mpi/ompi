/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
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

#include "opal/util/output.h"
#include "opal/mca/paffinity/paffinity.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/base/base.h"

opal_paffinity_locality_t orte_ess_base_proc_get_locality(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;

    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return OPAL_PROC_NON_LOCAL;
    }
    
    return pmap->locality;   
}

orte_vpid_t orte_ess_base_proc_get_daemon(orte_process_name_t *proc)
{
    orte_nid_t *nid;

    if( ORTE_JOBID_IS_DAEMON(proc->jobid) ) {
        return proc->vpid;
    }

    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        return ORTE_VPID_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s is hosted by daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         ORTE_VPID_PRINT(nid->daemon)));
    
    return nid->daemon;
}

char* orte_ess_base_proc_get_hostname(orte_process_name_t *proc)
{
    orte_nid_t *nid;
        
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nid->name));
    
    return nid->name;
}

orte_local_rank_t orte_ess_base_proc_get_local_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_LOCAL_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->local_rank));
    
    return pmap->local_rank;
}

orte_node_rank_t orte_ess_base_proc_get_node_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    orte_ns_cmp_bitmask_t mask;

    mask = ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
    
    /* is this me? */
    if (OPAL_EQUAL == orte_util_compare_name_fields(mask, proc, ORTE_PROC_MY_NAME)) {
        /* yes it is - reply with my rank. This is necessary
         * because the pidmap will not have arrived when I
         * am starting up, and if we use static ports, then
         * I need to know my node rank during init
         */
        return orte_process_info.my_node_rank;
    }
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        return ORTE_NODE_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->node_rank));
    
    return pmap->node_rank;
}

int orte_ess_base_update_pidmap(opal_byte_object_t *bo)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:base: updating pidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* build the pmap */
    if (ORTE_SUCCESS != (ret = orte_util_decode_pidmap(bo))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

int orte_ess_base_update_nidmap(opal_byte_object_t *bo)
{
    int rc;
    /* decode the nidmap - the util will know what to do */
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo))) {
        ORTE_ERROR_LOG(rc);
    }    
    return rc;
}


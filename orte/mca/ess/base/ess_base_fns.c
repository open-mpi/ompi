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
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
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

#include "opal/util/output.h"
#include "opal/mca/paffinity/paffinity.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/base/base.h"

static orte_proc_t* find_proc(orte_process_name_t *proc)  /* used by daemons */
{
    orte_job_t *jdata;
    
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }

    return (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);
}

opal_paffinity_locality_t orte_ess_base_proc_get_locality(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;

    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                             "%s LOOKING FOR PROC %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return OPAL_PROC_NON_LOCAL;
    }
    
    return pmap->locality;   
}

orte_vpid_t orte_ess_base_proc_get_daemon(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    orte_proc_t *pdata;
    orte_vpid_t vpid;

    if (NULL == proc) {
        return ORTE_VPID_INVALID;
    }

    if (ORTE_JOBID_IS_DAEMON(proc->jobid)) {
        return proc->vpid;
    }

    if (ORTE_PROC_IS_APP) {
        if (NULL == (nid = orte_util_lookup_nid(proc))) {
            return ORTE_VPID_INVALID;
        }
        vpid = nid->daemon;
    } else {
        /* get the job data */
        if (NULL == (pdata = find_proc(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_VPID_INVALID;
        }
        
        if (NULL == pdata->node || NULL == pdata->node->daemon) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_VPID_INVALID;
        }
        vpid = pdata->node->daemon->name.vpid;
    }

    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s is hosted by daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         ORTE_VPID_PRINT(vpid)));
    
    return vpid;
}

char* orte_ess_base_proc_get_hostname(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    orte_proc_t *pdata;
    char *hostname;

    if (NULL == proc) {
        return NULL;
    }

    if (ORTE_PROC_IS_APP) {
        if (NULL == (nid = orte_util_lookup_nid(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                                 "%s LOOKING FOR PROC %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(proc)));
            return NULL;
        }
        hostname = nid->name;
    } else {
        if (NULL == (pdata = find_proc(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return NULL;
        }
        hostname = pdata->node->name;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         hostname));
    
    return hostname;
}

orte_local_rank_t orte_ess_base_proc_get_local_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    orte_proc_t *pdata;
    orte_local_rank_t lrank;

    if (NULL == proc) {
        return ORTE_LOCAL_RANK_INVALID;
    }

    if (ORTE_PROC_IS_APP) {
        if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_LOCAL_RANK_INVALID;
        }
        lrank = pmap->local_rank;
    } else {
        if (NULL == (pdata = find_proc(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_LOCAL_RANK_INVALID;
        }
        lrank = pdata->local_rank;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)lrank));
    
    return lrank;
}

orte_node_rank_t orte_ess_base_proc_get_node_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    orte_proc_t *pdata;
    orte_node_rank_t nrank;

    if (NULL == proc) {
        return ORTE_NODE_RANK_INVALID;
    }

    if (ORTE_PROC_IS_APP) {
        /* is this me? */
        if (proc->jobid == ORTE_PROC_MY_NAME->jobid &&
            proc->vpid == ORTE_PROC_MY_NAME->vpid) {
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
        nrank = pmap->node_rank;
    } else {
        if (NULL == (pdata = find_proc(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_NODE_RANK_INVALID;
        }
        nrank = pdata->node_rank;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)nrank));
    
    return nrank;
}

int orte_ess_base_update_pidmap(opal_byte_object_t *bo)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:base: updating pidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* build the pmap */
    if (ORTE_PROC_IS_APP) {
        if (ORTE_SUCCESS != (ret = orte_util_decode_pidmap(bo))) {
            ORTE_ERROR_LOG(ret);
        }
    } else {
        if (ORTE_SUCCESS != (ret = orte_util_decode_daemon_pidmap(bo))) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    return ret;
}

int orte_ess_base_update_nidmap(opal_byte_object_t *bo)
{
    int rc;

    /* decode the nidmap - the util will know what to do */
    if (ORTE_PROC_IS_APP) {
        if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo))) {
            ORTE_ERROR_LOG(rc);
        }
    } else {
        if (ORTE_SUCCESS != (rc = orte_util_decode_daemon_nodemap(bo))) {
            ORTE_ERROR_LOG(rc);
        }
    }

    return rc;
}


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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_value_array.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/base/base.h"

int orte_ess_base_build_nidmap(opal_buffer_t *buffer,
                               opal_pointer_array_t *nidmap,
                               opal_value_array_t *pmap, orte_vpid_t *num_procs)
{
    int rc;
    opal_byte_object_t *bo;
    int32_t cnt;
   
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:build:nidmap: received buffer with %ld bytes",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)buffer->bytes_used));

    /* it is okay if the buffer is empty - could be a non-MPI proc */
    if (0 == buffer->bytes_used) {
        return ORTE_SUCCESS;
    }
    
    /* extract the byte object holding the daemonmap */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the node map */
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo, nidmap))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* the bytes in the object were free'd by the decode */
    
    /* extract the byte object holding the process map */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the process map */
    if (ORTE_SUCCESS != (rc = orte_util_decode_pidmap(bo, num_procs,
                                                      pmap, NULL, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* the bytes in the object were free'd by the decode */
                         
    return ORTE_SUCCESS;
}

orte_pmap_t* orte_ess_base_lookup_pmap(opal_pointer_array_t *jobmap, orte_process_name_t *proc)
{
    int i;
    orte_jmap_t **jmaps;
    orte_pmap_t *pmap;
    
    jmaps = (orte_jmap_t**)jobmap->addr;
    for (i=0; i < jobmap->size && NULL != jmaps[i]; i++) {
        if (proc->jobid == jmaps[i]->job) {
            pmap = (orte_pmap_t*)opal_value_array_get_item(&jmaps[i]->pmap, proc->vpid);
            return pmap;
        }
    }
    
    return NULL;
}

/* the daemon's vpid does not necessarily correlate
 * to the node's index in the node array since
 * some nodes may not have a daemon on them. Thus,
 * we have to search for the daemon in the array.
 * Fortunately, this is rarely done
 */
static orte_nid_t* find_daemon_node(opal_pointer_array_t *nidmap,
                                    orte_process_name_t *proc)
{
    int32_t i;
    orte_nid_t **nids;
    
    nids = (orte_nid_t**)nidmap->addr;
    for (i=0; i < nidmap->size && NULL != nids[i]; i++) {
        if (nids[i]->daemon == proc->vpid) {
            return nids[i];
        }
    }
    
    return NULL;
}

orte_nid_t* orte_ess_base_lookup_nid(opal_pointer_array_t *nidmap,
                                     opal_pointer_array_t *jobmap,
                                     orte_process_name_t *proc)
{
    orte_nid_t *nid;
    orte_nid_t **nids;
    orte_pmap_t *pmap;
    
    if (ORTE_PROC_IS_DAEMON(proc->jobid)) {
        if (ORTE_JOB_FAMILY(proc->jobid) !=
            ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid)) {
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
            return NULL;
        }
        /* looking for a daemon in my family */
        if (NULL == (nid = find_daemon_node(nidmap, proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        }
        return nid;
    }
    
    /* looking for an application proc */
    if (NULL == (pmap = orte_ess_base_lookup_pmap(jobmap, proc))) {
        opal_output(0, "proc: %s not found", ORTE_NAME_PRINT(proc));
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }
    
    if (nidmap->size < pmap->node ||
        pmap->node < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        return NULL;
    }
    
    nids = (orte_nid_t**)nidmap->addr;
    return nids[pmap->node];
}


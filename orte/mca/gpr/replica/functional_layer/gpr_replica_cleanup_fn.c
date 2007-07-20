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
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "orte/class/orte_pointer_array.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "orte/util/proc_info.h"

#include "orte/mca/ns/ns.h"

#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"

#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"


int orte_gpr_replica_cleanup_job_fn(orte_jobid_t jobid)
{
    int rc;
    char *jobidstring, *segment;
    orte_gpr_replica_segment_t *seg;
    
    OPAL_TRACE(2);
    
    if (ORTE_SUCCESS != orte_ns.convert_jobid_to_string(&jobidstring, jobid)) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    asprintf(&segment, "%s-%s", ORTE_JOB_SEGMENT, jobidstring);
    
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, false, segment))) {
        return rc;
    }
    
    /* delete the associated job segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_release_segment(&seg))) {
        return rc;
    }
    
    return ORTE_SUCCESS;
}


int orte_gpr_replica_cleanup_proc_fn(orte_process_name_t *proc)
{
    orte_gpr_replica_segment_t *seg, **seg2;
    orte_gpr_replica_container_t **cptr, *cptr2;
    orte_gpr_replica_itag_t itag;
    char *procname, *segment, *jobidstring;
    orte_jobid_t jobid;
    int rc;
    orte_std_cntr_t i, j;

    OPAL_TRACE(2);
    
	if (orte_gpr_replica_globals.debug) {
		opal_output(0, "[%lu,%lu,%lu] gpr_replica_cleanup_proc: function entered for process [%lu,%lu,%lu]",
					ORTE_NAME_ARGS(orte_process_info.my_name), ORTE_NAME_ARGS(proc));
	}
	
    if (ORTE_SUCCESS != (rc = orte_ns.get_proc_name_string(&procname, proc))) {
        return rc;
    }

    /* find the job segment */
    jobid = proc->jobid;
    
    if (ORTE_SUCCESS != orte_ns.convert_jobid_to_string(&jobidstring, jobid)) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    asprintf(&segment, "%s-%s", ORTE_JOB_SEGMENT, jobidstring);
    
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, false, segment))) {
        return rc;
    }
    
    /* find the container for this proc */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_lookup(&itag, seg, procname))) {
        return rc;
    }
    cptr = (orte_gpr_replica_container_t**)((seg->containers)->addr);
    cptr2 = NULL;
    for (i=0; i < (seg->containers)->size && NULL == cptr2; i++) {
        if (NULL != cptr[i]) {
            for (j=0; j < cptr[i]->num_itags && NULL == cptr2; j++) {
                if (itag == cptr[i]->itags[j]) {
                    cptr2 = cptr[i];
                }
            }
        }
    }
    
    if (NULL == cptr2) { /* container not found */
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* remove the container */
    orte_gpr_replica_release_container(seg, cptr2);

    /* search all segments for this process name - remove all references
     */
    seg2 = (orte_gpr_replica_segment_t**)((orte_gpr_replica.segments)->addr);
    for (i=0; i < (orte_gpr_replica.segments)->size; i++) {
        if (NULL != seg2[i]) {
            if (ORTE_SUCCESS == orte_gpr_replica_dict_lookup(&itag, seg2[i], procname)) {
                if (ORTE_SUCCESS != (rc = orte_gpr_replica_purge_itag(seg2[i], itag))) {
                    return rc;
                }
            }
        }
    }

    return ORTE_SUCCESS;

}

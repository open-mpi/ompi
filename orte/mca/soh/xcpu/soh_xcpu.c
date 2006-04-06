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

#include <pwd.h>
#include <grp.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/soh/base/base.h"
#include "orte/mca/soh/xcpu/soh_xcpu.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/base/rmaps_base_map.h"
#include "opal/util/output.h"

static int orte_soh_xcpu_begin_monitoring_job(orte_jobid_t);
static int orte_soh_xcpu_finalize(void);

#if 0
static int update_registry(orte_jobid_t jobid, char *proc_name){
    orte_gpr_value_t *value;
    int rc;
    char *segment;
    orte_proc_state_t state;
    orte_job_state_t jstate;
    
    orte_schema.get_job_segment_name(&segment, jobid);
    /*fprintf(stdout, "soh_xcpu: segment: %s\n", segment);*/
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
                    segment, 3, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /*fprintf(stdout, "debug 1\n");*/
   
    
   if(ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&(value->tokens), &(value->num_tokens),
                         orte_process_info.my_name) ) ){
        ORTE_ERROR_LOG(rc);
     }
   /*
   if(ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&(value->tokens), &(value->num_tokens), mca_soh_xcpu_component.cellid,
                         proc_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
   }*/
   /*fprintf(stdout, "debug 1.1\n");*/
    state=ORTE_PROC_STATE_TERMINATED;
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_STATE_KEY, 
                    ORTE_PROC_STATE, &state))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
   /*fprintf(stdout, "debug 2\n");*/
   
   if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]), ORTE_PROC_EXIT_CODE_KEY, 
                    ORTE_INT, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return 0;
    }
   /*fprintf(stdout, "debug 3\n");*/
   jstate=ORTE_JOB_STATE_TERMINATED;
   if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[2]), ORTE_JOB_STATE_KEY, 
                    ORTE_JOB_STATE, &jstate))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return 0;
    }
  
   /*fprintf(stdout, "debug 4\n");*/
   
    if ((rc = orte_gpr.put(1, &value)) != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
    }
    /*fprintf(stdout, "debug 4\n");*/
    OBJ_RELEASE(value);
    /*fprintf(stdout, "soh_xcpu: registry updated\n");*/

    return ORTE_SUCCESS;
}
#endif
        
/*
static int do_update(){
    return 1;
    
}

static void orte_soh_xcpu_notify_handler(int fd, short flags, void *user)
{

}
*/

/**
 * Register a callback to receive xcpu update notifications
 */
int orte_soh_xcpu_module_init(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_ns.get_cellid(&mca_soh_xcpu_component.cellid, orte_process_info.my_name))) {
        fprintf(stderr, "orte_soh_xcpu_module_init error\n");
    	ORTE_ERROR_LOG(rc);
    	return rc;
    }

    return ORTE_SUCCESS;
}   /*
     * Set initial node status
     */
/*    if(!do_update()){
        fprintf(stderr, "do_update error\n");
    }
*/
    /*
     * Now regiser notify event
     */

/*`    mca_soh_xcpu_component.notify_fd = 0;*/ /*bproc_notifier();*/
/*
    memset(&mca_soh_xcpu_component.notify_event, 0, sizeof(opal_event_t));

    opal_event_set(
    	&mca_soh_xcpu_component.notify_event,
    	mca_soh_xcpu_component.notify_fd,
    	OPAL_EV_READ|OPAL_EV_PERSIST,
    	orte_soh_xcpu_notify_handler,
    	0);

    opal_event_add(&mca_soh_xcpu_component.notify_event, 0);

    return ORTE_SUCCESS;
}
*/
orte_soh_base_module_t orte_soh_xcpu_module = {
    orte_soh_base_get_proc_soh,
    orte_soh_base_set_proc_soh,
    orte_soh_base_get_node_soh_not_available,
    orte_soh_base_set_node_soh_not_available,
    orte_soh_base_get_job_soh,
    orte_soh_base_set_job_soh,
    orte_soh_xcpu_begin_monitoring_job,
    orte_soh_xcpu_finalize
};

/* begin monitoring right now only trying to update registry so
 * that mpirun can exit normally
 * pls_xcpu is waiting for all threads to finish before calling this function
 */
static int orte_soh_xcpu_begin_monitoring_job(orte_jobid_t jobid){


#if 0
    int rc, nprocs, i;
    opal_list_item_t *item, *temp;
    orte_rmaps_base_map_t* map;
    opal_list_t mapping;
    
    OBJ_CONSTRUCT(&mapping, opal_list_t);
    /* 1. get map from registry*/
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_map(jobid, &mapping))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    fprintf(stdout, "soh_xcpu: begin monitoring\n");
    if (ORTE_SUCCESS != (rc = orte_ns.get_cellid(&mca_soh_xcpu_component.cellid, orte_process_info.my_name))) {
        fprintf(stderr, "soh_xcpu: get_cell_id error\n");
        ORTE_ERROR_LOG(rc);
        return rc;
    }else    
        for(item =  opal_list_get_first(&mapping);
                item != opal_list_get_end(&mapping);
                item =  opal_list_get_next(item)) {
            map = (orte_rmaps_base_map_t*) item;
            
            for(temp = opal_list_get_first(&map->nodes);
                    temp != opal_list_get_end(&map->nodes);
                    temp = opal_list_get_next(temp)){
                
                nprocs=((orte_rmaps_base_node_t*)temp)->node_procs.opal_list_length;
                
                for (i = 0; i<nprocs; ++i) { 
                    /*fprintf(stdout, "%s\n", ((orte_rmaps_base_node_t*)temp)->node->node_name);*/
                    update_registry(((orte_rmaps_base_node_t*)temp)->node->node_name);
                }
            }
        }
#endif

    /** all you need to do is set the proc soh for all procs (not nodes) in the job */
    int rc;
    size_t num_procs, i;
    orte_process_name_t *peers;
    
    if (ORTE_SUCCESS != (rc = orte_ns.get_job_peers(&peers, &num_procs, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    for (i=0; i < num_procs; i++) {
    if (ORTE_SUCCESS != (rc = orte_soh_base_set_proc_soh(peers[i], ORTE_PROC_STATE_TERMINATED, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    free(peers);
    
    return ORTE_SUCCESS;
}

/**
 *  Cleanup
 */

static int orte_soh_xcpu_finalize(void)
{
    fprintf(stdout, "soh_xcpu: finalize\n");
   /* opal_event_del(&mca_soh_xcpu_component.notify_event);*/
    return ORTE_SUCCESS;
}

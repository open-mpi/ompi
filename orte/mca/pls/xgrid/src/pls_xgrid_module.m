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

#import "orte_config.h"

#import <stdlib.h>
#import <unistd.h>
#import <errno.h>
#import <string.h>
#import <sys/types.h>
#import <sys/stat.h>
#import <sys/wait.h>
#import <fcntl.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#import "orte/orte_constants.h"
#import "opal/util/argv.h"
#import "opal/util/output.h"
#import "orte/util/session_dir.h"
#import "opal/event/event.h"
#import "orte/runtime/orte_wait.h"
#import "orte/mca/ns/ns.h"
#import "orte/mca/pls/pls.h"
#import "orte/mca/rml/rml.h"
#import "orte/mca/gpr/gpr.h"
#import "orte/mca/errmgr/errmgr.h"
#import "orte/mca/rmgr/base/base.h"
#import "orte/mca/smr/smr.h"
#import "orte/mca/smr/base/base.h"
#import "pls_xgrid.h"

int orte_pls_xgrid_launch(orte_jobid_t jobid);
int orte_pls_xgrid_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
int orte_pls_xgrid_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
int orte_pls_xgrid_terminate_proc(const orte_process_name_t* proc);
int orte_pls_xgrid_signal_job(orte_jobid_t job, int32_t signal, opal_list_t *attrs);
int orte_pls_xgrid_signal_proc(const orte_process_name_t* proc_name, int32_t signal);
int orte_pls_xgrid_cancel_operation(void);
int orte_pls_xgrid_finalize(void);

orte_pls_base_module_1_3_0_t orte_pls_xgrid_module = {
    orte_pls_xgrid_launch,
    orte_pls_xgrid_terminate_job,
    orte_pls_xgrid_terminate_orteds,
    orte_pls_xgrid_terminate_proc,
    orte_pls_xgrid_signal_job,
    orte_pls_xgrid_signal_proc,
    orte_pls_xgrid_cancel_operation,
    orte_pls_xgrid_finalize
};


/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */
int
orte_pls_xgrid_launch(orte_jobid_t jobid)
{
    /* handled entirely within the client.  Can't just call directly
       because the client has to be an ObjC class */
    return [mca_pls_xgrid_component.client launchJob:jobid];
}


/**
 * Terminate all processes for a given job
 */
int
orte_pls_xgrid_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* order them to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_kill_local_procs(&daemons, jobid, timeout))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
CLEANUP:
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);

    if (ORTE_SUCCESS != rc) {
	rc = [mca_pls_xgrid_component.client terminateJob:jobid];
    }

    return rc;
}


/**
* Terminate the orteds for a given job
 */
int
orte_pls_xgrid_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_exit(&daemons, timeout))) {
        ORTE_ERROR_LOG(rc);
    }
    
CLEANUP:
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);

    if (ORTE_SUCCESS != rc) {
	rc = [mca_pls_xgrid_component.client terminateJob:jobid];
    }

    return rc;
}


/*
 * Terminate a specific process
 */
int
orte_pls_xgrid_terminate_proc(const orte_process_name_t* proc)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


int
orte_pls_xgrid_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    int rc;
    opal_list_t daemons;
    opal_list_item_t *item;
    
    /* construct the list of active daemons on this job */
    OBJ_CONSTRUCT(&daemons, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_pls_base_get_active_daemons(&daemons, jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&daemons);
        return rc;
    }
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_signal_local_procs(&daemons, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);
    return rc;
}


int
orte_pls_xgrid_signal_proc(const orte_process_name_t* proc, int32_t signal)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


int
orte_pls_xgrid_cancel_operation(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_cancel_operation())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


int
orte_pls_xgrid_finalize(void)
{
    [mca_pls_xgrid_component.client release];
    [mca_pls_xgrid_component.pool release];

    opal_progress_unregister(orte_pls_xgrid_progress);

    return ORTE_SUCCESS;
}


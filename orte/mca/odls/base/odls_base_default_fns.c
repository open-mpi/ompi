/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif

#include <signal.h>

#include "opal/util/opal_environ.h"
#include "opal/util/argv.h"
#include "opal/util/os_path.h"
#include "opal/util/sys_limits.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/pstat/pstat.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/iof_base_setup.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/grpcomm/grpcomm.h"

#include "orte/util/context_fns.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/proc_info.h"
#include "orte/util/nidmap.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/orted/orted.h"

#if OPAL_ENABLE_FT == 1
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#endif

#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"

/* IT IS CRITICAL THAT ANY CHANGE IN THE ORDER OF THE INFO PACKED IN
 * THIS FUNCTION BE REFLECTED IN THE CONSTRUCT_CHILD_LIST PARSER BELOW
*/
int orte_odls_base_default_get_add_procs_data(opal_buffer_t *data,
                                              orte_jobid_t job)
{
    int rc;
    orte_job_t *jdata;
    orte_proc_t **procs;
    orte_job_map_t *map;
    opal_buffer_t *wireup;
    opal_byte_object_t bo, *boptr;
    int32_t numbytes;
    int8_t flag;
    int8_t *tmp;
    orte_vpid_t i;
    orte_daemon_cmd_flag_t command;

    /* get the job data pointer */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    procs = (orte_proc_t**)jdata->procs->addr;
    
    /* get a pointer to the job map */
    map = jdata->map;
    
    /* construct a nodemap */
    if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(&bo))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* store it */
    boptr = &bo;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &boptr, 1, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* release the data since it has now been copied into our buffer */
    free(bo.bytes);

    /* get wireup info for daemons per the selected routing module */
    wireup = OBJ_NEW(opal_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_routed.get_wireup_info(wireup))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(wireup);
        return rc;
    }
    /* if anything was inserted, put it in a byte object for xmission */
    if (0 < wireup->bytes_used) {
        opal_dss.unload(wireup, (void**)&bo.bytes, &numbytes);
        /* pack the number of bytes required by payload */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &numbytes, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(wireup);
            return rc;
        }
        /* pack the byte object */
        bo.size = numbytes;
        boptr = &bo;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &boptr, 1, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(wireup);
            return rc;
        }
        /* release the data since it has now been copied into our buffer */
        free(bo.bytes);
    } else {
        /* pack numbytes=0 so the unpack routine remains sync'd to us */
        numbytes = 0;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &numbytes, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(wireup);
            return rc;
        }
    }
    OBJ_RELEASE(wireup);

    /* insert an "add-procs" command here so we can cleanly process it on the
     * other end
     */
    command = ORTE_DAEMON_ADD_LOCAL_PROCS;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* are we co-locating debugger daemons? */
    if (NULL != orte_debugger_daemon) {
        orte_app_context_t **apps;
        
        /* flag that we are */
        flag = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &flag, 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* pack the jobid for the debugger daemons */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &orte_debugger_daemon->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }        
        /* pack the executable name */
        apps = (orte_app_context_t**)orte_debugger_daemon->apps->addr;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, apps, 1, ORTE_APP_CONTEXT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* pack the control flags */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &orte_debugger_daemon->controls, 1, ORTE_JOB_CONTROL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }        
    } else {
        /* flag that we are NOT */
        flag = 0;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &flag, 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* pack the jobid so it can be extracted later */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the number of procs in this launch */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->num_procs, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the total slots allocated to us */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->total_slots_alloc, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the control flags for this job */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->controls, 1, ORTE_JOB_CONTROL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the stdin target  */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->stdin_target, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the number of app_contexts for this job */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->num_apps, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the app_contexts for this job - we already checked early on that
     * there must be at least one, so don't bother checking here again
     */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, jdata->apps->addr, jdata->num_apps, ORTE_APP_CONTEXT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* encode the pidmap */
    if (ORTE_SUCCESS != (rc = orte_util_encode_pidmap(&bo))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* store it */
    boptr = &bo;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &boptr, 1, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(wireup);
        return rc;
    }
    /* release the data since it has now been copied into our buffer */
    free(bo.bytes);
    
    /* transfer and pack the app_idx array for this job in one pack */
    tmp = (int8_t*)malloc(jdata->num_procs);
    for (i=0; i < jdata->num_procs; i++) {
        tmp[i] = procs[i]->app_idx;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, tmp, jdata->num_procs, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    free(tmp);
    
    /* are there cpu_list strings? */
    if (jdata->map->cpu_lists) {
        flag = (int8_t)true;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &flag, 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        for (i=0; i < jdata->num_procs; i++) {
            if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &procs[i]->slot_list, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc; 
            }
        }
    } else {
        flag = (int8_t)false;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &flag, 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}

int orte_odls_base_default_update_daemon_info(opal_buffer_t *data)
{
    opal_buffer_t wireup;
    opal_byte_object_t *bo;
    int rc;
    orte_std_cntr_t cnt;
    int32_t numbytes;

    /* extract the byte object holding the daemonmap */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* retain a copy for downloading to child processes */
    if (NULL != orte_odls_globals.dmap) {
        free(orte_odls_globals.dmap->bytes);
        free(orte_odls_globals.dmap);
        orte_odls_globals.dmap = NULL;
    }
    opal_dss.copy((void**)&orte_odls_globals.dmap, bo, OPAL_BYTE_OBJECT);
    
    /* update our local nidmap, if required - the decode function
     * knows what to do - it will also free the bytes in the bo
     */
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:update:daemon:info updating nidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (ORTE_SUCCESS != (rc = orte_ess.update_nidmap(bo))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* update the routing tree */
    if (ORTE_SUCCESS != (rc = orte_routed.update_routing_tree())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* unpack the #bytes of daemon wireup info in the message */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &numbytes, &cnt, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* any bytes there? */
    if (0 < numbytes) {
        /* unpack the byte object */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &bo, &cnt, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* load it into a buffer */
        OBJ_CONSTRUCT(&wireup, opal_buffer_t);
        opal_dss.load(&wireup, bo->bytes, bo->size);
        /* pass it for processing */
        if (ORTE_SUCCESS != (rc = orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, &wireup))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&wireup);
            return rc;
        }
        /* done with the buffer - dump it */
        OBJ_DESTRUCT(&wireup);
    }
    
    return ORTE_SUCCESS;
}

int orte_odls_base_default_construct_child_list(opal_buffer_t *data,
                                                orte_jobid_t *job)
{
    int rc, ret;
    orte_vpid_t j, host_daemon;
    orte_odls_child_t *child;
    orte_std_cntr_t cnt;
    orte_process_name_t proc;
    orte_odls_job_t *jobdat=NULL;
    opal_byte_object_t *bo;
    opal_buffer_t alert;
    opal_list_item_t *item;
    int8_t flag;
    int8_t *app_idx=NULL;
    char **slot_str=NULL;
    orte_jobid_t debugger;
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:constructing child list",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* unpack the returned data to create the required structures
     * for a fork launch. Since the data will contain information
     * on procs for ALL nodes, we first have to find the value
     * struct that contains info for our node.
     */
    
    /* set the default values since they may not be included in the data */
    *job = ORTE_JOBID_INVALID;
    
    /* unpack the flag - are we co-locating debugger daemons? */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &flag, &cnt, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    if (0 != flag) {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:construct_child_list unpacking debugger daemon",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* yep - create a jobdat object for it. In this case, we don't have to
         * worry about race conditions as the debugger daemons do not use
         * the daemon collective system
         */
        orte_odls_globals.debugger = OBJ_NEW(orte_odls_job_t);
        /* get the debugger daemon jobid */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &debugger, &cnt, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto REPORT_ERROR;
        }
        orte_odls_globals.debugger->jobid = debugger;
        orte_odls_globals.debugger->num_apps = 1;
        orte_odls_globals.debugger->num_local_procs = 1;
        opal_list_append(&orte_local_jobdata, &(orte_odls_globals.debugger)->super);
        /* retrieve the info */
        orte_odls_globals.debugger->apps = (orte_app_context_t**)malloc(sizeof(orte_app_context_t*));
        if (NULL == orte_odls_globals.debugger->apps) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            goto REPORT_ERROR;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, orte_odls_globals.debugger->apps,
                                                  &(orte_odls_globals.debugger->num_apps), ORTE_APP_CONTEXT))) {
            ORTE_ERROR_LOG(rc);
            goto REPORT_ERROR;
        }
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &(orte_odls_globals.debugger->controls), &cnt, ORTE_JOB_CONTROL))) {
            ORTE_ERROR_LOG(rc);
            goto REPORT_ERROR;
        }
    }
    
    /* unpack the jobid we are to launch */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, job, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:construct_child_list unpacking data to launch job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(*job)));
    
    /* even though we are unpacking an add_local_procs cmd, we cannot assume
     * that no job record for this jobid exists. A race condition exists that
     * could allow another daemon's procs to call us with a collective prior
     * to our unpacking add_local_procs. So lookup the job record for this jobid
     * and see if it already exists
     */
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        orte_odls_job_t *jdat = (orte_odls_job_t*)item;
        
        /* is this the specified job? */
        if (jdat->jobid == *job) {
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:construct_child_list found existing jobdat for job %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(*job)));
            break;
        }
    }
    if (NULL == jobdat) {
        /* setup jobdat object for this job */
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:construct_child_list adding new jobdat for job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(*job)));
        jobdat = OBJ_NEW(orte_odls_job_t);
        jobdat->jobid = *job;
        opal_list_append(&orte_local_jobdata, &jobdat->super);
    }
    
    /* if we are doing a timing test, store the time the msg was recvd */
    if (orte_timing) {
        jobdat->launch_msg_recvd.tv_sec = orte_daemon_msg_recvd.tv_sec;
        jobdat->launch_msg_recvd.tv_usec = orte_daemon_msg_recvd.tv_usec;
    }
    
    /* UNPACK JOB-SPECIFIC DATA */
    /* unpack the number of procs in this launch */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jobdat->num_procs, &cnt, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }    
    /* unpack the total slots allocated to us */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jobdat->total_slots_alloc, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* unpack the control flags for the job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jobdat->controls, &cnt, ORTE_JOB_CONTROL))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* unpack the stdin target for the job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jobdat->stdin_target, &cnt, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* unpack the number of app_contexts for this job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jobdat->num_apps, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:construct_child_list unpacking %ld app_contexts",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)jobdat->num_apps));
    
    /* allocate space and unpack the app_contexts for this job - the HNP checked
     * that there must be at least one, so don't bother checking here again
     */
    jobdat->apps = (orte_app_context_t**)malloc(jobdat->num_apps * sizeof(orte_app_context_t*));
    if (NULL == jobdat->apps) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        goto REPORT_ERROR;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, jobdat->apps, &jobdat->num_apps, ORTE_APP_CONTEXT))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    
    /* unpack the pidmap byte object */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* retain a copy for downloading to child processes */
    opal_dss.copy((void**)&jobdat->pmap, bo, OPAL_BYTE_OBJECT);
    /* decode the pidmap  - this will also free the bytes in bo */
    if (ORTE_SUCCESS != (rc = orte_ess.update_pidmap(bo))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
   
    /* allocate memory for app_idx */
    app_idx = (int8_t*)malloc(jobdat->num_procs);
    /* unpack app_idx in one shot */
    cnt=jobdat->num_procs;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, app_idx, &cnt, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    
    /* unpack flag to indicate if slot_strings are present */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &flag, &cnt, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    
    if (flag) {
        /* allocate space */
        slot_str = (char**)malloc(jobdat->num_procs * sizeof(char*));
        for (j=0; j < jobdat->num_procs; j++) {
            cnt=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &slot_str[j], &cnt, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto REPORT_ERROR;
            }
        }
    }
    
    /* cycle through the procs and find mine */
    proc.jobid = jobdat->jobid;
    for (j=0; j < jobdat->num_procs; j++) {
        proc.vpid = j;
        /* get the vpid of the daemon that is to host this proc */
        if (ORTE_VPID_INVALID == (host_daemon = orte_ess.proc_get_daemon(&proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
            goto REPORT_ERROR;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:constructing child list - checking proc %s on daemon %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_VPID_PRINT(j),
                             ORTE_VPID_PRINT(host_daemon)));

        /* does this proc belong to us? */
        if (ORTE_PROC_MY_NAME->vpid == host_daemon) {
            
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:constructing child list - found proc %s for me!",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_VPID_PRINT(j)));
            
            /* keep tabs of the number of local procs */
            jobdat->num_local_procs++;
            /* add this proc to our child list */
            child = OBJ_NEW(orte_odls_child_t);
            /* copy the name to preserve it */
            if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&child->name, &proc, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                goto REPORT_ERROR;
            }
            child->app_idx = app_idx[j];  /* save the index into the app_context objects */
            if (NULL != slot_str && NULL != slot_str[j]) {
                child->slot_list = strdup(slot_str[j]);
            }
            /* mark that this app_context is being used on this node */
            jobdat->apps[app_idx[j]]->used_on_node = true;
            /* protect operation on the global list of children */
            OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
            opal_list_append(&orte_local_children, &child->super);
            opal_condition_signal(&orte_odls_globals.cond);
            OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
        }
    }
    
    /* flag that the launch msg has been processed so daemon collectives can proceed */
    jobdat->launch_msg_processed = true;
    
    if (NULL != app_idx) {
        free(app_idx);
        app_idx = NULL;
    }
    if (NULL != slot_str) {
        for (j=0; j < jobdat->num_procs; j++) {
            free(slot_str[j]);
        }
        free(slot_str);
        slot_str = NULL;
    }
    
    return ORTE_SUCCESS;

REPORT_ERROR:
    /* we have to report an error back to the HNP so we don't just
     * hang. Although there shouldn't be any errors once this is
     * all debugged, it is still good practice to have a way
     * for it to happen - especially so developers don't have to
     * deal with the hang!
     */
    OBJ_CONSTRUCT(&alert, opal_buffer_t);
    *job = ORTE_JOBID_INVALID;
    opal_dss.pack(&alert, job, 1, ORTE_JOBID);
    /* if we are the HNP, then we would rather not send this to ourselves -
     * instead, we queue it up for local processing
     */
    if (orte_process_info.hnp) {
        ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &alert,
                           ORTE_RML_TAG_APP_LAUNCH_CALLBACK,
                           orte_plm_base_app_report_launch);
    } else {
        /* go ahead and send the update to the HNP */
        if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_APP_LAUNCH_CALLBACK, 0))) {
            ORTE_ERROR_LOG(ret);
        }
    }
    OBJ_DESTRUCT(&alert);
    
    /* cleanup */
    if (NULL != app_idx) {
        free(app_idx);
        app_idx = NULL;
    }
    if (NULL != slot_str && NULL != jobdat) {
        for (j=0; j < jobdat->num_procs; j++) {
            free(slot_str[j]);
        }
        free(slot_str);
        slot_str = NULL;
    }
    
    return rc;
}

static int odls_base_default_setup_fork(orte_app_context_t *context,
                                        int32_t num_local_procs,
                                        orte_vpid_t vpid_range,
                                        orte_std_cntr_t total_slots_alloc,
                                        bool oversubscribed, char ***environ_copy)
{
    int i;
    char *param, *param2;

    /* check the system limits - if we are at our max allowed children, then
     * we won't be allowed to do this anyway, so we may as well abort now.
     * According to the documentation, num_procs = 0 is equivalent to
     * no limit, so treat it as unlimited here.
     */
    if (opal_sys_limits.initialized) {
        if (0 < opal_sys_limits.num_procs &&
            opal_sys_limits.num_procs <= (int)opal_list_get_size(&orte_local_children)) {
            /* at the system limit - abort */
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            return ORTE_ERR_SYS_LIMITS_CHILDREN;
        }
    }

    /* setup base environment: copy the current environ and merge
       in the app context environ */
    if (NULL != context->env) {
        *environ_copy = opal_environ_merge(orte_launch_environ, context->env);
    } else {
        *environ_copy = opal_argv_copy(orte_launch_environ);
    }

    /* special case handling for --prefix: this is somewhat icky,
        but at least some users do this.  :-\ It is possible that
        when using --prefix, the user will also "-x PATH" and/or
        "-x LD_LIBRARY_PATH", which would therefore clobber the
        work that was done in the prior pls to ensure that we have
        the prefix at the beginning of the PATH and
        LD_LIBRARY_PATH.  So examine the context->env and see if we
        find PATH or LD_LIBRARY_PATH.  If found, that means the
        prior work was clobbered, and we need to re-prefix those
        variables. */
    for (i = 0; NULL != context->prefix_dir && NULL != context->env && NULL != context->env[i]; ++i) {
        char *newenv;
        
        /* Reset PATH */
        if (0 == strncmp("PATH=", context->env[i], 5)) {
            asprintf(&newenv, "%s/bin:%s",
                     context->prefix_dir, context->env[i] + 5);
            opal_setenv("PATH", newenv, true, environ_copy);
            free(newenv);
        }
        
        /* Reset LD_LIBRARY_PATH */
        else if (0 == strncmp("LD_LIBRARY_PATH=", context->env[i], 16)) {
            asprintf(&newenv, "%s/lib:%s",
                     context->prefix_dir, context->env[i] + 16);
            opal_setenv("LD_LIBRARY_PATH", newenv, true, environ_copy);
            free(newenv);
        }
    }
    
    /* pass my contact info to the local proc so we can talk */
    param = mca_base_param_environ_variable("orte","local_daemon","uri");
    opal_setenv(param, orte_process_info.my_daemon_uri, true, environ_copy);
    free(param);
    
    /* pass the hnp's contact info to the local proc in case it
     * needs it
     */
    param = mca_base_param_environ_variable("orte","hnp","uri");
    opal_setenv(param, orte_process_info.my_hnp_uri, true, environ_copy);
    free(param);
    
    /* setup yield schedule - do not override any user-supplied directive! */
    if (oversubscribed) {
        param = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
        opal_setenv(param, "1", false, environ_copy);
    } else {
        param = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
        opal_setenv(param, "0", false, environ_copy);
    }
    free(param);
    
    /* set the app_context number into the environment */
    param = mca_base_param_environ_variable("orte","app","num");
    asprintf(&param2, "%ld", (long)context->idx);
    opal_setenv(param, param2, true, environ_copy);
    free(param);
    free(param2);
    
    /* although the total_slots_alloc is the universe size, users
     * would appreciate being given a public environmental variable
     * that also represents this value - something MPI specific - so
     * do that here. Also required by the ompi_attributes code!
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    asprintf(&param2, "%ld", (long)total_slots_alloc);
    opal_setenv("OMPI_UNIVERSE_SIZE", param2, true, environ_copy);
    free(param2);
    
    /* push data into environment - don't push any single proc
     * info, though. We are setting the environment up on a
     * per-context basis, and will add the individual proc
     * info later. This also sets the mca param to select
     * the "env" component in the SDS framework.
     */
    orte_ess_env_put(vpid_range, num_local_procs, environ_copy);
    
    return ORTE_SUCCESS;
}

static int pack_state_for_proc(opal_buffer_t *alert, bool include_startup_info, orte_odls_child_t *child)
{
    int rc;
    
    /* pack the child's vpid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &(child->name->vpid), 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* pack startup info if we need to report it */
    if (include_startup_info) {
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->pid, 1, OPAL_PID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* if we are timing things, pack the time the proc was launched */
        if (orte_timing) {
            int64_t tmp;
            tmp = child->starttime.tv_sec;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &tmp, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            tmp = child->starttime.tv_usec;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &tmp, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    /* pack its state */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->state, 1, ORTE_PROC_STATE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* pack its exit code */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->exit_code, 1, ORTE_EXIT_CODE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

static int pack_state_update(opal_buffer_t *alert, bool include_startup_info, orte_odls_job_t *jobdat)
{
    int rc;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    orte_vpid_t null=ORTE_VPID_INVALID;
    
    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &jobdat->jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* if we are timing things, pack the time the launch msg for this job was recvd */
    if (include_startup_info && orte_timing) {
        int64_t tmp;
        tmp = jobdat->launch_msg_recvd.tv_sec;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &tmp, 1, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        tmp = jobdat->launch_msg_recvd.tv_usec;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &tmp, 1, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        /* if this child is part of the job... */
        if (child->name->jobid == jobdat->jobid) {
            if (ORTE_SUCCESS != (rc = pack_state_for_proc(alert, include_startup_info, child))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    /* flag that this job is complete so the receiver can know */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &null, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}


int orte_odls_base_default_launch_local(orte_jobid_t job,
                                        orte_odls_base_fork_local_proc_fn_t fork_local)
{
    char *job_str, *vpid_str, *param, *value;
    opal_list_item_t *item;
    orte_app_context_t *app, **apps;
    orte_std_cntr_t num_apps;
    orte_odls_child_t *child=NULL;
    int i, num_processors;
    bool oversubscribed;
    int rc=ORTE_SUCCESS, ret;
    bool launch_failed=true;
    opal_buffer_t alert;
    orte_std_cntr_t proc_rank;
    orte_odls_job_t *jobdat;
    orte_local_rank_t local_rank;
    char *pathenv = NULL, *mpiexec_pathenv = NULL;
    char basedir[MAXPATHLEN];
    char dir[MAXPATHLEN];
    char **argvptr;
    char *full_search;
    char **argvsav=NULL;
    int inm;

    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);

    /* establish our baseline working directory - we will be potentially
     * bouncing around as we execute various apps, but we will always return
     * to this place as our default directory
     */
    getcwd(basedir, sizeof(basedir));

    /* find the jobdat for this job */
    jobdat = NULL;
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        jobdat = (orte_odls_job_t*)item;
        
        /* is this the specified job? */
        if (jobdat->jobid == job) {
            break;
        }
    }
    if (NULL == jobdat) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto CLEANUP;
    }
    apps = jobdat->apps;
    num_apps = jobdat->num_apps;
    
#if OPAL_ENABLE_FT == 1
    /*
     * Notify the local SnapC component regarding new job
     */
    if( ORTE_SUCCESS != (rc = orte_snapc.setup_job(job) ) ) {
        /* Silent Failure :/ JJH */
        ORTE_ERROR_LOG(rc);
    }
#endif
    
    /* Now we preload any files that are needed. This is done on a per
     * app context basis
     */
    for (i=0; i < num_apps; i++) {
        if(apps[i]->preload_binary ||
           NULL != apps[i]->preload_files) {
            if( ORTE_SUCCESS != (rc = orte_odls_base_preload_files_app_context(apps[i])) ) {
                ORTE_ERROR_LOG(rc);
                /* JJH: Do not fail here, instead try to execute without the preloaded options*/
            }
        }
    }
    
    if (ORTE_SUCCESS != opal_paffinity_base_get_processor_info(&num_processors)) {
        /* if we cannot find the number of local processors, we have no choice
         * but to default to conservative settings
         */
        oversubscribed = true;
    } else {
        /* don't typecast the num_processors to a size_t as the value could be < 0!
         * Instead, force the opal_list_get_size value to be an int as we surely
         * won't have a #children bigger than that!
         */
        if ((int)opal_list_get_size(&orte_local_children) > num_processors) {
            /* if the #procs > #processors, declare us oversubscribed. This
             * covers the case where the user didn't tell us anything about the
             * number of available slots, so we defaulted to a value of 1
             */
            oversubscribed = true;
        } else {
            /* otherwise, declare us to not be oversubscribed so we can be aggressive */
            oversubscribed = false;
        }
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:launch found %d processors for %d children and set oversubscribed to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         num_processors, (int)opal_list_get_size(&orte_local_children),
                         oversubscribed ? "true" : "false"));
    
    /* setup to report the proc state to the HNP */
    OBJ_CONSTRUCT(&alert, opal_buffer_t);
    
    for (i=0; i < num_apps; i++) {
        app = apps[i];
        
        /* if this app isn't being used on our node, skip it */
        if (!app->used_on_node) {
            continue;
        }
        
        /* setup the environment for this app */
        if (ORTE_SUCCESS != (rc = odls_base_default_setup_fork(app,
                                                               jobdat->num_local_procs,
                                                               jobdat->num_procs,
                                                               jobdat->total_slots_alloc,
                                                               oversubscribed,
                                                               &app->env))) {
            
            OPAL_OUTPUT_VERBOSE((10, orte_odls_globals.output,
                                 "%s odls:launch:setup_fork failed with error %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_ERROR_NAME(rc)));
            
            /* do not ERROR_LOG this failure - it will be reported
             * elsewhere. The launch is going to fail. Since we could have
             * multiple app_contexts, we need to ensure that we flag only
             * the correct one that caused this operation to fail. We then have
             * to flag all the other procs from the app_context as having "not failed"
             * so we can report things out correctly
             */
            /* cycle through children to find those for this jobid */
            for (item = opal_list_get_first(&orte_local_children);
                 item != opal_list_get_end(&orte_local_children);
                 item = opal_list_get_next(item)) {
                child = (orte_odls_child_t*)item;
                if (OPAL_EQUAL == opal_dss.compare(&job, &(child->name->jobid), ORTE_JOBID) &&
                    i == child->app_idx) {
                    child->exit_code = rc;
                }
            }
            /* okay, now tell the HNP we couldn't do it */
            goto CLEANUP;
        }
        
        
        /* Try to change to the app's cwd and check that the app
         exists and is executable The function will
         take care of outputting a pretty error message, if required
         */
        if (ORTE_SUCCESS != (rc = orte_util_check_context_cwd(app, true))) {
            /* do not ERROR_LOG - it will be reported elsewhere */
            /* cycle through children to find those for this jobid */
            for (item = opal_list_get_first(&orte_local_children);
                 item != opal_list_get_end(&orte_local_children);
                 item = opal_list_get_next(item)) {
                child = (orte_odls_child_t*)item;
                if (OPAL_EQUAL == opal_dss.compare(&job, &(child->name->jobid), ORTE_JOBID) &&
                    i == child->app_idx) {
                    child->exit_code = rc;
                }
            }
            goto CLEANUP;
        }
        
        /* The prior function will have done a chdir() to jump us to
         * wherever the app is to be executed. This could be either where
         * the user specified (via -wdir), or to the user's home directory
         * on this node if nothing was provided. It seems that chdir doesn't
         * adjust the $PWD enviro variable when it changes the directory. This
         * can cause a user to get a different response when doing getcwd vs
         * looking at the enviro variable. To keep this consistent, we explicitly
         * ensure that the PWD enviro variable matches the CWD we moved to.
         *
         * NOTE: if a user's program does a chdir(), then $PWD will once
         * again not match getcwd! This is beyond our control - we are only
         * ensuring they start out matching.
         */
        getcwd(dir, sizeof(dir));
        opal_setenv("PWD", dir, true, &app->env);
        
        /* Search for the OMPI_exec_path and PATH settings in the environment. */
        for (argvptr = app->env; *argvptr != NULL; argvptr++) { 
            if (0 == strncmp("OMPI_exec_path=", *argvptr, 15)) {
                mpiexec_pathenv = *argvptr + 15;
            }
            if (0 == strncmp("PATH=", *argvptr, 5)) {
                pathenv = *argvptr + 5;
            }
        }
        
        /* If OMPI_exec_path is set (meaning --path was used), then create a
         temporary environment to be used in the search for the executable.
         The PATH setting in this temporary environment is a combination of
         the OMPI_exec_path and PATH values.  If OMPI_exec_path is not set,
         then just use existing environment with PATH in it.  */
        if (NULL != mpiexec_pathenv) {
            argvptr = NULL;
            if (pathenv != NULL) {
                asprintf(&full_search, "%s:%s", mpiexec_pathenv, pathenv);
            } else {
                asprintf(&full_search, "%s", mpiexec_pathenv);
            }
            opal_setenv("PATH", full_search, true, &argvptr);
            free(full_search);
        } else {
            argvptr = app->env;
        }
        
        if (ORTE_SUCCESS != (rc = orte_util_check_context_app(app, argvptr))) {
            /* do not ERROR_LOG - it will be reported elsewhere */
            if (NULL != mpiexec_pathenv) {
                opal_argv_free(argvptr);
            }
            /* cycle through children to find those for this jobid */
            for (item = opal_list_get_first(&orte_local_children);
                 item != opal_list_get_end(&orte_local_children);
                 item = opal_list_get_next(item)) {
                child = (orte_odls_child_t*)item;
                if (OPAL_EQUAL == opal_dss.compare(&job, &(child->name->jobid), ORTE_JOBID) &&
                    i == child->app_idx) {
                    child->exit_code = rc;
                }
            }
            /* okay, now tell the HNP we couldn't do it */
            goto CLEANUP;
        }
        if (NULL != mpiexec_pathenv) {
            opal_argv_free(argvptr);
        }
        
        /* okay, now let's launch all the local procs for this app using the provided fork_local fn */
        for (proc_rank = 0, item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            
            /* does this child belong to this app? */
            if (i != child->app_idx) {
                continue;
            }
            
            /* is this child already alive? This can happen if
             * we are asked to launch additional processes.
             * If it has been launched, then do nothing
             */
            if (child->alive) {
                
                OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                     "%s odls:launch child %s is already alive",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name)));
                
                continue;
            }
            
            /* do we have a child from the specified job. Because the
             * job could be given as a WILDCARD value, we must use
             * the dss.compare function to check for equality.
             */
            if (OPAL_EQUAL != opal_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
                
                OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                     "%s odls:launch child %s is not in job %s being launched",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name),
                                     ORTE_JOBID_PRINT(job)));
                
                continue;
            }
            
            /* did the user request we display output in xterms? */
            if (NULL != orte_xterm) {
                opal_list_item_t *nmitem;
                orte_namelist_t *nm;
                /* see if this rank is one of those requested */
                for (nmitem = opal_list_get_first(&orte_odls_globals.xterm_ranks);
                     nmitem != opal_list_get_end(&orte_odls_globals.xterm_ranks);
                     nmitem = opal_list_get_next(nmitem)) {
                    nm = (orte_namelist_t*)nmitem;
                    if (ORTE_VPID_WILDCARD == nm->name.vpid ||
                        child->name->vpid == nm->name.vpid) {
                        /* we want this one - modify the app's command to include
                         * the orte xterm cmd. Need to be careful, though, that we
                         * don't modify the app for ALL ranks that use it! So we
                         * will create a copy of the argv so we can restore it later
                         */
                        argvsav = opal_argv_copy(app->argv);
                        /* free the argv */
                        opal_argv_free(app->argv);
                        app->argv = NULL;
                        /* now create a new one that starts with the xtermcmd */
                        for (inm=0; inm < opal_argv_count(orte_odls_globals.xtermcmd); inm++) {
                            opal_argv_append_nosize(&app->argv, orte_odls_globals.xtermcmd[inm]);
                        }
                        /* insert the rank into the correct place as a window title */
                        free(app->argv[2]);
                        asprintf(&app->argv[2], "Rank %s", ORTE_VPID_PRINT(child->name->vpid));
                        /* add back the original argv */
                        for (inm=0; inm < opal_argv_count(argvsav); inm++) {
                            opal_argv_append_nosize(&app->argv, argvsav[inm]);
                        }
                        /* the app exe name itself is in the argvsav array, so
                         * we can recover it from there later
                         */
                        free(app->app);
                        app->app = strdup(orte_odls_globals.xtermcmd[0]);
                        break;
                    } else if (jobdat->num_procs <= nm->name.vpid) {  /* check for bozo case */
                        /* can't be done! */
                        orte_show_help("help-orte-odls-base.txt",
                                       "orte-odls-base:xterm-rank-out-of-bounds",
                                       true, nm->name.vpid, jobdat->num_procs);
                        rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
                        goto CLEANUP;
                    }
                    
                }
            }
            
            /* setup the rest of the environment with the proc-specific items - these
             * will be overwritten for each child
             */
            if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&job_str, child->name->jobid))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            if (ORTE_SUCCESS != (rc = orte_util_convert_vpid_to_string(&vpid_str, child->name->vpid))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            if(NULL == (param = mca_base_param_environ_variable("orte","ess","jobid"))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            opal_setenv(param, job_str, true, &app->env);
            free(param);
            free(job_str);
            
            if(NULL == (param = mca_base_param_environ_variable("orte","ess","vpid"))) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            opal_setenv(param, vpid_str, true, &app->env);
            free(param);

            /* although the vpid IS the process' rank within the job, users
             * would appreciate being given a public environmental variable
             * that also represents this value - something MPI specific - so
             * do that here.
             *
             * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
             * We know - just live with it
             */
            opal_setenv("OMPI_COMM_WORLD_RANK", vpid_str, true, &app->env);
            free(vpid_str);  /* done with this now */
            
           /* users would appreciate being given a public environmental variable
             * that also represents the local rank value - something MPI specific - so
             * do that here.
             *
             * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
             * We know - just live with it
             */
            if (ORTE_LOCAL_RANK_INVALID == (local_rank = orte_ess.get_local_rank(child->name))) {
                ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
                rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
                goto CLEANUP;
            }
            asprintf(&value, "%lu", (unsigned long) local_rank);
            opal_setenv("OMPI_COMM_WORLD_LOCAL_RANK", value, true, &app->env);
            free(value);
            
           param = mca_base_param_environ_variable("opal", NULL, "paffinity_base_slot_list");
            if ( NULL != child->slot_list ) {
                asprintf(&value, "%s", child->slot_list);
                opal_setenv(param, value, true, &app->env);
                free(value);
            } else {
                opal_unsetenv(param,  &app->env);
            }
            free(param);
            
           /* if we are timing things, record when we are going to launch this proc */
            if (orte_timing) {
                gettimeofday(&child->starttime, NULL);
            }
            
            /* must unlock prior to fork to keep things clean in the
             * event library
             */
            opal_condition_signal(&orte_odls_globals.cond);
            OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
            
#if OPAL_ENABLE_FT    == 1
#if OPAL_ENABLE_FT_CR == 1
            /*
             * OPAL CRS components need the opportunity to take action before a process
             * is forked.
             * Needs access to:
             *   - Environment
             *   - Rank/ORTE Name
             *   - Binary to exec
             */
            if( NULL != opal_crs.crs_prelaunch ) {
                if( OPAL_SUCCESS != (rc = opal_crs.crs_prelaunch(child->name->vpid,
                                                                 orte_snapc_base_global_snapshot_loc,
                                                                 &(app->app),
                                                                 &(app->cwd),
                                                                 &(app->argv),
                                                                 &(app->env) ) ) ) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
            }
#endif
#endif
            if (5 < opal_output_get_verbosity(orte_odls_globals.output)) {
                opal_output(orte_odls_globals.output, "%s odls:launch: spawning child %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(child->name));
                
                /* dump what is going to be exec'd */
                if (7 < opal_output_get_verbosity(orte_odls_globals.output)) {
                    opal_dss.dump(orte_odls_globals.output, app, ORTE_APP_CONTEXT);
                }
            }
            
            rc = fork_local(app, child, app->env, jobdat->controls, jobdat->stdin_target);
            /* reaquire lock so we don't double unlock... */
            OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
            if (ORTE_SUCCESS != rc) {
                /* do NOT ERROR_LOG this error - it generates
                 * a message/node as most errors will be common
                 * across the entire cluster. Instead, we let orterun
                 * output a consolidated error message for us
                 */
                goto CLEANUP;
            } else {
                child->alive = true;
                child->state = ORTE_PROC_STATE_LAUNCHED;
            }
            /* move to next processor */
            proc_rank++;
            /* reset the exe name, if necessary */
            if (NULL != argvsav) {
                /* release the current argv array */
                opal_argv_free(app->argv);
                /* restore the original one */
                app->argv = argvsav;
                argvsav = NULL;
                /* the app exe name itself is now in the argv[0] posn */
                free(app->app);
                app->app = strdup(app->argv[0]);
            }
        }  /* complete launching all children for this app */
        /* reset our working directory back to our default location - if we
         * don't do this, then we will be looking for relative paths starting
         * from the last wdir option specified by the user. Thus, we would
         * be requiring that the user keep track on the cmd line of where
         * each app was located relative to the prior app, instead of relative
         * to their current location
         */
        chdir(basedir);
    }
    launch_failed = false;

CLEANUP:
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:launch reporting job %s launch status",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));
    /* pack the launch results */
    if (ORTE_SUCCESS != (ret = pack_state_update(&alert, true, jobdat))) {
        ORTE_ERROR_LOG(ret);
    }
    
    if (!launch_failed) {
        /* if the launch succeeded, check to see if we need to
         * co-locate any debugger daemons so that they get launched
         * before we report anything to the HNP. This ensures that
         * the debugger daemons are ready-to-go before mpirun returns
         * from the plm.spawn command. Only spawn the debugger, though,
         * if we have local children - otherwise, the HNP could spawn
         * a debugger when it doesn't have any local procs
         */
        if (NULL != orte_odls_globals.debugger &&
            !orte_odls_globals.debugger_launched &&
            0 < opal_list_get_size(&orte_local_children)) {
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:launch forking debugger with %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (ORTE_JOB_CONTROL_FORWARD_OUTPUT & orte_odls_globals.debugger->controls) ? "output forwarded" : "no output"));
            
            fork_local(orte_odls_globals.debugger->apps[0], NULL, NULL,
                       orte_odls_globals.debugger->controls, ORTE_VPID_INVALID);
            orte_odls_globals.debugger_launched = true;
        }
        
    }
    
    /* if we are the HNP, then we would rather not send this to ourselves -
     * instead, we queue it up for local processing
     */
    if (orte_process_info.hnp) {
        ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &alert,
                           ORTE_RML_TAG_APP_LAUNCH_CALLBACK,
                           orte_plm_base_app_report_launch);
    } else {
        /* go ahead and send the update to the HNP */
        if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_APP_LAUNCH_CALLBACK, 0))) {
            ORTE_ERROR_LOG(ret);
        }
    }
    OBJ_DESTRUCT(&alert);

    if (!launch_failed) {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:launch setting waitpids",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* if the launch didn't fail, setup the waitpids on the children */
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            
            if (ORTE_PROC_STATE_LAUNCHED == child->state) {
                child->state = ORTE_PROC_STATE_RUNNING;
                OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
                orte_wait_cb(child->pid, odls_base_default_wait_local_proc, NULL);
                OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
            }
        }
        
    }

    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
    return rc;
}

int orte_odls_base_default_deliver_message(orte_jobid_t job, opal_buffer_t *buffer, orte_rml_tag_t tag)
{
    int rc;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* do we have a child from the specified job. Because the
         *  job could be given as a WILDCARD value, we must use
         *  the dss.compare function to check for equality.
         */
        if (!child->alive ||
            OPAL_EQUAL != opal_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls: sending message to tag %lu on child %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (unsigned long)tag, ORTE_NAME_PRINT(child->name)));
        
        /* if so, send the message */
        rc = orte_rml.send_buffer(child->name, buffer, tag, 0);
        if (rc < 0 && rc != ORTE_ERR_ADDRESSEE_UNKNOWN) {
            /* ignore if the addressee is unknown as a race condition could
             * have allowed the child to exit before we send it a barrier
             * due to the vagaries of the event library
             */
            ORTE_ERROR_LOG(rc);
        }
    }
    
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
    return ORTE_SUCCESS;
}


/**
*  Pass a signal to my local procs
 */

int orte_odls_base_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal,
                                              orte_odls_base_signal_local_fn_t signal_local)
{
    int rc;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls: signaling proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == proc) ? "NULL" : ORTE_NAME_PRINT(proc)));
    
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    
    /* if procs is NULL, then we want to signal all
     * of the local procs, so just do that case
     */
    if (NULL == proc) {
        rc = ORTE_SUCCESS;  /* pre-set this as an empty list causes us to drop to bottom */
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            if (ORTE_SUCCESS != (rc = signal_local(child->pid, (int)signal))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        opal_condition_signal(&orte_odls_globals.cond);
        OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
        return rc;
    }
    
    /* we want it sent to some specified process, so find it */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (OPAL_EQUAL == opal_dss.compare(&(child->name), (orte_process_name_t*)proc, ORTE_NAME)) {
            /* unlock before signaling as this may generate a callback */
            opal_condition_signal(&orte_odls_globals.cond);
            OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
            if (ORTE_SUCCESS != (rc = signal_local(child->pid, (int)signal))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
    }
    
    /* only way to get here is if we couldn't find the specified proc.
     * report that as an error and return it
     */
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
    return ORTE_ERR_NOT_FOUND;
}

static bool all_children_registered(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    /* the thread is locked elsewhere - don't try to do it again here */
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if (OPAL_EQUAL == opal_dss.compare(&child->name->jobid, &job, ORTE_JOBID)) {
            /* if this child is *not* registered yet, return false */
            if (NULL == child->rml_uri) {
                return false;
            }
        }
    }
    
    /* if we get here, then everyone in the job is registered */
    return true;
    
}

static int pack_child_contact_info(orte_jobid_t job, opal_buffer_t *buf)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    int rc;
    
    /* the thread is locked elsewhere - don't try to do it again here */
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if (OPAL_EQUAL == opal_dss.compare(&child->name->jobid, &job, ORTE_JOBID)) {
            /* pack the contact info */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &child->rml_uri, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    return ORTE_SUCCESS;
    
}

void orte_odls_base_setup_singleton_jobdat(orte_jobid_t jobid)
{
    orte_odls_job_t *jobdat;
    int32_t one32;
    int8_t one8;
    orte_local_rank_t lrank;
    orte_node_rank_t nrank;
    opal_buffer_t buffer;
    opal_byte_object_t *bo;
    int rc;
    
    /* create a job tracking object for it */
    jobdat = OBJ_NEW(orte_odls_job_t);
    jobdat->jobid = jobid;
    jobdat->num_procs = 1;
    jobdat->num_local_procs = 1;
    opal_list_append(&orte_local_jobdata, &jobdat->super);
    /* need to setup a pidmap for it */
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);
    opal_dss.pack(&buffer, &jobid, 1, ORTE_JOBID); /* jobid */
    opal_dss.pack(&buffer, &(ORTE_PROC_MY_NAME->vpid), 1, ORTE_VPID); /* num_procs */
    one32 = 0;
    opal_dss.pack(&buffer, &one32, 1, OPAL_INT32); /* node index */
    lrank = 0;
    opal_dss.pack(&buffer, &lrank, 1, ORTE_LOCAL_RANK);  /* local rank */
    nrank = 0;
    opal_dss.pack(&buffer, &nrank, 1, ORTE_NODE_RANK);  /* node rank */
    one8 = 0;
    opal_dss.pack(&buffer, &one8, 1, OPAL_INT8);  /* app_idx */
    /* setup a byte object and unload the packed data to it */
    bo = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
    opal_dss.unload(&buffer, (void**)&bo->bytes, &bo->size);
    OBJ_DESTRUCT(&buffer);
    /* save a copy to send back to the proc */
    opal_dss.copy((void**)&jobdat->pmap, bo, OPAL_BYTE_OBJECT);
    /* update our ess data - this will release the byte object's data */
    if (ORTE_SUCCESS != (rc = orte_ess.update_pidmap(bo))) {
        ORTE_ERROR_LOG(rc);
    }
    free(bo);
    
    /* if we don't yet have a daemon map, then we have to generate one
     * to pass back to it
     */
    if (NULL == orte_odls_globals.dmap) {
        orte_odls_globals.dmap = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        /* construct a nodemap */
        if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(orte_odls_globals.dmap))) {
            ORTE_ERROR_LOG(rc);
        }
        /* we also need to update our local nidmap - copy the dmap
         * as this will release the byte object's data. The copy function
         * will automatically malloc the bo itself, so we don't need to do so here
         */
        opal_dss.copy((void**)&bo, orte_odls_globals.dmap, OPAL_BYTE_OBJECT);
        if (ORTE_SUCCESS != (rc = orte_ess.update_nidmap(bo))) {
            ORTE_ERROR_LOG(rc);
        }
        free(bo);
    }
    /* flag that the "launch msg" has been processed so that daemon
     * collectives can proceed
     */
    jobdat->launch_msg_processed = true;
}

int orte_odls_base_default_require_sync(orte_process_name_t *proc,
                                        opal_buffer_t *buf,
                                        bool drop_nidmap)
{
    opal_buffer_t buffer;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    orte_std_cntr_t cnt;
    int rc;
    bool found=false;

    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* find this child */
        if (OPAL_EQUAL == opal_dss.compare(proc, child->name, ORTE_NAME)) {

            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls: registering sync on child %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));
            
            found = true;
            break;
        }
    }
    
    /* if it wasn't found on the list, then we need to add it - must have
     * come from a singleton
     */
    if (!found) {
        child = OBJ_NEW(orte_odls_child_t);
        if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&child->name, proc, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        opal_list_append(&orte_local_children, &child->super);
        /* we don't know any other info about the child, so just indicate it's
         * alive
         */
        child->alive = true;
        /* setup jobdat object for its job so daemon collectives work */
        orte_odls_base_setup_singleton_jobdat(proc->jobid);
    }
    
    /* if the contact info is already set, then we are "de-registering" the child
     * so free the info and set it to NULL
     */
    if (NULL != child->rml_uri) {
        free(child->rml_uri);
        child->rml_uri = NULL;
    } else {
        /* if the contact info is not set, then we are registering the child so
         * unpack the contact info from the buffer and store it
         */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &(child->rml_uri), &cnt, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    /* ack the call */
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);
    /* do they want the nidmap? */
    if (drop_nidmap) {
        orte_odls_job_t *jobdat = NULL;
        /* get the jobdata object */
        for (item = opal_list_get_first(&orte_local_jobdata);
             item != opal_list_get_end(&orte_local_jobdata);
             item = opal_list_get_next(item)) {
            jobdat = (orte_odls_job_t*)item;
            if (jobdat->jobid == child->name->jobid) {
                break;
            }
        }
        if (NULL == jobdat) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            goto CLEANUP;
        }
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:sync nidmap requested for job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jobdat->jobid)));
        /* the proc needs a copy of both the daemon/node map, and
         * the process map for its peers
         */
        opal_dss.pack(&buffer, &orte_odls_globals.dmap, 1, OPAL_BYTE_OBJECT);
        opal_dss.pack(&buffer, &jobdat->pmap, 1, OPAL_BYTE_OBJECT);
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls: sending sync ack to child %s with %ld bytes of data",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(proc), (long)buffer.bytes_used));
    
    if (0 > (rc = orte_rml.send_buffer(proc, &buffer, ORTE_RML_TAG_SYNC, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        goto CLEANUP;
    }            
    OBJ_DESTRUCT(&buffer);
    
    /* now check to see if everyone in this job has registered */
    if (all_children_registered(proc->jobid)) {
        /* once everyone registers, send their contact info to
         * the HNP so it is available to debuggers and anyone
         * else that needs it
         */
        
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls: sending contact info to HNP",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        OBJ_CONSTRUCT(&buffer, opal_buffer_t);
        /* store jobid */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &proc->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buffer);
            goto CLEANUP;
        }
        /* add in contact info for all procs in the job */
        if (ORTE_SUCCESS != (rc = pack_child_contact_info(proc->jobid, &buffer))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buffer);
            goto CLEANUP;
        }
        /* if we are the HNP, then we would rather not send this to ourselves -
         * instead, we queue it up for local processing
         */
        if (orte_process_info.hnp) {
            ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &buffer,
                               ORTE_RML_TAG_INIT_ROUTES,
                               orte_routed_base_process_msg);
        } else {
            /* go ahead and send it */
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_INIT_ROUTES, 0))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&buffer);
                goto CLEANUP;
            }
        }
        OBJ_DESTRUCT(&buffer);
    }
    
CLEANUP:
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
    return ORTE_SUCCESS;
}

static bool any_live_children(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;

    /* the thread is locked elsewhere - don't try to do it again here */
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if (OPAL_EQUAL == opal_dss.compare(&child->name->jobid, &job, ORTE_JOBID) &&
            child->alive) {
            return true;
        }
    }
    
    /* if we get here, then nobody is left alive from that job */
    return false;

}

static void check_proc_complete(orte_odls_child_t *child)
{
    int rc;
    opal_buffer_t alert;
    orte_plm_cmd_flag_t cmd=ORTE_PLM_UPDATE_PROC_STATE;
    opal_list_item_t *item;
    orte_odls_job_t *jdat;
    
    /* is this proc fully complete? */
    if (!child->waitpid_recvd || !child->iof_complete) {
        /* apparently not - just return */
        return;
    }
    
    /* CHILD IS COMPLETE */
    child->alive = false;
    
    /* Release only the stdin IOF file descriptor for this child, if one
     * was defined. File descriptors for the other IOF channels - stdout,
     * stderr, and stddiag - were released when their associated pipes
     * were cleared and closed due to termination of the process
     */
    orte_iof.close(child->name, ORTE_IOF_STDIN);
    
    /* Clean up the session directory as if we were the process
     * itself.  This covers the case where the process died abnormally
     * and didn't cleanup its own session directory.
     */
    orte_session_dir_finalize(child->name);
    
    /* setup the alert buffer */
    OBJ_CONSTRUCT(&alert, opal_buffer_t);
    
    /* if the proc aborted, tell the HNP right away */
    if (ORTE_PROC_STATE_TERMINATED != child->state) {
        /* pack update state command */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &cmd, 1, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto unlock;
        }
        /* pack only the data for this proc - have to start with the jobid
         * so the receiver can unpack it correctly
         */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &child->name->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto unlock;
        }
        /* now pack the child's info */
        if (ORTE_SUCCESS != (rc = pack_state_for_proc(&alert, false, child))) {
            ORTE_ERROR_LOG(rc);
            goto unlock;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:proc_complete reporting proc %s aborted to HNP",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(child->name)));
        
        /* if we are the HNP, then we would rather not send this to ourselves -
         * instead, we queue it up for local processing
         */
        if (orte_process_info.hnp) {
            ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &alert,
                               ORTE_RML_TAG_PLM,
                               orte_plm_base_receive_process_msg);
        } else {
            /* go ahead and send it */
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_PLM, 0))) {
                ORTE_ERROR_LOG(rc);
                goto unlock;
            }
        }
    } else {
        /* since it didn't abort, let's see if all of that job's procs are done */
        if (!any_live_children(child->name->jobid)) {
            /* all those children are dead - alert the HNP */
            /* pack update state command */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &cmd, 1, ORTE_PLM_CMD))) {
                ORTE_ERROR_LOG(rc);
                goto unlock;
            }
            /* pack the data for the job */
            jdat = NULL;
            for (item = opal_list_get_first(&orte_local_jobdata);
                 item != opal_list_get_end(&orte_local_jobdata);
                 item = opal_list_get_next(item)) {
                 jdat = (orte_odls_job_t*)item;
                
                /* is this the specified job? */
                if (jdat->jobid == child->name->jobid) {
                    break;
                }
            }
            if (NULL == jdat) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                goto unlock;
            }
            if (ORTE_SUCCESS != (rc = pack_state_update(&alert, false, jdat))) {
                ORTE_ERROR_LOG(rc);
                goto unlock;
            }
            
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:proc_complete reporting all procs in %s terminated",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(child->name->jobid)));
            
            /* if we are the HNP, then we would rather not send this to ourselves -
             * instead, we queue it up for local processing
             */
            if (orte_process_info.hnp) {
                ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &alert,
                                   ORTE_RML_TAG_PLM,
                                   orte_plm_base_receive_process_msg);
            } else {
                /* go ahead and send it */
                if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_PLM, 0))) {
                    ORTE_ERROR_LOG(rc);
                    goto unlock;
                }
            }
        }
    }
    
unlock:
    OBJ_DESTRUCT(&alert);
}

/* receive external-to-odls notification that a proc has met some completion
 * requirements
 */
void orte_odls_base_notify_iof_complete(orte_process_name_t *proc)
{
    orte_odls_child_t *child;
    opal_list_item_t *item;
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:notify_iof_complete for child %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /* since we are going to be working with the global list of
     * children, we need to protect that list from modification
     * by other threads. This will also be used to protect us
     * from race conditions on any abort situation
     */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    
    /* find this child */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        if (child->name->jobid == proc->jobid &&
            child->name->vpid == proc->vpid) { /* found it */
            goto GOTCHILD;
        }
    }
    /* get here if we didn't find the child, or if the specified child
     * is already dead. If the latter, then we have a problem as it
     * means we are detecting it exiting multiple times
     */
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:proc_complete did not find child %s in table!",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /* it's just a race condition - don't error log it */
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
    return;
    
GOTCHILD:
    /* flag the iof as complete */
    child->iof_complete = true;
    /* now check to see if the proc is truly done */
    check_proc_complete(child);
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
}

void orte_base_default_waitpid_fired(orte_process_name_t *proc, int32_t status)
{
    orte_odls_child_t *child;
    opal_list_item_t *item;
    char *job, *vpid, *abort_file;
    struct stat buf;
    int rc;

    /* since we are going to be working with the global list of
     * children, we need to protect that list from modification
     * by other threads. This will also be used to protect us
     * from race conditions on any abort situation
     */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    
    /* find this child */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        if (proc->jobid == child->name->jobid &&
            proc->vpid == child->name->vpid) { /* found it */
            goto GOTCHILD;
        }
    }
    /* get here if we didn't find the child, or if the specified child
     * is already dead. If the latter, then we have a problem as it
     * means we are detecting it exiting multiple times
     */
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:waitpid_fired did not find child %s in table!",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /* it's just a race condition - don't error log it */
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
    return;
    
GOTCHILD:
    /* if the child was previously flagged as dead, then just
     * ensure that its exit state gets reported to avoid hanging
     */
    if (!child->alive) {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:waitpid_fired child %s was already dead",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(child->name)));
        goto MOVEON;
    }
    
    /* determine the state of this process */
    if(WIFEXITED(status)) {
        /* set the exit status appropriately */
        child->exit_code = WEXITSTATUS(status);
        
        /* even though the process exited "normally", it is quite
         * possible that this happened via an orte_abort call - in
         * which case, we need to indicate this was an "abnormal"
         * termination. See the note in "orte_abort.c" for
         * an explanation of this process.
         *
         * For our purposes here, we need to check for the existence
         * of an "abort" file in this process' session directory. If
         * we find it, then we know that this was an abnormal termination.
         */
        if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&job, child->name->jobid))) {
            ORTE_ERROR_LOG(rc);
            goto MOVEON;
        }
        if (ORTE_SUCCESS != (rc = orte_util_convert_vpid_to_string(&vpid, child->name->vpid))) {
            ORTE_ERROR_LOG(rc);
            free(job);
            goto MOVEON;
        }
        abort_file = opal_os_path(false, orte_process_info.tmpdir_base,
                                  orte_process_info.top_session_dir,
                                  job, vpid, "abort", NULL );
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:waitpid_fired checking abort file %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), abort_file));
        
        free(job);
        free(vpid);       
        if (0 == stat(abort_file, &buf)) {
            /* the abort file must exist - there is nothing in it we need. It's
             * meer existence indicates that an abnormal termination occurred
             */
            
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:waitpid_fired child %s died by abort",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));
            
            child->state = ORTE_PROC_STATE_ABORTED;
            free(abort_file);
        } else {
            /* okay, it terminated normally - check to see if a sync was required and
             * if it was received
             */
            if (NULL != child->rml_uri) {
                /* if this is set, then we required a sync and didn't get it, so this
                 * is considered an abnormal termination and treated accordingly
                 */
                child->state = ORTE_PROC_STATE_TERM_WO_SYNC;
                
                OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                     "%s odls:waitpid_fired child process %s terminated normally "
                                     "but did not provide a required sync - it "
                                     "will be treated as an abnormal termination",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name)));
                
                goto MOVEON;
            } else {
                child->state = ORTE_PROC_STATE_TERMINATED;
            }
            
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:waitpid_fired child process %s terminated normally",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));
            
        }
    } else {
        /* the process was terminated with a signal! That's definitely
         * abnormal, so indicate that condition
         */
        child->state = ORTE_PROC_STATE_ABORTED_BY_SIG;
        /* If a process was killed by a signal, then make the
         * exit code of orterun be "signo + 128" so that "prog"
         * and "orterun prog" will both yield the same exit code.
         *
         * This is actually what the shell does for you when
         * a process dies by signal, so this makes orterun treat
         * the termination code to exit status translation the
         * same way
         */
        child->exit_code = WTERMSIG(status) + 128;
        
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:waitpid_fired child process %s terminated with signal",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(child->name)));
    }
    
MOVEON:
    /* indicate the waitpid fired */
    child->waitpid_recvd = true;
    
    /* check for everything complete */
    check_proc_complete(child);
    
    /* done */
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
}

/*
 *  Wait for a callback indicating the child has completed.
 */

void odls_base_default_wait_local_proc(pid_t pid, int status, void* cbdata)
{
    orte_odls_child_t *child;
    opal_list_item_t *item;
    int rc;
    opal_buffer_t cmdbuf;
    orte_daemon_cmd_flag_t command;
    int32_t istatus;
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:wait_local_proc child process %ld terminated",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)pid));
    
    /* since we are going to be working with the global list of
     * children, we need to protect that list from modification
     * by other threads. This will also be used to protect us
     * from race conditions on any abort situation
     */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    
    /* find this child */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        if (pid == child->pid) { /* found it */
            /* this is an independent entry point from the event library. To avoid
             * race conditions, we need to get back into the progression of messages
             * and commands to be processed by the daemon. We do this by re-posting
             * the event into the daemon cmd processor
             */
            OBJ_CONSTRUCT(&cmdbuf, opal_buffer_t);
            command = ORTE_DAEMON_WAITPID_FIRED;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&cmdbuf, &command, 1, ORTE_DAEMON_CMD))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&cmdbuf, child->name, 1, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            istatus = status;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&cmdbuf, &istatus, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &cmdbuf, ORTE_RML_TAG_DAEMON, orte_daemon_cmd_processor);
            /* done */
            opal_condition_signal(&orte_odls_globals.cond);
            OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
            return;
        }
    }
    /* get here if we didn't find the child, or if the specified child
     * is already dead. If the latter, then we have a problem as it
     * means we are detecting it exiting multiple times
     */
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:wait_local_proc did not find pid %ld in table!",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)pid));
    
    /* it's just a race condition - don't error log it */
CLEANUP:
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
    return;
}

int orte_odls_base_default_kill_local_procs(orte_jobid_t job, bool set_state,
                                            orte_odls_base_kill_local_fn_t kill_local,
                                            orte_odls_base_child_died_fn_t child_died)
{
    orte_odls_child_t *child;
    opal_list_item_t *item, *next;
    int rc = ORTE_SUCCESS, exit_status = 0, err;
    opal_list_t procs_killed;
    opal_buffer_t alert;
    orte_plm_cmd_flag_t cmd=ORTE_PLM_UPDATE_PROC_STATE;
    orte_vpid_t null=ORTE_VPID_INVALID;
    orte_jobid_t last_job;

    OBJ_CONSTRUCT(&procs_killed, opal_list_t);
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:kill_local_proc working on job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));
    
    /* since we are going to be working with the global list of
     * children, we need to protect that list from modification
     * by other threads
     */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    
    /* setup the alert buffer - we will utilize the fact that
     * children are stored on the list in job order. In other words,
     * the children from one job are stored in sequence on the
     * list
     */
    OBJ_CONSTRUCT(&alert, opal_buffer_t);
    /* pack update state command */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &cmd, 1, ORTE_PLM_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    last_job = ORTE_JOBID_INVALID;
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = next) {
        child = (orte_odls_child_t*)item;
        
        /* preserve the pointer to the next item in list in case we release it */
        next = opal_list_get_next(item);
        
        
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:kill_local_proc checking child process %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(child->name)));
        
        /* do we have a child from the specified job? Because the
         *  job could be given as a WILDCARD value, we must use
         *  the dss.compare function to check for equality.
         */
        if (OPAL_EQUAL != opal_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
            
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:kill_local_proc child %s is not part of job %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name),
                                 ORTE_JOBID_PRINT(job)));
            
            continue;
        }
        
        /* remove the child from the list since it is either already dead or soon going to be dead */
        opal_list_remove_item(&orte_local_children, item);
        
        /* store the jobid, if required */
        if (last_job != child->name->jobid) {
            /* if it isn't the first time through, pack a job_end flag so the
             * receiver can correctly process the buffer
             */
            if (ORTE_JOBID_INVALID != last_job) {
                if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &null, 1, ORTE_VPID))) {
                    ORTE_ERROR_LOG(rc);
                    goto CLEANUP;
                }
            }
            /* pack the jobid */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &(child->name->jobid), 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            last_job = child->name->jobid;
        }
        
        /* is this process alive? if not, then nothing for us
         * to do to it
         */
        if (!child->alive) {
            
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:kill_local_proc child %s is not alive",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));
            
            /* ensure, though, that the state is terminated so we don't lockup if
             * the proc never started
             */
            goto RECORD;
        }
        
        /* de-register the SIGCHILD callback for this pid so we don't get
         * multiple alerts sent back to the HNP
         */
        if (ORTE_SUCCESS != (rc = orte_wait_cb_cancel(child->pid))) {
            /* no need to error_log this - it just means that the pid is already gone */
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:kill_local_proc child %s wait_cb_cancel failed",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));
            
            goto MOVEON;
        }

       /* First send a SIGCONT in case the process is in stopped state.
          If it is in a stopped state and we do not first change it to
          running, then SIGTERM will not get delivered.  Ignore return
          value. */
        kill_local(child->pid, SIGCONT);
        
        /* Send a sigterm to the process.  If we get ESRCH back, that
            means the process is already dead, so just move on. */
        if (0 != (err = kill_local(child->pid, SIGTERM))) {
            orte_show_help("help-odls-default.txt",
                           "odls-default:could-not-send-kill",
                           true, orte_process_info.nodename, child->pid, err);
            /* check the proc state - ensure it is in one of the termination
             * states so that we properly wakeup
             */
            if (ORTE_PROC_STATE_UNDEF == child->state ||
                ORTE_PROC_STATE_INIT == child->state ||
                ORTE_PROC_STATE_LAUNCHED == child->state ||
                ORTE_PROC_STATE_RUNNING == child->state) {
                /* we can't be sure what happened, but make sure we
                 * at least have a value that will let us eventually wakeup
                 */
                child->state = ORTE_PROC_STATE_TERMINATED;
            }
            goto MOVEON;
        }
        
        /* The kill succeeded.  Wait up to timeout_before_sigkill
            seconds to see if it died. */
        
        if (!child_died(child->pid, orte_odls_globals.timeout_before_sigkill, &exit_status)) {
            /* try killing it again */
            kill_local(child->pid, SIGKILL);
            /* Double check that it actually died this time */
            if (!child_died(child->pid, orte_odls_globals.timeout_before_sigkill, &exit_status)) {
                orte_show_help("help-odls-default.txt",
                               "odls-default:could-not-kill",
                               true, orte_process_info.nodename, child->pid);
            }
        }
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:kill_local_proc child %s killed",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(child->name)));
        child->state = ORTE_PROC_STATE_ABORTED_BY_SIG;  /* we may have sent it, but that's what happened */
        /* let this fall through to record the proc as "not alive" even
         * if child_died failed. We did our best, so as far as we are
         * concerned, this child is dead
         */
        
MOVEON:
        /* set the process to "not alive" */
        child->alive = false;

RECORD:
        /* store the child in the alert buffer */
        if (ORTE_SUCCESS != (rc = pack_state_for_proc(&alert, false, child))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    /* if set_state, alert the HNP to what happened */
    if (set_state) {
        /* if we are the HNP, then we would rather not send this to ourselves -
         * instead, we queue it up for local processing
         */
        if (orte_process_info.hnp) {
            ORTE_MESSAGE_EVENT(ORTE_PROC_MY_NAME, &alert,
                               ORTE_RML_TAG_PLM,
                               orte_plm_base_receive_process_msg);
        } else {
            /* go ahead and send it */
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_PLM, 0))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            rc = ORTE_SUCCESS; /* need to set this correctly if it wasn't an error */
        }
    }
    
CLEANUP:
    /* we are done with the global list, so we can now release
     * any waiting threads - this also allows any callbacks to work
     */
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);

    OBJ_DESTRUCT(&alert);
    
    return rc;    
}

int orte_odls_base_get_proc_stats(opal_buffer_t *answer,
                                  orte_process_name_t *proc)
{
    int rc;
    orte_odls_child_t *child;
    opal_list_item_t *item;
    opal_pstats_t stats, *statsptr;
    int j;
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:get_proc_stats for proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /* find this child */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        if (proc->jobid == child->name->jobid &&
            (proc->vpid == child->name->vpid ||
             ORTE_VPID_WILDCARD == proc->vpid)) { /* found it */

            OBJ_CONSTRUCT(&stats, opal_pstats_t);
            /* record node up to first '.' */
            for (j=0; j < (int)strlen(orte_process_info.nodename) &&
                 j < OPAL_PSTAT_MAX_STRING_LEN-1 &&
                 orte_process_info.nodename[j] != '.'; j++) {
                stats.node[j] = orte_process_info.nodename[j];
            }
            /* record rank */
            stats.rank = child->name->vpid;
            /* get stats */
            rc = opal_pstat.query(child->pid, &stats);
            if (ORTE_SUCCESS != rc) {
                OBJ_DESTRUCT(&stats);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, proc, 1, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&stats);
                return rc;
            }
            statsptr = &stats;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &statsptr, 1, OPAL_PSTAT))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&stats);
                return rc;
            }
            OBJ_DESTRUCT(&stats);
        }
    }

    return ORTE_SUCCESS;
}

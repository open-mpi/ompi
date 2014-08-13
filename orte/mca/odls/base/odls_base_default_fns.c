/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 Oracle and/or its affiliates.  All rights reserved. 
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
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
#include "opal/util/os_dirpath.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/util/sys_limits.h"
#include "opal/dss/dss.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/pstat/pstat.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/iof_base_setup.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/sensor/sensor.h"
#include "orte/mca/state/state.h"
#include "orte/mca/filem/filem.h"

#include "orte/util/context_fns.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/proc_info.h"
#include "orte/util/nidmap.h"
#include "orte/util/regex.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/orted/orted.h"

#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"
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
    orte_job_t *jdata=NULL;
    orte_job_map_t *map=NULL;
    opal_buffer_t *wireup;
    opal_byte_object_t bo, *boptr;
    int32_t numbytes;
    int8_t flag;
    int j;
    orte_app_context_t *app;

    /* get the job data pointer */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* get a pointer to the job map */
    map = jdata->map;
    /* if there is no map, just return */
    if (NULL == map) {
        return ORTE_SUCCESS;
    }
     
    /* construct a nodemap - only want updated items */
    if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(&bo, true))) {
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
    
    /* if we are not using static ports, we need to send the wireup info */
    if (!orte_static_ports) {
        /* pack a flag indicating wiring info is provided */
        flag = 1;
        opal_dss.pack(data, &flag, 1, OPAL_INT8);
        /* get wireup info for daemons per the selected routing module */
        wireup = OBJ_NEW(opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_routed.get_wireup_info(wireup))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(wireup);
            return rc;
        }
        /* put it in a byte object for xmission */
        opal_dss.unload(wireup, (void**)&bo.bytes, &numbytes);
        /* pack the byte object - zero-byte objects are fine */
        bo.size = numbytes;
        boptr = &bo;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &boptr, 1, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(wireup);
            return rc;
        }
        /* release the data since it has now been copied into our buffer */
        if (NULL != bo.bytes) {
            free(bo.bytes);
        }
        OBJ_RELEASE(wireup);
    } else {
        /* pack a flag indicating no wireup data is provided */
        flag = 0;
        opal_dss.pack(data, &flag, 1, OPAL_INT8);
    }

    /* pack the jobid so it can be extracted later */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the job state so it can be extracted later */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->state, 1, ORTE_JOB_STATE))) {
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
    
#if OPAL_HAVE_HWLOC
    /* pack the binding policy so the daemon knows if binding is required */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->map->binding, 1, OPAL_BINDING_POLICY))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
#endif

    /* pack the control flags for this job */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->controls, 1, ORTE_JOB_CONTROL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the MPI-allowed flag for this job */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->gang_launched, 1, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the stdin target  */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->stdin_target, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the stdout target  */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->stdout_target, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack whether or not process recovery is allowed for this job */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->enable_recovery, 1, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the number of app_contexts for this job */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->num_apps, 1, ORTE_APP_IDX))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the app_contexts for this job */
    for (j=0; j < jdata->apps->size; j++) {
        if (NULL != (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, j))) {
            if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &app, 1, ORTE_APP_CONTEXT))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    /* encode the pidmap, taking only the updated procs */
    if (ORTE_SUCCESS != (rc = orte_util_encode_pidmap(&bo, true))) {
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
    /* update our own version in case we have local procs */
    if (ORTE_SUCCESS != (rc = orte_util_encode_pidmap(&orte_pidmap, false))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* pack the collective ids */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->peer_modex, 1, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->peer_init_barrier, 1, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->peer_fini_barrier, 1, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

static int check_local_proc(orte_job_t *jdata, orte_proc_t *pptr)
{
    orte_vpid_t host_daemon;
    orte_app_context_t *app;

    /* get the vpid of the daemon that is to host this proc */
    OPAL_OUTPUT_VERBOSE((20, orte_odls_base_framework.framework_output,
                         "%s odls:constructing child list - looking for daemon for proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&pptr->name)));
    if (NULL == pptr->node || NULL == pptr->node->daemon) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_VPID_INVALID == (host_daemon = pptr->node->daemon->name.vpid)) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
        
    OPAL_OUTPUT_VERBOSE((20, orte_odls_base_framework.framework_output,
                         "%s odls:constructing child list - checking proc %s on daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&pptr->name),
                         ORTE_VPID_PRINT(host_daemon)));

    /* does this proc belong to us? */
    if (ORTE_PROC_MY_NAME->vpid != host_daemon) {
        return ORTE_SUCCESS;
    }
            
    OPAL_OUTPUT_VERBOSE((10, orte_odls_base_framework.framework_output,
                         "%s odls:constructing child list - found proc %s for me!",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&pptr->name)));
            
    /* is this child on our current list of children */
    if (!pptr->local_proc) {
        /* not on the local list */
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s adding proc %s to my local list",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&pptr->name)));
        /* keep tabs of the number of local procs */
        jdata->num_local_procs++;
        /* add this proc to our child list */
        OBJ_RETAIN(pptr);
        pptr->local_proc = true;
        opal_pointer_array_add(orte_local_children, pptr);
    }

    /* if the job is in restart mode, the child must not barrier when launched */
    if (ORTE_JOB_CONTROL_RESTART & jdata->controls) {
        pptr->do_not_barrier = true;
    }
    /* mark that this app_context is being used on this node */
    app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, pptr->app_idx);
    app->used_on_node = true;
    return ORTE_SUCCESS;
}

int orte_odls_base_default_construct_child_list(opal_buffer_t *data,
                                                orte_jobid_t *job)
{
    int rc;
    orte_vpid_t j;
    orte_std_cntr_t cnt;
    orte_job_t *jdata=NULL;
    opal_byte_object_t *bo;
    int8_t flag;
    int32_t n;
    orte_app_context_t *app;
    orte_proc_t *pptr;
    orte_grpcomm_collective_t *coll;
    orte_namelist_t *nm;

    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls:constructing child list",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    *job = ORTE_JOBID_INVALID;
    
    /* extract the byte object holding the daemon map - we dealt with it
     * during the xcast, so we can ignore it here
     */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }

    /* unpack the wireup info flag */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &flag, &cnt, OPAL_INT8))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* if it was given, unpack and discard it */
    if (0 != flag) {
        /* unpack the byte object */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &bo, &cnt, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            goto REPORT_ERROR;
        }
        if (0 < bo->size) {
            free(bo->bytes);
        }
        free(bo);
    }

    /* unpack the jobid we are to launch */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, job, &cnt, ORTE_JOBID))) {
        *job = ORTE_JOBID_INVALID;
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls:construct_child_list unpacking data to launch job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(*job)));
    
    /* even though we are unpacking an add_local_procs cmd, we cannot assume
     * that no job record for this jobid exists. A race condition exists that
     * could allow another daemon's procs to call us with a collective prior
     * to our unpacking add_local_procs. So lookup the job record for this jobid
     * and see if it already exists
     */
    if (NULL == (jdata = orte_get_job_data_object(*job))) {
        /* setup job object for this job */
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:construct_child_list adding new object for job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(*job)));
        jdata = OBJ_NEW(orte_job_t);
        jdata->jobid = *job;
        opal_pointer_array_set_item(orte_job_data, ORTE_LOCAL_JOBID(jdata->jobid), jdata);
    }

    /* if we are the HNP, we don't need to unpack this buffer - we already
     * have all the required info in our local job array. So just build the
     * array of local children
     */
    if (ORTE_PROC_IS_HNP) {
        /* we do need to ensure we have an up-to-date nidmap at hand
         * to pass down to any local apps
         */
        if (NULL != orte_nidmap.bytes) {
            free(orte_nidmap.bytes);
        }
        if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(&orte_nidmap, false))) {
            ORTE_ERROR_LOG(rc);
            goto REPORT_ERROR;
        }
        for (n=0; n < jdata->procs->size; n++) {
            if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, n))) {
                continue;
            }
            if (ORTE_PROC_STATE_UNDEF == pptr->state) {
                /* not ready for use yet */
                continue;
            }
            if (ORTE_SUCCESS != (rc = check_local_proc(jdata, pptr))) {
                ORTE_ERROR_LOG(rc);
                goto REPORT_ERROR;
            }
        }
        goto COMPLETE;
    }

    /* UNPACK JOB-SPECIFIC DATA */
    /* unpack the job state so we can know if this is a restart vs initial launch */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->state, &cnt, ORTE_JOB_STATE))) {
        *job = ORTE_JOBID_INVALID;
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    
    /* ensure the map object is present */
    if (NULL == jdata->map) {
        jdata->map = OBJ_NEW(orte_job_map_t);
    }
    /* unpack the number of procs in this launch */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->num_procs, &cnt, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }    
    /* unpack the total slots allocated to us */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->total_slots_alloc, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
#if OPAL_HAVE_HWLOC
    /* unpack the binding policy */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->map->binding, &cnt, OPAL_BINDING_POLICY))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
#endif
    /* unpack the control flags for the job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->controls, &cnt, ORTE_JOB_CONTROL))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* unpack the MPI-allowed flag for this job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->gang_launched, &cnt, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* unpack the stdin target for the job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->stdin_target, &cnt, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* unpack the stdout target for the job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->stdout_target, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* unpack whether or not process recovery is allowed for this job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->enable_recovery, &cnt, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* unpack the number of app_contexts for this job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->num_apps, &cnt, ORTE_APP_IDX))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls:construct_child_list unpacking %ld app_contexts",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)jdata->num_apps));
    for (j=0; j < jdata->num_apps; j++) {
        cnt = 1;
        app = NULL;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &app, &cnt, ORTE_APP_CONTEXT))) {
            ORTE_ERROR_LOG(rc);
            goto REPORT_ERROR;
        }
        opal_pointer_array_set_item(jdata->apps, app->idx, app);
    }
    
    /* unpack the pidmap byte object */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &bo, &cnt, OPAL_BYTE_OBJECT))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    /* decode the pidmap  - this will also free the bytes in bo, and
     * update our global pidmap object
     */
    if (ORTE_SUCCESS != (rc = orte_util_decode_daemon_pidmap(bo))) {
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
   
    /* unpack the collective ids */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->peer_modex, &cnt, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->peer_init_barrier, &cnt, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata->peer_fini_barrier, &cnt, ORTE_GRPCOMM_COLL_ID_T))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* check the procs */
    for (n=0; n < jdata->procs->size; n++) {
        if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, n))) {
            continue;
        }
        if (ORTE_PROC_STATE_UNDEF == pptr->state) {
            /* not ready for use yet */
            continue;
        }
        /* see if it belongs to us */
        if (ORTE_SUCCESS != (rc = check_local_proc(jdata, pptr))) {
            ORTE_ERROR_LOG(rc);
            goto REPORT_ERROR;
        }
    }
    
 COMPLETE:
    /* create the collectives so the job doesn't stall */
    coll = orte_grpcomm_base_setup_collective(jdata->peer_modex);
    nm = OBJ_NEW(orte_namelist_t);
    nm->name.jobid = jdata->jobid;
    nm->name.vpid = ORTE_VPID_WILDCARD;
    opal_list_append(&coll->participants, &nm->super);

    coll = orte_grpcomm_base_setup_collective(jdata->peer_init_barrier);
    nm = OBJ_NEW(orte_namelist_t);
    nm->name.jobid = jdata->jobid;
    nm->name.vpid = ORTE_VPID_WILDCARD;
    opal_list_append(&coll->participants, &nm->super);

    coll = orte_grpcomm_base_setup_collective(jdata->peer_fini_barrier);
    nm = OBJ_NEW(orte_namelist_t);
    nm->name.jobid = jdata->jobid;
    nm->name.vpid = ORTE_VPID_WILDCARD;
    opal_list_append(&coll->participants, &nm->super);
    
    /* progress any pending collectives */
    orte_grpcomm_base_progress_collectives();
    
    return ORTE_SUCCESS;

 REPORT_ERROR:
    /* we have to report an error back to the HNP so we don't just
     * hang. Although there shouldn't be any errors once this is
     * all debugged, it is still good practice to have a way
     * for it to happen - especially so developers don't have to
     * deal with the hang!
     */
    ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_NEVER_LAUNCHED);
   
    return rc;
}

static int odls_base_default_setup_fork(orte_app_context_t *context,
                                        int32_t num_local_procs,
                                        orte_vpid_t vpid_range,
                                        orte_std_cntr_t total_slots_alloc,
                                        int num_nodes,
                                        bool oversubscribed, char ***environ_copy)
{
    int i;
    char *param, *param2;

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
    (void) mca_base_var_env_name ("orte_local_daemon_uri", &param);
    opal_setenv(param, orte_process_info.my_daemon_uri, true, environ_copy);
    free(param);
    
    /* pass the hnp's contact info to the local proc in case it
     * needs it
     */
    if (NULL != orte_process_info.my_hnp_uri) {
        (void) mca_base_var_env_name ("orte_hnp_uri", &param);
        opal_setenv(param, orte_process_info.my_hnp_uri, true, environ_copy);
        free(param);
    }
    
    /* setup yield schedule - do not override any user-supplied directive! */
    if (oversubscribed) {
        (void) mca_base_var_env_name ("mpi_yield_when_idle", &param);
        opal_setenv(param, "1", false, environ_copy);
    } else {
        (void) mca_base_var_env_name ("mpi_yield_when_idle", &param);
        opal_setenv(param, "0", false, environ_copy);
    }
    free(param);
    
    /* set the app_context number into the environment */
    (void) mca_base_var_env_name ("orte_app_num", &param);
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
    
    /* pass the number of nodes involved in this job */
    (void) mca_base_var_env_name ("orte_num_nodes", &param);
    asprintf(&param2, "%ld", (long)num_nodes);
    opal_setenv(param, param2, true, environ_copy);
    free(param);
    free(param2);

#if OPAL_HAVE_HWLOC
    {
        /* pass a param telling the child what type and model of cpu we are on,
         * if we know it. If hwloc has the value, use what it knows. Otherwise,
         * see if we were explicitly given it and use that value.
         */
        hwloc_obj_t obj;
        char *htmp;
        if (NULL != opal_hwloc_topology) {
            obj = hwloc_get_root_obj(opal_hwloc_topology);
            if (NULL != (htmp = (char*)hwloc_obj_get_info_by_name(obj, "CPUType")) ||
                NULL != (htmp = orte_local_cpu_type)) {
                (void) mca_base_var_env_name ("orte_cpu_type", &param);
                opal_setenv(param, htmp, true, environ_copy);
                free(param);
            }
            if (NULL != (htmp = (char*)hwloc_obj_get_info_by_name(obj, "CPUModel")) ||
                NULL != (htmp = orte_local_cpu_model)) {
                (void) mca_base_var_env_name ("orte_cpu_model", &param);
                opal_setenv(param, htmp, true, environ_copy);
                free(param);
            }
        } else {
            if (NULL != orte_local_cpu_type) {
                (void) mca_base_var_env_name ("orte_cpu_type", &param);
                opal_setenv(param, orte_local_cpu_type, true, environ_copy);
                free(param);
            }
            if (NULL != orte_local_cpu_model) {
                (void) mca_base_var_env_name ("orte_cpu_model", &param);
                opal_setenv(param, orte_local_cpu_model, true, environ_copy);
                free(param);
            }
        }
    }
#endif

    /* get shmem's best component name so we can provide a hint to the shmem
     * framework. the idea here is to have someone figure out what component to
     * select (via the shmem framework) and then have the rest of the
     * components in shmem obey that decision. for more details take a look at
     * the shmem framework in opal.
     */
    if (NULL != (param2 = opal_shmem_base_best_runnable_component_name())) {
        if (OPAL_SUCCESS == mca_base_var_env_name ("shmem_RUNTIME_QUERY_hint",
                                                   &param)) {
            opal_setenv(param, param2, true, environ_copy);
            free(param);
        }
        free(param2);
    }
    
    /* Set an info MCA param that tells the launched processes that
     * any binding policy was applied by us (e.g., so that
     * MPI_INIT doesn't try to bind itself)
     */
    (void) mca_base_var_env_name ("orte_bound_at_launch", &param);
    opal_setenv(param, "1", true, environ_copy);
    free(param);

    /* push data into environment - don't push any single proc
     * info, though. We are setting the environment up on a
     * per-context basis, and will add the individual proc
     * info later. This also sets the mca param to select
     * the "env" component in the ESS framework.
     */
    orte_ess_env_put(vpid_range, num_local_procs, environ_copy);
    
    /* forcibly set the local tmpdir base to match ours */
    (void) mca_base_var_env_name ("orte_tmpdir_base", &param);
    opal_setenv(param, orte_process_info.tmpdir_base, true, environ_copy);
    free(param);

    /* since we are launching via orted, ensure the app
     * doesn't open any of the PMI components even if we
     * are in a PMI environment - saves overhead, and avoids
     * issues with bugs in some PMI implementations regarding
     * behavior after calling fork. Don't override any existing
     * directives, though!
     */
    opal_setenv("OMPI_MCA_grpcomm", "^pmi", false, environ_copy);
    opal_setenv("OMPI_MCA_db", "^pmi", false, environ_copy);
    opal_setenv("OMPI_MCA_pubsub", "^pmi", false, environ_copy);

    return ORTE_SUCCESS;
}

static int setup_child(orte_proc_t *child,
                       orte_job_t *jobdat,
                       orte_app_context_t *app)
{
    char *param, *value, ***env;
    int rc;

    /* for convenience */
    env = &app->env;

    /* setup the jobid */
    if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&value, child->name.jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (OPAL_SUCCESS != mca_base_var_env_name ("ess_base_jobid", &param)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        return rc;
    }
    opal_setenv(param, value, true, env);
    free(param);
    free(value);

    /* setup the vpid */
    if (ORTE_SUCCESS != (rc = orte_util_convert_vpid_to_string(&value, child->name.vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (OPAL_SUCCESS != mca_base_var_env_name ("ess_base_vpid", &param)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        return rc;
    }
    opal_setenv(param, value, true, env);
    free(param);

    /* although the vpid IS the process' rank within the job, users
     * would appreciate being given a public environmental variable
     * that also represents this value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    opal_setenv("OMPI_COMM_WORLD_RANK", value, true, env);
    free(value);  /* done with this now */
    
    /* users would appreciate being given a public environmental variable
     * that also represents the local rank value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    if (ORTE_LOCAL_RANK_INVALID == child->local_rank) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
        return rc;
    }
    asprintf(&value, "%lu", (unsigned long) child->local_rank);
    opal_setenv("OMPI_COMM_WORLD_LOCAL_RANK", value, true, env);
    free(value);
    
    /* users would appreciate being given a public environmental variable
     * that also represents the node rank value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    if (ORTE_NODE_RANK_INVALID ==child->node_rank) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
        return rc;
    }
    asprintf(&value, "%lu", (unsigned long) child->node_rank);
    opal_setenv("OMPI_COMM_WORLD_NODE_RANK", value, true, env);
    /* set an mca param for it too */
    if(OPAL_SUCCESS != mca_base_var_env_name ("orte_ess_node_rank", &param)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        return rc;
    }
    opal_setenv(param, value, true, env);
    free(param);
    free(value);
    
    /* pass the number of restarts for this proc - will be zero for
     * an initial start, but procs would like to know if they are being
     * restarted so they can take appropriate action
     */
    if (OPAL_SUCCESS != mca_base_var_env_name ("orte_num_restarts", &param)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        return rc;
    }
    asprintf(&value, "%d", child->restarts);
    opal_setenv(param, value, true, env);
    free(param);
    free(value);
    
    /* if the proc should not barrier in orte_init, tell it */
    if (child->do_not_barrier || 0 < child->restarts) {
        if (OPAL_SUCCESS != mca_base_var_env_name ("orte_do_not_barrier", &param)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            return rc;
        }
        opal_setenv(param, "1", true, env);
        free(param);
    }
    
    /* if we are using staged execution, tell it */
    if (orte_staged_execution) {
        if (OPAL_SUCCESS != mca_base_var_env_name ("orte_staged_execution", &param)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            return rc;
        }
        opal_setenv(param, "1", true, env);
        free(param);
    }

    /* if the proc isn't going to forward IO, then we need to flag that
     * it has "completed" iof termination as otherwise it will never fire
     */
    if (!(ORTE_JOB_CONTROL_FORWARD_OUTPUT & jobdat->controls)) {
        child->iof_complete = true;
    }

    /* construct the proc's session dir name */
    if (NULL != orte_process_info.tmpdir_base) {
        value = strdup(orte_process_info.tmpdir_base);
    } else {
        value = NULL;
    }
    param = NULL;
    if (ORTE_SUCCESS != (rc = orte_session_dir_get_name(&param, &value, NULL,
                                                        orte_process_info.nodename,
                                                        NULL, &child->name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    free(value);
    /* pass an envar so the proc can find any files it had prepositioned */
    opal_setenv("OMPI_FILE_LOCATION", param, true, env);

    /* if the user wanted the cwd to be the proc's session dir, then
     * switch to that location now
     */
    if (app->set_cwd_to_session_dir) {
        /* create the session dir - may not exist */
        if (OPAL_SUCCESS != (rc = opal_os_dirpath_create(param, S_IRWXU))) {
            ORTE_ERROR_LOG(rc);
            /* doesn't exist with correct permissions, and/or we can't
             * create it - either way, we are done
             */
            free(param);
            return rc;
        }
        /* change to it */
        if (0 != chdir(param)) {
            free(param);
            return ORTE_ERROR;
        }
        /* It seems that chdir doesn't
         * adjust the $PWD enviro variable when it changes the directory. This
         * can cause a user to get a different response when doing getcwd vs
         * looking at the enviro variable. To keep this consistent, we explicitly
         * ensure that the PWD enviro variable matches the CWD we moved to.
         *
         * NOTE: if a user's program does a chdir(), then $PWD will once
         * again not match getcwd! This is beyond our control - we are only
         * ensuring they start out matching.
         */
        opal_setenv("PWD", param, true, env);
        /* update the initial wdir value too */
        opal_setenv("OMPI_MCA_initial_wdir", param, true, env);
    }
    free(param);
    return ORTE_SUCCESS;
}

static int setup_path(orte_app_context_t *app)
{
    int rc;
    char dir[MAXPATHLEN];
    char **argvptr;
    char *pathenv = NULL, *mpiexec_pathenv = NULL;
    char *full_search;

    if (!app->set_cwd_to_session_dir) {
        /* Try to change to the app's cwd and check that the app
           exists and is executable The function will
           take care of outputting a pretty error message, if required
        */
        if (ORTE_SUCCESS != (rc = orte_util_check_context_cwd(app, true))) {
            /* do not ERROR_LOG - it will be reported elsewhere */
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
        /* update the initial wdir value too */
        opal_setenv("OMPI_MCA_initial_wdir", dir, true, &app->env);
    }

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
        
    rc = orte_util_check_context_app(app, argvptr);
    /* do not ERROR_LOG - it will be reported elsewhere */
    if (NULL != mpiexec_pathenv) {
        opal_argv_free(argvptr);
    }

 CLEANUP:
    return rc;
}


/* define a timer release point so that we can wait for
 * file descriptors to come available, if necessary
 */
static void timer_cb(int fd, short event, void *cbdata)
{
    orte_timer_t *tm = (orte_timer_t*)cbdata;
    orte_odls_launch_local_t *ll = (orte_odls_launch_local_t*)tm->payload;

    /* increment the number of retries */
    ll->retries++;

    /* re-attempt the launch */
    opal_event_active(ll->ev, OPAL_EV_WRITE, 1);

    /* release the timer event */
    OBJ_RELEASE(tm);
}

static int compute_num_procs_alive(orte_jobid_t job)
{
    int i;
    orte_proc_t *child;
    int num_procs_alive = 0;

    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        if (!child->alive) {
            continue;
        }
        /* do not include members of the specified job as they
         * will be added later, if required
         */
        if (job == child->name.jobid) {
            continue;
        }
        num_procs_alive++;
    }
    return num_procs_alive;
}


void orte_odls_base_default_launch_local(int fd, short sd, void *cbdata)
{
    orte_app_context_t *app;
    orte_proc_t *child=NULL;
    bool oversubscribed;
    int rc=ORTE_SUCCESS;
    orte_std_cntr_t proc_rank;
    char basedir[MAXPATHLEN];
    char **argvsav=NULL;
    int inm, j, idx;
    int total_num_local_procs = 0;
    orte_node_t *node;
    orte_odls_launch_local_t *caddy = (orte_odls_launch_local_t*)cbdata;
    orte_job_t *jobdat;
    orte_jobid_t job = caddy->job;
    orte_odls_base_fork_local_proc_fn_t fork_local = caddy->fork_local;
    char *num_app_ctx = NULL;
    char **nps, *npstring;
    char **firstranks, *firstrankstring;

    /* establish our baseline working directory - we will be potentially
     * bouncing around as we execute various apps, but we will always return
     * to this place as our default directory
     */
    getcwd(basedir, sizeof(basedir));

    /* find the jobdat for this job */
    if (NULL == (jobdat = orte_get_job_data_object(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        /* not much we can do here - we are just hosed, so
         * report that to the error manager
         */
        ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_FAILED_TO_LAUNCH);
        goto ERROR_OUT;
    }
    
    /* do we have any local procs to launch? */
    if (0 == jobdat->num_local_procs) {
        /* indicate that we are done trying to launch them */
        goto GETOUT;
    }
    
    /* see if the mapper thinks we are oversubscribed */
    oversubscribed = false;
    if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, ORTE_PROC_MY_NAME->vpid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        ORTE_ACTIVATE_JOB_STATE(jobdat, ORTE_JOB_STATE_FAILED_TO_LAUNCH);
        goto ERROR_OUT;
    }
    if (node->oversubscribed) {
        oversubscribed = true;
    }

#if OPAL_ENABLE_FT_CR == 1
    /*
     * Notify the local SnapC component regarding new job
     */
    if( ORTE_SUCCESS != (rc = orte_snapc.setup_job(job) ) ) {
        /* Silent Failure :/ JJH */
        ORTE_ERROR_LOG(rc);
    }
#endif
    
    /* MPI-3 requires we provide some further info to the procs,
     * so we pass them as envars to avoid introducing further
     * ORTE calls in the MPI layer
     */
    asprintf(&num_app_ctx, "%lu", (unsigned long)jobdat->num_apps);

    /* build some common envars we need to pass for MPI-3 compatibility */
    nps = NULL;
    firstranks = NULL;
    for (j=0; j < jobdat->apps->size; j++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jobdat->apps, j))) {
            continue;
        }
        opal_argv_append_nosize(&nps, ORTE_VPID_PRINT(app->num_procs));
        opal_argv_append_nosize(&firstranks, ORTE_VPID_PRINT(app->first_rank));
    }
    npstring = opal_argv_join(nps, ' ');
    firstrankstring = opal_argv_join(firstranks, ' ');
    opal_argv_free(nps);
    opal_argv_free(firstranks);

#if OPAL_ENABLE_FT_CR == 1
    for (j=0; j < jobdat->apps->size; j++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jobdat->apps, j))) {
            continue;
        }
        orte_sstore.fetch_app_deps(app);
    }
    orte_sstore.wait_all_deps();
#endif

    /* compute the total number of local procs currently alive and about to be launched */
    total_num_local_procs = compute_num_procs_alive(job) + jobdat->num_local_procs;

    for (j=0; j < jobdat->apps->size; j++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jobdat->apps, j))) {
            continue;
        }
        
        /* if this app isn't being used on our node, skip it */
        if (!app->used_on_node) {
            continue;
        }
        
        /* check the system limits - if we are at our max allowed children, then
         * we won't be allowed to do this anyway, so we may as well abort now.
         * According to the documentation, num_procs = 0 is equivalent to
         * no limit, so treat it as unlimited here.
         */
        if (0 < opal_sys_limits.num_procs) {
            OPAL_OUTPUT_VERBOSE((10,  orte_odls_base_framework.framework_output,
                                 "%s checking limit on num procs %d #children needed %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 opal_sys_limits.num_procs, total_num_local_procs));
            if (opal_sys_limits.num_procs < total_num_local_procs) {
                if (2 < caddy->retries) {
                    /* if we have already tried too many times, then just give up */
                    ORTE_ACTIVATE_JOB_STATE(jobdat, ORTE_JOB_STATE_FAILED_TO_LAUNCH);
                    goto ERROR_OUT;
                }
                /* set a timer event so we can retry later - this
                 * gives the system a chance to let other procs
                 * terminate, thus creating room for new ones
                 */
                ORTE_DETECT_TIMEOUT(1000, 1000, -1, timer_cb, caddy);
                return;
            }
        }
        
        /* setup the environment for this app */
        if (ORTE_SUCCESS != (rc = odls_base_default_setup_fork(app,
                                                               jobdat->num_local_procs,
                                                               jobdat->num_procs,
                                                               jobdat->total_slots_alloc,
                                                               jobdat->map->num_nodes,
                                                               oversubscribed,
                                                               &app->env))) {
            
            OPAL_OUTPUT_VERBOSE((10, orte_odls_base_framework.framework_output,
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
            for (idx=0; idx < orte_local_children->size; idx++) {
                if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, idx))) {
                    continue;
                }
                if (OPAL_EQUAL == opal_dss.compare(&job, &(child->name.jobid), ORTE_JOBID) &&
                    j == (int)child->app_idx) {
                    child->exit_code = rc;
                    ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_LAUNCH);
                }
            }
            goto GETOUT;
        }
        
        /* setup the working directory for this app - will jump us
         * to that directory
         */
        if (ORTE_SUCCESS != (rc = setup_path(app))) {
            OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                 "%s odls:launch:setup_path failed with error %s(%d)",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_ERROR_NAME(rc), rc));
            /* do not ERROR_LOG this failure - it will be reported
             * elsewhere. The launch is going to fail. Since we could have
             * multiple app_contexts, we need to ensure that we flag only
             * the correct one that caused this operation to fail. We then have
             * to flag all the other procs from the app_context as having "not failed"
             * so we can report things out correctly
             */
            /* cycle through children to find those for this jobid */
            for (idx=0; idx < orte_local_children->size; idx++) {
                if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, idx))) {
                    continue;
                }
                if (OPAL_EQUAL == opal_dss.compare(&job, &(child->name.jobid), ORTE_JOBID) &&
                    j == (int)child->app_idx) {
                    child->exit_code = rc;
                    ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_LAUNCH);
                }
            }
            goto GETOUT;
        }

        /* add the MPI-3 envars */
        opal_setenv("OMPI_NUM_APP_CTX", num_app_ctx, true, &app->env);
        opal_setenv("OMPI_FIRST_RANKS", firstrankstring, true, &app->env);
        opal_setenv("OMPI_APP_CTX_NUM_PROCS", npstring, true, &app->env);

        /* setup any local files that were prepositioned for us */
        if (ORTE_SUCCESS != (rc = orte_filem.link_local_files(jobdat, app))) {
            /* cycle through children to find those for this jobid */
            for (idx=0; idx < orte_local_children->size; idx++) {
                if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, idx))) {
                    continue;
                }
                if (OPAL_EQUAL == opal_dss.compare(&job, &(child->name.jobid), ORTE_JOBID) &&
                    j == (int)child->app_idx) {
                    child->exit_code = rc;
                    ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_LAUNCH);
                }
            }
            goto GETOUT;
        }

        /* okay, now let's launch all the local procs for this app using the provided fork_local fn */
        for (proc_rank = 0, idx=0; idx < orte_local_children->size; idx++) {
            if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, idx))) {
                continue;
            }
            
            /* does this child belong to this app? */
            if (j != (int)child->app_idx) {
                continue;
            }
            
            /* is this child already alive? This can happen if
             * we are asked to launch additional processes.
             * If it has been launched, then do nothing
             */
            if (child->alive) {
                
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s odls:launch child %s has already been launched",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name)));
                
                continue;
            }
            /* is this child a candidate to start? it may not be alive
             * because it already executed
             */
            if (ORTE_PROC_STATE_INIT != child->state &&
                ORTE_PROC_STATE_RESTART != child->state) {
                continue;
            }
            /* do we have a child from the specified job. Because the
             * job could be given as a WILDCARD value, we must use
             * the dss.compare function to check for equality.
             */
            if (OPAL_EQUAL != opal_dss.compare(&job, &(child->name.jobid), ORTE_JOBID)) {
                
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s odls:launch child %s is not in job %s being launched",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name),
                                     ORTE_JOBID_PRINT(job)));
                
                continue;
            }
            
            OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                 "%s odls:launch working child %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&child->name)));

            /* ensure we clear any prior info regarding state or exit status in
             * case this is a restart
             */
            child->exit_code = 0;
            child->waitpid_recvd = false;
            /* if we are not forwarding output for this job, then
             * flag iof as complete
             */
            if (ORTE_JOB_CONTROL_FORWARD_OUTPUT & jobdat->controls) {
                child->iof_complete = false;
            } else {
                child->iof_complete = true;
            }
            child->pid = 0;
            if (NULL != child->rml_uri) {
                free(child->rml_uri);
                child->rml_uri = NULL;
            }
            
            /* check to see if we have enough available file descriptors
             * to launch another child - if not, then let's wait a little
             * while to see if some come free. This can happen if we are
             * in a tight loop over comm_spawn
             */
            if (0 < opal_sys_limits.num_files) {
                int limit;
                limit = 4*total_num_local_procs + 6;
                OPAL_OUTPUT_VERBOSE((10,  orte_odls_base_framework.framework_output,
                                     "%s checking limit on file descriptors %d need %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     opal_sys_limits.num_files, limit));
                if (opal_sys_limits.num_files < limit) {
                    if (2 < caddy->retries) {
                        /* tried enough - give up */
                        child->exit_code = ORTE_PROC_STATE_FAILED_TO_LAUNCH;
                        ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_LAUNCH);
                        continue;
                    }
                    /* don't have enough - wait a little time */
                    ORTE_DETECT_TIMEOUT(1000, 1000, -1, timer_cb, caddy);
                    return;
                }
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
                        child->name.vpid == nm->name.vpid) {
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
                        asprintf(&app->argv[2], "Rank %s", ORTE_VPID_PRINT(child->name.vpid));
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
                        child->exit_code = ORTE_ERR_SILENT;
                        ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_LAUNCH);
                        continue;
                    }
                    
                }
            } else if (NULL != orte_fork_agent) {
                /* we were given a fork agent - use it */
                argvsav = opal_argv_copy(app->argv);
                /* free the argv */
                opal_argv_free(app->argv);
                app->argv = NULL;
                /* now create a new one that starts with the fork agent */
                app->argv = opal_argv_copy(orte_fork_agent);
                /* add back the original argv */
                for (inm=0; NULL != argvsav[inm]; inm++) {
                    opal_argv_append_nosize(&app->argv, argvsav[inm]);
                }
                /* the app exe name itself is in the argvsav array, so
                 * we can recover it from there later
                 */
                free(app->app);
                app->app = opal_path_findv(orte_fork_agent[0], X_OK, orte_launch_environ, NULL);
                if (NULL == app->app) {
                    orte_show_help("help-orte-odls-base.txt",
                                   "orte-odls-base:fork-agent-not-found",
                                   true, orte_process_info.nodename, orte_fork_agent[0]);
                    child->exit_code = ORTE_ERR_SILENT;
                    ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_LAUNCH);
                    continue;
                }
            }

            /* setup the rest of the environment with the proc-specific items - these
             * will be overwritten for each child
             */
            if (ORTE_SUCCESS != (rc = setup_child(child, jobdat, app))) {
                ORTE_ERROR_LOG(rc);
                child->exit_code = rc;
                ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_LAUNCH);
                continue;
            }

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
                if( OPAL_SUCCESS != (rc = opal_crs.crs_prelaunch(child->name.vpid,
                                                                 orte_sstore_base_prelaunch_location,
                                                                 &(app->app),
                                                                 &(app->cwd),
                                                                 &(app->argv),
                                                                 &(app->env) ) ) ) {
                    ORTE_ERROR_LOG(rc);
                    child->exit_code = ORTE_PROC_STATE_FAILED_TO_LAUNCH;
                    ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_LAUNCH);
                    continue;
                }
            }
#endif
            /* if we are indexing the argv by rank, do so now */
            if (ORTE_JOB_CONTROL_INDEX_ARGV & jobdat->controls) {
                char *param;
                asprintf(&param, "%s-%d", app->argv[0], (int)child->name.vpid);
                free(app->argv[0]);
                app->argv[0] = param;
            }

            if (5 < opal_output_get_verbosity(orte_odls_base_framework.framework_output)) {
                opal_output(orte_odls_base_framework.framework_output, "%s odls:launch: spawning child %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&child->name));
                
                /* dump what is going to be exec'd */
                if (7 < opal_output_get_verbosity(orte_odls_base_framework.framework_output)) {
                    opal_dss.dump(orte_odls_base_framework.framework_output, app, ORTE_APP_CONTEXT);
                }
            }
            
            rc = fork_local(app, child, app->env, jobdat);
            /* if we indexed the argv, we need to restore it to
             * its original form
             */
            if (ORTE_JOB_CONTROL_INDEX_ARGV & jobdat->controls) {
                /* restore the argv[0] */
                char *param;
                param = strrchr(app->argv[0], '-');
                *param = '\0';
            }
            if (ORTE_SUCCESS != rc) {
                /* do NOT ERROR_LOG this error - it generates
                 * a message/node as most errors will be common
                 * across the entire cluster. Instead, we let orterun
                 * output a consolidated error message for us
                 */
                child->exit_code = ORTE_ERR_SILENT; /* error message already output */
                ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_START);
                continue;
            } else {
                child->alive = true;
                ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_RUNNING);
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

    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls:launch setting waitpids",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
    /* start the sensors for this job (if any) */
    orte_sensor.start(jobdat->jobid);
        
    /* setup the waitpids on the children that started */
    for (idx=0; idx < orte_local_children->size; idx++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, idx))) {
            continue;
        }
        if (child->name.jobid == jobdat->jobid && child->alive) {
            orte_wait_cb(child->pid, odls_base_default_wait_local_proc, NULL);
        }
    }

 GETOUT:
    /* tell the state machine that all local procs for this job
     * were launched so that it can do whatever it needs to do,
     * like send a state update message for all procs to the HNP
     */
    ORTE_ACTIVATE_JOB_STATE(jobdat, ORTE_JOB_STATE_LOCAL_LAUNCH_COMPLETE);

 ERROR_OUT:
    /* ensure we reset our working directory back to our default location  */
    chdir(basedir);
    /* release the event */
    OBJ_RELEASE(caddy);
}

int orte_odls_base_default_deliver_message(orte_jobid_t job, opal_buffer_t *buffer, orte_rml_tag_t tag)
{
    int rc, exit_status = ORTE_SUCCESS;
    int i;
    orte_proc_t *child;
    opal_buffer_t *relay;

    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        
        /* do we have a child from the specified job. Because the
         *  job could be given as a WILDCARD value, we must use
         *  the dss.compare function to check for equality.
         */
        if (!child->alive ||
            OPAL_EQUAL != opal_dss.compare(&job, &(child->name.jobid), ORTE_JOBID)) {
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls: sending message to tag %lu on child %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (unsigned long)tag, ORTE_NAME_PRINT(&child->name)));
        
        /* if so, send the message */
        relay = OBJ_NEW(opal_buffer_t);
        opal_dss.copy_payload(relay, buffer);
        rc = orte_rml.send_buffer_nb(&child->name, relay, tag, orte_rml_send_callback, NULL);
        if (rc < 0 && rc != ORTE_ERR_ADDRESSEE_UNKNOWN) {
            /* ignore if the addressee is unknown as a race condition could
             * have allowed the child to exit before we send it a barrier
             * due to the vagaries of the event library.
             *
             * If we do get an error it is likely that the orte_local_children
             * has changed to reflect it, so we can no longer deliver messages.
             * So just break out and return the error code.
             */
            ORTE_ERROR_LOG(rc);
            exit_status = rc;
            OBJ_RELEASE(relay);
            goto cleanup;
        }
    }

 cleanup:
    return exit_status;
}


/**
*  Pass a signal to my local procs
 */

int orte_odls_base_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal,
                                              orte_odls_base_signal_local_fn_t signal_local)
{
    int rc, i;
    orte_proc_t *child;
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls: signaling proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == proc) ? "NULL" : ORTE_NAME_PRINT(proc)));
    
    /* if procs is NULL, then we want to signal all
     * of the local procs, so just do that case
     */
    if (NULL == proc) {
        rc = ORTE_SUCCESS;  /* pre-set this as an empty list causes us to drop to bottom */
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                continue;
            }
            if (ORTE_SUCCESS != (rc = signal_local(child->pid, (int)signal))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        return rc;
    }
    
    /* we want it sent to some specified process, so find it */
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                continue;
            }
        if (OPAL_EQUAL == opal_dss.compare(&(child->name), (orte_process_name_t*)proc, ORTE_NAME)) {
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
    return ORTE_ERR_NOT_FOUND;
}

int orte_odls_base_default_require_sync(orte_process_name_t *proc,
                                        opal_buffer_t *buf,
                                        bool drop_nidmap)
{
    opal_buffer_t *buffer;
    orte_proc_t *child;
    orte_std_cntr_t cnt;
    int rc=ORTE_SUCCESS, i;
    bool found=false, registering=false;
    orte_job_t *jobdat;
    uint8_t flag;
    opal_byte_object_t *boptr;

    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls: require sync on child %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
 
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        
        /* find this child */
        if (OPAL_EQUAL == opal_dss.compare(proc, &child->name, ORTE_NAME)) {

            OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                 "%s odls: registering sync on child %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&child->name)));
            
            found = true;
            break;
        }
    }
    
    /* if it wasn't found, that's an error */
    if (!found) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    /* if the child has registered, then we are "de-registering" the child */
    if (child->registered) {
        child->deregistered = true;
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls: require sync deregistering child %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&child->name)));
    } else {
        /* otherwise, we are registering the child so
         * unpack the contact info from the buffer and store it
         */
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls: require sync registering child %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&child->name)));
        child->registered = true;
        registering = true;
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &(child->rml_uri), &cnt, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* unpack the flag indicating MPI proc or not */
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &flag, &cnt, OPAL_UINT8))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        if (1 == flag) {
            child->mpi_proc = true;
        }
    }
    
    /* ack the call */
    buffer = OBJ_NEW(opal_buffer_t);
    /* do they want the nidmap? */
    if (drop_nidmap) {
        /* get the jobdata object */
        if (NULL == (jobdat = orte_get_job_data_object(child->name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            goto CLEANUP;
        }
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:sync nidmap requested for job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jobdat->jobid)));
        /* the proc needs a copy of both the daemon/node map and
         * the process map
         */
#if OPAL_HAVE_HWLOC
        /* send the local topology so the individual apps
         * don't hammer the system to collect it themselves
         */
        opal_dss.pack(buffer, &opal_hwloc_topology, 1, OPAL_HWLOC_TOPO);
#endif
        boptr = &orte_nidmap;
        opal_dss.pack(buffer, &boptr, 1, OPAL_BYTE_OBJECT);
        boptr = &orte_pidmap;
        opal_dss.pack(buffer, &boptr, 1, OPAL_BYTE_OBJECT);
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls: sending sync ack to child %s with %ld bytes of data",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(proc), (long)buffer->bytes_used));
    
    if (0 > (rc = orte_rml.send_buffer_nb(proc, buffer, ORTE_RML_TAG_SYNC,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        goto CLEANUP;
    }
    rc = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls: Finished sending sync ack to child %s (Registering %s)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc), (registering ? "True" : "False") ));

    /* if we are deregistering, then we are done */
    if (!registering) {
        orte_routed.delete_route(&child->name);
        if( NULL != child->rml_uri ) {
            free(child->rml_uri);
            child->rml_uri = NULL;
        }
        goto CLEANUP;
    }
    
    /* update the proc state */
    ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_REGISTERED);
    
CLEANUP:
    return rc;
}

/*
 *  Wait for a callback indicating the child has completed.
 */

void odls_base_default_wait_local_proc(pid_t pid, int status, void* cbdata)
{
    orte_proc_t *proc=NULL, *cptr;
    int i;
    orte_job_t *jobdat;
    orte_proc_state_t state=ORTE_PROC_STATE_WAITPID_FIRED;
    char *abortfile, *jobfam, *job, *vpidstr;

    /* find this child */
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (cptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        if (pid == cptr->pid) {
            proc = cptr;
            break;
        }
    }
    if (NULL == proc) {
        /* get here if we didn't find the child, or if the specified child
         * is already dead. If the latter, then we have a problem as it
         * means we are detecting it exiting multiple times
         */
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:wait_local_proc did not find pid %ld in table!",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (long)pid));
        return;
    }

    opal_output_verbose(5, orte_odls_base_framework.framework_output,
                        "%s odls:wait_local_proc child process %s pid %ld terminated",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&proc->name), (long)pid);
    
    /* if the child was previously flagged as dead, then just
     * ensure that its exit state gets reported to avoid hanging
     */
    if (!proc->alive) {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s was already dead",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name)));
        goto MOVEON;
    }

    /* get the jobdat for this child */
    if (NULL == (jobdat = orte_get_job_data_object(proc->name.jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        goto MOVEON;
    }

    /* if this is a debugger daemon, then just report the state
     * and return as we aren't monitoring it
     */
    if (ORTE_JOB_CONTROL_DEBUGGER_DAEMON & jobdat->controls)  {
        goto MOVEON;
    }

    /* if this child was ordered to die, then just pass that along
     * so we don't hang
     */
    if (ORTE_PROC_STATE_KILLED_BY_CMD == proc->state) {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s was ordered to die",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name)));
        goto MOVEON;
    }
    
    /* determine the state of this process */
    if (WIFEXITED(status)) {
        /* set the exit status appropriately */
        proc->exit_code = WEXITSTATUS(status);

        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s exit code %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name), proc->exit_code));

        /* provide a default state */
        state = ORTE_PROC_STATE_WAITPID_FIRED;

        /* check for the abort marker */
        if (0 > asprintf(&jobfam, "%d", ORTE_JOB_FAMILY(proc->name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            goto MOVEON;
        }
            
        if (0 > asprintf(&job, "%d", ORTE_LOCAL_JOBID(proc->name.jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(jobfam);
            goto MOVEON;
        }
            
        if (ORTE_SUCCESS != orte_util_convert_vpid_to_string(&vpidstr, proc->name.vpid)) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(jobfam);
            free(job);
            goto MOVEON;
        }
            
        abortfile = opal_os_path(false, orte_process_info.tmpdir_base,
                                 orte_process_info.top_session_dir,
                                 jobfam, job, vpidstr, "aborted", NULL);
        if (NULL == abortfile ) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(jobfam);
            free(job);
            free(vpidstr);
            goto MOVEON;
        }
        free(jobfam);
        free(job);
        free(vpidstr);

        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired checking abort file %s for child %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), abortfile,
                             ORTE_NAME_PRINT(&proc->name)));

        if (0 == access(abortfile, F_OK)) {
            unlink(abortfile);
            proc->aborted = true;
            /* even though the process exited "normally", it happened
             * via an orte_abort call
             */
            OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                 "%s odls:waitpid_fired child %s died by call to abort",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            state = ORTE_PROC_STATE_CALLED_ABORT;
            free(abortfile);
            /* since we are going down a different code path, we need to
             * flag that this proc has had its waitpid fired */
            proc->waitpid_recvd = true;
            /* if IOF_COMPLETE has already been recvd, then we need
             * to mark this proc as no longer alive */
            if (proc->iof_complete) {
                proc->alive = false;
            }
            goto MOVEON;
        }
        free(abortfile);

        /* check to see if a sync was required and if it was received */
        if (proc->registered) {
            if (proc->deregistered || orte_allowed_exit_without_sync || 0 != proc->exit_code) {
                /* if we did recv a finalize sync, or one is not required,
                 * then declare it normally terminated
                 * unless it returned with a non-zero status indicating the code
                 * felt it was non-normal - in this latter case, we do not
                 * require that the proc deregister before terminating
                 */
                if (0 != proc->exit_code) {
                    state = ORTE_PROC_STATE_TERM_NON_ZERO;
                    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                         "%s odls:waitpid_fired child process %s terminated normally "
                                         "but with a non-zero exit status - it "
                                         "will be treated as an abnormal termination",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_NAME_PRINT(&proc->name)));
                } else {
                    /* indicate the waitpid fired */
                    state = ORTE_PROC_STATE_WAITPID_FIRED;
                }
            } else {
                /* we required a finalizing sync and didn't get it, so this
                 * is considered an abnormal termination and treated accordingly
                 */
                state = ORTE_PROC_STATE_TERM_WO_SYNC;
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s odls:waitpid_fired child process %s terminated normally "
                                     "but did not provide a required finalize sync - it "
                                     "will be treated as an abnormal termination",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc->name)));
            }
        } else {
            /* has any child in this job already registered? */
            for (i=0; i < orte_local_children->size; i++) {
                if (NULL == (cptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                    continue;
                }
                if (cptr->name.jobid != proc->name.jobid) {
                    continue;
                }
                if (cptr->registered && !orte_allowed_exit_without_sync) {
                    /* someone has registered, and we didn't before
                     * terminating - this is an abnormal termination unless
                     * the allowed_exit_without_sync flag is set
                     */
                    if (0 != proc->exit_code) {
                        state = ORTE_PROC_STATE_TERM_NON_ZERO;
                        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                             "%s odls:waitpid_fired child process %s terminated normally "
                                             "but with a non-zero exit status - it "
                                             "will be treated as an abnormal termination",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ORTE_NAME_PRINT(&proc->name)));
                    } else {
                        state = ORTE_PROC_STATE_TERM_WO_SYNC;
                        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                             "%s odls:waitpid_fired child process %s terminated normally "
                                             "but did not provide a required init sync - it "
                                             "will be treated as an abnormal termination",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ORTE_NAME_PRINT(&proc->name)));
                    }
                    goto MOVEON;
                }
            }
            /* if no child has registered, then it is possible that
             * none of them will. This is considered acceptable. Still
             * flag it as abnormal if the exit code was non-zero
             */
            if (0 != proc->exit_code) {
                state = ORTE_PROC_STATE_TERM_NON_ZERO;
            } else {
                state = ORTE_PROC_STATE_WAITPID_FIRED;
            }
        }

        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child process %s terminated %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name),
                             (0 == proc->exit_code) ? "normally" : "with non-zero status"));
    } else {
        /* the process was terminated with a signal! That's definitely
         * abnormal, so indicate that condition
         */
        state = ORTE_PROC_STATE_ABORTED_BY_SIG;
        /* If a process was killed by a signal, then make the
         * exit code of orterun be "signo + 128" so that "prog"
         * and "orterun prog" will both yield the same exit code.
         *
         * This is actually what the shell does for you when
         * a process dies by signal, so this makes orterun treat
         * the termination code to exit status translation the
         * same way
         */
        proc->exit_code = WTERMSIG(status) + 128;

        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child process %s terminated with signal",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name) ));
        /* Do not decrement the number of local procs here. That is handled in the errmgr */
    }
    
 MOVEON:
    ORTE_ACTIVATE_PROC_STATE(&proc->name, state);
}

int orte_odls_base_default_kill_local_procs(opal_pointer_array_t *procs,
                                            orte_odls_base_kill_local_fn_t kill_local,
                                            orte_odls_base_child_died_fn_t child_died)
{
    orte_proc_t *child;
    opal_list_t procs_killed;
    orte_proc_t *proc, proctmp;
    int i, j;
    opal_pointer_array_t procarray, *procptr;
    bool do_cleanup;
    
    OBJ_CONSTRUCT(&procs_killed, opal_list_t);

    /* if the pointer array is NULL, then just kill everything */
    if (NULL == procs) {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:kill_local_proc working on WILDCARD",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        OBJ_CONSTRUCT(&procarray, opal_pointer_array_t);
        opal_pointer_array_init(&procarray, 1, 1, 1);
        OBJ_CONSTRUCT(&proctmp, orte_proc_t);
        proctmp.name.jobid = ORTE_JOBID_WILDCARD;
        proctmp.name.vpid = ORTE_VPID_WILDCARD;
        opal_pointer_array_add(&procarray, &proctmp);
        procptr = &procarray;
        do_cleanup = true;
    } else {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:kill_local_proc working on provided array",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        procptr = procs;
        do_cleanup = false;
    }
    
    /* cycle through the provided array of processes to kill */
    for (i=0; i < procptr->size; i++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(procptr, i))) {
            continue;
        }
        for(j=0; j < orte_local_children->size; j++) {
            if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, j))) {
                continue;
            }

            OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                 "%s odls:kill_local_proc checking child process %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&child->name)));
            
            /* do we have a child from the specified job? Because the
             *  job could be given as a WILDCARD value, we must
             *  check for that as well as for equality.
             */
            if (ORTE_JOBID_WILDCARD != proc->name.jobid &&
                proc->name.jobid != child->name.jobid) {
                
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s odls:kill_local_proc child %s is not part of job %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name),
                                     ORTE_JOBID_PRINT(proc->name.jobid)));
                continue;
            }
            
            /* see if this is the specified proc - could be a WILDCARD again, so check
             * appropriately
             */
            if (ORTE_VPID_WILDCARD != proc->name.vpid &&
                proc->name.vpid != child->name.vpid) {
                
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s odls:kill_local_proc child %s is not covered by rank %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name),
                                     ORTE_VPID_PRINT(proc->name.vpid)));
                continue;
            }
            
            /* is this process alive? if not, then nothing for us
             * to do to it
             */
            if (!child->alive || 0 == child->pid) {
                
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s odls:kill_local_proc child %s is not alive",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name)));
                
                /* ensure, though, that the state is terminated so we don't lockup if
                 * the proc never started
                 */
                if (ORTE_PROC_STATE_UNDEF == child->state ||
                    ORTE_PROC_STATE_INIT == child->state ||
                    ORTE_PROC_STATE_RUNNING == child->state) {
                    /* we can't be sure what happened, but make sure we
                     * at least have a value that will let us eventually wakeup
                     */
                    child->state = ORTE_PROC_STATE_TERMINATED;
                    /* ensure we realize that the waitpid will never come, if
                     * it already hasn't
                     */
                    child->waitpid_recvd = true;
                    child->pid = 0;
                    goto CLEANUP;
                } else {
                    continue;
                }
            }

            /* mark the child as "killed" since the waitpid will
             * fire as soon as we kill it
             */
            child->state = ORTE_PROC_STATE_KILLED_BY_CMD;  /* we ordered it to die */

            /* ensure the stdin IOF channel for this child is closed. The other
             * channels will automatically close when the proc is killed
             */
            if (NULL != orte_iof.close) {
                orte_iof.close(&child->name, ORTE_IOF_STDIN);
            }
            
            /* cancel the waitpid callback as this induces unmanageable race
             * conditions when we are deliberately killing the process
             */
            orte_wait_cb_cancel(child->pid);
            
            /* First send a SIGCONT in case the process is in stopped state.
               If it is in a stopped state and we do not first change it to
               running, then SIGTERM will not get delivered.  Ignore return
               value. */
            OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                 "%s SENDING SIGCONT TO %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&child->name)));
            kill_local(child->pid, SIGCONT);

            /* Send a sigterm to the process before sigkill to be nice */
            OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                 "%s SENDING SIGTERM TO %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&child->name)));
            kill_local(child->pid, SIGTERM);

            /* check to see if it died - the child_died function will continue
             * to check until we reach the timeout
	     *
	     * In practice, it doesn't matter what child_died reports
	     * - we KILL the process anyway, to be sure it's dead.
	     * However, what it does do is delay the KILL until either
	     * the process is verified dead or the timeout elapsed,
	     * which gives it time enough to shut down.
             */
            if (!child_died(child)) {
                /* if it still isn't dead, try killing it one more time */
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s SENDING SIGKILL TO %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name)));
            } else {
                /* Force the SIGKILL just to make sure things are dead
                 * This fixes an issue that, if the application is masking
                 * SIGTERM, then the child_died()
                 * may return 'true' even though waipid returns with 0.
                 * It does this to avoid a race condition, per documentation
                 * in odls_default_module.c.
                 */
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s SENDING FORCE SIGKILL TO %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name)));
            }
            kill_local(child->pid, SIGKILL);
            /* Double check that it actually died this time */
            if (!child_died(child)) {
                orte_show_help("help-orte-odls-base.txt",
                               "orte-odls-base:could-not-kill",
                               true, orte_process_info.nodename, child->pid);
            } else
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s odls:kill_local_proc child %s killed",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child->name)));
            
            /* indicate the waitpid fired as this is effectively what
             * has happened
             */
            child->waitpid_recvd = true;
            child->pid = 0;
            
        CLEANUP:
            /* ensure the child's session directory is cleaned up */
            orte_session_dir_finalize(&child->name);
            /* check for everything complete - this will remove
             * the child object from our local list
             */
            if (child->iof_complete && child->waitpid_recvd) {
                ORTE_ACTIVATE_PROC_STATE(&child->name, child->state);
            }
        }
    }
    
    /* cleanup, if required */
    if (do_cleanup) {
        OBJ_DESTRUCT(&procarray);
        OBJ_DESTRUCT(&proctmp);
    }
    
    return ORTE_SUCCESS;
}

int orte_odls_base_get_proc_stats(opal_buffer_t *answer,
                                  orte_process_name_t *proc)
{
    int rc;
    orte_proc_t *child;
    opal_pstats_t stats, *statsptr;
    int i, j;
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls:get_proc_stats for proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    /* find this child */
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        
        if (proc->jobid == child->name.jobid &&
            (proc->vpid == child->name.vpid ||
             ORTE_VPID_WILDCARD == proc->vpid)) { /* found it */

            OBJ_CONSTRUCT(&stats, opal_pstats_t);
            /* record node up to first '.' */
            for (j=0; j < (int)strlen(orte_process_info.nodename) &&
                 j < OPAL_PSTAT_MAX_STRING_LEN-1 &&
                 orte_process_info.nodename[j] != '.'; j++) {
                stats.node[j] = orte_process_info.nodename[j];
            }
            /* record rank */
            stats.rank = child->name.vpid;
            /* get stats */
            rc = opal_pstat.query(child->pid, &stats, NULL);
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

int orte_odls_base_default_restart_proc(orte_proc_t *child,
                                        orte_odls_base_fork_local_proc_fn_t fork_local)
{
    int rc;
    orte_app_context_t *app;
    orte_job_t *jobdat;
    char basedir[MAXPATHLEN];

    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls:restart_proc for proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&child->name)));
    
    /* establish our baseline working directory - we will be potentially
     * bouncing around as we execute this app, but we will always return
     * to this place as our default directory
     */
    getcwd(basedir, sizeof(basedir));

    /* find this child's jobdat */
    if (NULL == (jobdat = orte_get_job_data_object(child->name.jobid))) {
        /* not found */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    child->state = ORTE_PROC_STATE_FAILED_TO_START;
    child->exit_code = 0;
    child->waitpid_recvd = false;
    child->iof_complete = false;
    child->pid = 0;
    if (NULL != child->rml_uri) {
        free(child->rml_uri);
        child->rml_uri = NULL;
    }
    app = (orte_app_context_t*)opal_pointer_array_get_item(jobdat->apps, child->app_idx);

    /* reset envars to match this child */    
    if (ORTE_SUCCESS != (rc = setup_child(child, jobdat, app))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    /* setup the path */
    if (ORTE_SUCCESS != (rc = setup_path(app))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s restarting app %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), app->app));

    rc = fork_local(app, child, app->env, jobdat);
    if (ORTE_SUCCESS == rc) {
        orte_wait_cb(child->pid, odls_base_default_wait_local_proc, NULL);
    }
    
 CLEANUP:
    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls:restart of proc %s %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&child->name),
                         (ORTE_SUCCESS == rc) ? "succeeded" : "failed"));

    /* reset our working directory back to our default location - if we
     * don't do this, then we will be looking for relative paths starting
     * from the last wdir option specified by the user. Thus, we would
     * be requiring that the user keep track on the cmd line of where
     * each app was located relative to the prior app, instead of relative
     * to their current location
     */
    chdir(basedir);

    return rc;
}

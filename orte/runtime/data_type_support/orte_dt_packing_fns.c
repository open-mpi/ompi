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
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/types.h"

#include <sys/types.h>

#include "opal/util/argv.h"
#include "opal/dss/dss.h"
#include "opal/dss/dss_internal.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/errmgr/errmgr.h"
#include "opal/dss/dss.h"
#include "opal/dss/dss_internal.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

/*
 * ORTE_STD_CNTR
 */
int orte_dt_pack_std_cntr(opal_buffer_t *buffer, const void *src,
                            int32_t num_vals, opal_data_type_t type)
{
    int ret;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
                         ret = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_STD_CNTR_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * NAME
 */
int orte_dt_pack_name(opal_buffer_t *buffer, const void *src,
                           int32_t num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i;
    orte_process_name_t* proc;
    orte_jobid_t *jobid;
    orte_vpid_t *vpid;
    
    /* collect all the jobids in a contiguous array */
    jobid = (orte_jobid_t*)malloc(num_vals * sizeof(orte_jobid_t));
    if (NULL == jobid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    proc = (orte_process_name_t*)src;
    for (i=0; i < num_vals; i++) {
        jobid[i] = proc->jobid;
        proc++;
    }
    /* now pack them in one shot */
    if (ORTE_SUCCESS != (rc =
                         orte_dt_pack_jobid(buffer, jobid, num_vals, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        free(jobid);
        return rc;
    }
    free(jobid);
    
    /* collect all the vpids in a contiguous array */
    vpid = (orte_vpid_t*)malloc(num_vals * sizeof(orte_vpid_t));
    if (NULL == vpid) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    proc = (orte_process_name_t*)src;
    for (i=0; i < num_vals; i++) {
        vpid[i] = proc->vpid;
        proc++;
    }
    /* now pack them in one shot */
    if (ORTE_SUCCESS != (rc =
                         orte_dt_pack_vpid(buffer, vpid, num_vals, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        free(vpid);
        return rc;
    }
    free(vpid);

    return ORTE_SUCCESS;
}

/*
 * JOBID
 */
int orte_dt_pack_jobid(opal_buffer_t *buffer, const void *src,
                            int32_t num_vals, opal_data_type_t type)
{
    int ret;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
                         ret = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_JOBID_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * VPID
 */
int orte_dt_pack_vpid(opal_buffer_t *buffer, const void *src,
                           int32_t num_vals, opal_data_type_t type)
{
    int ret;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (
                         ret = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_VPID_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * JOB
 * NOTE: We do not pack all of the job object's fields as many of them have no
 * value in sending them to another location. The only purpose in packing and
 * sending a job object is to communicate the data required to dynamically
 * spawn another job - so we only pack that limited set of required data
 */
int orte_dt_pack_job(opal_buffer_t *buffer, const void *src,
                     int32_t num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i, j;
    orte_job_t **jobs;
    orte_app_context_t *app;
    
    /* array of pointers to orte_job_t objects - need to pack the objects a set of fields at a time */
    jobs = (orte_job_t**) src;

    for (i=0; i < num_vals; i++) {
        /* pack the jobid */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                        (void*)(&(jobs[i]->jobid)), 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of apps */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->num_apps)), 1, ORTE_APP_IDX))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* if there are apps, pack the app_contexts */
        if (0 < jobs[i]->num_apps) {
            for (j=0; j < jobs[i]->apps->size; j++) {
                if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jobs[i]->apps, j))) {
                    continue;
                }
                if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, (void*)&app, 1, ORTE_APP_CONTEXT))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
        
        /* pack the control flags */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->controls)), 1, ORTE_JOB_CONTROL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the stdin target */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->stdin_target)), 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the total slots allocated to the job */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->total_slots_alloc)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* if the map is NULL, then we cannot pack it as there is
         * nothing to pack. However, we have to flag whether or not
         * the map is included so the unpacking routine can know
         * what to do
         */
        if (NULL == jobs[i]->map) {
            /* pack a zero value */
            j=0;
        } else {
            /* pack a one to indicate a map is there */
        }
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                            (void*)&j, 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the map - this will only pack the fields that control
         * HOW a job is to be mapped. We do -not- pack the mapped procs
         * or nodes as this info does not need to be transmitted
         */
        if (NULL != jobs[i]->map) {
            if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                             (void*)(&(jobs[i]->map)), 1, ORTE_JOB_MAP))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* do not pack the bookmark or oversubscribe_override flags */
        
        /* pack the job state */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->state)), 1, ORTE_JOB_STATE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the number launched */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                        (void*)(&(jobs[i]->num_launched)), 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the number reported */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->num_reported)), 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the number terminated */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->num_terminated)), 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the abort flag */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->abort)), 1, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the recovery policy defined flag */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->recovery_defined)), 1, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the recovery flag */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->enable_recovery)), 1, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

#if OPAL_ENABLE_FT_CR == 1
        /* pack the ckpt state */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->ckpt_state)), 1, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the ckpt snapshot ref */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->ckpt_snapshot_ref)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the ckpt snapshot loc */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(jobs[i]->ckpt_snapshot_loc)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
#endif
    }
    return ORTE_SUCCESS;
}

/*
 *  NODE
 */
int orte_dt_pack_node(opal_buffer_t *buffer, const void *src,
                      int32_t num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i;
    orte_node_t **nodes;
    
    /* array of pointers to orte_node_t objects - need to pack the objects a set of fields at a time */
    nodes = (orte_node_t**) src;
    
    for (i=0; i < num_vals; i++) {
        /* do not pack the index - it is meaningless on the other end */
        
        /* pack the node name */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(nodes[i]->name)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* do not pack the daemon name or launch id */
        
        /* pack the number of procs on the node */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(nodes[i]->num_procs)), 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* do not pack the proc info */
        
        /* pack whether we are oversubscribed or not */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(nodes[i]->oversubscribed)), 1, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
                
        /* pack the state */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(nodes[i]->state)), 1, ORTE_NODE_STATE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the number of slots */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(nodes[i]->slots)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the number of slots in use */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(nodes[i]->slots_inuse)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the number of slots allocated */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(nodes[i]->slots)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the max number of slots */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(nodes[i]->slots_max)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* do not pack the username */
    }
    return ORTE_SUCCESS;
}

/*
 * PROC
 */
int orte_dt_pack_proc(opal_buffer_t *buffer, const void *src,
                      int32_t num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i;
    orte_proc_t **procs;
    
    /* array of pointers to orte_proc_t objects - need to pack the objects a set of fields at a time */
    procs = (orte_proc_t**) src;
    
    for (i=0; i < num_vals; i++) {
        /* pack the name */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(procs[i]->name)), 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the local rank */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(procs[i]->local_rank)), 1, ORTE_LOCAL_RANK))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the node rank */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(procs[i]->node_rank)), 1, ORTE_NODE_RANK))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
#if OPAL_HAVE_HWLOC
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                          (void*)(&procs[i]->cpu_bitmap), 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
#endif

        /* pack the state */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(procs[i]->state)), 1, ORTE_PROC_STATE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the app context index */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(procs[i]->app_idx)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the number of restarts */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)&(procs[i]->restarts), 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
#if OPAL_ENABLE_FT_CR == 1
        /* pack the ckpt state */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(procs[i]->ckpt_state)), 1, OPAL_SIZE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the ckpt snapshot ref */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(procs[i]->ckpt_snapshot_ref)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the ckpt snapshot loc */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                         (void*)(&(procs[i]->ckpt_snapshot_loc)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
#endif
    }
    
    return ORTE_SUCCESS;
}

/*
 * APP CONTEXT
 */
int orte_dt_pack_app_context(opal_buffer_t *buffer, const void *src,
                             int32_t num_vals, opal_data_type_t type)
{
    int rc;
    int8_t user_specified;
    int32_t i, count;
    orte_app_context_t **app_context;

    /* array of pointers to orte_app_context objects - need to pack the objects a set of fields at a time */
    app_context = (orte_app_context_t**) src;

    for (i=0; i < num_vals; i++) {
        /* pack the application index (for multiapp jobs) */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->idx)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the application name */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->app)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of processes */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->num_procs)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the first rank for this app */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->first_rank)), 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of entries in the argv array */
        count = opal_argv_count(app_context[i]->argv);
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, (void*)(&count), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are entries, pack the argv entries */
        if (0 < count) {
            if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                           (void*)(app_context[i]->argv), count, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* pack the number of entries in the enviro array */
        count = opal_argv_count(app_context[i]->env);
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, (void*)(&count), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are entries, pack the enviro entries */
        if (0 < count) {
            if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                           (void*)(app_context[i]->env), count, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* pack the cwd */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->cwd)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the user specified cwd flag */
        if (app_context[i]->user_specified_cwd) {
            user_specified = 1;
        } else {
            user_specified = 0;
        }
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&user_specified), 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the use-session-dir cwd flag */
        if (app_context[i]->set_cwd_to_session_dir) {
            user_specified = 1;
        } else {
            user_specified = 0;
        }
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&user_specified), 1, OPAL_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the hostfile name */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->hostfile)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the add_hostfile name */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->add_hostfile)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the add host argv array */
        count = opal_argv_count(app_context[i]->add_host);
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, (void*)(&count), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* if there are entries, pack the argv entries */
        if (0 < count) {
            if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                           (void*)(app_context[i]->add_host), count, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* pack the dash host argv array */
        count = opal_argv_count(app_context[i]->dash_host);
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, (void*)(&count), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are entries, pack the dash_host entries */
        if (0 < count) {
            if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                           (void*)(app_context[i]->dash_host), count, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* pack the prefix dir */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->prefix_dir)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->preload_binary)), 1, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* Pack the preload_files */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->preload_files)), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the recovery defined flag */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->recovery_defined)), 1, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* pack the restart limit */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                       (void*)(&(app_context[i]->max_restarts)), 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

#if OPAL_ENABLE_FT_CR == 1
        {
            int8_t have_sstore;
            /* Pack the preload_files_src_dir if we have one */
            if (NULL != app_context[i]->sstore_load) {
                have_sstore = 1;
            } else {
                have_sstore = 0;
            }
        
            if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                           (void*)(&have_sstore), 1, OPAL_INT8))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        
            if( have_sstore) {
                if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer,
                                                               (void*)(&(app_context[i]->sstore_load)), 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
#endif
    }
    
    return ORTE_SUCCESS;
}

/*
 * EXIT CODE
 */
int orte_dt_pack_exit_code(opal_buffer_t *buffer, const void *src,
                                 int32_t num_vals, opal_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_EXIT_CODE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * NODE STATE
 */
int orte_dt_pack_node_state(opal_buffer_t *buffer, const void *src,
                                  int32_t num_vals, opal_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_NODE_STATE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * PROC STATE
 */
int orte_dt_pack_proc_state(opal_buffer_t *buffer, const void *src,
                                  int32_t num_vals, opal_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_PROC_STATE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * JOB STATE
 */
int orte_dt_pack_job_state(opal_buffer_t *buffer, const void *src,
                                 int32_t num_vals, opal_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_JOB_STATE_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * JOB_MAP
 * NOTE: There is no obvious reason to include all the node information when
 * sending a map
 */
int orte_dt_pack_map(opal_buffer_t *buffer, const void *src,
                             int32_t num_vals, opal_data_type_t type)
{
    int rc;
    int32_t i;
    orte_job_map_t **maps;
    
    /* array of pointers to orte_job_map_t objects - need to pack the objects a set of fields at a time */
    maps = (orte_job_map_t**) src;
    
    for (i=0; i < num_vals; i++) {
        /* pack the requested mapper */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, &(maps[i]->req_mapper), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the policies */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, &(maps[i]->mapping), 1, ORTE_MAPPING_POLICY))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, &(maps[i]->ranking), 1, ORTE_RANKING_POLICY))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
#if OPAL_HAVE_HWLOC
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, &(maps[i]->binding), 1, OPAL_BINDING_POLICY))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
#endif
        /* pack any ppr */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, &(maps[i]->ppr), 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the display map flag */
        if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, &(maps[i]->display_map), 1, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}

/*
 * RML TAG
 */
int orte_dt_pack_tag(opal_buffer_t *buffer, const void *src,
                           int32_t num_vals, opal_data_type_t type)
{
    int rc;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (rc = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_RML_TAG_T))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * ORTE_DAEMON_CMD
 */
int orte_dt_pack_daemon_cmd(opal_buffer_t *buffer, const void *src, int32_t num_vals,
                              opal_data_type_t type)
{
    int ret;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (ret = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_DAEMON_CMD_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

/*
 * ORTE_IOF_TAG
 */
int orte_dt_pack_iof_tag(opal_buffer_t *buffer, const void *src, int32_t num_vals,
                              opal_data_type_t type)
{
    int ret;
    
    /* Turn around and pack the real type */
    if (ORTE_SUCCESS != (ret = opal_dss_pack_buffer(buffer, src, num_vals, ORTE_IOF_TAG_T))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

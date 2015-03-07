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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

#include "opal_stdint.h"
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
#include "orte/mca/schizo/schizo.h"
#include "orte/mca/state/state.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/dfs/dfs.h"

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
    int rc, i;
    orte_job_t *jdata=NULL, *jptr;
    orte_job_map_t *map=NULL;
    opal_buffer_t *wireup, jobdata;
    opal_byte_object_t bo, *boptr;
    int32_t numbytes, numjobs;
    int8_t flag;

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

    /* check if this job caused daemons to be spawned - if it did,
     * then we need to ensure that those daemons get a complete
     * copy of all active jobs so the grpcomm collectives can
     * properly work should a proc from one of the other jobs
     * interact with this one */
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_LAUNCHED_DAEMONS, NULL, OPAL_BOOL)) {
        OBJ_CONSTRUCT(&jobdata, opal_buffer_t);
        numjobs = 0;
        for (i=0; i < orte_job_data->size; i++) {
            if (NULL == (jptr = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, i))) {
                continue;
            }
            if (ORTE_JOB_STATE_UNTERMINATED < jptr->state) {
                /* job already terminated - ignore it */
                continue;
            }
            if (jptr == jdata) {
                /* ignore the job we are looking at - we'll get it separately */
                continue;
            }
            /* pack the job struct */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&jobdata, &jptr, 1, ORTE_JOB))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            ++numjobs;
        }
        if (0 < numjobs) {
            /* pack the number of jobs */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &numjobs, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* pack the jobdata buffer */
            wireup = &jobdata;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &wireup, 1, OPAL_BUFFER))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&jobdata);
                return rc;
            }
            OBJ_DESTRUCT(&jobdata);
        }
    } else {
        numjobs = 0;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &numjobs, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }


    /* pack the job struct */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata, 1, ORTE_JOB))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

static void fm_release(void *cbdata)
{
    opal_buffer_t *bptr = (opal_buffer_t*)cbdata;

    OBJ_RELEASE(bptr);
}

int orte_odls_base_default_construct_child_list(opal_buffer_t *data,
                                                orte_jobid_t *job)
{
    int rc;
    orte_std_cntr_t cnt;
    orte_job_t *jdata=NULL, *daemons;
    int32_t n, j, k;
    orte_proc_t *pptr, *dmn;
    opal_buffer_t *bptr;
    orte_app_context_t *app;
    bool found;
    orte_node_t *node;

    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls:constructing child list",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    *job = ORTE_JOBID_INVALID;

    /* unpack the flag to see if additional jobs are included in the data */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &n, &cnt, OPAL_INT32))) {
        *job = ORTE_JOBID_INVALID;
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }

    /* get the daemon job object */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);

    if (0 < n) {
        /* unpack the buffer containing the info */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &bptr, &cnt, OPAL_BUFFER))) {
            *job = ORTE_JOBID_INVALID;
            ORTE_ERROR_LOG(rc);
            goto REPORT_ERROR;
        }
        for (k=0; k < n; k++) {
            /* unpack each job and add it to the local orte_job_data array */
            cnt=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(bptr, &jdata, &cnt, ORTE_JOB))) {
                *job = ORTE_JOBID_INVALID;
                ORTE_ERROR_LOG(rc);
                goto REPORT_ERROR;
            }
            /* check to see if we already have this one */
            if (NULL == orte_get_job_data_object(jdata->jobid)) {
                /* nope - add it */
                opal_pointer_array_set_item(orte_job_data, ORTE_LOCAL_JOBID(jdata->jobid), jdata);
                /* connect each proc to its node object */
                for (j=0; j < jdata->procs->size; j++) {
                    if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, j))) {
                        continue;
                    }
                    if (NULL == (dmn = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, pptr->parent))) {
                        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                        return ORTE_ERR_NOT_FOUND;
                    }
                    OBJ_RETAIN(dmn->node);
                    pptr->node = dmn->node;
                    /* add proc to node - note that num_procs for the
                     * node was already correctly unpacked, so don't
                     * increment it here */
                    OBJ_RETAIN(pptr);
                    opal_pointer_array_add(dmn->node->procs, pptr);
                }
            } else {
                /* yep - so we can drop this copy */
                jdata->jobid = ORTE_JOBID_INVALID;
                OBJ_RELEASE(jdata);
            }
        }
        /* release the buffer */
        OBJ_RELEASE(bptr);
    }
    
    /* unpack the job we are to launch */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &jdata, &cnt, ORTE_JOB))) {
        *job = ORTE_JOBID_INVALID;
        ORTE_ERROR_LOG(rc);
        goto REPORT_ERROR;
    }
    if (ORTE_JOBID_INVALID == jdata->jobid) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        rc = ORTE_ERR_BAD_PARAM;
        goto REPORT_ERROR;
    }
    *job = jdata->jobid;

    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                         "%s odls:construct_child_list unpacking data to launch job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(*job)));
    
    /* if we are the HNP, we don't need to unpack this buffer - we already
     * have all the required info in our local job array. So just build the
     * array of local children
     */
    if (ORTE_PROC_IS_HNP) {
        /* we don't want/need the extra copy of the orte_job_t, but
         * we can't just release it as that will NULL the location in
         * the orte_job_data array. So set the jobid to INVALID to
         * protect the array, and then release the object to free
         * the storage */
        jdata->jobid = ORTE_JOBID_INVALID;
        OBJ_RELEASE(jdata);
        /* get the correct job object */
        if (NULL == (jdata = orte_get_job_data_object(*job))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
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
            /* see if it belongs to us */
            if (pptr->parent == ORTE_PROC_MY_NAME->vpid) {
                /* is this child on our current list of children */
                if (!ORTE_FLAG_TEST(pptr, ORTE_PROC_FLAG_LOCAL)) {
                    /* not on the local list */
                    OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                         "%s adding proc %s to my local list",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_NAME_PRINT(&pptr->name)));
                    /* keep tabs of the number of local procs */
                    jdata->num_local_procs++;
                    /* add this proc to our child list */
                    OBJ_RETAIN(pptr);
                    ORTE_FLAG_SET(pptr, ORTE_PROC_FLAG_LOCAL);
                    opal_pointer_array_add(orte_local_children, pptr);
                }

                /* if the job is in restart mode, the child must not barrier when launched */
                if (ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_RESTART)) {
                    orte_set_attribute(&pptr->attributes, ORTE_PROC_NOBARRIER, ORTE_ATTR_LOCAL, NULL, OPAL_BOOL);
                }
                /* mark that this app_context is being used on this node */
                app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, pptr->app_idx);
                ORTE_FLAG_SET(app, ORTE_APP_FLAG_USED_ON_NODE);
            }
        }
        goto COMPLETE;
    }

    if (NULL != orte_get_job_data_object(*job)) {
        opal_output(0, "ERROR - JOB ALREADY EXISTS");
        /* setup job object for this job */
        rc = ORTE_ERR_FATAL;
        goto REPORT_ERROR;
    }
    opal_pointer_array_set_item(orte_job_data, ORTE_LOCAL_JOBID(jdata->jobid), jdata);
    
    /* ensure the map object is present */
    if (NULL == jdata->map) {
        jdata->map = OBJ_NEW(orte_job_map_t);
    }

    /* if we have a file map, then we need to load it */
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_FILE_MAPS, (void**)&bptr, OPAL_BUFFER)) {
        if (NULL != orte_dfs.load_file_maps) {
            orte_dfs.load_file_maps(jdata->jobid, bptr, fm_release, bptr);
        } else {
            OBJ_RELEASE(bptr);
        }
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
        opal_output_verbose(5, orte_odls_base_framework.framework_output,
                            "%s GETTING DAEMON FOR PROC %s WITH PARENT %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&pptr->name),
                            ORTE_VPID_PRINT(pptr->parent));
        if (ORTE_VPID_INVALID == pptr->parent) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            ORTE_FORCED_TERMINATE(ORTE_ERR_BAD_PARAM);
            return ORTE_ERR_BAD_PARAM;
        }
        /* connect the proc to its node object */
        if (NULL == (dmn = (orte_proc_t*)opal_pointer_array_get_item(daemons->procs, pptr->parent))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        OBJ_RETAIN(dmn->node);
        pptr->node = dmn->node;
        /* add proc to node - note that num_procs for the
         * node was already correctly unpacked, so don't
         * increment it here */
        OBJ_RETAIN(pptr);
        opal_pointer_array_add(dmn->node->procs, pptr);

        /* add the node to the map, if not already there */
        found = false;
        for (k=0; k < jdata->map->nodes->size; k++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(jdata->map->nodes, k))) {
                continue;
            }
            if (node->daemon == dmn) {
                found = true;
                break;
            }
        }
        if (!found) {
            OBJ_RETAIN(dmn->node);
            opal_pointer_array_add(jdata->map->nodes, dmn->node);
            jdata->map->num_nodes++;
        }

        /* see if it belongs to us */
        if (pptr->parent == ORTE_PROC_MY_NAME->vpid) {
            /* is this child on our current list of children */
            if (!ORTE_FLAG_TEST(pptr, ORTE_PROC_FLAG_LOCAL)) {
                /* not on the local list */
                OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                     "%s adding proc %s to my local list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&pptr->name)));
                /* keep tabs of the number of local procs */
                jdata->num_local_procs++;
                /* add this proc to our child list */
                OBJ_RETAIN(pptr);
                ORTE_FLAG_SET(pptr, ORTE_PROC_FLAG_LOCAL);
                opal_pointer_array_add(orte_local_children, pptr);
            }

            /* if the job is in restart mode, the child must not barrier when launched */
            if (ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_RESTART)) {
                orte_set_attribute(&pptr->attributes, ORTE_PROC_NOBARRIER, ORTE_ATTR_LOCAL, NULL, OPAL_BOOL);
            }
            /* mark that this app_context is being used on this node */
            app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, pptr->app_idx);
            ORTE_FLAG_SET(app, ORTE_APP_FLAG_USED_ON_NODE);
        }
    }

 COMPLETE:
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

static int setup_path(orte_app_context_t *app)
{
    int rc;
    char dir[MAXPATHLEN];
    char **argvptr;
    char *pathenv = NULL, *mpiexec_pathenv = NULL;
    char *full_search;

    if (!orte_get_attribute(&app->attributes, ORTE_APP_SSNDIR_CWD, NULL, OPAL_BOOL)) {
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
        opal_setenv(OPAL_MCA_PREFIX"initial_wdir", dir, true, &app->env);
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
        if (!ORTE_FLAG_TEST(child, ORTE_PROC_FLAG_ALIVE)) {
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
    int rc=ORTE_SUCCESS;
    orte_std_cntr_t proc_rank;
    char basedir[MAXPATHLEN];
    char **argvsav=NULL;
    int inm, j, idx;
    int total_num_local_procs = 0;
    orte_odls_launch_local_t *caddy = (orte_odls_launch_local_t*)cbdata;
    orte_job_t *jobdat;
    orte_jobid_t job = caddy->job;
    orte_odls_base_fork_local_proc_fn_t fork_local = caddy->fork_local;
    bool index_argv;

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
        opal_output_verbose(5, orte_odls_base_framework.framework_output,
                            "%s local:launch no local procs",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        goto GETOUT;
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
    
#if OPAL_ENABLE_FT_CR == 1
    for (j=0; j < jobdat->apps->size; j++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jobdat->apps, j))) {
            continue;
        }
        orte_sstore.fetch_app_deps(app);
    }
    orte_sstore.wait_all_deps();
#endif

    /* track if we are indexing argvs so we don't check every time */
    index_argv = orte_get_attribute(&jobdat->attributes, ORTE_JOB_INDEX_ARGV, NULL, OPAL_BOOL);

    /* compute the total number of local procs currently alive and about to be launched */
    total_num_local_procs = compute_num_procs_alive(job) + jobdat->num_local_procs;

    for (j=0; j < jobdat->apps->size; j++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jobdat->apps, j))) {
            continue;
        }
        
        /* if this app isn't being used on our node, skip it */
        if (!ORTE_FLAG_TEST(app, ORTE_APP_FLAG_USED_ON_NODE)) {
            opal_output_verbose(5, orte_odls_base_framework.framework_output,
                                "%s app %d not used on node",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j);
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
        if (ORTE_SUCCESS != (rc = orte_schizo.setup_fork(jobdat, app))) {
            
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
            if (ORTE_FLAG_TEST(child, ORTE_PROC_FLAG_ALIVE)) {
                
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
            ORTE_FLAG_UNSET(child, ORTE_PROC_FLAG_WAITPID);
            /* if we are not forwarding output for this job, then
             * flag iof as complete
             */
            if (ORTE_FLAG_TEST(jobdat, ORTE_JOB_FLAG_FORWARD_OUTPUT)) {
                ORTE_FLAG_UNSET(child, ORTE_PROC_FLAG_IOF_COMPLETE);
            } else {
                ORTE_FLAG_SET(child, ORTE_PROC_FLAG_IOF_COMPLETE);
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
            if (ORTE_SUCCESS != (rc = orte_schizo.setup_child(jobdat, child, app))) {
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
            if (index_argv) {
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
            
            orte_wait_cb(child, odls_base_default_wait_local_proc, NULL);
            if (ORTE_SUCCESS != (rc = fork_local(app, child, app->env, jobdat))) {
                orte_wait_cb_cancel(child);
                child->exit_code = ORTE_ERR_SILENT; /* error message already output */
                ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_START);
            }
            /* if we indexed the argv, we need to restore it to
             * its original form
             */
            if (index_argv) {
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
                ORTE_FLAG_SET(child, ORTE_PROC_FLAG_ALIVE);
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
        if (!ORTE_FLAG_TEST(child, ORTE_PROC_FLAG_ALIVE) ||
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

/*
 *  Wait for a callback indicating the child has completed.
 */

void odls_base_default_wait_local_proc(orte_proc_t *proc, void* cbdata)
{
    int i;
    orte_job_t *jobdat;
    orte_proc_state_t state=ORTE_PROC_STATE_WAITPID_FIRED;
    orte_proc_t *cptr;

    opal_output_verbose(5, orte_odls_base_framework.framework_output,
                        "%s odls:wait_local_proc child process %s pid %ld terminated",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&proc->name), (long)proc->pid);
    
    /* if the child was previously flagged as dead, then just
     * update its exit status and
     * ensure that its exit state gets reported to avoid hanging
     * don't forget to check if the process was signaled.
     */
    if (!ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_ALIVE)) {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s was already dead exit code %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name),proc->exit_code));
        if (WIFEXITED(proc->exit_code)) {
            proc->exit_code = WEXITSTATUS(proc->exit_code);
        } else {
            if (WIFSIGNALED(proc->exit_code)) {
                state = ORTE_PROC_STATE_ABORTED_BY_SIG;
                proc->exit_code = WTERMSIG(proc->exit_code) + 128;
            }
        }
        goto MOVEON;
    }

    /* if the proc called "abort", then we just need to flag that it
     * came thru here */
    if (ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_ABORT)) {
        /* even though the process exited "normally", it happened
         * via an orte_abort call
         */
        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s died by call to abort",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name)));
        state = ORTE_PROC_STATE_CALLED_ABORT;
        /* regardless of our eventual code path, we need to
         * flag that this proc has had its waitpid fired */
        ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_WAITPID);
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
    if (ORTE_FLAG_TEST(jobdat, ORTE_JOB_FLAG_DEBUGGER_DAEMON))  {
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
        /* regardless of our eventual code path, we need to
         * flag that this proc has had its waitpid fired */
        ORTE_FLAG_SET(proc, ORTE_PROC_FLAG_WAITPID);
        goto MOVEON;
    }
    
    /* determine the state of this process */
    if (WIFEXITED(proc->exit_code)) {

        /* set the exit status appropriately */
        proc->exit_code = WEXITSTATUS(proc->exit_code);

        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child %s exit code %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name), proc->exit_code));

        /* provide a default state */
        state = ORTE_PROC_STATE_WAITPID_FIRED;

        /* check to see if a sync was required and if it was received */
        if (ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_REG)) {
            if (ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_HAS_DEREG) ||
                orte_allowed_exit_without_sync || 0 != proc->exit_code) {
                /* if we did recv a finalize sync, or one is not required,
                 * then declare it normally terminated
                 * unless it returned with a non-zero status indicating the code
                 * felt it was non-normal - in this latter case, we do not
                 * require that the proc deregister before terminating
                 */
                if (0 != proc->exit_code && orte_abort_non_zero_exit) {
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
                if (ORTE_FLAG_TEST(cptr, ORTE_PROC_FLAG_REG) && !orte_allowed_exit_without_sync) {
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
            if (0 != proc->exit_code && orte_abort_non_zero_exit) {
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
        proc->exit_code = WTERMSIG(proc->exit_code) + 128;

        OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                             "%s odls:waitpid_fired child process %s terminated with signal",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name) ));
        /* Do not decrement the number of local procs here. That is handled in the errmgr */
    }
    
 MOVEON:
    ORTE_ACTIVATE_PROC_STATE(&proc->name, state);
}

typedef struct {
    orte_proc_t *child;
    orte_odls_base_kill_local_fn_t kill_local;
} odls_kill_caddy_t;

static void kill_cbfunc(int fd, short args, void *cbdata)
{
    odls_kill_caddy_t *cd = (odls_kill_caddy_t*)cbdata;

    if (!ORTE_FLAG_TEST(cd->child, ORTE_PROC_FLAG_ALIVE) || 0 == cd->child->pid) {
        free(cd);
        return;
    }
    cd->kill_local(cd->child->pid, SIGKILL);
    free(cd);
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
            if (!ORTE_FLAG_TEST(child, ORTE_PROC_FLAG_ALIVE) || 0 == child->pid) {
                
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
                    ORTE_FLAG_SET(child, ORTE_PROC_FLAG_WAITPID);
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
            orte_wait_cb_cancel(child);
            
            if (!do_cleanup) {
                odls_kill_caddy_t *cd;

                /* if we are killing only selected procs, then do so in a gentle
                   fashion. First send a SIGCONT in case the process is in stopped state.
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
                /* provide a polite delay so the proc has a chance to react */
                cd = (odls_kill_caddy_t*)malloc(sizeof(odls_kill_caddy_t));
                OBJ_RETAIN(child);  // protect against race conditions
                cd->child = child;
                cd->kill_local = kill_local;
                ORTE_TIMER_EVENT(1, 0, kill_cbfunc, ORTE_SYS_PRI);
                continue;
            }

            /* Force the SIGKILL just to make sure things are dead
             * This fixes an issue that, if the application is masking
             * SIGTERM, then the child_died()
             * may return 'true' even though waipid returns with 0.
             * It does this to avoid a race condition, per documentation
             * in odls_default_module.c.
             */
            OPAL_OUTPUT_VERBOSE((5, orte_odls_base_framework.framework_output,
                                 "%s SENDING FORCE SIGKILL TO %s pid %lu",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&child->name), (unsigned long)child->pid));
            kill_local(child->pid, SIGKILL);
            
            /* indicate the waitpid fired as this is effectively what
             * has happened
             */
            ORTE_FLAG_SET(child, ORTE_PROC_FLAG_WAITPID);
            child->pid = 0;
            
        CLEANUP:
            /* ensure the child's session directory is cleaned up */
            orte_session_dir_finalize(&child->name);
            /* check for everything complete - this will remove
             * the child object from our local list
             */
            if (ORTE_FLAG_TEST(child, ORTE_PROC_FLAG_IOF_COMPLETE) &&
                ORTE_FLAG_TEST(child, ORTE_PROC_FLAG_WAITPID)) {
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
    ORTE_FLAG_UNSET(child, ORTE_PROC_FLAG_WAITPID);
    ORTE_FLAG_UNSET(child, ORTE_PROC_FLAG_IOF_COMPLETE);
    child->pid = 0;
    if (NULL != child->rml_uri) {
        free(child->rml_uri);
        child->rml_uri = NULL;
    }
    app = (orte_app_context_t*)opal_pointer_array_get_item(jobdat->apps, child->app_idx);

    /* reset envars to match this child */    
    if (ORTE_SUCCESS != (rc = orte_schizo.setup_child(jobdat, child, app))) {
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

    orte_wait_cb(child, odls_base_default_wait_local_proc, NULL);
    if (ORTE_SUCCESS != (rc = fork_local(app, child, app->env, jobdat))) {
        orte_wait_cb_cancel(child);
        child->exit_code = ORTE_ERR_SILENT; /* error message already output */
        ORTE_ACTIVATE_PROC_STATE(&child->name, ORTE_PROC_STATE_FAILED_TO_START);
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

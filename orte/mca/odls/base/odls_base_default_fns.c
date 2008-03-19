/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Sun Microsystems, Inc.  All rights reserved.
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
#include <signal.h>

#include "opal/util/opal_environ.h"
#include "opal/util/argv.h"
#include "opal/util/os_path.h"
#include "opal/util/num_procs.h"
#include "opal/util/sys_limits.h"
#include "opal/util/show_help.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/iof_base_setup.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/routed/base/base.h"

#include "orte/util/context_fns.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/sys_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#if OPAL_ENABLE_FT == 1
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#endif

#include "orte/mca/odls/base/odls_private.h"

/* IT IS CRITICAL THAT ANY CHANGE IN THE ORDER OF THE INFO PACKED IN
 * THIS FUNCTION BE REFLECTED IN THE CONSTRUCT_CHILD_LIST PARSER BELOW
*/
int orte_odls_base_default_get_add_procs_data(opal_buffer_t *data,
                                              orte_jobid_t job)
{
    orte_node_t **nodes, *node;
    orte_proc_t **procs, *proc;
    int rc;
    orte_job_t *jdata;
    orte_job_map_t *map;
    orte_std_cntr_t i;
    orte_vpid_t j;
    orte_vpid_t invalid_vpid=ORTE_VPID_INVALID;

    /* get the job data pointer */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* pack the jobid so it can be extracted later */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the number of procs in the job - equates to the vpid range for the job */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->num_procs, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the total slots allocated to us */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->total_slots_alloc, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* pack the override_oversubscribed flag */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &jdata->oversubscribe_override, 1, OPAL_BOOL))) {
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
    
    /* the remainder of our required info is in the node objects in this job's map,
     * so pickup a pointer to that map
     */
    map = jdata->map;

    /* pack the number of nodes participating in this launch */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &map->num_nodes, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* cycle through the participating nodes */
    nodes = (orte_node_t**)map->nodes->addr;
    for (i=0; i < map->num_nodes; i++) {
        node = nodes[i];
        /* PACK NODE-SPECIFIC DATA */
        /* pack the vpid of the daemon on this node - this will be
         * later used to tell the daemon it has something to do
         */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &(node->daemon->name.vpid), 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the number of procs on this node */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &node->num_procs, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* pack the oversubscribed flag for the node */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &node->oversubscribed, 1, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* PACK THE PROC-SPECIFIC DATA FOR THE PROCS **TO BE LAUNCHED** ON THIS NODE
         * FOR THIS JOB
         *
         * NOTE: The nodes object contains info on ALL procs on the node, not just those
         * to be launched for the specified job. Thus, we must take care to CLEARLY
         * demarcate info on those procs to be launched, or else we will get
         * duplicate processes!!
         */
        /* we already packed the number of procs on the node, so cycle
         * through them and pack each one's launch data
         */
        procs = (orte_proc_t**)node->procs->addr;
        for (j=0; j < node->num_procs; j++) {
            proc = procs[j];  /* convenience */
            /* the mapped node includes ALL procs on it, not just those for the
             * job to be launched. Hence, check first to see if this proc is
             * part of the indicated job - if not, don't include it here
             */
            if (proc->name.jobid != job) {
               continue;
            }
            /* pack the vpid for this proc */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &(proc->name.vpid), 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* pack the app_context index */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &proc->app_idx, 1, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* pack the local rank */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &proc->local_rank, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* pack the cpu_list string */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &proc->slot_list, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        /* pack an INVALID vpid as a flag that we are done with procs for this node */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(data, &invalid_vpid, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }
    
    return ORTE_SUCCESS;
}

int orte_odls_base_default_construct_child_list(opal_buffer_t *data,
                                                orte_jobid_t *job,
                                                orte_std_cntr_t *num_local_procs,
                                                orte_vpid_t *vpid_range,
                                                orte_std_cntr_t *total_slots_alloc,
                                                bool *node_included,
                                                bool *oversubscribed,
                                                bool *override_oversubscribed,
                                                orte_std_cntr_t *num_contexts,
                                                orte_app_context_t ***app_contexts)
{
    int rc;
    orte_vpid_t local_rank, num_procs;
    orte_odls_child_t *child;
    orte_std_cntr_t cnt, j, num_nodes, app_idx;
    orte_process_name_t proc, daemon;
    char *slot_str;
    bool node_oversubscribed;

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
    *num_local_procs = 0;
    *vpid_range = ORTE_VPID_INVALID;
    *total_slots_alloc = 0;
    *node_included = false;
    *oversubscribed = false;
    *override_oversubscribed = false;
    
    /* unpack the jobid we are to launch */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, job, &cnt, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:construct_child_list unpacking data to launch job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(*job)));
    
    /* UNPACK JOB-SPECIFIC DATA */
    /* unpack the number of procs in the job - equates to the vpid range */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, vpid_range, &cnt, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the total slots allocated to us */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, total_slots_alloc, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the override_oversubscribed flag */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, override_oversubscribed, &cnt, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of app_contexts for this job */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, num_contexts, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:construct_child_list unpacking %ld app_contexts",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)*num_contexts));
    
    /* allocate space and unpack the app_contexts for this job - the HNP checked
     * that there must be at least one, so don't bother checking here again
     */
    *app_contexts = (orte_app_context_t**)malloc(*num_contexts * sizeof(orte_app_context_t*));
    if (NULL == *app_contexts) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, *app_contexts, num_contexts, ORTE_APP_CONTEXT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* UNPACK THE NODE-SPECIFIC DATA */
    /* unpack the number of nodes participating in this launch */
    cnt=1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &num_nodes, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* setup the proc and daemon names */
    proc.jobid = *job;
    daemon.jobid = ORTE_PROC_MY_NAME->jobid;
    
    /* cycle through them */
    for (j=0; j < num_nodes; j++) {
        /* unpack the vpid of the daemon */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &daemon.vpid, &cnt, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* unpack the number of procs on this node */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &num_procs, &cnt, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* unpack the oversubscribed flag for the node */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &node_oversubscribed, &cnt, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* cycle through the procs and unpack their data */
        /* unpack the vpid for this proc */
        cnt=1;
        while (ORTE_SUCCESS == (rc = opal_dss.unpack(data, &(proc.vpid), &cnt, ORTE_VPID))) {
            if (ORTE_VPID_INVALID == proc.vpid) {
                /* this flags that all data from this node has been read - there
                 * will be no further entries for it
                 */
                break;
            }
            /* unpack the app_context index */
            cnt=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &app_idx, &cnt, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* unpack the local rank */
            cnt=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &local_rank, &cnt, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* unpack the cpu_list string */
            cnt=1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(data, &slot_str, &cnt, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* does this data belong to us? */
            if (ORTE_PROC_MY_NAME->vpid == daemon.vpid) {
                /* yes it does - add this proc to our child list */
                child = OBJ_NEW(orte_odls_child_t);
                /* copy the name to preserve it */
                if (ORTE_SUCCESS != (rc = opal_dss.copy((void**)&child->name, &proc, ORTE_NAME))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                child->app_idx = app_idx;  /* save the index into the app_context objects */
                child->local_rank = local_rank;  /* save the local_rank */
                if (NULL != slot_str) {
                    child->slot_list = strdup(slot_str);
                    free(slot_str);
                }
                /* protect operation on the global list of children */
                OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
                opal_list_append(&orte_odls_globals.children, &child->super);
                opal_condition_signal(&orte_odls_globals.cond);
                OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
                /* pass along the local info */
                *num_local_procs = num_procs;
                *oversubscribed = node_oversubscribed;
                /* set the routing info to be direct - we need to do this
                 * prior to launch as the procs may want to communicate right away
                 */
                if (ORTE_SUCCESS != (rc = orte_routed.update_route(&proc, &proc))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            } else {
                /* set the routing info through the other daemon - we need to do this
                 * prior to launch as the procs may want to communicate right away
                 */
                if (ORTE_SUCCESS != (rc = orte_routed.update_route(&proc, &daemon))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
        if (ORTE_SUCCESS != rc && ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            ORTE_ERROR_LOG(rc);
        }

        /* do we have any launching to do? */
        if (ORTE_PROC_MY_NAME->vpid == daemon.vpid) {
            /* pass that along for later */
            *node_included = true;
        }
    }

    return ORTE_SUCCESS;
}

static int odls_base_default_setup_fork(orte_app_context_t *context,
                                        orte_std_cntr_t num_local_procs,
                                        orte_vpid_t vpid_range,
                                        orte_std_cntr_t total_slots_alloc,
                                        bool oversubscribed, char ***environ_copy)
{
    int rc;
    int i;
    char *param, *param2;
    char *full_search;
    char *pathenv = NULL, *mpiexec_pathenv = NULL;
    char **argvptr;

    /* check the system limits - if we are at our max allowed children, then
     * we won't be allowed to do this anyway, so we may as well abort now.
     * According to the documentation, num_procs = 0 is equivalent to
     * no limit, so treat it as unlimited here.
     */
    if (opal_sys_limits.initialized) {
        if (0 < opal_sys_limits.num_procs &&
            opal_sys_limits.num_procs <= (int)opal_list_get_size(&orte_odls_globals.children)) {
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

    /* Try to change to the context cwd and check that the app
        exists and is executable The function will
        take care of outputting a pretty error message, if required
        */
    if (ORTE_SUCCESS != (rc = orte_util_check_context_cwd(context, true))) {
        /* do not ERROR_LOG - it will be reported elsewhere */
        return rc;
    }

    /* Search for the OMPI_exec_path and PATH settings in the environment. */
    for (argvptr = *environ_copy; *argvptr != NULL; argvptr++) { 
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
    if (mpiexec_pathenv) {
        argvptr = NULL;
        if (pathenv != NULL) {
            asprintf(&full_search, "%s:%s", mpiexec_pathenv, pathenv);
        } else {
            asprintf(&full_search, "%s", mpiexec_pathenv);
        }
        opal_setenv("PATH", full_search, true, &argvptr);
        free(full_search);
    } else {
        argvptr = *environ_copy;
    }

    if (ORTE_SUCCESS != (rc = orte_util_check_context_app(context, argvptr))) {
        /* do not ERROR_LOG - it will be reported elsewhere */
        if (mpiexec_pathenv) {
            opal_argv_free(argvptr);
        }
        return rc;
    }
    if (mpiexec_pathenv) {
        opal_argv_free(argvptr);
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
    for (i = 0; NULL != context->env && NULL != context->env[i]; ++i) {
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
    
    /* pass our vpid to the process as a "nodeid" so it can
     * identify which procs are local to it
     */
    if (ORTE_SUCCESS != (rc = orte_util_convert_vpid_to_string(&param2, ORTE_PROC_MY_NAME->vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    param = mca_base_param_environ_variable("orte","nodeid",NULL);
    opal_setenv(param, param2, true, environ_copy);
    free(param);
    free(param2);
    
    /* setup yield schedule and processor affinity
     * We default here to always setting the affinity processor if we want
     * it. The processor affinity system then determines
     * if processor affinity is enabled/requested - if so, it then uses
     * this value to select the process to which the proc is "assigned".
     * Otherwise, the paffinity subsystem just ignores this value anyway
     */
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
    
    /* set the universe size in the environment */
    param = mca_base_param_environ_variable("orte","universe","size");
    asprintf(&param2, "%ld", (long)total_slots_alloc);
    opal_setenv(param, param2, true, environ_copy);
    free(param);

    /* although the total_slots_alloc is the universe size, users
     * would appreciate being given a public environmental variable
     * that also represents this value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    opal_setenv("OMPI_UNIVERSE_SIZE", param2, true, environ_copy);
    free(param2);
    
    /* use same nodename as the starting daemon (us) */
    param = mca_base_param_environ_variable("orte", "base", "nodename");
    opal_setenv(param, orte_system_info.nodename, true, environ_copy);
    free(param);
    
    /* push data into environment - don't push any single proc
     * info, though. We are setting the environment up on a
     * per-context basis, and will add the individual proc
     * info later. This also sets the mca param to select
     * the "env" component in the SDS framework.
     */
    orte_ess_env_put(vpid_range, num_local_procs, environ_copy);
    
    return ORTE_SUCCESS;
}

static int pack_state_for_proc(opal_buffer_t *alert, bool pack_pid, orte_odls_child_t *child)
{
    int rc;
    
    /* pack the child's vpid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &(child->name->vpid), 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* pack its pid if we need to report it */
    if (pack_pid) {
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->pid, 1, OPAL_PID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
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

static int pack_state_update(opal_buffer_t *alert, bool pack_pid, orte_jobid_t job)
{
    int rc;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    orte_vpid_t null=ORTE_VPID_INVALID;
    
    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    for (item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        /* if this child is part of the job... */
        if (child->name->jobid == job) {
            if (ORTE_SUCCESS != (rc = pack_state_for_proc(alert, pack_pid, child))) {
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
                                        orte_std_cntr_t num_apps,
                                        orte_app_context_t **apps,
                                        orte_std_cntr_t num_local_procs,
                                        orte_vpid_t vpid_range,
                                        orte_std_cntr_t total_slots_alloc,
                                        bool node_oversubscribed,
                                        bool override_oversubscribed,
                                        orte_odls_base_fork_local_proc_fn_t fork_local)
{
    char *job_str, *vpid_str, *param, *value;
    opal_list_item_t *item;
    orte_app_context_t *app;
    orte_odls_child_t *child;
    int i, num_processors;
    bool want_processor, oversubscribed;
    int rc=ORTE_SUCCESS, ret;
    bool launch_failed=true;
    opal_buffer_t alert;
    orte_std_cntr_t proc_rank;
    
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);

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
                goto unlock;
            }
        }
    }
    
    /* default oversubscribe to what the mapper told us */
    oversubscribed = node_oversubscribed;
    
    /* setup for processor affinity. If there are enough physical processors on this node, then
     * we indicate which processor each process should be assigned to, IFF the user has requested
     * processor affinity be used - the paffinity subsystem will make that final determination. All
     * we do here is indicate that we should do the definitions just in case paffinity is active
     */
    if (OPAL_SUCCESS != opal_get_num_processors(&num_processors)) {
        /* if we cannot find the number of local processors, then default to conservative
         * settings
         */
        want_processor = false;  /* default to not being a hog */
        
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:launch could not get number of processors - using conservative settings",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
    } else {
        
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:launch got %ld processors",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)num_processors));
        
        /* only do this if we can actually get info on the number of processors */
        if (opal_list_get_size(&orte_odls_globals.children) > (size_t)num_processors) {
            want_processor = false;
        } else {
            want_processor = true;
        }
        
        /* now let's deal with the oversubscribed flag - and the use-case where a hostfile or some
         * other non-guaranteed-accurate method was used to inform us about our allocation. Since
         * the information on the number of slots on this node could have been incorrect, we need
         * to check it against the local number of processors to ensure we don't overload them
         */
        if (override_oversubscribed) {
            
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:launch overriding oversubscription",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            if (opal_list_get_size(&orte_odls_globals.children) > (size_t)num_processors) {
                /* if the #procs > #processors, declare us oversubscribed regardless
                 * of what the mapper claimed - the user may have told us something
                 * incorrect
                 */
                oversubscribed = true;
            } else {
                /* likewise, if there are more processors here than we were told,
                 * declare us to not be oversubscribed so we can be aggressive. This
                 * covers the case where the user didn't tell us anything about the
                 * number of available slots, so we defaulted to a value of 1
                 */
                oversubscribed = false;
            }
        }
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:launch oversubscribed set to %s want_processor set to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         oversubscribed ? "true" : "false", want_processor ? "true" : "false"));
    
    /* setup to report the proc state to the HNP */
    OBJ_CONSTRUCT(&alert, opal_buffer_t);

    /* setup the environment for each context */
    for (i=0; i < num_apps; i++) {
        if (ORTE_SUCCESS != (rc = odls_base_default_setup_fork(apps[i],
                                                               num_local_procs,
                                                               vpid_range,
                                                               total_slots_alloc,
                                                               oversubscribed,
                                                               &apps[i]->env))) {
            
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
            for (item = opal_list_get_first(&orte_odls_globals.children);
                 item != opal_list_get_end(&orte_odls_globals.children);
                 item = opal_list_get_next(item)) {
                child = (orte_odls_child_t*)item;
                if (OPAL_EQUAL == opal_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
                    if (i == child->app_idx) {
                        child->exit_code = rc;
                    } else {
                        child->state = ORTE_PROC_STATE_UNDEF;
                        child->exit_code = 0;
                    }
                }
            }
            /* okay, now tell the HNP we couldn't do it */
            goto CLEANUP;
        }
    }
    
    
    /* okay, now let's launch our local procs using the provided fork_local fn */
    for (proc_rank = 0, item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
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

        /* find the app context for this child */
        if (child->app_idx > num_apps ||
            NULL == apps[child->app_idx]) {
            /* get here if we couldn't find the app_context */
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
            goto CLEANUP;
        }
        app = apps[child->app_idx];
        
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
        
        asprintf(&value, "%lu", (unsigned long) child->local_rank);
        if(NULL == (param = mca_base_param_environ_variable("orte","ess","local_rank"))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        opal_setenv(param, value, true, &app->env);
        free(param);
        /* users would appreciate being given a public environmental variable
         * that also represents this value - something MPI specific - so
         * do that here.
         *
         * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
         * We know - just live with it
         */
        opal_setenv("OMPI_COMM_WORLD_LOCAL_RANK", value, true, &app->env);
        free(value);
        if (want_processor) {
            param = mca_base_param_environ_variable("mpi", NULL,
                                                    "paffinity_processor");
            asprintf(&value, "%lu", (unsigned long) proc_rank);
            opal_setenv(param, value, true, &app->env);
            free(param);
            free(value);
        } else {
            param = mca_base_param_environ_variable("mpi", NULL,
                                                    "paffinity_processor");
            opal_unsetenv(param, &app->env);
            free(param);
        }

        if ( NULL != child->slot_list ) {
           opal_setenv("slot_list", child->slot_list, true, &app->env);
        }else{
           opal_unsetenv("slot_list", &app->env);
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

        rc = fork_local(app, child, app->env);
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
    }
    launch_failed = false;

CLEANUP:
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:launch reporting job %s launch status",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));
    /* pack the launch results */
    if (ORTE_SUCCESS != (ret = pack_state_update(&alert, true, job))) {
        ORTE_ERROR_LOG(ret);
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

unlock:
    if (!launch_failed) {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:launch setting waitpids",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        /* if the launch didn't fail, setup the waitpids on the children */
        for (item = opal_list_get_first(&orte_odls_globals.children);
             item != opal_list_get_end(&orte_odls_globals.children);
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
    
    for (item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
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
        for (item = opal_list_get_first(&orte_odls_globals.children);
             item != opal_list_get_end(&orte_odls_globals.children);
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
    for (item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
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
    
    for (item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
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
    
    for (item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
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


int orte_odls_base_default_require_sync(orte_process_name_t *proc, opal_buffer_t *buf)
{
    opal_buffer_t buffer;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    int8_t dummy;
    orte_std_cntr_t cnt;
    int rc;
    bool found=false;
    
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_globals.mutex);
    
    for (item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
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
            return rc;
        }
        opal_list_append(&orte_odls_globals.children, &child->super);
        /* we don't know any other info about the child, so just indicate it's
         * alive
         */
        child->alive = true;
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
    opal_dss.pack(&buffer, &dummy, 1, OPAL_INT8);  /* put anything in */

    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls: sending sync ack to child %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));

    if (0 > (rc = orte_rml.send_buffer(proc, &buffer, ORTE_RML_TAG_SYNC, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
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
    
    for (item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
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

/*
 *  Wait for a callback indicating the child has completed.
 */

void odls_base_default_wait_local_proc(pid_t pid, int status, void* cbdata)
{
    orte_odls_child_t *child;
    opal_list_item_t *item;
    bool aborted=false;
    char *job, *vpid, *abort_file;
    struct stat buf;
    int rc;
    opal_buffer_t alert;
    orte_plm_cmd_flag_t cmd=ORTE_PLM_UPDATE_PROC_STATE;
    
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
    for (item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        if (pid == child->pid) { /* found it */
            goto GOTCHILD;
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
    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);
    return;

GOTCHILD:
    /* if the child was previously flagged as dead, then just
     * ensure that its exit state gets reported to avoid hanging
     */
    if (!child->alive) {
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:wait_local_proc child %s was already dead",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(child->name)));
        goto MOVEON;
    }

    /* save the exit code */
    child->exit_code = status;

    /* If this child was the (vpid==0), we hooked it up to orterun's
       STDIN SOURCE earlier (do not change this without also changing
       odsl_default_fork_local_proc()).  So we have to tell the SOURCE
       a) that we don't want any more data and b) that it should not
       expect any more ACKs from this endpoint (so that the svc
       component can still flush/shut down cleanly).

       Note that the source may have already detected that this
       process died as part of an OOB/RML exception, but that's ok --
       its "exception" detection capabilities are not reliable, so we
       *have* to do this unpublish here, even if it arrives after an
       exception is detected and handled (in which case this unpublish
       request will be ignored/discarded. */

    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:wait_local_proc pid %ld corresponds to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (long)pid,
                         ORTE_NAME_PRINT(child->name)));

    if (0 == child->name->vpid) {
        rc = orte_iof.iof_unpublish(child->name, ORTE_NS_CMP_ALL, 
                                    ORTE_IOF_STDIN);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            /* We can't really abort, so keep going... */
        }
    }

    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:wait_local_proc orted sent IOF unpub message!",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

#if 0
    /* Note that the svc IOF component will detect an exception on the
       oob because we're shutting it down, so it will take care of
       closing down any streams that it has open to us. */
    orte_iof.iof_flush();
#endif

    /* determine the state of this process */
    if(WIFEXITED(status)) {
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
                             "%s odls:wait_local_proc checking abort file %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), abort_file));
        
        free(job);
        free(vpid);        
        if (0 == stat(abort_file, &buf)) {
            /* the abort file must exist - there is nothing in it we need. It's
             * meer existence indicates that an abnormal termination occurred
             */

            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:wait_local_proc child %s died by abort",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));

            aborted = true;
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
                aborted = true;
                child->state = ORTE_PROC_STATE_TERM_WO_SYNC;
                
                OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                     "%s odls:wait_local_proc child process %s terminated normally "
                                     "but did not provide a required sync - it "
                                     "will be treated as an abnormal termination",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name)));
                
                goto MOVEON;
            } else {
                child->state = ORTE_PROC_STATE_TERMINATED;
            }

            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:wait_local_proc child process %s terminated normally",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(child->name)));

        }
    } else {
        /* the process was terminated with a signal! That's definitely
         * abnormal, so indicate that condition
         */
        child->state = ORTE_PROC_STATE_ABORTED_BY_SIG;
        OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                             "%s odls:wait_local_proc child process %s terminated with signal",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(child->name)));
        
        aborted = true;
    }

MOVEON:
    /* indicate the child is no longer alive */
    child->alive = false;

    /* Clean up the session directory as if we were the process
     * itself.  This covers the case where the process died abnormally
     * and didn't cleanup its own session directory.
     */
    orte_session_dir_finalize(child->name);

    /* setup the alert buffer */
    OBJ_CONSTRUCT(&alert, opal_buffer_t);

    /* if the proc aborted, tell the HNP right away */
    if (aborted) {
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
                             "%s odls:wait_local_proc reporting proc %s aborted to HNP",
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
            if (ORTE_SUCCESS != (rc = pack_state_update(&alert, false, child->name->jobid))) {
                ORTE_ERROR_LOG(rc);
                goto unlock;
            }
            
            OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                 "%s odls:wait_local_proc reporting all procs in %s terminated",
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

    opal_condition_signal(&orte_odls_globals.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_globals.mutex);

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
    
    for (item = opal_list_get_first(&orte_odls_globals.children);
         item != opal_list_get_end(&orte_odls_globals.children);
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
        opal_list_remove_item(&orte_odls_globals.children, item);
        
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
            goto MOVEON;
        }
        
        /* Send a sigterm to the process.  If we get ESRCH back, that
            means the process is already dead, so just move on. */
        if (0 != (err = kill_local(child->pid, SIGTERM))) {
            opal_show_help("help-odls-default.txt",
                           "odls-default:could-not-send-kill",
                           true, orte_system_info.nodename, child->pid, err);
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
                opal_show_help("help-odls-default.txt",
                               "odls-default:could-not-kill",
                               true, orte_system_info.nodename, child->pid);
            }
        }
        child->state = ORTE_PROC_STATE_ABORTED_BY_SIG;  /* we may have sent it, but that's what happened */
        goto RECORD;
        
MOVEON:
        /* set the process to "not alive" */
        child->alive = false;

RECORD:
        /* store the child in the alert buffer */
        if (ORTE_SUCCESS != (rc = pack_state_for_proc(&alert, false, child))) {
            ORTE_ERROR_LOG(rc);
        }
        goto CLEANUP;
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

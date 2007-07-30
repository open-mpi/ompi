/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <signal.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */

#if defined(HAVE_SCHED_YIELD)
/* Only if we have sched_yield() */
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#else
/* Only do these if we don't have <sched.h> */
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#endif /* HAVE_SCHED_YIELD */

#include "opal/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/util/show_help.h"
#include "opal/util/path.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/num_procs.h"
#include "opal/util/sys_limits.h"

#include "orte/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/params.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/iof_base_setup.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#if OPAL_ENABLE_FT == 1
#include "orte/mca/snapc/snapc.h"
#endif

#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/default/odls_default.h"

static int orte_pls_fork_preload_append_binary(orte_app_context_t* context,
                                               orte_filem_base_request_t *filem_request);
static int orte_pls_fork_preload_append_files(orte_app_context_t* context,
                                              orte_filem_base_request_t *filem_request);
static bool is_preload_local_dup(char *local_ref, orte_filem_base_request_t *filem_request);

/*
 * External Interface
 */
static int orte_odls_default_get_add_procs_data(orte_gpr_notify_data_t **data, orte_job_map_t *map);
static int orte_odls_default_launch_local_procs(orte_gpr_notify_data_t *data);
static int orte_odls_default_kill_local_procs(orte_jobid_t job, bool set_state);
static int orte_odls_default_signal_local_procs(const orte_process_name_t *proc,
                                                int32_t signal);
static int orte_odls_default_deliver_message(orte_jobid_t job, orte_buffer_t *buffer, orte_rml_tag_t tag);

static void set_handler_default(int sig);

orte_odls_base_module_t orte_odls_default_module = {
    orte_odls_default_get_add_procs_data,
    orte_odls_default_launch_local_procs,
    orte_odls_default_kill_local_procs,
    orte_odls_default_signal_local_procs,
    orte_odls_default_deliver_message
};

int orte_odls_default_get_add_procs_data(orte_gpr_notify_data_t **data,
                                         orte_job_map_t *map)
{
    orte_gpr_notify_data_t *ndat;
    orte_gpr_value_t **values, *value;
    orte_std_cntr_t cnt;
    char *glob_tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    char *glob_keys[] = {
        ORTE_JOB_APP_CONTEXT_KEY,
        ORTE_JOB_VPID_START_KEY,
        ORTE_JOB_VPID_RANGE_KEY,
        ORTE_JOB_TOTAL_SLOTS_ALLOC_KEY,
        NULL
    };
    opal_list_item_t *item, *m_item;
    orte_mapped_node_t *node;
    orte_mapped_proc_t *proc;
    int rc;
    char *segment;
    
    /* set default answer */
    *data = NULL;
    
    ndat = OBJ_NEW(orte_gpr_notify_data_t);
    if (NULL == ndat) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* construct a fake trigger name so that the we can extract the jobid from it later */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&(ndat->target), "bogus", map->job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(ndat);
        return rc;
    }
    
    /* get the segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, map->job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(ndat);
        return rc;
    }
    
    /* get the info from the job globals container first */
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                                           segment, glob_tokens, glob_keys, &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(ndat);
        return rc;
    }
    
    /* there can only be one value here since we only specified a single container.
     * Just transfer the returned value to the ndat structure
     */
    if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&cnt, ndat->values, values[0]))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(ndat);
        OBJ_RELEASE(values[0]);
        return rc;
    }
    ndat->cnt = 1;
    
    /* the remainder of our required info is in the mapped_node objects, so all we
     * have to do is transfer it over
     */
    for (m_item = opal_list_get_first(&map->nodes);
         m_item != opal_list_get_end(&map->nodes);
         m_item = opal_list_get_next(m_item)) {
        node = (orte_mapped_node_t*)m_item;
        
        for (item = opal_list_get_first(&node->procs);
             item != opal_list_get_end(&node->procs);
             item = opal_list_get_next(item)) {
            proc = (orte_mapped_proc_t*)item;
            
            /* must not have any tokens so that launch_procs can process it correctly */
            if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, 0, segment, 5, 0))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
            
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]),
                                                            ORTE_PROC_NAME_KEY,
                                                            ORTE_NAME, &proc->name))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
          
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]),
                                                            ORTE_PROC_APP_CONTEXT_KEY,
                                                            ORTE_STD_CNTR, &proc->app_idx))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
          
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[2]),
                                                            ORTE_NODE_NAME_KEY,
                                                            ORTE_STRING, node->nodename))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
          
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[3]),
                                                             ORTE_PROC_LOCAL_RANK_KEY,
                                                             ORTE_VPID, &proc->local_rank))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[3]),
                                                             ORTE_PROC_CPU_LIST_KEY,
                                                             ORTE_STRING, proc->slot_list))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[4]),
                                                             ORTE_NODE_NUM_PROCS_KEY,
                                                             ORTE_STD_CNTR, &node->num_procs))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
            
            if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&cnt, ndat->values, value))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(values[0]);
                return rc;
            }
            ndat->cnt += 1;
        }
    }
    
    *data = ndat;
    return ORTE_SUCCESS;
}

/**
 *  This function receives a slot string ant translate it to
 *  cpu_set (long bitmap) using the PLPA module.
 *  currently this function is doing nothig because PLPA is not
 *  part of the oprn MPI project.
 */

static int socket_to_cpu_set(char **socket_list, int socket_cnt, orte_odls_child_t *child)
{
    int i, j;
    char **range;
    int range_cnt;
    int lower_range, upper_range, socket_num;

    for (i=0; i < socket_cnt; i++) {
        if (0 == strcmp("*", socket_list[i])) {
            /* use PLPA to construct the child->cpu_set */
            opal_output(orte_odls_globals.output,"rank %d can run on any socket", child->name->vpid);
            continue;
        }
        range = opal_argv_split(socket_list[i], '-');
        range_cnt = opal_argv_count(range);
        switch (range_cnt) {
        case 1:
            socket_num = atoi(range[0]);
            /* use PLPA to construct the child->cpu_set */
            opal_output(orte_odls_globals.output,"rank %d runs on socket #%d", child->name->vpid, socket_num); 
            break;
        case 2:
            lower_range = atoi(range[0]);
            upper_range = atoi(range[1]);
            for (j=lower_range; j<= upper_range; j++) {
                socket_num = j;
                /* use PLPA to construct the child->cpu_set */
                opal_output(orte_odls_globals.output,"rank %d runs on socket #%d", child->name->vpid, socket_num); 
            }
            break;
        default:
            opal_argv_free(range);
            ORTE_ERROR_LOG(ORTE_ERROR);
            return ORTE_ERROR;
        }
        opal_argv_free(range);
    }
    return ORTE_SUCCESS;
}

static int socket_core_to_cpu_set(char **socket_core_list, int socket_core_list_cnt, orte_odls_child_t *child)
{
    int i, j;
    char **socket_core;
    int socket_core_cnt;
    char **range;
    int range_cnt;
    int lower_range, upper_range;
    int socket, core;

    socket_core = opal_argv_split (socket_core_list[0], ':');
    socket_core_cnt = opal_argv_count(socket_core);

    socket = atoi(socket_core[0]);
    if (0 == strcmp("*",socket_core[1])) {
        /* use PLPA to construct the child->cpu_set */
        opal_output(orte_odls_globals.output,"rank %d runs on socket #%d any core", child->name->vpid, socket);
    }
    else {
        range = opal_argv_split(socket_core[1], '-');
        range_cnt = opal_argv_count(range);
        switch (range_cnt) {
        case 1:
            core = atoi(range[0]);
            /* use PLPA to construct the child->cpu_set */
            opal_output(orte_odls_globals.output,"rank %d runs on socket #%d core #%d", child->name->vpid, socket, core);
            break;
        case 2:
            lower_range = atoi(range[0]);
            upper_range = atoi(range[1]);
            for (j=lower_range; j<= upper_range; j++) {
                core = j;
                /* use PLPA to construct the child->cpu_set */
                opal_output(orte_odls_globals.output,"rank %d runs on socket #%d core #%d", child->name->vpid, socket, core);
            }
            break;
        default:
            opal_argv_free(range);
            opal_argv_free(socket_core);
            ORTE_ERROR_LOG(ORTE_ERROR);
            return ORTE_ERROR;
        }
        opal_argv_free(range);
        opal_argv_free(socket_core);
    }
    for (i=1; i < socket_core_list_cnt; i++) {
        socket_core = opal_argv_split (socket_core_list[i], ':');
        socket_core_cnt = opal_argv_count(socket_core);
        switch (socket_core_cnt) {
        case 1:
            range = opal_argv_split(socket_core[0], '-');
            range_cnt = opal_argv_count(range);
            switch (range_cnt) {
            case 1:
                core = atoi(range[0]);
                /* use PLPA to construct the child->cpu_set */
                opal_output(orte_odls_globals.output,"and on core #%d", core);
                break;
            case 2:
                lower_range = atoi(range[0]);
                upper_range = atoi(range[1]);
                for (j=lower_range; j<= upper_range; j++) {
                    core = j;
                    /* use PLPA to construct the child->cpu_set */
                    opal_output(orte_odls_globals.output,"and on core #%d", core);
                }
                break;
            default:
                opal_argv_free(range);
                opal_argv_free(socket_core);
                ORTE_ERROR_LOG(ORTE_ERROR);
                return ORTE_ERROR;
            }
            opal_argv_free(range);
            break;
        case 2:
            socket = atoi(socket_core[0]);
            if (0 == strcmp("*",socket_core[1])) {
                /* use PLPA to construct the child->cpu_set */
                opal_output(orte_odls_globals.output,"and on socket #%d any core", socket);
            }
            else {
                range = opal_argv_split(socket_core[1], '-');
                range_cnt = opal_argv_count(range);
                switch (range_cnt) {
                case 1:
                    core = atoi(range[0]);
                    /* use PLPA to construct the child->cpu_set */
                    opal_output(orte_odls_globals.output,"and on socket #%d core #%d", socket, core);
                    break;
                case 2:
                    lower_range = atoi(range[0]);
                    upper_range = atoi(range[1]);
                    for (j=lower_range; j<= upper_range; j++) {
                        core = j;
                        /* use PLPA to construct the child->cpu_set */
                        opal_output(orte_odls_globals.output,"and on socket #%d core #%d", socket, core);
                    }
                    break;
                default:
                    opal_argv_free(range);
                    opal_argv_free(socket_core);
                    ORTE_ERROR_LOG(ORTE_ERROR);
                    return ORTE_ERROR;
                }
                opal_argv_free(range);
            }
            break;
        default: 
            opal_argv_free(socket_core);
            ORTE_ERROR_LOG(ORTE_ERROR);
            return ORTE_ERROR;
        }
        opal_argv_free(socket_core);
    }
    return ORTE_SUCCESS;
}

static int slot_list_to_cpu_set(char *slot_str, orte_odls_child_t *child)
{
    char **item;
    char **socket_core;
    int item_cnt, socket_core_cnt;
    int rc;

    item = opal_argv_split (slot_str, ',');
    item_cnt = opal_argv_count (item);

    socket_core = opal_argv_split (item[0], ':');
    socket_core_cnt = opal_argv_count(socket_core);
    opal_argv_free(socket_core);

    switch (socket_core_cnt) {
    case 1:
        if (ORTE_SUCCESS != (rc = socket_to_cpu_set(item, item_cnt, child))) {
            opal_argv_free(item);
            ORTE_ERROR_LOG(rc);
            return ORTE_ERROR;
        }
        break;
    case 2:
        if (ORTE_SUCCESS != (rc = socket_core_to_cpu_set(item, item_cnt, child))) {
            opal_argv_free(item);
            ORTE_ERROR_LOG(rc);
            return ORTE_ERROR;
        }
        break;
    default:
        opal_argv_free(item);
        return ORTE_ERROR;
    }
    opal_argv_free(item);
    return ORTE_SUCCESS;
}

static bool odls_default_child_died(pid_t pid, unsigned int timeout,
                                    int *exit_status)
{
    time_t end;
    pid_t ret;
#if !defined(HAVE_SCHED_YIELD)
    struct timeval t;
    fd_set bogus;
#endif
        
    end = time(NULL) + timeout;
    do {
        ret = waitpid(pid, exit_status, WNOHANG);
        if (pid == ret) {
            /* It died -- return success */
            return true;
        } else if (-1 == ret && ECHILD == errno) {
            /* The pid no longer exists, so we'll call this "good
               enough for government work" */
            return true;
        }

#if defined(HAVE_SCHED_YIELD)
        sched_yield();
#else
        /* Bogus delay for 1 usec */
        t.tv_sec = 0;
        t.tv_usec = 1;
        FD_ZERO(&bogus);
        FD_SET(0, &bogus);
        select(1, &bogus, NULL, NULL, &t);
#endif
        
    } while (time(NULL) < end);

    /* The child didn't die, so return false */
    return false;
}

int orte_odls_default_kill_local_procs(orte_jobid_t job, bool set_state)
{
    orte_odls_child_t *child;
    opal_list_item_t *item, *next;
    int rc = 0, exit_status = 0;
    opal_list_t procs_killed;
    orte_namelist_t *proc;

    OBJ_CONSTRUCT(&procs_killed, opal_list_t);
    
    opal_output(orte_odls_globals.output, "%s odls_kill_local_proc: working on job %ld",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)job);

    /* since we are going to be working with the global list of
     * children, we need to protect that list from modification
     * by other threads
     */
    OPAL_THREAD_LOCK(&orte_odls_default.mutex);
    
    for (item = opal_list_get_first(&orte_odls_default.children);
         item != opal_list_get_end(&orte_odls_default.children);
         item = next) {
        child = (orte_odls_child_t*)item;
        
        /* preserve the pointer to the next item in list in case we release it */
        next = opal_list_get_next(item);
        
        opal_output(orte_odls_globals.output, "%s odls_kill_local_proc: checking child process %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(child->name));

        /* do we have a child from the specified job? Because the
         *  job could be given as a WILDCARD value, we must use
         *  the dss.compare function to check for equality.
         */
        if (ORTE_EQUAL != orte_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
            continue;
        }
        
        /* remove the child from the list since it is either already dead or soon going to be dead */
        opal_list_remove_item(&orte_odls_default.children, item);
        
        /* is this process alive? if not, then nothing for us
         * to do to it
         */
        if (!child->alive) {
            opal_output(orte_odls_globals.output, "%s odls_kill_local_proc: child %s is not alive",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(child->name));
            /* ensure, though, that the state is terminated so we don't lockup if
             * the proc never started
             */
            continue;
        }
        
        /* de-register the SIGCHILD callback for this pid */
        if (ORTE_SUCCESS != (rc = orte_wait_cb_cancel(child->pid))) {
            /* no need to error_log this - it just means that the pid is already gone */
            goto MOVEON;
        }
        
        /* Send a sigterm to the process.  If we get ESRCH back, that
           means the process is already dead, so just move on. */
        if (0 != kill(child->pid, SIGTERM) && ESRCH != errno) {
            int err = errno;
            opal_show_help("help-odls-default.txt",
                           "odls-default:could-not-send-kill",
                           true, orte_system_info.nodename, child->pid, err);
            goto MOVEON;
        }

        /* The kill succeeded.  Wait up to timeout_before_sigkill
           seconds to see if it died. */

        if (!odls_default_child_died(child->pid, orte_odls_globals.timeout_before_sigkill, &exit_status)) {
            /* try killing it again */
            kill(child->pid, SIGKILL);
            /* Double check that it actually died this time */
            if (!odls_default_child_died(child->pid, orte_odls_globals.timeout_before_sigkill, &exit_status)) {
                opal_show_help("help-odls-default.txt",
                               "odls-default:could-not-kill",
                               true, orte_system_info.nodename, child->pid);
            }
        }
        
MOVEON:
        /* set the process to "not alive" */
        child->alive = false;
        
        /* add this proc to the local list */
        proc = OBJ_NEW(orte_namelist_t);
        if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(proc->name), child->name, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            opal_condition_signal(&orte_odls_default.cond);
            OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
            return rc;
        }
        opal_list_append(&procs_killed, &proc->item);

        /* release the object since we killed it */
        OBJ_RELEASE(child);
    }
    
    /* we are done with the global list, so we can now release
     * any waiting threads - this also allows any callbacks to work
     */
    opal_condition_signal(&orte_odls_default.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
        
    /* deconstruct the local list and update the process states on the registry, if indicated */
    while (NULL != (item = opal_list_remove_first(&procs_killed))) {
        proc = (orte_namelist_t*)item;
        if (set_state) {
            if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(proc->name, ORTE_PROC_STATE_TERMINATED, exit_status))) {
                ORTE_ERROR_LOG(rc);
                /* don't exit out even if this didn't work - we still might need to kill more
                 * processes, so just keep trucking
                 */
            }
        }
        OBJ_RELEASE(proc);
    }
    
    OBJ_DESTRUCT(&procs_killed);

    return ORTE_SUCCESS;
}

/*
 *  Wait for a callback indicating the child has completed.
 */

static void odls_default_wait_local_proc(pid_t pid, int status, void* cbdata)
{
    orte_odls_child_t *child;
    opal_list_item_t *item;
    bool aborted;
    char *job, *vpid, *abort_file;
    struct stat buf;
    int rc;

    opal_output(orte_odls_globals.output, "odls: child process %ld terminated", (long)pid);
    
    /* since we are going to be working with the global list of
     * children, we need to protect that list from modification
     * by other threads. This will also be used to protect us
     * from race conditions on any abort situation
     */
    OPAL_THREAD_LOCK(&orte_odls_default.mutex);
 
    /* find this child */
    for (item = opal_list_get_first(&orte_odls_default.children);
         item != opal_list_get_end(&orte_odls_default.children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (child->alive && pid == child->pid) { /* found it */
            goto GOTCHILD;
        }
    }
   /* get here if we didn't find the child, or if the specified child
    * is already dead. If the latter, then we have a problem as it
    * means we are detecting it exiting multiple times
    */
    opal_output(orte_odls_globals.output, "odls: did not find pid %ld in table!", (long) pid);
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    opal_condition_signal(&orte_odls_default.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
    return;

GOTCHILD:
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
    opal_output(orte_odls_globals.output,
                "odls: pid %ld corresponds to %s\n",
                (long) pid, ORTE_NAME_PRINT(child->name));
    if (0 == child->name->vpid) {
        rc = orte_iof.iof_unpublish(child->name, ORTE_NS_CMP_ALL, 
                                    ORTE_IOF_STDIN);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            /* We can't really abort, so keep going... */
        }
    }
    opal_output(orte_odls_globals.output, "orted sent IOF unpub message!\n");

#if 0
    /* Note that the svc IOF component will detect an exception on the
       oob because we're shutting it down, so it will take care of
       closing down any streams that it has open to us. */
    orte_iof.iof_flush();
#endif

    /* determine the state of this process */
    aborted = false;
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
        if (ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&job, child->name->jobid))) {
            ORTE_ERROR_LOG(rc);
            goto MOVEON;
        }
        if (ORTE_SUCCESS != (rc = orte_ns.convert_vpid_to_string(&vpid, child->name->vpid))) {
            ORTE_ERROR_LOG(rc);
            free(job);
            goto MOVEON;
        }
        abort_file = opal_os_path(false, orte_process_info.universe_session_dir,
                                  job, vpid, "abort", NULL );
        free(job);
        free(vpid);        
        if (0 == stat(abort_file, &buf)) {
            /* the abort file must exist - there is nothing in it we need. It's
             * meer existence indicates that an abnormal termination occurred
             */
            opal_output(orte_odls_globals.output, "odls: child %s died by abort",
                        ORTE_NAME_PRINT(child->name));
            aborted = true;
            free(abort_file);
        } else {
            opal_output(orte_odls_globals.output, "odls: child process %s terminated normally",
                        ORTE_NAME_PRINT(child->name));
        }
    } else {
        /* the process was terminated with a signal! That's definitely
         * abnormal, so indicate that condition
         */
        opal_output(orte_odls_globals.output, "odls: child process %s terminated with signal",
                    ORTE_NAME_PRINT(child->name));
        aborted = true;
    }

MOVEON:
    /* set this proc to "not alive" */
    child->alive = false;

    /* Clean up the session directory as if we were the process
     * itself.  This covers the case where the process died abnormally
     * and didn't cleanup its own session directory.
     */
    orte_session_dir_finalize(child->name);

    /* set the proc state in the child structure */
    if (aborted) {
        child->state = ORTE_PROC_STATE_ABORTED;
    } else {
        child->state = ORTE_PROC_STATE_TERMINATED;
    }

    /* Need to unlock before we call set_proc_state as this is going to generate
     * a trigger that will eventually callback to us
     */
    opal_condition_signal(&orte_odls_default.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);

    if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(child->name, child->state, status))) {
        ORTE_ERROR_LOG(rc);
    }
}

/**
 *  Fork/exec the specified processes
 */

static int odls_default_fork_local_proc(
    orte_app_context_t* context,
    orte_odls_child_t *child,
    orte_vpid_t vpid_start,
    orte_vpid_t vpid_range,
    orte_std_cntr_t total_slots_alloc,
    bool want_processor,
    size_t processor,
    bool oversubscribed)
{
    pid_t pid;
    orte_iof_base_io_conf_t opts;
    int rc;
    sigset_t sigs;
    int i = 0, p[2];

    /* check the system limits - if we are at our max allowed children, then
     * we won't be allowed to do this anyway, so we may as well abort now.
     * According to the documentation, num_procs = 0 is equivalent to
     * to no limit, so treat it as unlimited here.
     */
    if (opal_sys_limits.initialized) {
        if (0 < opal_sys_limits.num_procs &&
            opal_sys_limits.num_procs <= (int)opal_list_get_size(&orte_odls_default.children)) {
            /* at the system limit - abort */
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            child->state = ORTE_PROC_STATE_FAILED_TO_START;
            child->exit_code = ORTE_ERR_SYS_LIMITS_CHILDREN;
            return ORTE_ERR_SYS_LIMITS_CHILDREN;
        }
    }
    
    /* should pull this information from MPIRUN instead of going with
       default */
    opts.usepty = OMPI_ENABLE_PTY_SUPPORT;

    /* BWB - Fix post beta.  Should setup stdin in orterun and make
       part of the app_context.  Do not change this without also
       changing the reverse of this in
       odls_default_wait_local_proc(). */
    if (child->name->vpid == 0) {
        opts.connect_stdin = true;
    } else {
        opts.connect_stdin = false;
    }

    if (ORTE_SUCCESS != (rc = orte_iof_base_setup_prefork(&opts))) {
        ORTE_ERROR_LOG(rc);
        child->state = ORTE_PROC_STATE_FAILED_TO_START;
        child->exit_code = rc;
        return rc;
    }
    
    /* A pipe is used to communicate between the parent and child to
       indicate whether the exec ultiimately succeeded or failed.  The
       child sets the pipe to be close-on-exec; the child only ever
       writes anything to the pipe if there is an error (e.g.,
       executable not found, exec() fails, etc.).  The parent does a
       blocking read on the pipe; if the pipe closed with no data,
       then the exec() succeeded.  If the parent reads something from
       the pipe, then the child was letting us know that it failed. */
    if (pipe(p) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_PIPES);
        child->state = ORTE_PROC_STATE_FAILED_TO_START;
        child->exit_code = ORTE_ERR_SYS_LIMITS_PIPES;
        return ORTE_ERR_SYS_LIMITS_PIPES;
    }

    /* Fork off the child */
    pid = fork();
    if(pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
        child->state = ORTE_PROC_STATE_FAILED_TO_START;
        child->exit_code = ORTE_ERR_SYS_LIMITS_CHILDREN;
        return ORTE_ERR_SYS_LIMITS_CHILDREN;
    }

    if (pid == 0) {
        char *param, *param2;
        char *uri;
        char **environ_copy;
        long fd, fdmax = sysconf(_SC_OPEN_MAX);

        /* Setup the pipe to be close-on-exec */
        close(p[0]);
        fcntl(p[1], F_SETFD, FD_CLOEXEC);

        /* Try to change to the context cwd and check that the app
           exists and is executable The resource manager functions will
           take care of outputting a pretty error message, if required
         */
        if (ORTE_SUCCESS != (i = orte_rmgr.check_context_cwd(context, true))) {
           /* Tell the parent that Badness happened */
            write(p[1], &i, sizeof(int));
            exit(1);
        }
        if (ORTE_SUCCESS != (i = orte_rmgr.check_context_app(context))) {
            /* Tell the parent that Badness happened */
            write(p[1], &i, sizeof(int));
            exit(1);
        }

        /*  setup stdout/stderr so that any error messages that we may
            print out will get displayed back at orterun.
            
            NOTE: Definitely do this AFTER we check contexts so that any
            error message from those two functions doesn't come out to the
            user. IF we didn't do it in this order, THEN a user who gives
            us a bad executable name or working directory would get N
            error messages, where N=num_procs. This would be very annoying
            for large jobs, so instead we set things up so that orterun
            always outputs a nice, single message indicating what happened
        */
        if (ORTE_SUCCESS != (i = orte_iof_base_setup_child(&opts))) {
            write(p[1], &i, sizeof(int));
            exit(1);
        }
        
        /* setup base environment: copy the current environ and merge
           in the app context environ */
        if (NULL != context->env) {
            environ_copy = opal_environ_merge(orte_launch_environ, context->env);
        } else {
            environ_copy = opal_argv_copy(orte_launch_environ);
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
                opal_setenv("PATH", newenv, true, &environ_copy);
                free(newenv);
            }

            /* Reset LD_LIBRARY_PATH */
            else if (0 == strncmp("LD_LIBRARY_PATH=", context->env[i], 16)) {
                asprintf(&newenv, "%s/lib:%s",
                         context->prefix_dir, context->env[i] + 16);
                opal_setenv("LD_LIBRARY_PATH", newenv, true, &environ_copy);
                free(newenv);
            }
        }

        /* pass my contact info to the local proc so we can talk */
        uri = orte_rml.get_contact_info();
        param = mca_base_param_environ_variable("orte","local_daemon","uri");
        opal_setenv(param, uri, true, &environ_copy);
        free(param);
        free(uri);
        
        /* setup yield schedule and processor affinity
         * We default here to always setting the affinity processor if we want
         * it. The processor affinity system then determines
         * if processor affinity is enabled/requested - if so, it then uses
         * this value to select the process to which the proc is "assigned".
         * Otherwise, the paffinity subsystem just ignores this value anyway
         */
        if (oversubscribed) {
            param = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
            opal_setenv(param, "1", false, &environ_copy);
       } else {
            param = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
            opal_setenv(param, "0", false, &environ_copy);
        }
        free(param);
        
        if (want_processor) {
            param = mca_base_param_environ_variable("mpi", NULL,
                                                    "paffinity_processor");
            asprintf(&param2, "%lu", (unsigned long) processor);
            opal_setenv(param, param2, false, &environ_copy);
            free(param);
            free(param2);
        } else {
            param = mca_base_param_environ_variable("mpi", NULL,
                                                    "paffinity_processor");
            opal_unsetenv(param, &environ_copy);
            free(param);
        }
        
        /* setup universe info */
        if (NULL != orte_universe_info.name) {
            param = mca_base_param_environ_variable("universe", NULL, NULL);
            asprintf(&uri, "%s@%s:%s", orte_universe_info.uid,
                                       orte_universe_info.host,
                                       orte_universe_info.name);
            opal_setenv(param, uri, true, &environ_copy);
            free(param);
            free(uri);
        }

        /* setup ns contact info */
        if(NULL != orte_process_info.ns_replica_uri) {
            uri = strdup(orte_process_info.ns_replica_uri);
        } else {
            uri = orte_rml.get_contact_info();
        }
        param = mca_base_param_environ_variable("ns","replica","uri");
        opal_setenv(param, uri, true, &environ_copy);
        free(param);
        free(uri);

        /* setup gpr contact info */
        if(NULL != orte_process_info.gpr_replica_uri) {
            uri = strdup(orte_process_info.gpr_replica_uri);
        } else {
            uri = orte_rml.get_contact_info();
        }
        param = mca_base_param_environ_variable("gpr","replica","uri");
        opal_setenv(param, uri, true, &environ_copy);
        free(param);
        free(uri);
        
        /* set the app_context number into the environment */
        param = mca_base_param_environ_variable("orte","app","num");
        asprintf(&param2, "%ld", (long)child->app_idx);
        opal_setenv(param, param2, true, &environ_copy);
        free(param);
        free(param2);
        
        /* set the universe size in the environment */
        param = mca_base_param_environ_variable("orte","universe","size");
        asprintf(&param2, "%ld", (long)total_slots_alloc);
        opal_setenv(param, param2, true, &environ_copy);
        free(param);
        free(param2);
        
        /* use same nodename as the starting daemon (us) */
        param = mca_base_param_environ_variable("orte", "base", "nodename");
        opal_setenv(param, orte_system_info.nodename, true, &environ_copy);
        free(param);

        /* push data into environment */
        orte_ns_nds_env_put(child->name, vpid_start, vpid_range,
                            child->local_rank, child->num_procs,
                            &environ_copy);

        /* close all file descriptors w/ exception of stdin/stdout/stderr */
        for(fd=3; fd<fdmax; fd++)
            close(fd);

        if (context->argv == NULL) {
            context->argv = malloc(sizeof(char*)*2);
            context->argv[0] = strdup(context->app);
            context->argv[1] = NULL;
        }

        /* Set signal handlers back to the default.  Do this close to
           the exev() because the event library may (and likely will)
           reset them.  If we don't do this, the event library may
           have left some set that, at least on some OS's, don't get
           reset via fork() or exec().  Hence, the launched process
           could be unkillable (for example). */

        set_handler_default(SIGTERM);
        set_handler_default(SIGINT);
        set_handler_default(SIGHUP);
        set_handler_default(SIGPIPE);
        set_handler_default(SIGCHLD);

        /* Unblock all signals, for many of the same reasons that we
           set the default handlers, above.  This is noticable on
           Linux where the event library blocks SIGTERM, but we don't
           want that blocked by the launched process. */
        sigprocmask(0, 0, &sigs);
        sigprocmask(SIG_UNBLOCK, &sigs, 0);

        /* Exec the new executable */

        execve(context->app, context->argv, environ_copy);
        opal_show_help("help-odls-default.txt", "orte-odls-default:execv-error",
                       true, context->app, strerror(errno));
        exit(1);
    } else {

        /* connect endpoints IOF */
        rc = orte_iof_base_setup_parent(child->name, &opts);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* Wait to read something from the pipe or close */
        close(p[1]);
        while (1) {
            rc = read(p[0], &i, sizeof(int));
            if (rc < 0) {
                /* Signal interrupts are ok */
                if (errno == EINTR) {
                    continue;
                }
                /* Other errno's are bad */
                child->state = ORTE_PROC_STATE_FAILED_TO_START;
                child->exit_code = ORTE_ERR_PIPE_READ_FAILURE;
                opal_output(orte_odls_globals.output, "odls: got code %d back from child", i);
                return ORTE_ERR_PIPE_READ_FAILURE;
                break;
            } else if (0 == rc) {
                /* Child was successful in exec'ing! */
                break;
            } else {
                /*  Doh -- child failed.
                    Let the calling function
                    know about the failure.  The actual exit status of child proc
                    cannot be found here - all we can do is report the ORTE error
                    code that was reported back to us. The calling func needs to report the
                    failure to launch this process through the SMR or else
                    everyone else will hang.
                */
                child->state = ORTE_PROC_STATE_FAILED_TO_START;
                child->exit_code = i;
                opal_output(orte_odls_globals.output, "odls: got code %d back from child", i);
                return i;
            }
        }

        /* set the proc state to LAUNCHED and save the pid */
        child->state = ORTE_PROC_STATE_LAUNCHED;
        child->pid = pid;
        child->alive = true;
    }
    
    return ORTE_SUCCESS;
}


/**
 * Launch all processes allocated to the current node.
 */

int orte_odls_default_launch_local_procs(orte_gpr_notify_data_t *data)
{
    int rc;
    orte_std_cntr_t i, j, kv, kv2, *sptr, total_slots_alloc = 0;
    orte_gpr_value_t *value, **values;
    orte_gpr_keyval_t *kval;
    orte_app_context_t *app;
    orte_jobid_t job;
    orte_vpid_t *vptr, start, range;
    char *node_name;
    opal_list_t app_context_list;
    orte_odls_child_t *child;
    odls_default_app_context_t *app_item;
    int num_processors;
    bool oversubscribed=false, want_processor, *bptr, override_oversubscribed=false;
    opal_list_item_t *item, *item2;
    bool quit_flag;
    bool node_included;
    orte_filem_base_request_t *filem_request;
    char *job_str, *uri_file, *my_uri, *session_dir=NULL, *slot_str;
    FILE *fp;

    /* parse the returned data to create the required structures
     * for a fork launch. Since the data will contain information
     * on procs for ALL nodes, we first have to find the value
     * struct that contains info for our node.
     */

    /* first, retrieve the job number we are to launch from the
     * returned data - we can extract the jobid directly from the
     * subscription name we created
     */
    if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_std_trigger_name(&job, data->target))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    opal_output(orte_odls_globals.output, "odls: setting up launch for job %ld", (long)job);

    /* We need to create a list of the app_contexts
     * so we can know what to launch - the process info only gives
     * us an index into the app_context array, not the app_context
     * info itself.
     */
    
    OBJ_CONSTRUCT(&app_context_list, opal_list_t);
    
    /* set the default values to INVALID */
    start = ORTE_VPID_INVALID;
    range = ORTE_VPID_INVALID;
    
    /* set the flag indicating this node is not included in the launch data */
    node_included = false;
    
    values = (orte_gpr_value_t**)(data->values)->addr;
    for (j=0, i=0; i < data->cnt && j < (data->values)->size; j++) {  /* loop through all returned values */
        if (NULL != values[j]) {
            i++;
            value = values[j];
            
            if (NULL != value->tokens) {
               /* this came from the globals container, so it must contain
                * the app_context(s), vpid_start, and vpid_range entries. Only one
                * value object should ever come from that container
                */
                for (kv=0; kv < value->cnt; kv++) {
                    kval = value->keyvals[kv];
                    if (strcmp(kval->key, ORTE_JOB_VPID_START_KEY) == 0) {
                        /* this can only occur once, so just store it */
                        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, kval->value, ORTE_VPID))) {
                            ORTE_ERROR_LOG(rc);
                            return rc;
                        }
                        start = *vptr;
                        continue;
                    }
                    if (strcmp(kval->key, ORTE_JOB_VPID_RANGE_KEY) == 0) {
                        /* this can only occur once, so just store it */
                        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, kval->value, ORTE_VPID))) {
                            ORTE_ERROR_LOG(rc);
                            return rc;
                        }
                        range = *vptr;
                        continue;
                    }
                    if (strcmp(kval->key, ORTE_JOB_APP_CONTEXT_KEY) == 0) {
                        /* this can occur multiple times since we allow multiple
                         * app_contexts on the orterun command line. Add them
                         * to the list
                         */
                        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&app, kval->value, ORTE_APP_CONTEXT))) {
                            ORTE_ERROR_LOG(rc);
                            return rc;
                        }
                        app_item = OBJ_NEW(odls_default_app_context_t);
                        if (NULL == app_item) {
                            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                            return ORTE_ERR_OUT_OF_RESOURCE;
                        }
                        app_item->app_context = app;
                        opal_list_append(&app_context_list, &app_item->super);
                        kval->value->data = NULL;  /* protect the data storage from later release */
                    }
                    if (strcmp(kval->key, ORTE_JOB_OVERSUBSCRIBE_OVERRIDE_KEY) == 0) {
                        /* this can only occur once, so just store it */
                        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&bptr, kval->value, ORTE_BOOL))) {
                            ORTE_ERROR_LOG(rc);
                            return rc;
                        }
                        override_oversubscribed = *bptr;
                        continue;
                    }
                    if (strcmp(kval->key, ORTE_JOB_TOTAL_SLOTS_ALLOC_KEY) == 0) {
                        /* this can only occur once, so just store it */
                        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, kval->value, ORTE_STD_CNTR))) {
                            ORTE_ERROR_LOG(rc);
                            return rc;
                        }
                        total_slots_alloc = *sptr;
                        continue;
                    }
                } /* end for loop to process global data */
            } else {
                /* this must have come from one of the process containers, so it must
                * contain data for a proc structure - see if it
                * belongs to this node
                */
                for (kv=0; kv < value->cnt; kv++) {
                    kval = value->keyvals[kv];
                    if (strcmp(kval->key, ORTE_NODE_NAME_KEY) == 0) {
                        /* Most C-compilers will bark if we try to directly compare the string in the
                         * kval data area against a regular string, so we need to "get" the data
                         * so we can access it */
                       if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&node_name, kval->value, ORTE_STRING))) {
                            ORTE_ERROR_LOG(rc);
                            return rc;
                        }
                        /* if this is our node...must also protect against a zero-length string  */
                        if (NULL != node_name && 0 == strcmp(node_name, orte_system_info.nodename)) {
                            /* indicate that there is something for us to do */
                            node_included = true;
                            
                            /* ...harvest the info into a new child structure */
                            child = OBJ_NEW(orte_odls_child_t);
                            for (kv2 = 0; kv2 < value->cnt; kv2++) {
                                kval = value->keyvals[kv2];
                                if(strcmp(kval->key, ORTE_PROC_NAME_KEY) == 0) {
                                    /* copy the name into the child object */
                                    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(child->name), kval->value->data, ORTE_NAME))) {
                                        ORTE_ERROR_LOG(rc);
                                        return rc;
                                    }
                                    continue;
                                }
                                if(strcmp(kval->key, ORTE_PROC_CPU_LIST_KEY) == 0) {
                                    /* copy the name into the child object */
                                    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&slot_str, kval->value->data, ORTE_STRING))) {
                                        ORTE_ERROR_LOG(rc);
                                        return rc;
                                    }
                                    if (NULL != slot_str) {
                                        if (ORTE_SUCCESS != (rc = slot_list_to_cpu_set(slot_str, child))){
                                            ORTE_ERROR_LOG(rc);
                                            free(slot_str);
                                            return rc;
                                        }
                                        free(slot_str);
                                    }
                                    continue;
                                }
                                if(strcmp(kval->key, ORTE_PROC_APP_CONTEXT_KEY) == 0) {
                                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, kval->value, ORTE_STD_CNTR))) {
                                        ORTE_ERROR_LOG(rc);
                                        return rc;
                                    }
                                    child->app_idx = *sptr;  /* save the index into the app_context objects */
                                    continue;
                                }
                                if(strcmp(kval->key, ORTE_PROC_LOCAL_RANK_KEY) == 0) {
                                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, kval->value, ORTE_VPID))) {
                                        ORTE_ERROR_LOG(rc);
                                        return rc;
                                    }
                                    child->local_rank = *vptr;  /* save the local_rank */
                                    continue;
                                }
                                if(strcmp(kval->key, ORTE_NODE_NUM_PROCS_KEY) == 0) {
                                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, kval->value, ORTE_STD_CNTR))) {
                                        ORTE_ERROR_LOG(rc);
                                        return rc;
                                    }
                                    child->num_procs = *sptr;  /* save the number of procs from this job on this node */
                                    continue;
                                }
                                if(strcmp(kval->key, ORTE_NODE_OVERSUBSCRIBED_KEY) == 0) {
                                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&bptr, kval->value, ORTE_BOOL))) {
                                        ORTE_ERROR_LOG(rc);
                                        return rc;
                                    }
                                    oversubscribed = *bptr;
                                    continue;
                                }
                            } /* kv2 */
                            /* protect operation on the global list of children */
                            OPAL_THREAD_LOCK(&orte_odls_default.mutex);
                            opal_list_append(&orte_odls_default.children, &child->super);
                            opal_condition_signal(&orte_odls_default.cond);
                            OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);

                        }
                    }
                } /* for kv */
            }
        } /* for j */
    }

    /* if there is nothing for us to do, just return */
    if (!node_included) {
        return ORTE_SUCCESS;
    }

    /* record my uri in a file within the session directory so the local proc
     * can contact me
     */
    opal_output(orte_odls_globals.output, "odls: dropping local uri file");

    /* put the file in the job session dir for the job being launched */
    orte_ns.convert_jobid_to_string(&job_str, job);
    if (ORTE_SUCCESS != (rc = orte_session_dir(true, NULL, NULL, NULL,
                                               NULL, NULL, job_str, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* get the session dir name so we can put the file there */
    if (ORTE_SUCCESS != (rc = orte_session_dir_get_name(&session_dir, NULL, NULL, NULL,
                                                        NULL, NULL, NULL, job_str, NULL))) {
        ORTE_ERROR_LOG(rc);
        free(job_str);
        return rc;
    }
    free(job_str);
    
    /* create the file and put my uri into it */
    uri_file = opal_os_path(false, session_dir, "orted-uri.txt", NULL);
    fp = fopen(uri_file, "w");
    if (NULL == fp) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }
    my_uri = orte_rml.get_contact_info();
    fprintf(fp, "%s\n", my_uri);
    fclose(fp);
    free(uri_file);
    free(my_uri);

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
     * app context basis */
    for (item = opal_list_get_first(&app_context_list);
         item != opal_list_get_end(&app_context_list);
         item = opal_list_get_next(item)) {
        app_item = (odls_default_app_context_t*)item;
        if(app_item->app_context->preload_binary || NULL != app_item->app_context->preload_files) {
            filem_request = OBJ_NEW(orte_filem_base_request_t);
            filem_request->num_procs = 1;
            filem_request->proc_name = (orte_process_name_t*)malloc(sizeof(orte_process_name_t) * filem_request->num_procs);
            filem_request->proc_name[0].jobid  = orte_process_info.gpr_replica->jobid;
            filem_request->proc_name[0].vpid   = orte_process_info.gpr_replica->vpid;
            if(app_item->app_context->preload_binary) {
                if( ORTE_SUCCESS != (rc = orte_pls_fork_preload_append_binary(app_item->app_context, 
                                                                              filem_request) ) ){
                    opal_show_help("help-orte-odls-default.txt",
                                   "orte-odls-default:could-not-preload-binary",
                                   true, app_item->app_context->app);
                    ORTE_ERROR_LOG(rc);
                    /* Keep accumulating files anyway */
                }
            }
            if( NULL != app_item->app_context->preload_files) {
                if( ORTE_SUCCESS != (rc = orte_pls_fork_preload_append_files(app_item->app_context, 
                                                                             filem_request) ) ){
                    opal_show_help("help-orte-odls-default.txt",
                                   "orte-odls-default:could-not-preload-files",
                                   true, app_item->app_context->preload_files);
                    ORTE_ERROR_LOG(rc);
                    /* Keep accumulating files anyway */
                }
            }
            /* Actually bring over the files */
            if( ORTE_SUCCESS != (rc = orte_filem.get(filem_request)) ) {
                opal_show_help("help-orte-odls-default.txt",
                               "orte-odls-default:could-not-preload",
                               true, opal_argv_join(filem_request->local_targets, ' '));
                ORTE_ERROR_LOG(rc);
            }
            OBJ_DESTRUCT(filem_request);
        }
    }

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
        opal_output(orte_odls_globals.output,
                    "odls: could not get number of processors - using conservative settings");
    } else {
        opal_output(orte_odls_globals.output,
                    "odls: got %ld processors", (long)num_processors);
        
        /* only do this if we can actually get info on the number of processors */
        if (opal_list_get_size(&orte_odls_default.children) > (size_t)num_processors) {
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
            opal_output(orte_odls_globals.output, "odls: overriding oversubscription");
            if (opal_list_get_size(&orte_odls_default.children) > (size_t)num_processors) {
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
    opal_output(orte_odls_globals.output, "odls: oversubscribed set to %s want_processor set to %s",
                oversubscribed ? "true" : "false", want_processor ? "true" : "false");

    /* okay, now let's launch our local procs using a fork/exec */
    i = 0;
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_default.mutex);

    quit_flag = false;
    for (item = opal_list_get_first(&orte_odls_default.children);
         !quit_flag && item != opal_list_get_end(&orte_odls_default.children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;

        /* is this child already alive? This can happen if
         * we are asked to launch additional processes.
         * If it has been launched, then do nothing
         */
        if (child->alive) {
            opal_output(orte_odls_globals.output, "odls: child %s is already alive",
                        ORTE_NAME_PRINT(child->name));            
            continue;
        }
        
        /* do we have a child from the specified job. Because the
        *  job could be given as a WILDCARD value, we must use
        *  the dss.compare function to check for equality.
        */
        if (ORTE_EQUAL != orte_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
            opal_output(orte_odls_globals.output, "odls: child %s is not in job %ld being launched",
                        ORTE_NAME_PRINT(child->name), (long)job);            
            continue;
        }
        
        opal_output(orte_odls_globals.output, "odls: preparing to launch child %s",
                                              ORTE_NAME_PRINT(child->name));

        /* find the indicated app_context in the list */
        for (item2 = opal_list_get_first(&app_context_list);
             item2 != opal_list_get_end(&app_context_list);
             item2 = opal_list_get_next(item2)) {
            app_item = (odls_default_app_context_t*)item2;
            if (child->app_idx == app_item->app_context->idx) {
                app = app_item->app_context;
                goto DOFORK;
            }
        }
        /* get here if we couldn't find the app_context */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        opal_condition_signal(&orte_odls_default.cond);
        OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
        return ORTE_ERR_NOT_FOUND;
        
DOFORK:
        /* must unlock prior to fork to keep things clean in the
         * event library
         */
        opal_condition_signal(&orte_odls_default.cond);
        OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
        
        if (ORTE_SUCCESS != (rc = odls_default_fork_local_proc(app, child, start,
                                                               range, total_slots_alloc,
                                                               want_processor,
                                                               i, oversubscribed))) {
            /* do NOT ERROR_LOG this error - it generates
             * a message/node as most errors will be common
             * across the entire cluster. Instead, we let orterun
             * output a consolidated error message for us
             */
            quit_flag = true;
        }
        /* reaquire lock so we don't double unlock... */
        OPAL_THREAD_LOCK(&orte_odls_default.mutex);
        i++;
    }

    /* report the proc info and state in the registry */
    if (ORTE_SUCCESS != (rc = orte_odls_base_report_spawn(&orte_odls_default.children))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* setup the waitpids on the children */
    for (item = opal_list_get_first(&orte_odls_default.children);
         item != opal_list_get_end(&orte_odls_default.children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        if (ORTE_PROC_STATE_LAUNCHED == child->state) {
            OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
            orte_wait_cb(child->pid, odls_default_wait_local_proc, NULL);
            OPAL_THREAD_LOCK(&orte_odls_default.mutex);
            child->state = ORTE_PROC_STATE_RUNNING;
        }
    }

    /* cleanup */
    while (NULL != (item = opal_list_remove_first(&app_context_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&app_context_list);

    opal_condition_signal(&orte_odls_default.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
    return rc;
}


/**
 *  Pass a signal to my local procs
 */

static int send_signal(pid_t pid, int signal)
{
    int rc = ORTE_SUCCESS;
    
    if (kill(pid, signal) != 0) {
        switch(errno) {
            case EINVAL:
                ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                rc = ORTE_ERR_BAD_PARAM;
                break;
            case ESRCH:
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                rc = ORTE_ERR_NOT_FOUND;
                break;
            case EPERM:
                ORTE_ERROR_LOG(ORTE_ERR_PERM);
                rc = ORTE_ERR_PERM;
                break;
            default:
                ORTE_ERROR_LOG(ORTE_ERROR);
                rc = ORTE_ERROR;
        }
    }
    
    return rc;
}
int orte_odls_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal)
{
    int rc;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_default.mutex);

    /* if procs is NULL, then we want to signal all
     * of the local procs, so just do that case
     */
    if (NULL == proc) {
        rc = ORTE_SUCCESS;  /* pre-set this as an empty list causes us to drop to bottom */
        for (item = opal_list_get_first(&orte_odls_default.children);
             item != opal_list_get_end(&orte_odls_default.children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            if (ORTE_SUCCESS != (rc = send_signal(child->pid, (int)signal))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        opal_condition_signal(&orte_odls_default.cond);
        OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
        return rc;
    }
    
    /* we want it sent to some specified process, so find it */
    for (item = opal_list_get_first(&orte_odls_default.children);
         item != opal_list_get_end(&orte_odls_default.children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (ORTE_EQUAL == orte_dss.compare(&(child->name), (orte_process_name_t*)proc, ORTE_NAME)) {
            /* unlock before signaling as this may generate a callback */
            opal_condition_signal(&orte_odls_default.cond);
            OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
            if (ORTE_SUCCESS != (rc = send_signal(child->pid, (int)signal))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
    }
    
    /* only way to get here is if we couldn't find the specified proc.
     * report that as an error and return it
     */
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    opal_condition_signal(&orte_odls_default.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
    return ORTE_ERR_NOT_FOUND;
}


int orte_odls_default_deliver_message(orte_jobid_t job, orte_buffer_t *buffer, orte_rml_tag_t tag)
{
    int rc;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_default.mutex);
    
    for (item = opal_list_get_first(&orte_odls_default.children);
         item != opal_list_get_end(&orte_odls_default.children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* do we have a child from the specified job. Because the
        *  job could be given as a WILDCARD value, we must use
        *  the dss.compare function to check for equality.
        */
        if (ORTE_EQUAL != orte_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
            continue;
        }
        opal_output(orte_odls_globals.output, "odls: sending message to tag %lu on child %s",
                    (unsigned long)tag, ORTE_NAME_PRINT(child->name));
        
        /* if so, send the message */
        rc = orte_rml.send_buffer(child->name, buffer, tag, 0);
        if (rc < 0) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    opal_condition_signal(&orte_odls_default.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_default.mutex);
    return ORTE_SUCCESS;
}


static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}

/*
 * The difference between preloading a file, and a binary file is that 
 * we may need to update the app_context to reflect the placement of the binary file
 * on the local machine.
 */
static int orte_pls_fork_preload_append_binary(orte_app_context_t* context, 
                                               orte_filem_base_request_t *filem_request) {
    char * local_bin = NULL;
    int tmp_argc = 0;
    /*
     * Append the local placement
     */
    asprintf(&local_bin, "%s/%s", orte_process_info.job_session_dir, opal_basename(context->app));
    if(is_preload_local_dup(local_bin, filem_request) ) {
        goto cleanup;
    }
    opal_argv_append(&filem_request->num_targets, &(filem_request->local_targets), local_bin);

    /*
     * Append the remote file
     */
    tmp_argc = 0;
    opal_argv_append(&tmp_argc, &filem_request->remote_targets, context->app);

    /*
     * Append the flag
     */
    filem_request->target_flags = (int *)realloc(filem_request->target_flags, 
                                               sizeof(int) * (filem_request->num_targets + 1));
    filem_request->target_flags[filem_request->num_targets-1] = ORTE_FILEM_TYPE_FILE;

 cleanup:
    /*
     * Adjust the process name
     */
    if(NULL != context->app)
        free(context->app);
    context->app = local_bin;
    
    return ORTE_SUCCESS;
}


static int orte_pls_fork_preload_append_files(orte_app_context_t* context, 
                                              orte_filem_base_request_t *filem_request) {
    char * local_ref = NULL;
    int i, tmp_argc = 0, remote_argc = 0;
    char **remote_targets = NULL;
    char * temp = NULL;

    remote_targets = opal_argv_split(context->preload_files, ',');
    remote_argc  = opal_argv_count(remote_targets);

    for(i = 0; i < remote_argc; ++i) {
        if(NULL != context->preload_files_dest_dir) {
            if(context->preload_files_dest_dir[0] == '.') {
                asprintf(&local_ref, "%s/%s/%s", context->cwd, context->preload_files_dest_dir, opal_basename(remote_targets[i]) );
            }
            else {
                asprintf(&local_ref, "%s/%s", context->preload_files_dest_dir, opal_basename(remote_targets[i]) );
            }
        }
        else {
            /* 
             * If the preload_files_dest_dir is not specified
             * If this is an absolute path, copy it to that path. Otherwise copy it to the cwd.
             */
            if('/' == remote_targets[i][0]) {
                asprintf(&local_ref, "%s", remote_targets[i]);
            } else {
                asprintf(&local_ref, "%s/%s", context->cwd, opal_basename(remote_targets[i]) );
            }
        }

        asprintf(&temp, "test -e %s", local_ref);
        if(0 == system(temp)) {
            char hostname[MAXHOSTNAMELEN];
            gethostname(hostname, sizeof(hostname));
            opal_show_help("help-orte-pls-fork.txt",
                           "orte-pls-fork:preload-file-exists",
                           true, local_ref, hostname);
            free(temp);
            temp = NULL;
            free(local_ref);
            local_ref = NULL;
            continue;
        }
        free(temp);
        temp = NULL;
        
        /*
         * Is this a duplicate
         */
        if(is_preload_local_dup(local_ref, filem_request) ) {
            free(local_ref);
            local_ref = NULL;
            continue;
        }

        /*
         * Append the local files we want
         */
        opal_argv_append(&filem_request->num_targets, &filem_request->local_targets, local_ref);

        /*
         * Append the remote files we want
         */
        tmp_argc = filem_request->num_targets - 1;
        opal_argv_append(&tmp_argc, &filem_request->remote_targets, remote_targets[i]);
        
        /*
         * Set the flags
         */
        filem_request->target_flags = (int *)realloc(filem_request->target_flags, sizeof(int) * 1);
        filem_request->target_flags[filem_request->num_targets-1] = ORTE_FILEM_TYPE_UNKNOWN;

        free(local_ref);
        local_ref = NULL;
    }

    if(NULL != local_ref)
        free(local_ref);
    if(NULL != remote_targets)
        opal_argv_free(remote_targets);

    return ORTE_SUCCESS;
}

/*
 * Keeps us from transfering the same file more than once.
 */
static bool is_preload_local_dup(char *local_ref, orte_filem_base_request_t *filem_request) {
    int i;

    for(i = 0; i < filem_request->num_targets; ++i) {
        if(0 == strncmp(local_ref, filem_request->local_targets[i], strlen(local_ref)+1) ) {
            return true;
        }
    }

    return false;
}

/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <signal.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <time.h>

#include "opal/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/util/show_help.h"
#include "opal/util/path.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/base/base.h"

#include "orte/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_wait.h"
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

#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/process/odls_process.h"

static void set_handler_default(int sig);

/* this entire function gets called within a GPR compound command,
 * so the subscription actually doesn't get done until the orted
 * executes the compound command
 */
static int orte_odls_process_subscribe_launch_data( orte_jobid_t job,
                                                    orte_gpr_notify_cb_fn_t cbfunc )
{
    char *segment;
    orte_gpr_value_t *values[2];
    orte_gpr_subscription_t *subs, sub=ORTE_GPR_SUBSCRIPTION_EMPTY;
    orte_gpr_trigger_t *trigs, trig=ORTE_GPR_TRIGGER_EMPTY;
    char *glob_keys[] = {
        ORTE_JOB_APP_CONTEXT_KEY,
        ORTE_JOB_VPID_START_KEY,
        ORTE_JOB_VPID_RANGE_KEY
    };
    int num_glob_keys = 3;
    char* keys[] = {
        ORTE_PROC_NAME_KEY,
        ORTE_PROC_APP_CONTEXT_KEY,
        ORTE_NODE_NAME_KEY,
    };
    int num_keys = 3;
    int i, rc;
    
    /* get the job segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* attach ourselves to the "standard" orted trigger */
    if (ORTE_SUCCESS !=
        (rc = orte_schema.get_std_trigger_name(&(trig.name),
                                                ORTED_LAUNCH_STAGE_GATE_TRIGGER, job))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    
    /* ask for return of all data required for launching local processes */
    subs = &sub;
    sub.action = ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG;
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_subscription_name(&(sub.name),
                                                                     ORTED_LAUNCH_STG_SUB,
                                                                     job))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        free(trig.name);
        return rc;
    }
    sub.cnt = 2;
    sub.values = values;
    
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[0]), ORTE_GPR_TOKENS_OR, segment,
                                                     num_glob_keys, 1))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        free(sub.name);
        free(trig.name);
        return rc;
    }
    values[0]->tokens[0] = strdup(ORTE_JOB_GLOBALS);
    for (i=0; i < num_glob_keys; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[0]->keyvals[i]),
                                                          glob_keys[i], ORTE_UNDEF, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(segment);
            free(sub.name);
            free(trig.name);
            OBJ_RELEASE(values[0]);
            return rc;
        }
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[1]), ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR,
                                                     segment, num_keys, 0))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        free(sub.name);
        free(trig.name);
        OBJ_RELEASE(values[0]);
        return rc;
    }
    for (i=0; i < num_keys; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[1]->keyvals[i]),
                                                          keys[i], ORTE_UNDEF, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(segment);
            free(sub.name);
            free(trig.name);
            OBJ_RELEASE(values[0]);
            OBJ_RELEASE(values[1]);
            return rc;
        }
    }
    
    sub.cbfunc = cbfunc;
    
    trigs = &trig; 
    
    /* do the subscription */
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe(1, &subs, 1, &trigs))) {
        ORTE_ERROR_LOG(rc);
    }
    free(segment);
    free(sub.name);
    free(trig.name);
    OBJ_RELEASE(values[0]);
    OBJ_RELEASE(values[1]);

    return rc;
}

static bool orte_odls_process_child_died( pid_t pid, unsigned int timeout,
                                          int* exit_status )
{
    int error;
    HANDLE handle = OpenProcess( PROCESS_TERMINATE | SYNCHRONIZE, FALSE,
                                 (DWORD)pid );
    if( INVALID_HANDLE_VALUE == handle ) {
        error = GetLastError();
        /* Let's suppose that the process dissapear ... by now */
        return true;
    }
    CloseHandle(handle);
    /* The child didn't die, so return false */
    return false;
}

static int orte_odls_process_kill_local( pid_t pid, int sig_num )
{
    if( false == TerminateProcess( (HANDLE)pid, 1 ) ) {
        return (int)GetLastError();
    }
    return 0;
}

static int orte_odls_process_kill_local_procs(orte_jobid_t job, bool set_state)
{
    odls_process_child_t *child;
    opal_list_item_t *item;
    int rc, exit_status;
    opal_list_t procs_killed;
    orte_namelist_t *proc;

    OBJ_CONSTRUCT(&procs_killed, opal_list_t);
    
    /* since we are going to be working with the global list of
     * children, we need to protect that list from modification
     * by other threads
     */
    OPAL_THREAD_LOCK(&orte_odls_process.mutex);
    
    for (item = opal_list_get_first(&orte_odls_process.children);
         item != opal_list_get_end(&orte_odls_process.children);
         item = opal_list_get_next(item)) {
        child = (odls_process_child_t*)item;
        
        /* is this process alive? if not, then nothing for us
         * to do to it
         */
        if (!child->alive) {
            continue;
        }
        
        /* do we have a child from the specified job? Because the
        *  job could be given as a WILDCARD value, we must use
        *  the dss.compare function to check for equality.
        */
        if (ORTE_EQUAL != orte_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
            continue;
        }
        
        /* de-register the SIGCHILD callback for this pid */
        orte_wait_cb_cancel(child->pid);

        /* Send a sigterm to the process. */
        if (0 != orte_odls_process_kill_local(child->pid, SIGTERM)) {
            int err = GetLastError();
            opal_show_help("help-odls-default.txt",
                           "odls-default:could-not-send-kill",
                           true, orte_system_info.nodename, child->pid, err);
            goto MOVEON;
        }

        /* The kill succeeded.  Wait up to timeout_before_sigkill
           seconds to see if it died. */

        if (!orte_odls_process_child_died(child->pid, orte_odls_globals.timeout_before_sigkill, &exit_status)) {
            /* try killing it again */
            orte_odls_process_kill_local(child->pid, SIGKILL);
            /* Double check that it actually died this time */
            if (!orte_odls_process_child_died(child->pid, orte_odls_globals.timeout_before_sigkill, &exit_status)) {
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
            opal_condition_signal(&orte_odls_process.cond);
            OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);
            return rc;
        }
        opal_list_append(&procs_killed, &proc->item);
    }
    
    /* we are done with the global list, so we can now release
     * any waiting threads - this also allows any callbacks to work
     */
    opal_condition_signal(&orte_odls_process.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);
        
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
static void odls_process_wait_local_proc(pid_t pid, int status, void* cbdata)
{
    odls_process_child_t *child;
    opal_list_item_t *item;
    bool aborted;
    char *job, *vpid, *abort_file;
    struct _stat buf;
    int rc;

    /* since we are going to be working with the global list of
     * children, we need to protect that list from modification
     * by other threads. This will also be used to protect us
     * from race conditions on any abort situation
     */
    OPAL_THREAD_LOCK(&orte_odls_process.mutex);
 
    /* find this child */
    for (item = opal_list_get_first(&orte_odls_process.children);
         item != opal_list_get_end(&orte_odls_process.children);
         item = opal_list_get_next(item)) {
        child = (odls_process_child_t*)item;
        if (child->alive && pid == child->pid) { /* found it */
            goto GOTCHILD;
        }
    }
    /* get here if we didn't find the child, or if the specified child is already
    * dead. If the latter, then we have a problem as it means we are detecting
    * it exiting multiple times
    */
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    opal_condition_signal(&orte_odls_process.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);
    return;

GOTCHILD:
    orte_iof.iof_flush();

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
        if (0 == _stat(abort_file, &buf)) {
            /* the abort file must exist - there is nothing in it we need. It's
             * meer existence indicates that an abnormal termination occurred
             */
            aborted = true;
            free(abort_file);
        }
    } else {
        /* the process was terminated with a signal! That's definitely
         * abnormal, so indicate that condition
         */
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

    /* Need to unlock before we call set_proc_state as this is going to generate
     * a trigger that will eventually callback to us
     */
    opal_condition_signal(&orte_odls_process.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);

    if (aborted) {
        rc = orte_smr.set_proc_state(child->name, ORTE_PROC_STATE_ABORTED, status);        
    } else {
        rc = orte_smr.set_proc_state(child->name, ORTE_PROC_STATE_TERMINATED, status);
    }

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
}

/**
 *  Fork/exec the specified processes
 */

static int orte_odls_process_fork_local_proc(
    orte_app_context_t* context,
    odls_process_child_t *child,
    orte_vpid_t vpid_start,
    orte_vpid_t vpid_range,
    bool want_processor,
    size_t processor,
    char **base_environ)
{
    pid_t pid;
    orte_iof_base_io_conf_t opts;
    int rc;
    int i = 0;
    char** environ_copy;
    char *param, *param2;
    char *uri;

    /* should pull this information from MPIRUN instead of going with
       default */
    opts.usepty = OMPI_ENABLE_PTY_SUPPORT;

    /* BWB - Fix post beta.  Should setup stdin in orterun and
       make part of the app_context */
    if( 0 == child->name->vpid ) {
        opts.connect_stdin = true;
    } else {
        opts.connect_stdin = false;
    }

    /* Try to change to the context cwd and check that the app
       exists and is executable. The RMGR functions will print
       out a pretty error message if either of these operations fails
     */
    if (ORTE_SUCCESS != (i = orte_rmgr.check_context_cwd(context, true))) {
        /* Tell the parent that Badness happened */
        return ORTE_ERR_FATAL;
    }
    if (ORTE_SUCCESS != (i = orte_rmgr.check_context_app(context))) {
        /* Tell the parent that Badness happened */
        return ORTE_ERR_FATAL;
    }

    /* setup base environment: copy the current environ and merge
       in the app context environ */
    if (NULL != context->env) {
        environ_copy = opal_environ_merge(base_environ, context->env);
    } else {
        environ_copy = opal_argv_copy(base_environ);
    }

    /* special case handling for --prefix: this is somewhat icky,
       but at least some users do this.  :-\ It is possible that
       when using --prefix, the user will also "-x PATH" and/or
       "-x LD_LIBRARY_PATH", which would therefore clobber the
       work that was done in the prior odls to ensure that we have
       the prefix at the beginning of the PATH and
       LD_LIBRARY_PATH.  So examine the context->env and see if we
       find PATH or LD_LIBRARY_PATH.  If found, that means the
       prior work was clobbered, and we need to re-prefix those
       variables. */
    for (i = 0; NULL != context->env && NULL != context->env[i]; ++i) {
        char *newenv;

        /* Reset PATH */
        if (0 == strncmp("PATH=", context->env[i], 5)) {
            asprintf(&newenv, "%s\\bin;%s",
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

    param = mca_base_param_environ_variable("rmgr","bootproxy","jobid");
    opal_unsetenv(param, &environ_copy);
    free(param);

    if (want_processor) {
        param = mca_base_param_environ_variable("mpi", NULL,
                                                "paffinity_processor");
        asprintf(&param2, "%lu", (unsigned long) processor);
        opal_setenv(param, param2, true, &environ_copy);
        free(param);
        free(param2);
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
        uri = orte_rml.get_uri();
    }
    param = mca_base_param_environ_variable("ns","replica","uri");
    opal_setenv(param, uri, true, &environ_copy);
    free(param);
    free(uri);

    /* setup gpr contact info */
    if(NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    param = mca_base_param_environ_variable("gpr","replica","uri");
    opal_setenv(param, uri, true, &environ_copy);
    free(param);
    free(uri);

    /* use same nodename as the starting daemon (us) */
    param = mca_base_param_environ_variable("orte", "base", "nodename");
    opal_setenv(param, orte_system_info.nodename, true, &environ_copy);
    free(param);

    /* push name into environment */
    orte_ns_nds_env_put(child->name, vpid_start, vpid_range,
                        &environ_copy);

    if (context->argv == NULL) {
        context->argv = (char**)malloc(sizeof(char*)*2);
        context->argv[0] = strdup(context->app);
        context->argv[1] = NULL;
    }

    /* Flush all standard handles (stdin, stdout & stderr). */
    _flushall();

    {
        intptr_t handle = _spawnve( _P_NOWAIT, context->app, context->argv, environ_copy );

        if( -1 == handle ) {
            opal_show_help("help-orted-launcer.txt", "orted-launcher:execv-error",
                           true, context->app, "TODO: some error");
            orte_smr.set_proc_state(child->name, ORTE_PROC_STATE_ABORTED, -1);
            return ORTE_ERROR;
        }
        pid = handle;
    }

    /* set the proc state to LAUNCHED and increment that counter so the trigger can fire
     */
    if (ORTE_SUCCESS !=
        (rc = orte_smr.set_proc_state(child->name, ORTE_PROC_STATE_LAUNCHED, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
        
    /* save the pid and indicate we've been launched */
    child->pid = pid;
    child->alive = true;

    /* wait for the child process - dont register for wait
     * callback until after I/O is setup and the pid registered -
     * otherwise can receive the wait callback before the above is
     * ever completed
     */
    orte_wait_cb(pid, odls_process_wait_local_proc, NULL);
    return ORTE_SUCCESS;
}


/**
 * Launch all processes allocated to the current node.
 */

static int orte_odls_process_launch_local_procs(orte_gpr_notify_data_t *data, char **base_environ)
{
    int rc;
    orte_std_cntr_t i, j, kv, kv2, *sptr;
    orte_gpr_value_t *value, **values;
    orte_gpr_keyval_t *kval;
    orte_app_context_t *app;
    orte_jobid_t job;
    orte_vpid_t *vptr, start, range;
    char *node_name;
    opal_list_t app_context_list;
    odls_process_child_t *child;
    odls_process_app_context_t *app_item;
    size_t num_processors;
    bool want_processor;
    opal_list_item_t *item, *item2;

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
    /* We need to create a list of the app_contexts
     * so we can know what to launch - the process info only gives
     * us an index into the app_context array, not the app_context
     * info itself.
     */
    
    OBJ_CONSTRUCT(&app_context_list, opal_list_t);
    
    /* set the default values to INVALID */
    start = ORTE_VPID_INVALID;
    range = ORTE_VPID_INVALID;
    
    values = (orte_gpr_value_t**)(data->values)->addr;
    for (j=0, i=0; i < data->cnt && j < (data->values)->size; j++) {  /* loop through all returned values */
        if (NULL != values[j]) {
            i++;
            value = values[j];
            
            if (0 == strcmp(value->tokens[0], ORTE_JOB_GLOBALS)) {
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
                        app_item = OBJ_NEW(odls_process_app_context_t);
                        if (NULL == app_item) {
                            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                            return ORTE_ERR_OUT_OF_RESOURCE;
                        }
                        app_item->app_context = app;
                        opal_list_append(&app_context_list, &app_item->super);
                        kval->value->data = NULL;  /* protect the data storage from later release */
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
                            /* ...harvest the info into a new child structure */
                            child = OBJ_NEW(odls_process_child_t);
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
                                if(strcmp(kval->key, ORTE_PROC_APP_CONTEXT_KEY) == 0) {
                                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, kval->value, ORTE_STD_CNTR))) {
                                        ORTE_ERROR_LOG(rc);
                                        return rc;
                                    }
                                    child->app_idx = *sptr;  /* save the index into the app_context objects */
                                    continue;
                                }
                            } /* kv2 */
                            /* protect operation on the global list of children */
                            OPAL_THREAD_LOCK(&orte_odls_process.mutex);
                            opal_list_append(&orte_odls_process.children, &child->super);
                            opal_condition_signal(&orte_odls_process.cond);
                            OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);

                        }
                    }
                } /* for kv */
            }
        } /* for j */
    }

    /* determine if we are oversubscribed */
    want_processor = true;  /* default to taking it for ourselves */
    opal_paffinity_base_get_num_processors(&rc);
    num_processors = (size_t)rc;
    if (opal_list_get_size(&orte_odls_process.children) > num_processors) { /* oversubscribed */
        want_processor = false;
    }

    /* okay, now let's launch our local procs using a fork/exec */
    i = 0;
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_process.mutex);

    for (item = opal_list_get_first(&orte_odls_process.children);
         item != opal_list_get_end(&orte_odls_process.children);
         item = opal_list_get_next(item)) {
        child = (odls_process_child_t*)item;

        /* is this child already alive? This can happen if
         * we are asked to launch additional processes.
         * If it has been launched, then do nothing
         */
        if (child->alive) {
            continue;
        }
        
        /* do we have a child from the specified job. Because the
        *  job could be given as a WILDCARD value, we must use
        *  the dss.compare function to check for equality.
        */
        if (ORTE_EQUAL != orte_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
            continue;
        }
        
        /* find the indicated app_context in the list */
        for (item2 = opal_list_get_first(&app_context_list);
             item2 != opal_list_get_end(&app_context_list);
             item2 = opal_list_get_next(item2)) {
            app_item = (odls_process_app_context_t*)item2;
            if (child->app_idx == app_item->app_context->idx) {
                app = app_item->app_context;
                goto DOFORK;
            }
        }
        /* get here if we couldn't find the app_context */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        opal_condition_signal(&orte_odls_process.cond);
        OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);
        return ORTE_ERR_NOT_FOUND;
        
DOFORK:
        /* must unlock prior to fork to keep things clean in the
         * event library
         */
        opal_condition_signal(&orte_odls_process.cond);
        OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);
        
        if (ORTE_SUCCESS != (rc = orte_odls_process_fork_local_proc(app, child, start,
                                                                    range, want_processor,
                                                                    i, base_environ))) {
            ORTE_ERROR_LOG(rc);
            orte_smr.set_proc_state(child->name, ORTE_PROC_STATE_ABORTED, 0);
            opal_condition_signal(&orte_odls_process.cond);
            return rc;
        }
        /* reaquire lock so we don't double unlock... */
        OPAL_THREAD_LOCK(&orte_odls_process.mutex);
        i++;
    }

    /* cleanup */
    while (NULL != (item = opal_list_remove_first(&app_context_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&app_context_list);

    opal_condition_signal(&orte_odls_process.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);
    return ORTE_SUCCESS;
}

static int send_signal(pid_t pid, int signal)
{
    return ORTE_ERROR;
}

static int orte_odls_process_signal_local_proc(const orte_process_name_t *proc, int32_t signal)
{
    int rc;
    opal_list_item_t *item;
    odls_process_child_t *child;
    
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&orte_odls_process.mutex);

    /* if procs is NULL, then we want to signal all
     * of the local procs, so just do that case
     */
    if (NULL == proc) {
        rc = ORTE_SUCCESS;  /* pre-set this as an empty list causes us to drop to bottom */
        for (item = opal_list_get_first(&orte_odls_process.children);
             item != opal_list_get_end(&orte_odls_process.children);
             item = opal_list_get_next(item)) {
            child = (odls_process_child_t*)item;
            if (ORTE_SUCCESS != (rc = send_signal(child->pid, (int)signal))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        opal_condition_signal(&orte_odls_process.cond);
        OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);
        return rc;
    }
    
    /* we want it sent to some specified process, so find it */
    for (item = opal_list_get_first(&orte_odls_process.children);
         item != opal_list_get_end(&orte_odls_process.children);
         item = opal_list_get_next(item)) {
        child = (odls_process_child_t*)item;
        if (ORTE_EQUAL == orte_dss.compare(&(child->name), (void*)proc, ORTE_NAME)) {
            /* unlock before signaling as this may generate a callback */
            opal_condition_signal(&orte_odls_process.cond);
            OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);
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
    opal_condition_signal(&orte_odls_process.cond);
    OPAL_THREAD_UNLOCK(&orte_odls_process.mutex);
    return ORTE_ERR_NOT_FOUND;
}

orte_odls_base_module_1_3_0_t orte_odls_process_module = {
    orte_odls_process_subscribe_launch_data,
    orte_odls_process_launch_local_procs,
    orte_odls_process_kill_local_procs,
    orte_odls_process_signal_local_proc
};

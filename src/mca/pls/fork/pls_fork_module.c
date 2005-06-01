/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>

#include "include/orte_constants.h"
#include "event/event.h"
#include "util/argv.h"
#include "util/output.h"
#include "util/sys_info.h"
#include "util/univ_info.h"
#include "util/ompi_environ.h"
#include "util/session_dir.h"
#include "runtime/orte_wait.h"
#include "mca/errmgr/errmgr.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/iof_base_setup.h"
#include "mca/base/mca_base_param.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/ns_base_nds.h"
#include "mca/pls/pls.h"
#include "mca/pls/base/base.h"
#include "mca/rml/rml.h"
#include "mca/gpr/gpr.h"
#include "mca/rmaps/base/base.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "mca/soh/soh.h"
#include "mca/soh/base/base.h"
#include "pls_fork.h"


extern char **environ;

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
static int orte_pls_fork_launch_threaded(orte_jobid_t);
#endif


orte_pls_base_module_1_0_0_t orte_pls_fork_module = {
#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
    orte_pls_fork_launch_threaded,
#else
    orte_pls_fork_launch,
#endif
    orte_pls_fork_terminate_job,
    orte_pls_fork_terminate_proc,
    orte_pls_fork_finalize
};

static void set_handler_default(int sig);


/*
 *  Wait for a callback indicating the child has completed.
 */

static void orte_pls_fork_wait_proc(pid_t pid, int status, void* cbdata)
{
    orte_rmaps_base_proc_t* proc = (orte_rmaps_base_proc_t*)cbdata;
    int rc;

    /* Clean up the session directory as if we were the process
       itself.  This covers the case where the process died abnormally
       and didn't cleanup its own session directory. */
 
    orte_session_dir_finalize(&proc->proc_name);
    orte_iof.iof_flush();

    /* set the state of this process */
    if(WIFEXITED(status)) {
        rc = orte_soh.set_proc_soh(&proc->proc_name, ORTE_PROC_STATE_TERMINATED, status);
    } else {
        rc = orte_soh.set_proc_soh(&proc->proc_name, ORTE_PROC_STATE_ABORTED, status);
    }
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(proc);

    /* release any waiting threads */
    OMPI_THREAD_LOCK(&mca_pls_fork_component.lock);
    mca_pls_fork_component.num_children--;
    ompi_condition_signal(&mca_pls_fork_component.cond);
    OMPI_THREAD_UNLOCK(&mca_pls_fork_component.lock);
}

/**
 *  Fork/exec the specified processes
 */

static int orte_pls_fork_proc(
    orte_app_context_t* context, 
    orte_rmaps_base_proc_t* proc,
    orte_vpid_t vpid_start,
    orte_vpid_t vpid_range)
{
    pid_t pid;
    orte_iof_base_io_conf_t opts;
    int rc;
    sigset_t sigs;
    orte_vpid_t vpid;

    /* should pull this information from MPIRUN instead of going with
       default */
    opts.usepty = OMPI_ENABLE_PTY_SUPPORT;

    /* BWB - Fix post beta.  Should setup stdin in orterun and
       make part of the app_context */
    if (ORTE_SUCCESS == orte_ns.get_vpid(&vpid, &proc->proc_name) &&
        vpid == 0) {
        opts.connect_stdin = true;
    } else {
        opts.connect_stdin = false;
    }

    rc = orte_iof_base_setup_prefork(&opts);
    if (OMPI_SUCCESS != rc) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    pid = fork();
    if(pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if(pid == 0) {
        char* param;
        char* uri;
        char **environ_copy;

#if 0
        /* for gperf - setup a new directory for each executable */
        char path[PATH_MAX];

        /* set working directory */
        sprintf(path, "%s/%d", context->cwd, getpid());
        if(mkdir(path,0777) != 0) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        }
        if(chdir(path) != 0) {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        }
#else
        if(chdir(context->cwd) != 0) {
            perror("chdir");
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        }
#endif

        /* setup base environment: copy the current environ and merge
           in the app context environ */
        if (NULL != context->env) {
            environ_copy = ompi_environ_merge(environ, context->env);
        } else {
            environ_copy = ompi_argv_copy(environ);
        }
        param = mca_base_param_environ_variable("rmgr","bootproxy","jobid");
        ompi_unsetenv(param, &environ_copy);

        /* setup universe info */
        if (NULL != orte_universe_info.name) {
            param = mca_base_param_environ_variable("universe", NULL, NULL);
            asprintf(&uri, "%s@%s:%s", orte_universe_info.uid,
                                       orte_universe_info.host,
                                       orte_universe_info.name);
            ompi_setenv(param, uri, true, &environ_copy);
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
        ompi_setenv(param, uri, true, &environ_copy);
        free(param);
        free(uri);

        /* setup gpr contact info */
        if(NULL != orte_process_info.gpr_replica_uri) {
            uri = strdup(orte_process_info.gpr_replica_uri);
        } else {
            uri = orte_rml.get_uri();
        }
        param = mca_base_param_environ_variable("gpr","replica","uri");
        ompi_setenv(param, uri, true, &environ_copy);
        free(param);
        free(uri);

        /* use same nodename as the starting daemon (us) */
        param = mca_base_param_environ_variable("orte", "base", "nodename");
        ompi_setenv(param, orte_system_info.nodename, true, &environ_copy);
        free(param);

        /* push name into environment */
        orte_ns_nds_env_put(&proc->proc_name, vpid_start, vpid_range, 
                            &environ_copy);

        /* setup stdout/stderr */
        orte_iof_base_setup_child(&opts);

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
        set_handler_default(SIGCHLD);
        set_handler_default(SIGPIPE);
        
        /* Unblock all signals, for many of the same reasons that we
           set the default handlers, above.  This is noticable on
           Linux where the event library blocks SIGTERM, but we don't
           want that blocked by the launched process. */

        sigprocmask(0, 0, &sigs);
        sigprocmask(SIG_UNBLOCK, &sigs, 0);

        /* Exec the new executable */

        execve(context->app, context->argv, environ_copy);
        ompi_output(0, "orte_pls_fork: %s - %s\n", context->app, 
            ompi_argv_join(context->argv, ' '));
        ompi_output(0, "orte_pls_fork: execv failed with errno=%d\n", errno);
        exit(-1);

    } else {

        /* connect endpoints IOF */
        rc = orte_iof_base_setup_parent(&proc->proc_name, &opts);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* save the pid in the registry */
        if(ORTE_SUCCESS != (rc = orte_pls_base_set_proc_pid(&proc->proc_name, pid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* wait for the child process - dont register for wait callback until
         * after I/O is setup and the pid registered - otherwise can receive the
         * wait callback before the above is ever completed
        */
        OMPI_THREAD_LOCK(&mca_pls_fork_component.lock);
        mca_pls_fork_component.num_children++;
        OMPI_THREAD_UNLOCK(&mca_pls_fork_component.lock);
        OBJ_RETAIN(proc);
        orte_wait_cb(pid, orte_pls_fork_wait_proc, proc);
    }
    return ORTE_SUCCESS;
}


/**
 * Launch all processes allocated to the current node.
 */

int orte_pls_fork_launch(orte_jobid_t jobid)
{
    ompi_list_t map;
    ompi_list_item_t* item;
    orte_vpid_t vpid_start;
    orte_vpid_t vpid_range;
    int rc;

    /* query the allocation for this node */
    OBJ_CONSTRUCT(&map, ompi_list_t);
    rc = orte_rmaps_base_get_node_map(
        orte_process_info.my_name->cellid,jobid,orte_system_info.nodename,&map);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    rc = orte_rmaps_base_get_vpid_range(jobid, &vpid_start, &vpid_range);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* attempt to launch each of the apps */
    for(item =  ompi_list_get_first(&map);
        item != ompi_list_get_end(&map);
        item =  ompi_list_get_next(item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)item;
        size_t i;
        for(i=0; i<map->num_procs; i++) {
            rc = orte_pls_fork_proc(map->app, map->procs[i], vpid_start, vpid_range);
            if(ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
    }

cleanup:
    while(NULL != (item = ompi_list_remove_first(&map))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&map);
    return rc;
}

/**
 *  Query for all processes allocated to the job and terminate
 *  those on the current node.
 */

int orte_pls_fork_terminate_job(orte_jobid_t jobid)
{
    /* query for the pids allocated on this node */
    char *segment;
    char *keys[3];
    orte_gpr_value_t** values = NULL;
    size_t i, k, num_values = 0;
    int rc;

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
                                                                                                    
    keys[0] = ORTE_NODE_NAME_KEY;
    keys[1] = ORTE_PROC_PID_KEY;
    keys[2] = NULL;
                                                                                                    
    rc = orte_gpr.get(
        ORTE_GPR_KEYS_AND|ORTE_GPR_TOKENS_OR,
        segment,
        NULL,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        free(segment);
        return rc;
    }

    for(i=0; i<num_values; i++) {
        orte_gpr_value_t* value = values[i];
        pid_t pid = 0;
        for(k=0; k<value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                if(strcmp(keyval->value.strptr, orte_system_info.nodename) != 0) {
                    break;
                }
            } else if (strcmp(keyval->key, ORTE_PROC_PID_KEY) == 0) {
                pid = keyval->value.ui32;
            } 
        }
        if (0 != pid) {
            kill(pid, SIGKILL);
        }
        OBJ_RELEASE(value);
    }
    if(NULL != values) {
        free(values);
    }
    free(segment);
    return ORTE_SUCCESS;
}


int orte_pls_fork_terminate_proc(const orte_process_name_t* proc)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_pls_fork_finalize(void)
{
    if(mca_pls_fork_component.reap) {
        OMPI_THREAD_LOCK(&mca_pls_fork_component.lock);
        while(mca_pls_fork_component.num_children > 0) {
            ompi_condition_wait(&mca_pls_fork_component.cond,
                &mca_pls_fork_component.lock);
        }
        OMPI_THREAD_UNLOCK(&mca_pls_fork_component.lock);
    }
    return ORTE_SUCCESS;
}


/**
 * Handle threading issues.
 */

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS

struct orte_pls_fork_stack_t {
    ompi_condition_t cond;
    ompi_mutex_t mutex;
    bool complete;
    orte_jobid_t jobid;
    int rc;
};
typedef struct orte_pls_fork_stack_t orte_pls_fork_stack_t;

static void orte_pls_fork_stack_construct(orte_pls_fork_stack_t* stack)
{
    OBJ_CONSTRUCT(&stack->mutex, ompi_mutex_t);
    OBJ_CONSTRUCT(&stack->cond, ompi_condition_t);
    stack->rc = 0;
    stack->complete = false;
}

static void orte_pls_fork_stack_destruct(orte_pls_fork_stack_t* stack)
{
    OBJ_DESTRUCT(&stack->mutex);
    OBJ_DESTRUCT(&stack->cond);
}

static OBJ_CLASS_INSTANCE(
    orte_pls_fork_stack_t,
    ompi_object_t,
    orte_pls_fork_stack_construct,
    orte_pls_fork_stack_destruct);


static void orte_pls_fork_launch_cb(int fd, short event, void* args)
{
    orte_pls_fork_stack_t *stack = (orte_pls_fork_stack_t*)args;
    OMPI_THREAD_LOCK(&stack->mutex);
    stack->rc = orte_pls_fork_launch(stack->jobid);
    stack->complete = true;
    ompi_condition_signal(&stack->cond);
    OMPI_THREAD_UNLOCK(&stack->mutex);
}

static int orte_pls_fork_launch_threaded(orte_jobid_t jobid)
{

    struct timeval tv = { 0, 0 };
    struct ompi_event event;
    struct orte_pls_fork_stack_t stack;

    OBJ_CONSTRUCT(&stack, orte_pls_fork_stack_t);

    stack.jobid = jobid;
    ompi_evtimer_set(&event, orte_pls_fork_launch_cb, &stack);
    ompi_evtimer_add(&event, &tv);

    OMPI_THREAD_LOCK(&stack.mutex);
    while(false == stack.complete) {
         ompi_condition_wait(&stack.cond, &stack.mutex);
    }
    OMPI_THREAD_UNLOCK(&stack.mutex);
    OBJ_DESTRUCT(&stack);
    return stack.rc;
}

#endif



static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}

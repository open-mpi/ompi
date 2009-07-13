/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
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

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <fcntl.h>
#include <signal.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/util/bit_ops.h"

#include "orte/util/show_help.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/routed/routed.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/rshd/plm_rshd.h"

#if OPAL_HAVE_POSIX_THREADS && OPAL_THREADS_HAVE_DIFFERENT_PIDS && OPAL_ENABLE_PROGRESS_THREADS
static int orte_plm_rshd_launch_threaded(orte_job_t *jdata);
#endif

orte_plm_base_module_t orte_plm_rshd_module = {
    orte_plm_rshd_init,
    orte_plm_base_set_hnp_name,
#if OPAL_HAVE_POSIX_THREADS && OPAL_THREADS_HAVE_DIFFERENT_PIDS && OPAL_ENABLE_PROGRESS_THREADS
    orte_plm_rshd_launch_threaded,
#else
    orte_plm_rshd_launch,
#endif
    NULL,
    orte_plm_rshd_terminate_job,
    orte_plm_rshd_terminate_orteds,
    NULL,
    orte_plm_rshd_signal_job,
    orte_plm_rshd_finalize
};


/*
 * Local functions
 */
static void set_handler_default(int sig);

/**
 * Init the module
 */
int orte_plm_rshd_init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/**
 * Callback on daemon exit.
 */

static void wait_cb(pid_t pid, int status, void* cbdata)
{
    orte_proc_t *proc = (orte_proc_t*)cbdata;
    orte_job_t *jdata;
    
    /* get the associated job object */
    jdata = orte_get_job_data_object(proc->name.jobid);
    
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) { /* if abnormal exit */
        
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s proc %d failed with status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (int)proc->name.vpid, WEXITSTATUS(status)));
        /* note that this daemon failed */
        proc->state = ORTE_PROC_STATE_ABORTED;
    }
    /* increment the #procs terminated so we will exit properly */
    jdata->num_terminated++;

    /* check for job completion */
    orte_plm_base_check_job_completed(jdata);
    
    /* release any waiting threads */
    OPAL_THREAD_LOCK(&mca_plm_rshd_component.lock);

    /* decrement our #children */
    mca_plm_rshd_component.num_children--;
    
    /* see if we can allow launching to continue */
    if (mca_plm_rshd_component.num_children <=
        mca_plm_rshd_component.num_concurrent ||
        mca_plm_rshd_component.num_children == 0) {
        opal_condition_signal(&mca_plm_rshd_component.cond);
    }
    
    OPAL_THREAD_UNLOCK(&mca_plm_rshd_component.lock);

}


/* actually ssh the child */
static void ssh_child(char *cmd, char **argv)
{
    char** env;
    char* var;
    long fd, fdmax = sysconf(_SC_OPEN_MAX);
    int fdin;
    sigset_t sigs;

    /* setup environment */
    env = opal_argv_copy(orte_launch_environ);
    
    /* Don't let ssh slurp all of our stdin! */
    fdin = open("/dev/null", O_RDWR);
    dup2(fdin, 0);
    close(fdin);
    
    /* close all file descriptors w/ exception of stdin/stdout/stderr */
    for(fd=3; fd<fdmax; fd++)
        close(fd);
    
    /* Set signal handlers back to the default.  Do this close
     to the execve() because the event library may (and likely
     will) reset them.  If we don't do this, the event
     library may have left some set that, at least on some
     OS's, don't get reset via fork() or exec().  Hence, the
     orted could be unkillable (for example). */
    
    set_handler_default(SIGTERM);
    set_handler_default(SIGINT);
    set_handler_default(SIGHUP);
    set_handler_default(SIGPIPE);
    set_handler_default(SIGCHLD);
    
    /* Unblock all signals, for many of the same reasons that
     we set the default handlers, above.  This is noticable
     on Linux where the event library blocks SIGTERM, but we
     don't want that blocked by the orted (or, more
     specifically, we don't want it to be blocked by the
     orted and then inherited by the ORTE processes that it
     forks, making them unkillable by SIGTERM). */
    sigprocmask(0, 0, &sigs);
    sigprocmask(SIG_UNBLOCK, &sigs, 0);
    
    /* exec the cmd */
    var = opal_argv_join(argv, ' ');
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rshd: executing: (%s) [%s]",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         cmd, (NULL == var) ? "NULL" : var));
    if (NULL != var) free(var);
    
    execve(cmd, argv, env);
    opal_output(0, "plm:rshd: execv of %s failed with errno=%s(%d)\n",
                cmd, strerror(errno), errno);
    exit(-1);

}

/**
 * Directly launch each specified process.
 */

/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
int orte_plm_rshd_launch(orte_job_t *jdata)
{
    orte_job_map_t *map = NULL;
    char **argv = NULL;
    char *cmd, *param;
    int rc, i;
    bool failed_launch = true;
    orte_app_context_t *app;
    orte_node_t *node;
    orte_proc_t *proc;
    orte_jobid_t failed_job = ORTE_JOBID_INVALID;
    orte_job_state_t job_state = ORTE_JOB_NEVER_LAUNCHED;
    pid_t pid;
    
    if (jdata->controls & ORTE_JOB_CONTROL_LOCAL_SLAVE) {
        /* if this is a request to launch a local slave,
         * then we will not be launching an orted - we will
         * directly ssh the slave process itself. No mapping
         * is performed to support this - the caller must
         * provide all the info required to launch the job,
         * including the target hosts
         */
        return orte_plm_base_local_slave_launch(jdata);
    }
    
    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rshd: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* default to declaring the job launch as having failed */
    failed_job = jdata->jobid;
    
    /* launch each proc */
    for (i=0; i < jdata->procs->size; i++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            continue;
        }
        /* only launch this proc if it isn't already running */
        if (ORTE_PROC_STATE_LAUNCHED <= proc->state) {
            continue;
        }
        
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rshd: launching proc %s on node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name), proc->nodename));
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, proc->app_idx))) {
            continue;
        }
        node = (orte_node_t*)proc->node;
        /* setup the launch */
        if (ORTE_SUCCESS != (rc = orte_plm_base_setup_rsh_launch(proc->nodename, app,
                                                                 "orte-bootproxy.sh",
                                                                 &argv, &cmd))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* add the bootproxy cmd line options */
        if (ORTE_SUCCESS != (rc = orte_plm_base_append_bootproxy_args(app, &argv,
                                                                      proc->name.jobid, proc->name.vpid,
                                                                      jdata->map->num_nodes, jdata->num_procs,
                                                                      proc->node_rank, proc->local_rank,
                                                                      node->num_procs, jdata->total_slots_alloc,
                                                                      false))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* final cmd */
        if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
            param = opal_argv_join(argv, ' ');
            opal_output(0, "%s plm:rshd: final cmd:\n\t%s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == param) ? "NULL" : param);
            if (NULL != param) free(param);
        }

        /* fork a child to exec the rsh/ssh session */
        pid = fork();
        if (pid < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            rc = ORTE_ERR_SYS_LIMITS_CHILDREN;
            goto cleanup;
        }
        
        /* child */
        if (pid == 0) {            
            /* do the ssh launch - this will exit if it fails */
            ssh_child(cmd, argv);
        }
        /* father */
        /* declare the child launched */
        proc->state = ORTE_PROC_STATE_LAUNCHED;
        /* track number launched */
        OPAL_THREAD_LOCK(&mca_plm_rshd_component.lock);
        if (mca_plm_rshd_component.num_children++ >=
            mca_plm_rshd_component.num_concurrent) {
            opal_condition_wait(&mca_plm_rshd_component.cond, &mca_plm_rshd_component.lock);
        }
        OPAL_THREAD_UNLOCK(&mca_plm_rshd_component.lock);
        
        /* cleanup */
        opal_argv_free(argv);
        argv = NULL;
        free(cmd);
        cmd = NULL;
        
        /* setup callback on sigchild - wait until setup above is complete
         * as the callback can occur in the call to orte_wait_cb
         */
        orte_wait_cb(pid, wait_cb, (void*)proc);
    }

    /* flag the launch as successful */
    failed_launch = false;
    if (jdata->state < ORTE_JOB_STATE_UNTERMINATED) {
        jdata->state = ORTE_JOB_STATE_LAUNCHED;
    }
    
cleanup:
    if (NULL != argv) {
        opal_argv_free(argv);
    }
    if (NULL != cmd) {
        free(cmd);
    }
    
    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_plm_base_launch_failed(failed_job, -1, ORTE_ERROR_DEFAULT_EXIT_CODE, job_state);
    }

    /* setup a "heartbeat" timer to periodically check on
     * the state-of-health of the orteds, if requested AND
     * we actually launched some daemons!
     */
    if ((NULL != map) && (0 < map->num_new_daemons)) {
        orte_plm_base_start_heart();
    }
    
    return rc;
}


/**
 * Terminate all processes for a given job
 */
int orte_plm_rshd_terminate_job(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

/**
 * No orteds to terminate
 */
int orte_plm_rshd_terminate_orteds(void)
{    
    orte_trigger_event(&orteds_exit);
    return ORTE_SUCCESS;
}

int orte_plm_rshd_signal_job(orte_jobid_t jobid, int32_t signal)
{
    /* no way to do this */
    return ORTE_SUCCESS;
}

int orte_plm_rshd_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/**
 * Handle threading issues.
 */

#if OPAL_HAVE_POSIX_THREADS && OPAL_THREADS_HAVE_DIFFERENT_PIDS && OPAL_ENABLE_PROGRESS_THREADS

struct orte_plm_rshd_stack_t {
    opal_condition_t cond;
    opal_mutex_t mutex;
    bool complete;
    orte_jobid_t jobid;
    int rc;
};
typedef struct orte_plm_rshd_stack_t orte_plm_rshd_stack_t;

static void orte_plm_rshd_stack_construct(orte_plm_rshd_stack_t* stack)
{
    OBJ_CONSTRUCT(&stack->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&stack->cond, opal_condition_t);
    stack->rc = 0;
    stack->complete = false;
}

static void orte_plm_rshd_stack_destruct(orte_plm_rshd_stack_t* stack)
{
    OBJ_DESTRUCT(&stack->mutex);
    OBJ_DESTRUCT(&stack->cond);
}

static OBJ_CLASS_INSTANCE(
    orte_plm_rshd_stack_t,
    opal_object_t,
    orte_plm_rshd_stack_construct,
    orte_plm_rshd_stack_destruct);

static void orte_plm_rshd_launch_cb(int fd, short event, void* args)
{
    orte_plm_rshd_stack_t *stack = (orte_plm_rshd_stack_t*)args;
    OPAL_THREAD_LOCK(&stack->mutex);
    stack->rc = orte_plm_rshd_launch(stack->jobid);
    stack->complete = true;
    opal_condition_signal(&stack->cond);
    OPAL_THREAD_UNLOCK(&stack->mutex);
}

static int orte_plm_rshd_launch_threaded(orte_jobid_t jobid)
{
    struct timeval tv = { 0, 0 };
    struct opal_event event;
    struct orte_plm_rshd_stack_t stack;

    OBJ_CONSTRUCT(&stack, orte_plm_rshd_stack_t);

    stack.jobid = jobid;
    if( opal_event_progress_thread() ) {
        stack.rc = orte_plm_rshd_launch( jobid );
    } else {
        opal_evtimer_set(&event, orte_plm_rshd_launch_cb, &stack);
        opal_evtimer_add(&event, &tv);

        OPAL_THREAD_LOCK(&stack.mutex);
        while (stack.complete == false) {
            opal_condition_wait(&stack.cond, &stack.mutex);
        }
        OPAL_THREAD_UNLOCK(&stack.mutex);
    }
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



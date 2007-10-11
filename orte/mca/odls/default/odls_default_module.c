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
#include "orte/mca/routed/routed.h"

#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/default/odls_default.h"

/*
 * External Interface
 */
static int orte_odls_default_launch_local_procs(orte_gpr_notify_data_t *data);
static int orte_odls_default_kill_local_procs(orte_jobid_t job, bool set_state);
static int orte_odls_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal);

static void set_handler_default(int sig);

orte_odls_base_module_t orte_odls_default_module = {
    orte_odls_base_default_get_add_procs_data,
    orte_odls_default_launch_local_procs,
    orte_odls_default_kill_local_procs,
    orte_odls_default_signal_local_procs,
    orte_odls_base_default_deliver_message,
    orte_odls_base_default_extract_proc_map_info,
    orte_odls_base_default_require_sync
};

static bool odls_default_child_died(pid_t pid, unsigned int timeout, int *exit_status)
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

static int odls_default_kill_local(pid_t pid, int signum)
{
    if (0 != kill(pid, signum)) {
        if (ESRCH != errno) return errno;
    }
    return 0;
}

int orte_odls_default_kill_local_procs(orte_jobid_t job, bool set_state)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_kill_local_procs(job, set_state,
                                    odls_default_kill_local, odls_default_child_died))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}

/**
 *  Fork/exec the specified processes
 */

static int odls_default_fork_local_proc(
    orte_app_context_t* context,
    orte_odls_child_t *child,
    char **environ_copy)
{
    pid_t pid;
    orte_iof_base_io_conf_t opts;
    int rc;
    sigset_t sigs;
    int i, p[2];

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
        long fd, fdmax = sysconf(_SC_OPEN_MAX);

        /* Setup the pipe to be close-on-exec */
        close(p[0]);
        fcntl(p[1], F_SETFD, FD_CLOEXEC);

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
    orte_std_cntr_t total_slots_alloc, num_local_procs;
    orte_jobid_t job;
    orte_vpid_t range;
    opal_list_item_t *item;
    bool node_included;
    bool override_oversubscribed;
    bool oversubscribed;
    opal_list_t app_context_list;

    /* We need to create a list of the app_contexts
     * so we can know what to launch - the process info only gives
     * us an index into the app_context array, not the app_context
     * info itself.
     */
    
    OBJ_CONSTRUCT(&app_context_list, opal_list_t);
    
    /* construct the list of children we are to launch */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_construct_child_list(data, &job,
                                                                          &num_local_procs,
                                                                          &range,
                                                                          &total_slots_alloc,
                                                                          &node_included,
                                                                          &oversubscribed,
                                                                          &override_oversubscribed,
                                                                          &app_context_list))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* if there is nothing for us to do, just return */
    if (!node_included) {
        rc = ORTE_SUCCESS;
        goto CLEANUP;
    }

    /* launch the local procs */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_launch_local(job, &app_context_list,
                                                                  num_local_procs,
                                                                  range, total_slots_alloc,
                                                                  oversubscribed,
                                                                  override_oversubscribed,
                                                                  odls_default_fork_local_proc))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
CLEANUP:
    /* cleanup */
    while (NULL != (item = opal_list_remove_first(&app_context_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&app_context_list);
    
    return rc;
}


static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}

static int send_signal(pid_t pid, int signal)
{
    int rc = ORTE_SUCCESS;
    
    OPAL_OUTPUT_VERBOSE((1, orte_odls_globals.output,
                         "%s sending signal %d to pid %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         signal, (long)pid));
    
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

static int orte_odls_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_signal_local_procs(proc, signal, send_signal))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}

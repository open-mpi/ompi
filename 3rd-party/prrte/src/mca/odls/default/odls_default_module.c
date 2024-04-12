/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 * Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Rutgers, The State University of New Jersey.
 *                         All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * There is a complicated sequence of events that occurs when the
 * parent forks a child process that is intended to launch the target
 * executable.
 *
 * Before the child process exec's the target executable, it might tri
 * to set the affinity of that new child process according to a
 * complex series of rules.  This binding may fail in a myriad of
 * different ways.  A lot of this code deals with reporting that error
 * occurately to the end user.  This is a complex task in itself
 * because the child process is not "really" an PRTE process -- all
 * error reporting must be proxied up to the parent who can use normal
 * PRTE error reporting mechanisms.
 *
 * Here's a high-level description of what is occurring in this file:
 *
 * - parent opens a pipe
 * - parent forks a child
 * - parent blocks reading on the pipe: the pipe will either close
 *   (indicating that the child successfully exec'ed) or the child will
 *   write some proxied error data up the pipe
 *
 * - the child tries to set affinity and do other housekeeping in
 *   preparation of exec'ing the target executable
 * - if the child fails anywhere along the way, it sends a message up
 *   the pipe to the parent indicating what happened -- including a
 *   rendered error message detailing the problem (i.e., human-readable).
 * - it is important that the child renders the error message: there
 *   are so many errors that are possible that the child is really the
 *   only entity that has enough information to make an accuate error string
 *   to report back to the user.
 * - the parent reads this message + rendered string in and uses PRTE
 *   reporting mechanisms to display it to the user
 * - if the problem was only a warning, the child continues processing
 *   (potentially eventually exec'ing the target executable).
 * - if the problem was an error, the child exits and the parent
 *   handles the death of the child as appropriate (i.e., this ODLS
 *   simply reports the error -- other things decide what to do).
 */

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif
#include <signal.h>
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#include <stdlib.h>
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#include <stdarg.h>
#ifdef HAVE_SYS_SELECT_H
#    include <sys/select.h>
#endif
#ifdef HAVE_DIRENT_H
#    include <dirent.h>
#endif
#include <ctype.h>
#ifdef HAVE_SYS_PTRACE_H
#    include <sys/ptrace.h>
#endif

#include "src/class/pmix_pointer_array.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_fd.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_show_help.h"
#include "src/util/sys_limits.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/mca/iof/base/iof_base_setup.h"
#include "src/mca/plm/plm.h"
#include "src/mca/rtc/rtc.h"
#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_wait.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/odls/base/base.h"
#include "src/mca/odls/default/odls_default.h"
#include "src/prted/pmix/pmix_server.h"

/*
 * Module functions (function pointers used in a struct)
 */
static int prte_odls_default_launch_local_procs(pmix_data_buffer_t *data);
static int prte_odls_default_kill_local_procs(pmix_pointer_array_t *procs);
static int prte_odls_default_signal_local_procs(const pmix_proc_t *proc, int32_t signal);
static int prte_odls_default_restart_proc(prte_proc_t *child);

/*
 * Explicitly declared functions so that we can get the noreturn
 * attribute registered with the compiler.
 */
static void send_error_show_help(int fd, int exit_status, const char *file, const char *topic,
                                 ...) __prte_attribute_noreturn__;

static void do_child(prte_odls_spawn_caddy_t *cd, int write_fd) __prte_attribute_noreturn__;

/*
 * Module
 */
prte_odls_base_module_t prte_odls_default_module
    = {.get_add_procs_data = prte_odls_base_default_get_add_procs_data,
       .launch_local_procs = prte_odls_default_launch_local_procs,
       .kill_local_procs = prte_odls_default_kill_local_procs,
       .signal_local_procs = prte_odls_default_signal_local_procs,
       .restart_proc = prte_odls_default_restart_proc};

/* deliver a signal to a specified pid. */
static int odls_default_kill_local(pid_t pid, int signum)
{
    pid_t pgrp;

#if HAVE_SETPGID
    pgrp = getpgid(pid);
    if (-1 != pgrp) {
        /* target the lead process of the process
         * group so we ensure that the signal is
         * seen by all members of that group. This
         * ensures that the signal is seen by any
         * child processes our child may have
         * started
         */
        pid = -pgrp;
    }
#endif

    if (0 != kill(pid, signum)) {
        if (ESRCH != errno) {
            PMIX_OUTPUT_VERBOSE((2, prte_odls_base_framework.framework_output,
                                 "%s odls:default:SENT KILL %d TO PID %d GOT ERRNO %d",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), signum, (int) pid, errno));
            return errno;
        }
    }
    PMIX_OUTPUT_VERBOSE((2, prte_odls_base_framework.framework_output,
                         "%s odls:default:SENT KILL %d TO PID %d SUCCESS",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), signum, (int) pid));
    return 0;
}

int prte_odls_default_kill_local_procs(pmix_pointer_array_t *procs)
{
    int rc;

    if (PRTE_SUCCESS
        != (rc = prte_odls_base_default_kill_local_procs(procs, odls_default_kill_local))) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }
    return PRTE_SUCCESS;
}

static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *) 0);
}

/*
 * Internal function to write a rendered show_help message back up the
 * pipe to the waiting parent.
 */
static int write_help_msg(int fd, prte_odls_pipe_err_msg_t *msg, const char *file,
                          const char *topic, va_list ap)
{
    int ret;
    char *str;

    if (NULL == file || NULL == topic) {
        return PRTE_ERR_BAD_PARAM;
    }

    str = pmix_show_help_vstring(file, topic, true, ap);

    msg->file_str_len = (int) strlen(file);
    if (msg->file_str_len > PRTE_ODLS_MAX_FILE_LEN) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }
    msg->topic_str_len = (int) strlen(topic);
    if (msg->topic_str_len > PRTE_ODLS_MAX_TOPIC_LEN) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }
    msg->msg_str_len = (int) strlen(str);

    /* Only keep writing if each write() succeeds */
    if (PRTE_SUCCESS != (ret = pmix_fd_write(fd, sizeof(*msg), msg))) {
        goto out;
    }
    if (msg->file_str_len > 0
        && PRTE_SUCCESS != (ret = pmix_fd_write(fd, msg->file_str_len, file))) {
        goto out;
    }
    if (msg->topic_str_len > 0
        && PRTE_SUCCESS != (ret = pmix_fd_write(fd, msg->topic_str_len, topic))) {
        goto out;
    }
    if (msg->msg_str_len > 0 && PRTE_SUCCESS != (ret = pmix_fd_write(fd, msg->msg_str_len, str))) {
        goto out;
    }

out:
    free(str);
    return ret;
}

/* Called from the child to send an error message up the pipe to the
   waiting parent. */
static void send_error_show_help(int fd, int exit_status, const char *file, const char *topic, ...)
{
    va_list ap;
    prte_odls_pipe_err_msg_t msg;

    msg.fatal = true;
    msg.exit_status = exit_status;

    /* Send it */
    va_start(ap, topic);
    write_help_msg(fd, &msg, file, topic, ap);
    va_end(ap);

    _exit(exit_status);
}

static void do_child(prte_odls_spawn_caddy_t *cd, int write_fd)
{
    int i;
    sigset_t sigs;
    char dir[MAXPATHLEN];

#if HAVE_SETPGID
    /* Set a new process group for this child, so that any
     * signals we send to it will reach any children it spawns */
    setpgid(0, 0);
#endif

    /* Setup the pipe to be close-on-exec */
    i = pmix_fd_set_cloexec(write_fd);
    if (0 != i) {
        PRTE_ERROR_LOG(i);
        send_error_show_help(write_fd, 1, "help-prte-odls-default.txt", "iof setup failed",
                             prte_process_info.nodename, cd->app->app);
        /* Does not return */
    }

    if (NULL != cd->child) {
        /* setup stdout/stderr so that any error messages that we
           may print out will get displayed back at prun.

           NOTE: Definitely do this AFTER we check contexts so
           that any error message from those two functions doesn't
           come out to the user. IF we didn't do it in this order,
           THEN a user who gives us a bad executable name or
           working directory would get N error messages, where
           N=num_procs. This would be very annoying for large
           jobs, so instead we set things up so that prun
           always outputs a nice, single message indicating what
           happened
        */
        if (PRTE_FLAG_TEST(cd->jdata, PRTE_JOB_FLAG_FORWARD_OUTPUT)) {
            if (PRTE_SUCCESS != (i = prte_iof_base_setup_child(&cd->opts, &cd->env))) {
                PRTE_ERROR_LOG(i);
                send_error_show_help(write_fd, 1, "help-prte-odls-default.txt", "iof setup failed",
                                     prte_process_info.nodename, cd->app->app);
                /* Does not return */
            }
        }

        /* now set any child-level controls such as binding */
        prte_rtc.set(cd, write_fd);

    } else if (!PRTE_FLAG_TEST(cd->jdata, PRTE_JOB_FLAG_FORWARD_OUTPUT)) {
        /* tie stdin/out/err/internal to /dev/null */
        int fdnull;
        for (i = 0; i < 3; i++) {
            fdnull = open("/dev/null", O_RDONLY, 0);
            if (fdnull > i && i != write_fd) {
                dup2(fdnull, i);
            }
            close(fdnull);
        }
    }

    /* close all open file descriptors w/ exception of stdin/stdout/stderr,
       the pipe used for the IOF INTERNAL messages, and the pipe up to
       the parent. */
    pmix_close_open_file_descriptors(write_fd);

    if (cd->argv == NULL) {
        cd->argv = malloc(sizeof(char *) * 2);
        cd->argv[0] = strdup(cd->app->app);
        cd->argv[1] = NULL;
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
    set_handler_default(SIGTRAP);

    /* Unblock all signals, for many of the same reasons that we
       set the default handlers, above.  This is noticable on
       Linux where the event library blocks SIGTERM, but we don't
       want that blocked by the launched process. */
    sigprocmask(0, 0, &sigs);
    sigprocmask(SIG_UNBLOCK, &sigs, 0);

    /* take us to the correct wdir */
    if (NULL != cd->wdir) {
        if (0 != chdir(cd->wdir)) {
            send_error_show_help(write_fd, 1, "help-prun.txt", "prun:wdir-not-found", "prted",
                                 cd->wdir, prte_process_info.nodename,
                                 (NULL == cd->child) ? 0 : cd->child->app_rank);
            /* Does not return */
        }
    }

#if PRTE_HAVE_STOP_ON_EXEC
    {
        if (prte_get_attribute(&cd->jdata->attributes, PRTE_JOB_STOP_ON_EXEC, NULL, PMIX_BOOL)) {
            errno = 0;
            i = ptrace(PRTE_TRACEME, 0, 0, 0);
            if (0 != errno) {
                send_error_show_help(write_fd, 1, "help-prun.txt", "prun:stop-on-exec", "prted",
                                     strerror(errno), prte_process_info.nodename,
                                     (NULL == cd->child) ? 0 : cd->child->app_rank);
            }
        }
    }
#endif

    /* Exec the new executable */
    execve(cd->cmd, cd->argv, cd->env);
    /* If we get here, an error has occurred. */
    (void) getcwd(dir, sizeof(dir));
    struct stat stats;
    char *msg;
    /* If errno is ENOENT, that indicates either cd->cmd does not exist, or
     * cd->cmd is a script, but has a bad interpreter specified. */
    if (ENOENT == errno && 0 == stat(cd->app->app, &stats)) {
        asprintf(&msg, "%s has a bad interpreter on the first line.", cd->app->app);
    } else {
        msg = strdup(strerror(errno));
    }
    send_error_show_help(write_fd, 1, "help-prte-odls-default.txt", "execve error",
                         prte_process_info.nodename, dir, cd->app->app, msg);
    // does not return
}

static int do_parent(prte_odls_spawn_caddy_t *cd, int read_fd)
{
    int rc, status;
    prte_odls_pipe_err_msg_t msg;
    char file[PRTE_ODLS_MAX_FILE_LEN + 1], topic[PRTE_ODLS_MAX_TOPIC_LEN + 1], *str = NULL;

    if (cd->opts.connect_stdin) {
        close(cd->opts.p_stdin[0]);
    }
    close(cd->opts.p_stdout[1]);
    close(cd->opts.p_stderr[1]);

#if PRTE_HAVE_STOP_ON_EXEC
    if (NULL != cd->child) {
        if (prte_get_attribute(&cd->jdata->attributes, PRTE_JOB_STOP_ON_EXEC, NULL, PMIX_BOOL)) {
            rc = waitpid(cd->child->pid, &status, WUNTRACED);
            if (-1 == rc) {
                /* doomed */
                cd->child->state = PRTE_PROC_STATE_FAILED_TO_START;
                PRTE_FLAG_UNSET(cd->child, PRTE_PROC_FLAG_ALIVE);
                close(read_fd);
                return PRTE_ERR_FAILED_TO_START;
            }
            /* tell the child to stop */
            if (WIFSTOPPED(status)) {
                rc = kill(cd->child->pid, SIGSTOP);
                if (-1 == rc) {
                    /* doomed */
                    cd->child->state = PRTE_PROC_STATE_FAILED_TO_START;
                    PRTE_FLAG_UNSET(cd->child, PRTE_PROC_FLAG_ALIVE);
                    close(read_fd);
                    return PRTE_ERR_FAILED_TO_START;
                }
                errno = 0;
#    if PRTE_HAVE_LINUX_PTRACE
                ptrace(PRTE_DETACH, cd->child->pid, 0, (void *) SIGSTOP);
#    else
                ptrace(PRTE_DETACH, cd->child->pid, 0, SIGSTOP);
#    endif
                if (0 != errno) {
                    /* couldn't detach */
                    cd->child->state = PRTE_PROC_STATE_FAILED_TO_START;
                    PRTE_FLAG_UNSET(cd->child, PRTE_PROC_FLAG_ALIVE);
                    close(read_fd);
                    return PRTE_ERR_FAILED_TO_START;
                }
                /* record that this proc is ready for debug */
                PRTE_ACTIVATE_PROC_STATE(&cd->child->name, PRTE_PROC_STATE_READY_FOR_DEBUG);
            }
        }
        cd->child->state = PRTE_PROC_STATE_RUNNING;
        PRTE_FLAG_SET(cd->child, PRTE_PROC_FLAG_ALIVE);
        close(read_fd);
        return PRTE_SUCCESS;
    }
#endif

    /* Block reading a message from the pipe */
    while (1) {
        rc = pmix_fd_read(read_fd, sizeof(msg), &msg);

        /* If the pipe closed, then the child successfully launched */
        if (PMIX_ERR_TIMEOUT == rc) {
            break;
        }

        /* If Something Bad happened in the read, error out */
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            close(read_fd);

            if (NULL != cd->child) {
                cd->child->state = PRTE_PROC_STATE_UNDEF;
            }
            rc = prte_pmix_convert_status(rc);
            return rc;
        }

        /* Otherwise, we got a warning or error message from the child */
        if (NULL != cd->child) {
            if (msg.fatal) {
                PRTE_FLAG_UNSET(cd->child, PRTE_PROC_FLAG_ALIVE);
            } else {
                PRTE_FLAG_SET(cd->child, PRTE_PROC_FLAG_ALIVE);
            }
        }

        /* Read in the strings; ensure to terminate them with \0 */
        if (msg.file_str_len > 0) {
            rc = pmix_fd_read(read_fd, msg.file_str_len, file);
            if (PMIX_SUCCESS != rc) {
                pmix_show_help("help-prte-odls-default.txt", "syscall fail", true,
                               prte_process_info.nodename, cd->app->app, "pmix_fd_read", __FILE__,
                               __LINE__);
                if (NULL != cd->child) {
                    cd->child->state = PRTE_PROC_STATE_UNDEF;
                }
                rc = prte_pmix_convert_status(rc);
                return rc;
            }
            file[msg.file_str_len] = '\0';
        }
        if (msg.topic_str_len > 0) {
            rc = pmix_fd_read(read_fd, msg.topic_str_len, topic);
            if (PMIX_SUCCESS != rc) {
                pmix_show_help("help-prte-odls-default.txt", "syscall fail", true,
                               prte_process_info.nodename, cd->app->app, "pmix_fd_read", __FILE__,
                               __LINE__);
                if (NULL != cd->child) {
                    cd->child->state = PRTE_PROC_STATE_UNDEF;
                }
                rc = prte_pmix_convert_status(rc);
                return rc;
            }
            topic[msg.topic_str_len] = '\0';
        }
        if (msg.msg_str_len > 0) {
            str = calloc(1, msg.msg_str_len + 1);
            if (NULL == str) {
                pmix_show_help("help-prte-odls-default.txt", "syscall fail", true,
                               prte_process_info.nodename, cd->app->app, "pmix_fd_read", __FILE__,
                               __LINE__);
                if (NULL != cd->child) {
                    cd->child->state = PRTE_PROC_STATE_UNDEF;
                }
                rc = prte_pmix_convert_status(rc);
                return rc;
            }
            rc = pmix_fd_read(read_fd, msg.msg_str_len, str);
        }

        /* Print out what we got.  We already have a rendered string,
           so use pmix_show_help_norender(). */
        if (msg.msg_str_len > 0) {
            pmix_show_help_norender(file, topic, str);
            free(str);
            str = NULL;
        }

        /* If msg.fatal is true, then the child exited with an error.
           Otherwise, whatever we just printed was a warning, so loop
           around and see what else is on the pipe (or if the pipe
           closed, indicating that the child launched
           successfully). */
        if (msg.fatal) {
            if (NULL != cd->child) {
                cd->child->state = PRTE_PROC_STATE_FAILED_TO_START;
                PRTE_FLAG_UNSET(cd->child, PRTE_PROC_FLAG_ALIVE);
            }
            close(read_fd);
            return PRTE_ERR_FAILED_TO_START;
        }
    }

    /* If we got here, it means that the pipe closed without
       indication of a fatal error, meaning that the child process
       launched successfully. */
    if (NULL != cd->child) {
        cd->child->state = PRTE_PROC_STATE_RUNNING;
        PRTE_FLAG_SET(cd->child, PRTE_PROC_FLAG_ALIVE);
    }
    close(read_fd);

    return PRTE_SUCCESS;
}

/**
 *  Fork/exec the specified processes
 */
static int odls_default_fork_local_proc(void *cdptr)
{
    prte_odls_spawn_caddy_t *cd = (prte_odls_spawn_caddy_t *) cdptr;
    int p[2];
    pid_t pid;
    prte_proc_t *child = cd->child;

    /* A pipe is used to communicate between the parent and child to
       indicate whether the exec ultimately succeeded or failed.  The
       child sets the pipe to be close-on-exec; the child only ever
       writes anything to the pipe if there is an error (e.g.,
       executable not found, exec() fails, etc.).  The parent does a
       blocking read on the pipe; if the pipe closed with no data,
       then the exec() succeeded.  If the parent reads something from
       the pipe, then the child was letting us know why it failed. */
    if (pipe(p) < 0) {
        PRTE_ERROR_LOG(PMIX_ERR_SYS_LIMITS_PIPES);
        if (NULL != child) {
            child->state = PRTE_PROC_STATE_FAILED_TO_START;
            child->exit_code = PMIX_ERR_SYS_LIMITS_PIPES;
        }
        return PMIX_ERR_SYS_LIMITS_PIPES;
    }

    /* Fork off the child */
    pid = fork();
    if (NULL != child) {
        child->pid = pid;
    }

    if (pid < 0) {
        PRTE_ERROR_LOG(PMIX_ERR_SYS_LIMITS_CHILDREN);
        if (NULL != child) {
            child->state = PRTE_PROC_STATE_FAILED_TO_START;
            child->exit_code = PMIX_ERR_SYS_LIMITS_CHILDREN;
        }
        return PMIX_ERR_SYS_LIMITS_CHILDREN;
    }

    if (pid == 0) {
        close(p[0]);
        do_child(cd, p[1]);
        /* Does not return */
    }

    close(p[1]);
    return do_parent(cd, p[0]);
}

/**
 * Launch all processes allocated to the current node.
 */

int prte_odls_default_launch_local_procs(pmix_data_buffer_t *data)
{
    int rc;
    pmix_nspace_t job;

    /* construct the list of children we are to launch */
    rc = prte_odls_base_default_construct_child_list(data, &job);
    if (PRTE_SUCCESS != rc) {
        PMIX_OUTPUT_VERBOSE((2, prte_odls_base_framework.framework_output,
                             "%s odls:default:launch:local failed to construct child list on error %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_ERROR_NAME(rc)));
        return rc;
    }

    /* launch the local procs */
    PRTE_ACTIVATE_LOCAL_LAUNCH(job, odls_default_fork_local_proc);

    return PRTE_SUCCESS;
}

/**
 * Send a signal to a pid.  Note that if we get an error, we set the
 * return value and let the upper layer print out the message.
 */
static int send_signal(pid_t pd, int signal)
{
    int rc = PRTE_SUCCESS;
    pid_t pid;

    if (prte_odls_globals.signal_direct_children_only) {
        pid = pd;
    } else {
#if HAVE_SETPGID
        /* send to the process group so that any children of our children
         * also receive the signal*/
        pid = -pd;
#else
        pid = pd;
#endif
    }

    PMIX_OUTPUT_VERBOSE((1, prte_odls_base_framework.framework_output,
                         "%s sending signal %d to pid %ld", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         signal, (long) pid));

    if (kill(pid, signal) != 0) {
        switch (errno) {
        case EINVAL:
            rc = PRTE_ERR_BAD_PARAM;
            break;
        case ESRCH:
            /* This case can occur when we deliver a signal to a
               process that is no longer there.  This can happen if
               we deliver a signal while the job is shutting down.
               This does not indicate a real problem, so just
               ignore the error.  */
            break;
        case EPERM:
            rc = PRTE_ERR_PERM;
            break;
        default:
            rc = PRTE_ERROR;
        }
    }

    return rc;
}

static int prte_odls_default_signal_local_procs(const pmix_proc_t *proc, int32_t signal)
{
    int rc;

    rc = prte_odls_base_default_signal_local_procs(proc, signal, send_signal);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }
    return PRTE_SUCCESS;
}

static int prte_odls_default_restart_proc(prte_proc_t *child)
{
    int rc;

    /* restart the local proc */
    rc = prte_odls_base_default_restart_proc(child, odls_default_fork_local_proc);
    if (PRTE_SUCCESS != rc) {
        PMIX_OUTPUT_VERBOSE((2, prte_odls_base_framework.framework_output,
                             "%s odls:default:restart_proc failed to launch on error %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_ERROR_NAME(rc)));
    }
    return rc;
}

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
 * Copyright (c) 2008-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Rutgers, The State University of New Jersey.
 *                         All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
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
 * because the child process is not "really" an PMIX process -- all
 * error reporting must be proxied up to the parent who can use normal
 * PMIX error reporting mechanisms.
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
 * - the parent reads this message + rendered string in and uses PMIX
 *   reporting mechanisms to display it to the user
 * - if the problem was only a warning, the child continues processing
 *   (potentially eventually exec'ing the target executable).
 * - if the problem was an error, the child exits and the parent
 *   handles the death of the child as appropriate (i.e., this PFEXEC
 *   simply reports the error -- other things decide what to do).
 */

#include "pmix_config.h"
#include "pmix.h"
#include "src/include/types.h"

#include <string.h>
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
#include <stdlib.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#include <stdarg.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#include <ctype.h>

#include "src/hwloc/hwloc-internal.h"
#include "src/class/pmix_pointer_array.h"
#include "src/util/pmix_environ.h"
#include "src/util/show_help.h"
#include "src/util/fd.h"
#include "src/util/error.h"

#include "src/include/pmix_globals.h"
#include "src/util/name_fns.h"
#include "src/threads/threads.h"

#include "src/mca/pfexec/base/base.h"
#include "src/mca/pfexec/linux/pfexec_linux.h"

/*
 * Module functions (function pointers used in a struct)
 */
static pmix_status_t spawn_proc(const pmix_info_t job_info[], size_t ninfo,
                                const pmix_app_t apps[], size_t napps);
static pmix_status_t kill_proc(pmix_rank_t rank);
static pmix_status_t signal_proc(pmix_rank_t rank, int32_t signal);

/*
 * Explicitly declared functions so that we can get the noreturn
 * attribute registered with the compiler.
 */
static void send_error_show_help(int fd, int exit_status,
                                 const char *file, const char *topic, ...)
    __pmix_attribute_noreturn__;

static void do_child(pmix_app_t *cd, char **env, pmix_pfexec_child_t *child, int write_fd)
    __pmix_attribute_noreturn__;


/*
 * Module
 */
pmix_pfexec_base_module_t pmix_pfexec_linux_module = {
    .spawn_proc = spawn_proc,
    .kill_proc = kill_proc,
    .signal_proc = signal_proc,
};


/* deliver a signal to a specified pid. */
static pmix_status_t sigproc(pid_t pd, int signum)
{
    pid_t pgrp;
    pid_t pid;

    pid = pd;

#if HAVE_SETPGID
    pgrp = getpgid(pd);
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
            PMIX_OUTPUT_VERBOSE((2, pmix_pfexec_base_framework.framework_output,
                                 "%s pfexec:linux:SENT SIGNAL %d TO PID %d GOT ERRNO %d",
                                 PMIX_NAME_PRINT(&pmix_globals.myid), signum, (int)pid, errno));
            return errno;
        }
    }
    PMIX_OUTPUT_VERBOSE((2, pmix_pfexec_base_framework.framework_output,
                         "%s pfexec:linux:SENT SIGNAL %d TO PID %d SUCCESS",
                         PMIX_NAME_PRINT(&pmix_globals.myid), signum, (int)pid));
    return 0;
}

static pmix_status_t kill_proc(pmix_rank_t rank)
{
    pmix_status_t rc;
    pmix_lock_t mylock;
    pmix_pfexec_signal_caddy_t *kcd;

    PMIX_CONSTRUCT_LOCK(&mylock);
    PMIX_PFEXEC_KILL(kcd, rank, sigproc, &mylock);
    PMIX_WAIT_THREAD(&mylock);
    rc = mylock.status;
    PMIX_DESTRUCT_LOCK(&mylock);
    PMIX_RELEASE(kcd);

    return rc;
}

static pmix_status_t signal_proc(pmix_rank_t rank, int32_t signal)
{
    pmix_status_t rc;
    pmix_lock_t mylock;
    pmix_pfexec_signal_caddy_t *scd;

    PMIX_CONSTRUCT_LOCK(&mylock);
    PMIX_PFEXEC_SIGNAL(scd, rank, signal, sigproc, &mylock);
    PMIX_WAIT_THREAD(&mylock);
    rc = mylock.status;
    PMIX_DESTRUCT_LOCK(&mylock);
    PMIX_RELEASE(scd);

    return rc;
}

static void set_handler_linux(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}

/*
 * Internal function to write a rendered show_help message back up the
 * pipe to the waiting parent.
 */
static int write_help_msg(int fd, pmix_pfexec_pipe_err_msg_t *msg, const char *file,
                          const char *topic, va_list ap)
{
    int ret;
    char *str;

    if (NULL == file || NULL == topic) {
        return PMIX_ERR_BAD_PARAM;
    }

    str = pmix_show_help_vstring(file, topic, true, ap);

    msg->file_str_len = (int) strlen(file);
    if (msg->file_str_len > PMIX_PFEXEC_MAX_FILE_LEN) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }
    msg->topic_str_len = (int) strlen(topic);
    if (msg->topic_str_len > PMIX_PFEXEC_MAX_TOPIC_LEN) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }
    msg->msg_str_len = (int) strlen(str);

    /* Only keep writing if each write() succeeds */
    if (PMIX_SUCCESS != (ret = pmix_fd_write(fd, sizeof(*msg), msg))) {
        goto out;
    }
    if (msg->file_str_len > 0 &&
        PMIX_SUCCESS != (ret = pmix_fd_write(fd, msg->file_str_len, file))) {
        goto out;
    }
    if (msg->topic_str_len > 0 &&
        PMIX_SUCCESS != (ret = pmix_fd_write(fd, msg->topic_str_len, topic))) {
        goto out;
    }
    if (msg->msg_str_len > 0 &&
        PMIX_SUCCESS != (ret = pmix_fd_write(fd, msg->msg_str_len, str))) {
        goto out;
    }

 out:
    free(str);
    return ret;
}


/* Called from the child to send an error message up the pipe to the
   waiting parent. */
static void send_error_show_help(int fd, int exit_status,
                                 const char *file, const char *topic, ...)
{
    va_list ap;
    pmix_pfexec_pipe_err_msg_t msg;

    msg.fatal = true;
    msg.exit_status = exit_status;

    /* Send it */
    va_start(ap, topic);
    write_help_msg(fd, &msg, file, topic, ap);
    va_end(ap);

    exit(exit_status);
}

/* close all open file descriptors w/ exception of stdin/stdout/stderr
   and the pipe up to the parent. */
static int close_open_file_descriptors(int write_fd) {
    DIR *dir = opendir("/proc/self/fd");
    if (NULL == dir) {
        return PMIX_ERR_FILE_OPEN_FAILURE;
    }
    struct dirent *files;

    /* grab the fd of the opendir above so we don't close in the
     * middle of the scan. */
    int dir_scan_fd = dirfd(dir);
    if(dir_scan_fd < 0 ) {
        return PMIX_ERR_FILE_OPEN_FAILURE;
    }


    while (NULL != (files = readdir(dir))) {
        if (!isdigit(files->d_name[0])) {
            continue;
        }
        int fd = strtol(files->d_name, NULL, 10);
        if (errno == EINVAL || errno == ERANGE) {
            closedir(dir);
            return PMIX_ERR_TYPE_MISMATCH;
        }
        if (fd >=3 &&
            fd != write_fd &&
	        fd != dir_scan_fd) {
            close(fd);
        }
    }
    closedir(dir);
    return PMIX_SUCCESS;
}

static void do_child(pmix_app_t *app, char **env,
                     pmix_pfexec_child_t *child, int write_fd)
{
    int i, errval;
    sigset_t sigs;
    long fd, fdmax = sysconf(_SC_OPEN_MAX);
    char dir[MAXPATHLEN];

#if HAVE_SETPGID
    /* Set a new process group for this child, so that any
     * signals we send to it will reach any children it spawns */
    setpgid(0, 0);
#endif

    /* Setup the pipe to be close-on-exec */
    pmix_fd_set_cloexec(write_fd);

    /* setup stdout/stderr so that any error messages that we
       may print out will get displayed back at pmixrun.

       NOTE: Definitely do this AFTER we check contexts so
       that any error message from those two functions doesn't
       come out to the user. IF we didn't do it in this order,
       THEN a user who gives us a bad executable name or
       working directory would get N error messages, where
       N=num_procs. This would be very annoying for large
       jobs, so instead we set things up so that pmixrun
       always outputs a nice, single message indicating what
       happened
    */
    if (PMIX_SUCCESS != (i = pmix_pfexec_base_setup_child(child))) {
        PMIX_ERROR_LOG(i);
        send_error_show_help(write_fd, 1,
                             "help-pfexec-linux.txt",
                             "iof setup failed",
                             pmix_globals.hostname, app->cmd);
        /* Does not return */
    }

    /* close all open file descriptors w/ exception of stdin/stdout/stderr,
       the pipe used for the IOF INTERNAL messages, and the pipe up to
       the parent. */
    if (PMIX_SUCCESS != close_open_file_descriptors(write_fd)) {
        // close *all* file descriptors -- slow
        for(fd=3; fd<fdmax; fd++) {
            if (
                fd != write_fd) {
                close(fd);
            }
        }
    }

    /* Set signal handlers back to the default.  Do this close to
       the exev() because the event library may (and likely will)
       reset them.  If we don't do this, the event library may
       have left some set that, at least on some OS's, don't get
       reset via fork() or exec().  Hence, the launched process
       could be unkillable (for example). */

    set_handler_linux(SIGTERM);
    set_handler_linux(SIGINT);
    set_handler_linux(SIGHUP);
    set_handler_linux(SIGPIPE);
    set_handler_linux(SIGCHLD);

    /* Unblock all signals, for many of the same reasons that we
       set the default handlers, above.  This is noticable on
       Linux where the event library blocks SIGTERM, but we don't
       want that blocked by the launched process. */
    sigprocmask(0, 0, &sigs);
    sigprocmask(SIG_UNBLOCK, &sigs, 0);

    /* take us to the correct wdir */
    if (NULL != app->cwd) {
        if (0 != chdir(app->cwd)) {
            send_error_show_help(write_fd, 1,
                                 "help-pfexec-linux.txt",
                                 "wdir-not-found",
                                 "pmixd",
                                 app->cwd,
                                 pmix_globals.hostname);
            /* Does not return */
        }
    }

    /* Exec the new executable */
    execve(app->cmd, app->argv, env);
    errval = errno;
    getcwd(dir, sizeof(dir));
    send_error_show_help(write_fd, 1,
                         "help-pfexec-linux.txt", "execve error",
                         pmix_globals.hostname, dir, app->cmd, strerror(errval));
    /* Does not return */
}


static pmix_status_t do_parent(pmix_app_t *app, pmix_pfexec_child_t *child, int read_fd)
{
    pmix_status_t rc;
    pmix_pfexec_pipe_err_msg_t msg;
    char file[PMIX_PFEXEC_MAX_FILE_LEN + 1], topic[PMIX_PFEXEC_MAX_TOPIC_LEN + 1], *str = NULL;

    if (child->opts.connect_stdin) {
        close(child->opts.p_stdin[0]);
    }
    close(child->opts.p_stdout[1]);
    close(child->opts.p_stderr[1]);

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
            return rc;
        }

        /* Read in the strings; ensure to terminate them with \0 */
        if (msg.file_str_len > 0) {
            rc = pmix_fd_read(read_fd, msg.file_str_len, file);
            if (PMIX_SUCCESS != rc) {
                pmix_show_help("help-pfexec-linux.txt", "syscall fail",
                               true,
                               pmix_globals.hostname, app->cmd,
                               "pmix_fd_read", __FILE__, __LINE__);
                return rc;
            }
            file[msg.file_str_len] = '\0';
        }
        if (msg.topic_str_len > 0) {
            rc = pmix_fd_read(read_fd, msg.topic_str_len, topic);
            if (PMIX_SUCCESS != rc) {
                pmix_show_help("help-pfexec-linux.txt", "syscall fail",
                               true,
                               pmix_globals.hostname, app->cmd,
                               "pmix_fd_read", __FILE__, __LINE__);
                return rc;
            }
            topic[msg.topic_str_len] = '\0';
        }
        if (msg.msg_str_len > 0) {
            str = calloc(1, msg.msg_str_len + 1);
            if (NULL == str) {
                pmix_show_help("help-pfexec-linux.txt", "syscall fail",
                               true,
                               pmix_globals.hostname, app->cmd,
                               "calloc", __FILE__, __LINE__);
                return PMIX_ERR_NOMEM;
            }
            rc = pmix_fd_read(read_fd, msg.msg_str_len, str);
            if (PMIX_SUCCESS != rc) {
                pmix_show_help("help-pfexec-linux.txt", "syscall fail",
                               true,
                               pmix_globals.hostname, app->cmd,
                               "pmix_fd_read", __FILE__, __LINE__);
		free(str);
                return rc;
            }
        }

        /* Print out what we got.  We already have a rendered string,
           so use pmix_show_help_norender(). */
        if (msg.msg_str_len > 0) {
            fprintf(stderr, "%s\n", str);
            free(str);
            str = NULL;
        }

        /* If msg.fatal is true, then the child exited with an error.
           Otherwise, whatever we just printed was a warning, so loop
           around and see what else is on the pipe (or if the pipe
           closed, indicating that the child launched
           successfully). */
        if (msg.fatal) {
            close(read_fd);
	    if (NULL != str) {
                free(str);
            }
            return PMIX_ERR_SYS_OTHER;
        }
        if (NULL != str) {
            free(str);
            str = NULL;
        }
    }

    /* If we got here, it means that the pipe closed without
       indication of a fatal error, meaning that the child process
       launched successfully. */
    close(read_fd);
    return PMIX_SUCCESS;
}


/**
 *  Fork/exec the specified processes
 */
static int fork_proc(pmix_app_t *app, pmix_pfexec_child_t *child, char **env)
{
    int p[2];

    /* A pipe is used to communicate between the parent and child to
       indicate whether the exec ultimately succeeded or failed.  The
       child sets the pipe to be close-on-exec; the child only ever
       writes anything to the pipe if there is an error (e.g.,
       executable not found, exec() fails, etc.).  The parent does a
       blocking read on the pipe; if the pipe closed with no data,
       then the exec() succeeded.  If the parent reads something from
       the pipe, then the child was letting us know why it failed. */
    if (pipe(p) < 0) {
        PMIX_ERROR_LOG(PMIX_ERR_SYS_OTHER);
        return PMIX_ERR_SYS_OTHER;
    }

    /* Fork off the child */
    child->pid = fork();

    if (child->pid < 0) {
        PMIX_ERROR_LOG(PMIX_ERR_SYS_OTHER);
        return PMIX_ERR_SYS_OTHER;
    }

    if (child->pid == 0) {
        close(p[0]);
        do_child(app, env, child, p[1]);
        /* Does not return */
    }

    close(p[1]);
    return do_parent(app, child, p[0]);
}


/**
 * Launch all processes allocated to the current node.
 */

static pmix_status_t spawn_proc(const pmix_info_t job_info[], size_t ninfo,
                                const pmix_app_t apps[], size_t napps)
{
    pmix_status_t rc;
    pmix_lock_t mylock;
    pmix_pfexec_fork_caddy_t *scd;

    pmix_output_verbose(5, pmix_pfexec_base_framework.framework_output,
                        "%s pfexec:linux spawning child job",
                        PMIX_NAME_PRINT(&pmix_globals.myid));

    PMIX_CONSTRUCT_LOCK(&mylock);
    PMIX_PFEXEC_SPAWN(scd, job_info, ninfo, apps, napps, fork_proc, (void*)&mylock);
    PMIX_WAIT_THREAD(&mylock);
    if (PMIX_SUCCESS == mylock.status) {
        mylock.status = PMIX_OPERATION_SUCCEEDED;
    }
    rc = mylock.status;
    PMIX_DESTRUCT_LOCK(&mylock);
    PMIX_RELEASE(scd);

    return rc;
}

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
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
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
 * because the child process is not "really" an ORTE process -- all
 * error reporting must be proxied up to the parent who can use normal
 * ORTE error reporting mechanisms.
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
 * - the parent reads this message + rendered string in and uses ORTE
 *   reporting mechanisms to display it to the user
 * - if the problem was only a warning, the child continues processing
 *   (potentially eventually exec'ing the target executable).
 * - if the problem was an error, the child exits and the parent
 *   handles the death of the child as appropriate (i.e., this ODLS
 *   simply reports the error -- other things decide what to do).
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#include "opal/mca/maffinity/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/util/opal_environ.h"
#include "opal/util/opal_sos.h"
#include "opal/util/show_help.h"
#include "opal/util/fd.h"

#include "orte/util/show_help.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/iof/base/iof_base_setup.h"
#include "orte/mca/plm/plm.h"
#include "orte/util/name_fns.h"

#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/default/odls_default.h"

/*
 * Struct written up the pipe from the child to the parent.
 */
typedef struct {
    /* True if the child has died; false if this is just a warning to
       be printed. */
    bool fatal;
    /* Relevant only if fatal==true */
    int exit_status;

    /* Length of the strings that are written up the pipe after this
       struct */
    int file_str_len;
    int topic_str_len;
    int msg_str_len;
} pipe_err_msg_t;

/* 
 * Max length of strings from the pipe_err_msg_t
 */
#define MAX_FILE_LEN 511
#define MAX_TOPIC_LEN MAX_FILE_LEN

/*
 * Module functions (function pointers used in a struct)
 */
static int orte_odls_default_launch_local_procs(opal_buffer_t *data);
static int orte_odls_default_kill_local_procs(opal_pointer_array_t *procs);
static int orte_odls_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal);
static int orte_odls_default_restart_proc(orte_odls_child_t *child);

/*
 * Explicitly declared functions so that we can get the noreturn
 * attribute registered with the compiler.
 */
static void send_error_show_help(int fd, int exit_status, 
                                 const char *file, const char *topic, ...)
    __opal_attribute_noreturn__;
static int do_child(orte_app_context_t* context,
                    orte_odls_child_t *child,
                    char **environ_copy,
                    orte_odls_job_t *jobdat, int write_fd,
                    orte_iof_base_io_conf_t opts)
    __opal_attribute_noreturn__;


/*
 * Module
 */
orte_odls_base_module_t orte_odls_default_module = {
    orte_odls_base_default_get_add_procs_data,
    orte_odls_default_launch_local_procs,
    orte_odls_default_kill_local_procs,
    orte_odls_default_signal_local_procs,
    orte_odls_base_default_deliver_message,
    orte_odls_base_default_require_sync,
    orte_odls_default_restart_proc
};


static bool odls_default_child_died(orte_odls_child_t *child)
{
    time_t end;
    pid_t ret;
    struct timeval t;
    fd_set bogus;
        
    end = time(NULL) + orte_odls_globals.timeout_before_sigkill;
    do {
        ret = waitpid(child->pid, &child->exit_code, WNOHANG);
        if (child->pid == ret) {
            OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                 "%s odls:default:WAITPID INDICATES PROC %d IS DEAD",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)(child->pid)));
            /* It died -- return success */
            return true;
        } else if (0 == ret) {
            /* with NOHANG specified, if a process has already exited
             * while waitpid was registered, then waitpid returns 0
             * as there is no error - this is a race condition problem
             * that occasionally causes us to incorrectly report a proc
             * as refusing to die. Unfortunately, errno may not be reset
             * by waitpid in this case, so we cannot check it - just assume
             * the proc has indeed died
             */
            OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                 "%s odls:default:WAITPID INDICATES PROC %d HAS ALREADY EXITED",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)(child->pid)));
            return true;
        } else if (-1 == ret && ECHILD == errno) {
            /* The pid no longer exists, so we'll call this "good
               enough for government work" */
            OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                 "%s odls:default:WAITPID INDICATES PID %d NO LONGER EXISTS",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)(child->pid)));
            return true;
        }
        
        /* Bogus delay for 1 usec (sched_yeild() -- even if we have it
           -- changed behavior in 2.6.3x Linux flavors to be
           undesirable. */
        t.tv_sec = 0;
        t.tv_usec = 1;
        FD_ZERO(&bogus);
        FD_SET(0, &bogus);
        select(1, &bogus, NULL, NULL, &t);
    } while (time(NULL) < end);

    /* The child didn't die, so return false */
    return false;
}

static int odls_default_kill_local(pid_t pid, int signum)
{
    if (orte_forward_job_control) {
        pid = -pid;
    }
    if (0 != kill(pid, signum)) {
        if (ESRCH != errno) {
            OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                 "%s odls:default:SENT KILL %d TO PID %d GOT ERRNO %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), signum, (int)pid, errno));
            return errno;
        }
    }
    OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                         "%s odls:default:SENT KILL %d TO PID %d SUCCESS",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), signum, (int)pid));
    return 0;
}

int orte_odls_default_kill_local_procs(opal_pointer_array_t *procs)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_kill_local_procs(procs,
                                    odls_default_kill_local, odls_default_child_died))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
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
 * Internal function to write a rendered show_help message back up the
 * pipe to the waiting parent.
 */
static int write_help_msg(int fd, pipe_err_msg_t *msg, const char *file,
                          const char *topic, va_list ap)
{
    int ret;
    char *str;

    if (NULL == file || NULL == topic) {
        return OPAL_ERR_BAD_PARAM;
    }

    str = opal_show_help_vstring(file, topic, true, ap);

    msg->file_str_len = (int) strlen(file);
    if (msg->file_str_len > MAX_FILE_LEN) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    msg->topic_str_len = (int) strlen(topic);
    if (msg->topic_str_len > MAX_TOPIC_LEN) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    msg->msg_str_len = (int) strlen(str);

    /* Only keep writing if each write() succeeds */
    if (OPAL_SUCCESS != (ret = opal_fd_write(fd, sizeof(*msg), msg))) {
        goto out;
    }
    if (msg->file_str_len > 0 &&
        OPAL_SUCCESS != (ret = opal_fd_write(fd, msg->file_str_len, file))) {
        goto out;
    }
    if (msg->topic_str_len > 0 &&
        OPAL_SUCCESS != (ret = opal_fd_write(fd, msg->topic_str_len, topic))) {
        goto out;
    }
    if (msg->msg_str_len > 0 &&
        OPAL_SUCCESS != (ret = opal_fd_write(fd, msg->msg_str_len, str))) {
        goto out;
    }

 out:
    free(str);
    return ret;
}


/* Called from the child to send a warning show_help message up the
   pipe to the waiting parent. */
static int send_warn_show_help(int fd, const char *file, 
                               const char *topic, ...)
{
    int ret;
    va_list ap;
    pipe_err_msg_t msg;

    msg.fatal = false;
    msg.exit_status = 0; /* ignored */

    /* Send it */
    va_start(ap, topic);
    ret = write_help_msg(fd, &msg, file, topic, ap);
    va_end(ap);

    return ret;
}


/* Called from the child to send an error message up the pipe to the
   waiting parent. */
static void send_error_show_help(int fd, int exit_status,
                                 const char *file, const char *topic, ...)
{
    int ret;
    va_list ap;
    pipe_err_msg_t msg;

    msg.fatal = true;
    msg.exit_status = exit_status;

    /* Send it */
    va_start(ap, topic);
    ret = write_help_msg(fd, &msg, file, topic, ap);
    va_end(ap);

    exit(exit_status);
}


/*
 * Bind the process to a specific slot list
 */
static int bind_to_slot_list(orte_app_context_t* context,
                             orte_odls_child_t *child,
                             orte_odls_job_t *jobdat,
                             bool *bound, int pipe_fd)
{
    int rc;
    opal_paffinity_base_cpu_set_t mask;
    char *msg = NULL;

    *bound = false;

    OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                         "%s odls:default:fork binding child %s to slot_list %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(child->name),
                         child->slot_list));
    if (opal_paffinity_alone) {
        send_error_show_help(pipe_fd, 1, 
                             "help-orte-odls-default.txt",
                             "slot list and paffinity_alone",
                             orte_process_info.nodename, context->app);
        /* Does not return */
    }
    if (orte_report_bindings) {
        opal_output(0, "%s odls:default:fork binding child %s to slot_list %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(child->name), child->slot_list);
    }
    rc = opal_paffinity_base_slot_list_set((long)child->name->vpid, 
                                           child->slot_list, &mask);
    if (ORTE_SUCCESS != rc) {
        if (ORTE_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
            /* OS doesn't support providing topology information */
            send_error_show_help(pipe_fd, 1, "help-orte-odls-default.txt",
                                 "binding not supported",
                                 orte_process_info.nodename, context->app);
            /* Does not return */
        }
        asprintf(&msg, "opal_paffinity_base_slot_list_set() returned \"%s\"",
                 opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
        if (NULL == msg) {
            msg = "opal_paffinity_base_slot_list_set() returned failure";
        }
        send_error_show_help(pipe_fd, 1, "help-orte-odls-default.txt",
                             "binding generic error",
                             orte_process_info.nodename, context->app, msg,
                             __FILE__, __LINE__);
        /* Does not return */
    }

    /* if we didn't wind up bound, then generate a warning unless
       suppressed */
    OPAL_PAFFINITY_PROCESS_IS_BOUND((mask), bound);
    if (!bound && orte_odls_base.warn_if_not_bound) {
        send_warn_show_help(pipe_fd, "help-orte-odls-base.txt",
                            "warn not bound", "slot list"
                            "Request resulted in binding to all available processors",
                            orte_process_info.nodename, context->app,
                            "bind to slot list", child->slot_list);
    }

    return ORTE_SUCCESS;
}


/*
 * This function always prints a message: it may be a warning or an
 * error.
 *
 * If binding is not required for this process, then print a simple
 * warning message and return an error code.  If binding *is*
 * required, then send an error message up the pipe to the parent and
 * exit.
 */
static int bind_failed_msg(const char *msg, orte_mapping_policy_t policy,
                           int return_code_if_warning,
                           int pipe_fd, const char *app_name,
                           const char *filename, int line_num)
{
    /* If binding is not required, then send a warning up the pipe and
       then return an error code. */
    if (ORTE_BINDING_NOT_REQUIRED(policy)) {
        send_warn_show_help(pipe_fd, 
                            "help-orte-odls-default.txt", "not bound",
                            orte_process_info.nodename, app_name, msg,
                            filename, line_num);
        return return_code_if_warning;
    } 

    /* If binding is required, send an error up the pipe (which exits
       -- it doesn't return). */
    send_error_show_help(pipe_fd, 1, "help-orte-odls-default.txt",
                         "binding generic error",
                         orte_process_info.nodename, app_name, msg, 
                         filename, line_num);
    /* Does not return */
}


/*
 * Similar to bind_failed_msg(), but if binding is not required, do
 * not output a message (just return an error code).  If binding is
 * required, handling is the same as for bind_failed_msg().
 */
static int bind_failed(const char *msg, orte_mapping_policy_t policy,
                      int return_code_if_warning,
                      int pipe_fd, const char *app_name,
                      const char *filename, int line_num)
{
    if (ORTE_BINDING_NOT_REQUIRED(policy)) {
        return return_code_if_warning;
    }

    /* This won't return, but use "return" statement here so that the
       compiler won't complain. */
    return bind_failed_msg(msg, policy, 0, pipe_fd, app_name, 
                           filename, line_num);
}

/*
 * Bind the process to a core
 */
static int bind_to_core(orte_app_context_t* context,
                        orte_odls_child_t *child,
                        orte_odls_job_t *jobdat,
                        bool *bound, int pipe_fd)
{
    bool flag;
    int i, rc;
    char *tmp, *msg;
    int16_t n;
    orte_node_rank_t nrank, lrank;
    opal_paffinity_base_cpu_set_t mask;
    int target_socket, npersocket, logical_skt;
    int logical_cpu, phys_core, phys_cpu, ncpu;

    *bound = false;

    /* we want to bind this proc to a specific core, or multiple cores
       if the cpus_per_rank is > 0 */
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:default:fork binding child %s to core(s) cpus/rank %d stride %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(child->name),
                         (int)jobdat->cpus_per_rank, (int)jobdat->stride));

    /* get the node rank */
    if (ORTE_NODE_RANK_INVALID == 
        (nrank = orte_ess.get_node_rank(child->name))) {
        send_error_show_help(pipe_fd, 1, "help-orte-odls-default.txt",
                             "binding generic error",
                             orte_process_info.nodename, context->app,
                             "ess.get_node_rank returned NODE_RANK_INVALID",
                             __FILE__, __LINE__);
        /* Does not return */
    }

    /* get the local rank */
    if (ORTE_LOCAL_RANK_INVALID == 
        (lrank = orte_ess.get_local_rank(child->name))) {
        send_error_show_help(pipe_fd, 1, "help-orte-odls-default.txt",
                             "binding generic error",
                             orte_process_info.nodename, context->app,
                             "ess.get_local_rank returned LOCAL_RANK_INVALID",
                             __FILE__, __LINE__);
        /* Does not return */
    }

    /* init the mask */
    OPAL_PAFFINITY_CPU_ZERO(mask);
    if (ORTE_MAPPING_NPERXXX & jobdat->policy) {
        /* we need to balance the children from this job
           across the available sockets */
        npersocket = jobdat->num_local_procs / orte_odls_globals.num_sockets;
        /* determine the socket to use based on those available */
        if (npersocket < 2) {
            /* if we only have 1/sock, or we have less procs than
               sockets, then just put it on the lrank socket */
            logical_skt = lrank;
        } else if (ORTE_MAPPING_BYSOCKET & jobdat->policy) {
            logical_skt = lrank % npersocket;
        } else {
            logical_skt = lrank / npersocket;
        }
        if (orte_odls_globals.bound) {
            /* if we are already bound (by some other entity), use
               this as an index into our available sockets */
            for (n = target_socket = 0;
                 n < logical_skt &&
                 target_socket < opal_bitmap_size(&orte_odls_globals.sockets);
                 target_socket++) {
                if (opal_bitmap_is_set_bit(&orte_odls_globals.sockets, 
                                           target_socket)) {
                    n++;
                }
            }
            /* Did we have enough sockets? */
            if (n < logical_skt) {
                return bind_failed_msg("not enough processor sockets available",
                                       jobdat->policy, 
                                       ORTE_ERR_NOT_FOUND,
                                       pipe_fd, context->app, 
                                       __FILE__, __LINE__);
            }
        } else {
            rc = opal_paffinity_base_get_physical_socket_id(logical_skt, 
                                                            &target_socket);
            if (ORTE_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                return bind_failed_msg("OS does not provide processor topology info (physical socket ID)",
                                       jobdat->policy,
                                       ORTE_ERR_NOT_FOUND,
                                       pipe_fd, context->app,
                                       __FILE__, __LINE__);
            }
        }
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:default:fork child %s local rank %d npersocket %d logical socket %d target socket %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(child->name), lrank,
                             npersocket, logical_skt, target_socket));
        /* set the starting point */
        logical_cpu = (lrank % npersocket) * jobdat->cpus_per_rank;
        /* bind to this socket */
        goto bind_socket;
    } else if (ORTE_MAPPING_BYSOCKET & jobdat->policy) {
        /* this corresponds to a mapping policy where
         * local rank 0 goes on socket 0, and local
         * rank 1 goes on socket 1, etc. - round robin
         * until all ranks are mapped
         *
         * NOTE: we already know our number of sockets
         * from when we initialized
         */
        rc = opal_paffinity_base_get_physical_socket_id(lrank % orte_odls_globals.num_sockets, &target_socket);
        if (OPAL_SUCCESS != rc) {
            /* This may be a small memory leak, but this child is
               exiting soon anyway; keep the logic simple by not
               worrying about the small leak. */
            asprintf(&msg, "opal_paffinity_base_get_physical_socket_id(%d) returned \"%s\"",
                     (lrank % orte_odls_globals.num_sockets),
                     opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
            if (NULL == msg) {
                msg = "opal_paffinity_base_get_physical_socket_id() failed";
            }
            if (OPAL_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                msg = "OS does not provide processor topology information (physical socket ID)";
            }
            return bind_failed(msg, jobdat->policy, ORTE_ERR_NOT_SUPPORTED,
                              pipe_fd, context->app, __FILE__, __LINE__);
        }
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "bysocket lrank %d numsocks %d logical socket %d target socket %d", (int)lrank,
                             (int)orte_odls_globals.num_sockets,
                             (int)(lrank % orte_odls_globals.num_sockets),
                             target_socket));
        /* my starting core within this socket has to be
           offset by cpus_per_rank */
        logical_cpu = (lrank / orte_odls_globals.num_sockets) * jobdat->cpus_per_rank;
        
    bind_socket:
        /* cycle across the cpus_per_rank */
        for (n=0; n < jobdat->cpus_per_rank; n++) {
            /* get the physical core within this target socket */
            rc = opal_paffinity_base_get_physical_core_id(target_socket, logical_cpu, &phys_core);
            if (OPAL_SUCCESS != rc) {
                /* Seem comment above about "This may be a small
                   memory leak" */
                asprintf(&msg, "opal_paffinity_base_get_physical_core_id(%d, %d) returned \"%s\"",
                         target_socket, logical_cpu, 
                         opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
                if (NULL == msg) {
                    msg = "opal_paffinity_base_get_physical_core_id() failed";
                }
                if (OPAL_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                    msg = "OS does not provide processor topology information (physical core ID)";
                }
                return bind_failed(msg, jobdat->policy, ORTE_ERR_NOT_SUPPORTED,
                                   pipe_fd, context->app, __FILE__, __LINE__);
            }
            /* map this to a physical cpu on this node */
            if (ORTE_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id(target_socket, phys_core, &phys_cpu))) {
                /* Seem comment above about "This may be a small
                   memory leak" */
                asprintf(&msg, "opal_paffinity_base_get_map_to_processor_id(%d, %d) returned \"%s\"",
                         target_socket, phys_core, 
                         opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
                if (NULL == msg) {
                    msg = "opal_paffinity_base_get_map_to_processor_id() failed";
                }
                if (OPAL_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                    msg = "OS does not provide processor topology information (map socket,core->ID)";
                }
                return bind_failed(msg, jobdat->policy, ORTE_ERR_NOT_SUPPORTED,
                                   pipe_fd, context->app, __FILE__, __LINE__);
            }
            /* are we bound? */
            if (orte_odls_globals.bound) {
                /* see if this physical cpu is available to us */
                if (!OPAL_PAFFINITY_CPU_ISSET(phys_cpu, orte_odls_globals.my_cores)) {
                    /* no it isn't - skip it */
                    continue;
                }
            }
            OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                 "%s odls:default:fork mapping phys socket %d core %d to phys_cpu %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 target_socket, phys_core, phys_cpu));
            OPAL_PAFFINITY_CPU_SET(phys_cpu, mask);
            /* increment logical cpu */
            logical_cpu += jobdat->stride;
        }
        if (orte_report_bindings) {
            tmp = opal_paffinity_base_print_binding(mask);
            opal_output(0, "%s odls:default:fork binding child %s to socket %d cpus %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(child->name), target_socket, tmp);
            free(tmp);
        }
    } else {
        /* my starting core has to be offset by cpus_per_rank */
        logical_cpu = nrank * jobdat->cpus_per_rank;
        for (n=0; n < jobdat->cpus_per_rank; n++) {
            /* are we bound? */
            if (orte_odls_globals.bound) {
                /* if we are bound, then use the logical_cpu as an
                   index against our available cores */
                ncpu = 0;
                for (i = 0; i < OPAL_PAFFINITY_BITMASK_CPU_MAX && 
                         ncpu <= logical_cpu; i++) {
                    if (OPAL_PAFFINITY_CPU_ISSET(i, 
                                                 orte_odls_globals.my_cores)) {
                        ncpu++;
                        phys_cpu = i;
                    }
                }
                /* if we don't have enough processors, that is an
                   error */
                if (ncpu <= logical_cpu) {
                    if (ORTE_BINDING_NOT_REQUIRED(jobdat->policy)) {
                        return ORTE_ERR_NOT_SUPPORTED;
                    }
                    send_error_show_help(pipe_fd, 1, 
                                         "help-orte-odls-default.txt",
                                         "binding generic error",
                                         orte_process_info.nodename, 
                                         context->app,
                                         "not enough logical processors",
                                         __FILE__, __LINE__);
                    /* Does not return */
                }
            } else {
                /* if we are not bound, then all processors are
                   available to us, so index into the node's array to
                   get the physical cpu */
                rc = opal_paffinity_base_get_physical_processor_id(logical_cpu,
                                                                   &phys_cpu);
                if (OPAL_SUCCESS != rc) {
                    /* No processor to bind to */
                    /* Seem comment above about "This may be a small
                       memory leak" */
                    asprintf(&msg, "opal_paffinity_base_get_physical_processor_id(%d) returned \"%s\"",
                             logical_cpu,
                             opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
                    if (NULL == msg) {
                        msg = "opal_paffinity_base_get_physical_processor_id() failed";
                    }
                    if (OPAL_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                        msg = "OS does not provide processor topology information (physical processor ID)";
                    }
                    return bind_failed(msg, jobdat->policy, 
                                       ORTE_ERR_NOT_SUPPORTED,
                                       pipe_fd, context->app,
                                       __FILE__, __LINE__);
                }
            }
            OPAL_PAFFINITY_CPU_SET(phys_cpu, mask);
            /* increment logical cpu */
            logical_cpu += jobdat->stride;
        }
        if (orte_report_bindings) {
            tmp = opal_paffinity_base_print_binding(mask);
            opal_output(0, "%s odls:default:fork binding child %s to cpus %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(child->name), tmp);
            free(tmp);
        }
    }

    /* Bind me! */
    if (ORTE_SUCCESS != (rc = opal_paffinity_base_set(mask))) {
        /* Seem comment above about "This may be a small memory
           leak" */
        asprintf(&msg, "opal_paffinity_base_set returned \"%s\"",
                 opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
        if (NULL == msg) {
            msg = "opal_paffinity_base_set() failed";
        }
        return bind_failed(msg,
                          jobdat->policy, 
                          OPAL_SOS_GET_ERROR_CODE(rc),
                          pipe_fd, context->app, __FILE__, __LINE__);
    }
    *bound = true;

    /* If the above work resulted in binding to everything (i.e.,
       effectively not binding), warn -- unless the warning is
       suppressed. */
    OPAL_PAFFINITY_PROCESS_IS_BOUND(mask, &flag);
    if (!flag && orte_odls_base.warn_if_not_bound) {
        send_warn_show_help(pipe_fd,
                            "help-orte-odls-default.txt",
                            "bound to everything",
                            orte_process_info.nodename, context->app,
                            __FILE__, __LINE__);
    }

    return ORTE_SUCCESS;
}


static int bind_to_socket(orte_app_context_t* context,
                          orte_odls_child_t *child,
                          orte_odls_job_t *jobdat,
                          bool *bound, int pipe_fd)
{
    bool flag;
    int i, rc;
    char *tmp, *msg;
    int16_t n;
    orte_node_rank_t lrank;
    opal_paffinity_base_cpu_set_t mask;
    int target_socket, npersocket, logical_skt;
    int logical_cpu, phys_core, phys_cpu, ncpu;

    *bound = false;

    /* bind this proc to a socket */
    OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                         "%s odls:default:fork binding child %s to socket",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(child->name)));
    /* layout this process across the sockets based on
     * the provided mapping policy
     */
    if (ORTE_LOCAL_RANK_INVALID == (lrank = orte_ess.get_local_rank(child->name))) {
        send_error_show_help(pipe_fd, 1, "help-orte-odls-default.txt",
                             "binding generic error",
                             orte_process_info.nodename, context->app,
                             "ess.get_local_rank returned NODE_RANK_INVALID",
                             __FILE__, __LINE__);
        /* Does not return */
    }
    if (ORTE_MAPPING_NPERXXX & jobdat->policy) {
        /* we need to balance the children from this job
           across the available sockets */
        npersocket = jobdat->num_local_procs / orte_odls_globals.num_sockets;
        /* determine the socket to use based on those available */
        if (npersocket < 2) {
            /* if we only have 1/sock, or we have less
             * procs than sockets, then just put it on the
             * lrank socket
             */
            logical_skt = lrank;
        } else if (ORTE_MAPPING_BYSOCKET & jobdat->policy) {
            logical_skt = lrank % npersocket;
        } else {
            logical_skt = lrank / npersocket;
        }
        if (orte_odls_globals.bound) {
            /* if we are bound, use this as an index into
               our available sockets */
            for (target_socket=0, n = 0; target_socket < opal_bitmap_size(&orte_odls_globals.sockets) && n < logical_skt; target_socket++) {
                if (opal_bitmap_is_set_bit(&orte_odls_globals.sockets, target_socket)) {
                    n++;
                }
            }
            /* if we don't have enough sockets, that is an error */
            if (n < logical_skt) {
                return bind_failed_msg("not enough processor sockets available",
                                       jobdat->policy, 
                                       ORTE_ERR_NOT_FOUND,
                                       pipe_fd, context->app, 
                                       __FILE__, __LINE__);
            }
        } else {
            rc = opal_paffinity_base_get_physical_socket_id(logical_skt, &target_socket);
            if (ORTE_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                /* OS doesn't support providing topology
                   information */
                return bind_failed_msg("OS does not provide processor topology info (physical socket ID)",
                                       jobdat->policy,
                                       ORTE_ERR_NOT_FOUND,
                                       pipe_fd, context->app,
                                       __FILE__, __LINE__);
            }
        }
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:default:fork child %s local rank %d npersocket %d logical socket %d target socket %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(child->name), lrank,
                             npersocket, logical_skt, target_socket));
    } else if (ORTE_MAPPING_BYSOCKET & jobdat->policy) {
        /* this corresponds to a mapping policy where
         * local rank 0 goes on socket 0, and local
         * rank 1 goes on socket 1, etc. - round robin
         * until all ranks are mapped
         *
         * NOTE: we already know our number of sockets
         * from when we initialized
         */
        rc = opal_paffinity_base_get_physical_socket_id(lrank % orte_odls_globals.num_sockets, &target_socket);
        if (ORTE_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
            /* OS does not support providing topology
               information */
            return bind_failed_msg("OS does not provide processor topology info(physical socket ID)",
                                   jobdat->policy,
                                   ORTE_ERR_NOT_FOUND,
                                   pipe_fd, context->app,
                                   __FILE__, __LINE__);
        }
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "bysocket lrank %d numsocks %d logical socket %d target socket %d", (int)lrank,
                             (int)orte_odls_globals.num_sockets,
                             (int)(lrank % orte_odls_globals.num_sockets),
                             target_socket));
    } else {
        /* use a byslot-like policy where local rank 0 goes on
         * socket 0, and local rank 1 goes on socket 0, etc.
         * following round-robin until all ranks mapped
         */
        if (orte_odls_globals.bound) {
            /* if we are bound, then we compute the
             * logical socket id based on the number of
             * available cores in each socket so that each
             * rank gets its own core, adjusting for the
             * cpus_per_task
             */
            /* Find the lrank available core, accounting
               for cpus_per_task */
            logical_cpu = lrank * jobdat->cpus_per_rank;
            /* use the logical_cpu as an index against our
               available cores */
            ncpu = 0;
            for (i=0; i < orte_odls_globals.num_processors && ncpu <= logical_cpu; i++) {
                if (OPAL_PAFFINITY_CPU_ISSET(i, orte_odls_globals.my_cores)) {
                    ncpu++;
                    phys_cpu = i;
                }
            }
            /* if we don't have enough processors, that is
               an error */
            if (ncpu < logical_cpu) {
                send_error_show_help(pipe_fd, 1, 
                                     "help-orte-odls-default.txt",
                                     "binding generic error",
                                     orte_process_info.nodename, 
                                     context->app,
                                     "not enough logical processors",
                                     __FILE__, __LINE__);
                /* Does not return */
            }
            /* get the physical socket of that cpu */
            if (ORTE_SUCCESS != (rc = opal_paffinity_base_get_map_to_socket_core(phys_cpu, &target_socket, &phys_core))) {
                /* Seem comment above about "This may be a small
                   memory leak" */
                asprintf(&msg, "opal_paffinity_base_get_map_to_socket_core(%d) returned \"%s\"",
                         phys_cpu, opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
                if (NULL == msg) {
                    msg = "opal_paffinity_base_get_map_to_socket_core() failed";
                }
                if (OPAL_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                    msg = "OS does not provide processor topology information (map socket,core->ID)";
                }
                return bind_failed(msg, jobdat->policy, 
                                   ORTE_ERR_NOT_SUPPORTED,
                                   pipe_fd, context->app,
                                   __FILE__, __LINE__);
            }
        } else {
            /* if we are not bound, then just use all sockets */
            if (1 == orte_odls_globals.num_sockets) {
                /* if we only have one socket, then just
                   put it there */
                rc = opal_paffinity_base_get_physical_socket_id(0, &target_socket);
                if (ORTE_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                    /* OS doesn't support providing
                       topology information */
                    return bind_failed_msg("OS does not provide processor topology info (physical socket ID)",
                                           jobdat->policy,
                                           ORTE_ERR_NOT_FOUND,
                                           pipe_fd, context->app,
                                           __FILE__, __LINE__);
                }
            } else {
                /* compute the logical socket,
                   compensating for the number of
                   cpus_per_rank */
                logical_skt = lrank / (orte_default_num_cores_per_socket / jobdat->cpus_per_rank);
                /* wrap that around the number of sockets
                   so we round-robin */
                logical_skt = logical_skt % orte_odls_globals.num_sockets;
                /* now get the target physical socket */
                rc = opal_paffinity_base_get_physical_socket_id(logical_skt, &target_socket);
                if (ORTE_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                    /* OS doesn't support providing
                       topology information */
                    return bind_failed_msg("OS does not provide processor topology info (physical socket ID)",
                                           jobdat->policy,
                                           ORTE_ERR_NOT_FOUND,
                                           pipe_fd, context->app,
                                           __FILE__, __LINE__);
                }
            }
            OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                 "byslot lrank %d socket %d", (int)lrank, target_socket));
        }
    }
    
    OPAL_PAFFINITY_CPU_ZERO(mask);
    
    for (n=0; n < orte_default_num_cores_per_socket; n++) {
        /* get the physical core within this target socket */
        rc = opal_paffinity_base_get_physical_core_id(target_socket, n, &phys_core);
        if (OPAL_SUCCESS != rc) {
            /* Seem comment above about "This may be a small memory
               leak" */
            asprintf(&msg, "opal_paffinity_base_get_physical_core_id(%d, %d) returned \"%s\"",
                     target_socket, n,
                     opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
            if (NULL == msg) {
                msg = "opal_paffinity_base_get_physical_core_id() failed";
            }
            if (OPAL_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                msg = "OS does not provide processor topology information (physical core ID)";
            }
            return bind_failed(msg, jobdat->policy, 
                               ORTE_ERR_NOT_SUPPORTED,
                               pipe_fd, context->app,
                               __FILE__, __LINE__);
        }
        /* map this to a physical cpu on this node */
        if (ORTE_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id(target_socket, phys_core, &phys_cpu))) {
            /* Seem comment above about "This may be a small memory
               leak" */
            asprintf(&msg, "opal_paffinity_base_get_map_to_processor_id(%d, %d) returned \"%s\"",
                     target_socket, phys_core,
                     opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
            if (NULL == msg) {
                msg = "opal_paffinity_base_get_map_to_processor_id()";
            }
            if (OPAL_ERR_NOT_SUPPORTED == OPAL_SOS_GET_ERROR_CODE(rc)) {
                msg = "OS does not provide processor topology information (map socket,core->ID)";
            }
            return bind_failed(msg, jobdat->policy, 
                               ORTE_ERR_NOT_SUPPORTED,
                               pipe_fd, context->app,
                               __FILE__, __LINE__);
        }
        /* are we bound? */
        if (orte_odls_globals.bound) {
            /* see if this physical cpu is available to us */
            if (!OPAL_PAFFINITY_CPU_ISSET(phys_cpu, orte_odls_globals.my_cores)) {
                /* no it isn't - skip it */
                continue;
            }
        }
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:default:fork mapping phys socket %d core %d to phys_cpu %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             target_socket, phys_core, phys_cpu));
        OPAL_PAFFINITY_CPU_SET(phys_cpu, mask);
    }

    /* Bind me! */
    if (ORTE_SUCCESS != (rc = opal_paffinity_base_set(mask))) {
        /* Seem comment above about "This may be a small memory
           leak" */
        asprintf(&msg, "opal_paffinity_base_set() returned \"%s\"",
                 opal_strerror(OPAL_SOS_GET_ERROR_CODE(rc)));
        if (NULL == msg) {
            msg = "opal_paffinity_base_set() failed";
        }
        return bind_failed(msg,
                           jobdat->policy, 
                           OPAL_SOS_GET_ERROR_CODE(rc),
                           pipe_fd, context->app, __FILE__, __LINE__);
    }
    *bound = true;

    /* If the above work resulted in binding to everything (i.e.,
       effectively not binding), warn -- unless the warning is
       suppressed. */
    OPAL_PAFFINITY_PROCESS_IS_BOUND(mask, &flag);
    if (!flag && orte_odls_base.warn_if_not_bound) {
        send_warn_show_help(pipe_fd,
                            "help-orte-odls-default.txt",
                            "bound to everything",
                            orte_process_info.nodename, context->app,
                            __FILE__, __LINE__);
    } else if (orte_report_bindings) {
        tmp = opal_paffinity_base_print_binding(mask);
        opal_output(0, "%s odls:default:fork binding child %s to socket %d cpus %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(child->name), target_socket, tmp);
        free(tmp);
    }

    return ORTE_SUCCESS;
}


static int bind_to_board(orte_app_context_t* context,
                         orte_odls_child_t *child,
                         orte_odls_job_t *jobdat,
                         bool *bound, int pipe_fd)
{
    /* Not currently supported until multi-board paffinity enabled.
       But this is not an error -- for now. */
    *bound = false;
    if (orte_odls_base.warn_if_not_bound) {
        send_warn_show_help(pipe_fd, "help-orte-odls-base.txt",
                            "warn not bound", "board",
                            "Not currently supported by Open MPI",
                            orte_process_info.nodename, context->app,
                            "Bind to board", "");
    }        

    return ORTE_ERR_NOT_SUPPORTED;
}


static int do_child(orte_app_context_t* context,
                    orte_odls_child_t *child,
                    char **environ_copy,
                    orte_odls_job_t *jobdat, int write_fd,
                    orte_iof_base_io_conf_t opts)
{
    int i;
    sigset_t sigs;
    long fd, fdmax = sysconf(_SC_OPEN_MAX);
    bool paffinity_enabled = false;
    char *param, *tmp;
    opal_paffinity_base_cpu_set_t mask;
    
    if (orte_forward_job_control) {
        /* Set a new process group for this child, so that a
           SIGSTOP can be sent to it without being sent to the
           orted. */
        setpgid(0, 0);
    }
    
    /* Setup the pipe to be close-on-exec */
    fcntl(write_fd, F_SETFD, FD_CLOEXEC);

    if (NULL != child) {
        /* setup stdout/stderr so that any error messages that we
           may print out will get displayed back at orterun.
           
           NOTE: Definitely do this AFTER we check contexts so
           that any error message from those two functions doesn't
           come out to the user. IF we didn't do it in this order,
           THEN a user who gives us a bad executable name or
           working directory would get N error messages, where
           N=num_procs. This would be very annoying for large
           jobs, so instead we set things up so that orterun
           always outputs a nice, single message indicating what
           happened
        */
        if (ORTE_SUCCESS != (i = orte_iof_base_setup_child(&opts, 
                                                           &environ_copy))) {
            ORTE_ERROR_LOG(i);
            send_error_show_help(write_fd, 1, 
                                 "help-orte-odls-default.txt", 
                                 "iof setup failed",
                                 orte_process_info.nodename, context->app);
            /* Does not return */
        }
        
        /* Setup process affinity.  Not for the meek. */
        
        if (NULL != child->slot_list) {
            bind_to_slot_list(context, child, jobdat, 
                              &paffinity_enabled, write_fd);
        } else if (ORTE_BIND_TO_CORE & jobdat->policy) {
            bind_to_core(context, child, jobdat, 
                         &paffinity_enabled, write_fd);
        } else if (ORTE_BIND_TO_SOCKET & jobdat->policy) {
            bind_to_socket(context, child, jobdat, 
                           &paffinity_enabled, write_fd);
        } else if (ORTE_BIND_TO_BOARD & jobdat->policy) {
            bind_to_board(context, child, jobdat, 
                          &paffinity_enabled, write_fd);
        }
        
        /* If we were able to set processor affinity, then also
           setup memory affinity. */
        if (paffinity_enabled) {
            if (OPAL_SUCCESS == opal_maffinity_base_open() &&
                OPAL_SUCCESS == opal_maffinity_base_select()) {
                opal_maffinity_setup = true;
            }
        }
    } else if (!(ORTE_JOB_CONTROL_FORWARD_OUTPUT & jobdat->controls)) {
        /* tie stdin/out/err/internal to /dev/null */
        int fdnull;
        for (i=0; i < 3; i++) {
            fdnull = open("/dev/null", O_RDONLY, 0);
            if (fdnull > i && i != write_fd) {
                dup2(fdnull, i);
            }
            close(fdnull);
        }
        fdnull = open("/dev/null", O_RDONLY, 0);
        if (fdnull > opts.p_internal[1]) {
            dup2(fdnull, opts.p_internal[1]);
        }
        close(fdnull);
    }
    
    /* If we are able to bind, then set an info MCA param that tells
       the launched processes that it was bound by us (e.g., so that
       MPI_INIT doesn't try to bind itself) */
    if (paffinity_enabled) {
        param = mca_base_param_environ_variable("paffinity","base","bound");
        opal_setenv(param, "1", true, &environ_copy);
        free(param);
        /* ...and provide a nice string representation of what we
           bound to */
        if (OPAL_SUCCESS == opal_paffinity_base_get(&mask)) {
            tmp = opal_paffinity_base_print_binding(mask);
            if (NULL != tmp) {
                param = mca_base_param_environ_variable("paffinity","base","applied_binding");
                opal_setenv(param, tmp, true, &environ_copy);
                free(tmp);                
            }
        }
    }
    
    /* close all file descriptors w/ exception of stdin/stdout/stderr,
       the pipe used for the IOF INTERNAL messages, and the pipe up to
       the parent. */
    for(fd=3; fd<fdmax; fd++) {
        if (fd != opts.p_internal[1] && fd != write_fd) {
            close(fd);
        }
    }
    
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
    
    if (10 < opal_output_get_verbosity(orte_odls_globals.output)) {
        int jout;
        opal_output(0, "%s STARTING %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), context->app);
        for (jout=0; NULL != context->argv[jout]; jout++) {
            opal_output(0, "%s\tARGV[%d]: %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), jout, context->argv[jout]);
        }
        for (jout=0; NULL != environ_copy[jout]; jout++) {
            opal_output(0, "%s\tENVIRON[%d]: %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), jout, environ_copy[jout]);
        }
    }
    
    execve(context->app, context->argv, environ_copy);
    send_error_show_help(write_fd, 1, 
                         "help-orte-odls-default.txt", "execve error",
                         context->app, strerror(errno));
    /* Does not return */
}


static int do_parent(orte_app_context_t* context,
                     orte_odls_child_t *child,
                     char **environ_copy,
                     orte_odls_job_t *jobdat, int read_fd,
                     orte_iof_base_io_conf_t opts)
{
    int rc;
    pipe_err_msg_t msg;
    char file[MAX_FILE_LEN + 1], topic[MAX_TOPIC_LEN + 1], *str = NULL;

    if (NULL != child && (ORTE_JOB_CONTROL_FORWARD_OUTPUT & jobdat->controls)) {
        /* connect endpoints IOF */
        rc = orte_iof_base_setup_parent(child->name, &opts);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            close(read_fd);

            if (NULL != child) {
                child->state = ORTE_PROC_STATE_UNDEF;
            }
            return rc;
        }
    }
    
    /* Block reading a message from the pipe */
    while (1) {
        rc = opal_fd_read(read_fd, sizeof(msg), &msg);

        /* If the pipe closed, then the child successfully launched */
        if (OPAL_ERR_TIMEOUT == OPAL_SOS_GET_ERROR_CODE(rc)) {
            break;
        }
        
        /* If Something Bad happened in the read, error out */
        if (OPAL_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            close(read_fd);
            
            if (NULL != child) {
                child->state = ORTE_PROC_STATE_UNDEF;
            }
            return rc;
        }

        /* Otherwise, we got a warning or error message from the child */
        if (NULL != child) {
            child->alive = msg.fatal ? 0 : 1;
        }

        /* Read in the strings; ensure to terminate them with \0 */
        if (msg.file_str_len > 0) {
            rc = opal_fd_read(read_fd, msg.file_str_len, file);
            if (OPAL_SUCCESS != rc) {
                orte_show_help("help-orte-odls-default.txt", "syscall fail", 
                               true,
                               orte_process_info.nodename, context->app,
                               "opal_fd_read", __FILE__, __LINE__);
                if (NULL != child) {
                    child->state = ORTE_PROC_STATE_UNDEF;
                }
                return rc;
            }
            file[msg.file_str_len] = '\0';
        }
        if (msg.topic_str_len > 0) {
            rc = opal_fd_read(read_fd, msg.topic_str_len, topic);
            if (OPAL_SUCCESS != rc) {
                orte_show_help("help-orte-odls-default.txt", "syscall fail", 
                               true,
                               orte_process_info.nodename, context->app,
                               "opal_fd_read", __FILE__, __LINE__);
                if (NULL != child) {
                    child->state = ORTE_PROC_STATE_UNDEF;
                }
                return rc;
            }
            topic[msg.topic_str_len] = '\0';
        }
        if (msg.msg_str_len > 0) {
            str = calloc(1, msg.msg_str_len + 1);
            if (NULL == str) {
                orte_show_help("help-orte-odls-default.txt", "syscall fail", 
                               true,
                               orte_process_info.nodename, context->app,
                               "opal_fd_read", __FILE__, __LINE__);
                if (NULL != child) {
                    child->state = ORTE_PROC_STATE_UNDEF;
                }
                return rc;
            }
            rc = opal_fd_read(read_fd, msg.msg_str_len, str);
        }

        /* Print out what we got.  We already have a rendered string,
           so use orte_show_help_norender(). */
        if (msg.msg_str_len > 0) {
            orte_show_help_norender(file, topic, false, str);
            free(str);
            str = NULL;
        }

        /* If msg.fatal is true, then the child exited with an error.
           Otherwise, whatever we just printed was a warning, so loop
           around and see what else is on the pipe (or if the pipe
           closed, indicating that the child launched
           successfully). */
        if (msg.fatal) {
            if (NULL != child) {
                child->state = ORTE_PROC_STATE_FAILED_TO_START;
                child->alive = false;
            }
            close(read_fd);
            return ORTE_SUCCESS;
        }
    }

    /* If we got here, it means that the pipe closed without
       indication of a fatal error, meaning that the child process
       launched successfully. */
    if (NULL != child) {
        child->state = ORTE_PROC_STATE_LAUNCHED;
        child->alive = true;
    }
    close(read_fd);
    
    return ORTE_SUCCESS;
}


/**
 *  Fork/exec the specified processes
 */
static int odls_default_fork_local_proc(orte_app_context_t* context,
                                        orte_odls_child_t *child,
                                        char **environ_copy,
                                        orte_odls_job_t *jobdat)
{
    orte_iof_base_io_conf_t opts;
    int rc, p[2];
    pid_t pid;
    
    if (NULL != child) {
        /* should pull this information from MPIRUN instead of going with
         default */
        opts.usepty = OPAL_ENABLE_PTY_SUPPORT;
        
        /* do we want to setup stdin? */
        if (NULL != child &&
            (jobdat->stdin_target == ORTE_VPID_WILDCARD || child->name->vpid == jobdat->stdin_target)) {
            opts.connect_stdin = true;
        } else {
            opts.connect_stdin = false;
        }
        
        if (ORTE_SUCCESS != (rc = orte_iof_base_setup_prefork(&opts))) {
            ORTE_ERROR_LOG(rc);
            if (NULL != child) {
                child->state = ORTE_PROC_STATE_FAILED_TO_START;
                child->exit_code = rc;
            }
            return rc;
        }
    }
    
    /* A pipe is used to communicate between the parent and child to
       indicate whether the exec ultimately succeeded or failed.  The
       child sets the pipe to be close-on-exec; the child only ever
       writes anything to the pipe if there is an error (e.g.,
       executable not found, exec() fails, etc.).  The parent does a
       blocking read on the pipe; if the pipe closed with no data,
       then the exec() succeeded.  If the parent reads something from
       the pipe, then the child was letting us know why it failed. */
    if (pipe(p) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_PIPES);
        if (NULL != child) {
            child->state = ORTE_PROC_STATE_FAILED_TO_START;
            child->exit_code = ORTE_ERR_SYS_LIMITS_PIPES;
        }
        return ORTE_ERR_SYS_LIMITS_PIPES;
    }
    
    /* Fork off the child */
    pid = fork();
    if (NULL != child) {
        child->pid = pid;
    }
    
    if (pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
        if (NULL != child) {
            child->state = ORTE_PROC_STATE_FAILED_TO_START;
            child->exit_code = ORTE_ERR_SYS_LIMITS_CHILDREN;
        }
        return ORTE_ERR_SYS_LIMITS_CHILDREN;
    }
    
    if (pid == 0) {
	close(p[0]);
        do_child(context, child, environ_copy, jobdat, p[1], opts);
        /* Does not return */
    } 

    close(p[1]);
    return do_parent(context, child, environ_copy, jobdat, p[0], opts);
}


/**
 * Launch all processes allocated to the current node.
 */

int orte_odls_default_launch_local_procs(opal_buffer_t *data)
{
    int rc;
    orte_jobid_t job;
    orte_job_t *jdata;

    /* construct the list of children we are to launch */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_construct_child_list(data, &job))) {
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:default:launch:local failed to construct child list on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(rc)));
        goto CLEANUP;
    }
    
    /* launch the local procs */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_launch_local(job, odls_default_fork_local_proc))) {
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:default:launch:local failed to launch on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(rc)));
        goto CLEANUP;
    }
    
    /* look up job data object */
    if (NULL != (jdata = orte_get_job_data_object(job))) {
        if (jdata->state & ORTE_JOB_STATE_SUSPENDED) {
            if (ORTE_PROC_IS_HNP) {
		/* Have the plm send the signal to all the nodes.
		   If the signal arrived before the orteds started,
		   then they won't know to suspend their procs.
		   The plm also arranges for any local procs to
		   be signaled.
		 */
                orte_plm.signal_job(jdata->jobid, SIGTSTP);
            } else {
                orte_odls_default_signal_local_procs(NULL, SIGTSTP);
            }
        }
    }

CLEANUP:
   
    return rc;
}


/**
 * Send a sigal to a pid.  Note that if we get an error, we set the
 * return value and let the upper layer print out the message.  
 */
static int send_signal(pid_t pid, int signal)
{
    int rc = ORTE_SUCCESS;
    
    OPAL_OUTPUT_VERBOSE((1, orte_odls_globals.output,
                         "%s sending signal %d to pid %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         signal, (long)pid));

    if (orte_forward_job_control) {
	/* Send the signal to the process group rather than the
	   process.  The child is the leader of its process group. */
	pid = -pid;
    }
    if (kill(pid, signal) != 0) {
        switch(errno) {
            case EINVAL:
                rc = ORTE_ERR_BAD_PARAM;
                break;
            case ESRCH:
                /* This case can occur when we deliver a signal to a
                   process that is no longer there.  This can happen if
                   we deliver a signal while the job is shutting down. 
                   This does not indicate a real problem, so just 
                   ignore the error.  */
                break;
            case EPERM:
                rc = ORTE_ERR_PERM;
                break;
            default:
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

static int orte_odls_default_restart_proc(orte_odls_child_t *child)
{
    int rc;
    
    /* restart the local proc */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_restart_proc(child, odls_default_fork_local_proc))) {
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:default:restart_proc failed to launch on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(rc)));
    }
    return rc;
}


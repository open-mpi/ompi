/* Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef PRTE_MCA_ODLS_TYPES_H
#define PRTE_MCA_ODLS_TYPES_H

#include "prte_config.h"
#include "types.h"

#include "src/pmix/pmix-internal.h"

BEGIN_C_DECLS

/* define the orted command flag type */
typedef uint8_t prte_daemon_cmd_flag_t;
#define PRTE_DAEMON_CMD PMIX_UINT8

/*
 * Definitions needed for communication
 */
#define PRTE_DAEMON_CONTACT_QUERY_CMD     (prte_daemon_cmd_flag_t) 1
#define PRTE_DAEMON_KILL_LOCAL_PROCS      (prte_daemon_cmd_flag_t) 2
#define PRTE_DAEMON_SIGNAL_LOCAL_PROCS    (prte_daemon_cmd_flag_t) 3
#define PRTE_DAEMON_ADD_LOCAL_PROCS       (prte_daemon_cmd_flag_t) 4
#define PRTE_DAEMON_HEARTBEAT_CMD         (prte_daemon_cmd_flag_t) 6
#define PRTE_DAEMON_EXIT_CMD              (prte_daemon_cmd_flag_t) 7
#define PRTE_DAEMON_PROCESS_AND_RELAY_CMD (prte_daemon_cmd_flag_t) 9
#define PRTE_DAEMON_NULL_CMD              (prte_daemon_cmd_flag_t) 11

/* commands for use by tools */
#define PRTE_DAEMON_REPORT_JOB_INFO_CMD  (prte_daemon_cmd_flag_t) 14
#define PRTE_DAEMON_REPORT_NODE_INFO_CMD (prte_daemon_cmd_flag_t) 15
#define PRTE_DAEMON_REPORT_PROC_INFO_CMD (prte_daemon_cmd_flag_t) 16
#define PRTE_DAEMON_SPAWN_JOB_CMD        (prte_daemon_cmd_flag_t) 17
#define PRTE_DAEMON_TERMINATE_JOB_CMD    (prte_daemon_cmd_flag_t) 18
#define PRTE_DAEMON_HALT_VM_CMD          (prte_daemon_cmd_flag_t) 19
#define PRTE_DAEMON_HALT_DVM_CMD         (prte_daemon_cmd_flag_t) 20
#define PRTE_DAEMON_REPORT_JOB_COMPLETE  (prte_daemon_cmd_flag_t) 21
#define PRTE_DAEMON_DEFINE_PSET          (prte_daemon_cmd_flag_t) 50

/* request proc resource usage */
#define PRTE_DAEMON_TOP_CMD (prte_daemon_cmd_flag_t) 22

/* bootstrap */
#define PRTE_DAEMON_NAME_REQ_CMD (prte_daemon_cmd_flag_t) 23
#define PRTE_DAEMON_CHECKIN_CMD  (prte_daemon_cmd_flag_t) 24
#define PRTE_TOOL_CHECKIN_CMD    (prte_daemon_cmd_flag_t) 25

/* process msg command */
#define PRTE_DAEMON_PROCESS_CMD (prte_daemon_cmd_flag_t) 26

/* process called "errmgr.abort_procs" */
#define PRTE_DAEMON_ABORT_PROCS_CALLED (prte_daemon_cmd_flag_t) 28

/* add procs for the DVM */
#define PRTE_DAEMON_DVM_ADD_PROCS (prte_daemon_cmd_flag_t) 30

/* for debug purposes, get stack traces from all application procs */
#define PRTE_DAEMON_GET_STACK_TRACES (prte_daemon_cmd_flag_t) 31

/* for memory profiling */
#define PRTE_DAEMON_GET_MEMPROFILE (prte_daemon_cmd_flag_t) 32

/* request full topology string */
#define PRTE_DAEMON_REPORT_TOPOLOGY_CMD (prte_daemon_cmd_flag_t) 33

/* tell DVM daemons to cleanup resources from job */
#define PRTE_DAEMON_DVM_CLEANUP_JOB_CMD (prte_daemon_cmd_flag_t) 34

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
} prte_odls_pipe_err_msg_t;

/*
 * Max length of strings from the prte_odls_pipe_err_msg_t
 */
#define PRTE_ODLS_MAX_FILE_LEN  511
#define PRTE_ODLS_MAX_TOPIC_LEN PRTE_ODLS_MAX_FILE_LEN

END_C_DECLS

#endif

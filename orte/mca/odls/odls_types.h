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
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_MCA_ODLS_TYPES_H
#define ORTE_MCA_ODLS_TYPES_H

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/class/opal_ring_buffer.h"
#include "opal/dss/dss_types.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/event/event.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/mca/grpcomm/grpcomm_types.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

/* define the orted command flag type */
typedef uint8_t orte_daemon_cmd_flag_t;
#define ORTE_DAEMON_CMD_T   OPAL_UINT8

    
/*
 * Definitions needed for communication
 */
#define ORTE_DAEMON_CONTACT_QUERY_CMD       (orte_daemon_cmd_flag_t) 1
#define ORTE_DAEMON_KILL_LOCAL_PROCS        (orte_daemon_cmd_flag_t) 2
#define ORTE_DAEMON_SIGNAL_LOCAL_PROCS      (orte_daemon_cmd_flag_t) 3
#define ORTE_DAEMON_ADD_LOCAL_PROCS         (orte_daemon_cmd_flag_t) 4
#define ORTE_DAEMON_TREE_SPAWN              (orte_daemon_cmd_flag_t) 5
#define ORTE_DAEMON_HEARTBEAT_CMD           (orte_daemon_cmd_flag_t) 6
#define ORTE_DAEMON_EXIT_CMD                (orte_daemon_cmd_flag_t) 7
#define ORTE_DAEMON_PROCESS_AND_RELAY_CMD   (orte_daemon_cmd_flag_t) 9
#define ORTE_DAEMON_MESSAGE_LOCAL_PROCS     (orte_daemon_cmd_flag_t) 10
#define ORTE_DAEMON_NULL_CMD                (orte_daemon_cmd_flag_t) 11
#define ORTE_DAEMON_SYNC_BY_PROC            (orte_daemon_cmd_flag_t) 12
#define ORTE_DAEMON_SYNC_WANT_NIDMAP        (orte_daemon_cmd_flag_t) 13

/* commands for use by tools */
#define ORTE_DAEMON_REPORT_JOB_INFO_CMD     (orte_daemon_cmd_flag_t) 14
#define ORTE_DAEMON_REPORT_NODE_INFO_CMD    (orte_daemon_cmd_flag_t) 15
#define ORTE_DAEMON_REPORT_PROC_INFO_CMD    (orte_daemon_cmd_flag_t) 16
#define ORTE_DAEMON_SPAWN_JOB_CMD           (orte_daemon_cmd_flag_t) 17
#define ORTE_DAEMON_TERMINATE_JOB_CMD       (orte_daemon_cmd_flag_t) 18
#define ORTE_DAEMON_HALT_VM_CMD             (orte_daemon_cmd_flag_t) 19

/* request proc resource usage */
#define ORTE_DAEMON_TOP_CMD                 (orte_daemon_cmd_flag_t) 22

/* bootstrap */
#define ORTE_DAEMON_NAME_REQ_CMD            (orte_daemon_cmd_flag_t) 23
#define ORTE_DAEMON_CHECKIN_CMD             (orte_daemon_cmd_flag_t) 24
#define ORTE_TOOL_CHECKIN_CMD               (orte_daemon_cmd_flag_t) 25

/* process msg command */
#define ORTE_DAEMON_PROCESS_CMD             (orte_daemon_cmd_flag_t) 26

/* process called "errmgr.abort_procs" */
#define ORTE_DAEMON_ABORT_PROCS_CALLED      (orte_daemon_cmd_flag_t) 28


END_C_DECLS

#endif

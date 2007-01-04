/* Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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
#include "orte/orte_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* define the orted command flag type */
typedef uint8_t orte_daemon_cmd_flag_t;
#define ORTE_DAEMON_CMD_T   ORTE_UINT8

    
/*
 * Definitions needed for communication
 */
#define ORTE_DAEMON_HOSTFILE_CMD            (orte_daemon_cmd_flag_t) 1
#define ORTE_DAEMON_SCRIPTFILE_CMD          (orte_daemon_cmd_flag_t) 2
#define ORTE_DAEMON_CONTACT_QUERY_CMD       (orte_daemon_cmd_flag_t) 3
#define ORTE_DAEMON_KILL_LOCAL_PROCS        (orte_daemon_cmd_flag_t) 4
#define ORTE_DAEMON_SIGNAL_LOCAL_PROCS      (orte_daemon_cmd_flag_t) 5
#define ORTE_DAEMON_ADD_LOCAL_PROCS			(orte_daemon_cmd_flag_t) 6
#define ORTE_DAEMON_HEARTBEAT_CMD           (orte_daemon_cmd_flag_t) 7
#define ORTE_DAEMON_EXIT_CMD                (orte_daemon_cmd_flag_t) 8
#define ORTE_DAEMON_HALT_VM_CMD             (orte_daemon_cmd_flag_t) 9


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

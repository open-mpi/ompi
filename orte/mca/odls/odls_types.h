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
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "opal/dss/dss_types.h"
#include "orte/mca/grpcomm/grpcomm_types.h"
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
#define ORTE_DAEMON_EXIT_WITH_REPLY_CMD     (orte_daemon_cmd_flag_t) 7
#define ORTE_DAEMON_EXIT_NO_REPLY_CMD       (orte_daemon_cmd_flag_t) 8
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
    
/* collective-based cmds */
#define ORTE_DAEMON_COLL_CMD                (orte_daemon_cmd_flag_t) 23

/* proc termination sync cmds */
#define ORTE_DAEMON_WAITPID_FIRED           (orte_daemon_cmd_flag_t) 25
#define ORTE_DAEMON_IOF_COMPLETE            (orte_daemon_cmd_flag_t) 26

/* List object to locally store the process names and pids of
 * our children. This can subsequently be used to order termination
 * or pass signals without looking the info up again.
 */
typedef struct {
    opal_list_item_t super;      /* required to place this on a list */
    orte_process_name_t *name;   /* the OpenRTE name of the proc */
    pid_t pid;                   /* local pid of the proc */
    orte_std_cntr_t app_idx;     /* index of the app_context for this proc */
    bool alive;                  /* is this proc alive? */
    bool coll_recvd;             /* collective operation recvd */
    orte_proc_state_t state;     /* the state of the process */
    orte_exit_code_t exit_code;  /* process exit code */
    char *rml_uri;               /* contact info for this child */
    char *slot_list;             /* list of slots for this child */
    bool waitpid_recvd;          /* waitpid has detected proc termination */
    bool iof_complete;           /* IOF has noted proc terminating all channels */
} orte_odls_child_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_odls_child_t);

/*
 * List object to locally store job related info
 */
typedef struct orte_odls_job_t {
    opal_list_item_t        super;                  /* required to place this on a list */
    orte_jobid_t            jobid;                  /* jobid for this data */
    orte_app_context_t      **apps;                 /* app_contexts for this job */
    orte_std_cntr_t         num_apps;               /* number of app_contexts */
    orte_mapping_policy_t   policy;                 /* mapping policy */
    int16_t                 cpus_per_rank;          /* number of cpus/rank */
    int16_t                 stride;                 /* step size between cores of multi-core/rank procs */
    orte_job_controls_t     controls;               /* control flags for job */
    orte_vpid_t             stdin_target;           /* where stdin is to go */
    orte_std_cntr_t         total_slots_alloc;
    orte_vpid_t             num_procs;
    int32_t                 num_local_procs;
    opal_value_array_t      procmap;                /* map of procs/node, local ranks */
    opal_byte_object_t      *pmap;                  /* byte object version of procmap */
    opal_buffer_t           collection_bucket;
    opal_buffer_t           local_collection;
    orte_grpcomm_coll_t     collective_type;
    int32_t                 num_contributors;
    int                     num_participating;
    int                     num_collected;
} orte_odls_job_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_odls_job_t);

END_C_DECLS

#endif

/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_PLM_TYPES_H
#define PRTE_PLM_TYPES_H

#include "prte_config.h"
#include "types.h"

BEGIN_C_DECLS

/*
 * Process exit codes
 */

typedef int32_t prte_exit_code_t;

/*
 * Process state codes
 */

#define PRTE_PROC_STATE_ANY 0xffff

#define PRTE_PROC_STATE_UNDEF                0 /* undefined process state */
#define PRTE_PROC_STATE_INIT                 1 /* process entry has been created by rmaps */
#define PRTE_PROC_STATE_RESTART              2 /* the proc is ready for restart */
#define PRTE_PROC_STATE_TERMINATE            3 /* process is marked for termination */
#define PRTE_PROC_STATE_RUNNING              4 /* daemon has locally fork'd process */
#define PRTE_PROC_STATE_REGISTERED           5 /* proc registered sync */
#define PRTE_PROC_STATE_IOF_COMPLETE         6 /* io forwarding pipes have closed */
#define PRTE_PROC_STATE_WAITPID_FIRED        7 /* waitpid fired on process */
#define PRTE_PROC_STATE_MODEX_READY          8 /* all modex info has been stored */
#define PRTE_PROC_STATE_READY_FOR_DEBUG      9 /* ready for debug */

/*
 * Define a "boundary" so we can easily and quickly determine
 * if a proc is still running or not - any value less than
 * this one means that we are not terminated
 */
#define PRTE_PROC_STATE_UNTERMINATED        15

#define PRTE_PROC_STATE_TERMINATED          20 /* process has terminated and is no longer running */
/* Define a boundary so we can easily and quickly determine
 * if a proc abnormally terminated - leave a little room
 * for future expansion
 */
#define PRTE_PROC_STATE_ERROR               50
/* Define specific error code values */
#define PRTE_PROC_STATE_KILLED_BY_CMD           (PRTE_PROC_STATE_ERROR +  1) /* process was killed by PRTE cmd */
#define PRTE_PROC_STATE_ABORTED                 (PRTE_PROC_STATE_ERROR +  2) /* process aborted */
#define PRTE_PROC_STATE_FAILED_TO_START         (PRTE_PROC_STATE_ERROR +  3) /* process failed to start */
#define PRTE_PROC_STATE_ABORTED_BY_SIG          (PRTE_PROC_STATE_ERROR +  4) /* process aborted by signal */
#define PRTE_PROC_STATE_TERM_WO_SYNC            (PRTE_PROC_STATE_ERROR +  5) /* process exit'd w/o required sync */
#define PRTE_PROC_STATE_COMM_FAILED             (PRTE_PROC_STATE_ERROR +  6) /* process communication has failed */
#define PRTE_PROC_STATE_SENSOR_BOUND_EXCEEDED   (PRTE_PROC_STATE_ERROR +  7) /* process exceeded a sensor limit */
#define PRTE_PROC_STATE_CALLED_ABORT            (PRTE_PROC_STATE_ERROR +  8) /* process called "errmgr.abort" \
                                                                  */
#define PRTE_PROC_STATE_HEARTBEAT_FAILED        (PRTE_PROC_STATE_ERROR +  9) /* heartbeat failed to arrive */
#define PRTE_PROC_STATE_MIGRATING               (PRTE_PROC_STATE_ERROR + 10) /* process failed and is waiting for resources before restarting \
                                  */
#define PRTE_PROC_STATE_CANNOT_RESTART          (PRTE_PROC_STATE_ERROR + 11) /* process failed and cannot be restarted */
#define PRTE_PROC_STATE_TERM_NON_ZERO           (PRTE_PROC_STATE_ERROR + 12) /* process exited with a non-zero status, indicating abnormal */
#define PRTE_PROC_STATE_FAILED_TO_LAUNCH        (PRTE_PROC_STATE_ERROR + 13) /* unable to launch process \
                                                                       */
#define PRTE_PROC_STATE_UNABLE_TO_SEND_MSG      (PRTE_PROC_STATE_ERROR + 14) /* unable to send a message */
#define PRTE_PROC_STATE_LIFELINE_LOST           (PRTE_PROC_STATE_ERROR + 15) /* connection to lifeline lost \
                                                                    */
#define PRTE_PROC_STATE_NO_PATH_TO_TARGET       (PRTE_PROC_STATE_ERROR + 16) /* no path for communicating to target peer */
#define PRTE_PROC_STATE_FAILED_TO_CONNECT       (PRTE_PROC_STATE_ERROR + 17) /* unable to connect to target peer */
#define PRTE_PROC_STATE_PEER_UNKNOWN            (PRTE_PROC_STATE_ERROR + 18) /* unknown peer */

/* Define a boundary so that external developers
 * have a starting point for defining their own
 * proc states
 */
#define PRTE_PROC_STATE_DYNAMIC 100

/*
 * App_context state codes
 */
typedef int32_t prte_app_state_t;

#define PRTE_APP_STATE_UNDEF      0
#define PRTE_APP_STATE_INIT       1
#define PRTE_APP_STATE_ALL_MAPPED 2
#define PRTE_APP_STATE_RUNNING    3
#define PRTE_APP_STATE_COMPLETED  4

/*
 * Job state codes
 */

typedef int32_t prte_job_state_t;
#define PRTE_JOB_STATE_ANY INT_MAX

#define PRTE_JOB_STATE_UNDEF                 0
#define PRTE_JOB_STATE_INIT                  1 /* ready to be assigned id */
#define PRTE_JOB_STATE_INIT_COMPLETE         2 /* jobid assigned and setup */
#define PRTE_JOB_STATE_ALLOCATE              3 /* ready to be allocated */
#define PRTE_JOB_STATE_ALLOCATION_COMPLETE   4 /* allocation completed */
#define PRTE_JOB_STATE_MAP                   5 /* ready to be mapped */
#define PRTE_JOB_STATE_MAP_COMPLETE          6 /* mapping complete */
#define PRTE_JOB_STATE_SYSTEM_PREP           7 /* ready for final sanity check and system values updated */
#define PRTE_JOB_STATE_LAUNCH_DAEMONS        8  /* ready to launch daemons */
#define PRTE_JOB_STATE_DAEMONS_LAUNCHED      9  /* daemons for this job have been launched */
#define PRTE_JOB_STATE_DAEMONS_REPORTED      10 /* all launched daemons have reported */
#define PRTE_JOB_STATE_VM_READY              11 /* the VM is ready for operation */
#define PRTE_JOB_STATE_LAUNCH_APPS           12 /* ready to launch apps */
#define PRTE_JOB_STATE_SEND_LAUNCH_MSG       13 /* send launch msg to daemons */
#define PRTE_JOB_STATE_RUNNING               14 /* all procs have been fork'd */
#define PRTE_JOB_STATE_SUSPENDED             15 /* job has been suspended */
#define PRTE_JOB_STATE_REGISTERED            16 /* all procs registered for sync */
#define PRTE_JOB_STATE_LOCAL_LAUNCH_COMPLETE 18 /* all local procs have attempted launch */
#define PRTE_JOB_STATE_READY_FOR_DEBUG       19 /* all local procs report ready for debug */
#define PRTE_JOB_STATE_STARTED               20 /* first process has been started */

/*
 * Define a "boundary" so we can easily and quickly determine
 * if a job is still running or not - any value less than
 * this one means that we are not terminated
 */
#define PRTE_JOB_STATE_UNTERMINATED 30

#define PRTE_JOB_STATE_TERMINATED           31 /* all processes have terminated and job is no longer running */
#define PRTE_JOB_STATE_ALL_JOBS_COMPLETE    32
#define PRTE_JOB_STATE_DAEMONS_TERMINATED   33
#define PRTE_JOB_STATE_NOTIFY_COMPLETED     34 /* callback to notify when job completes */
#define PRTE_JOB_STATE_NOTIFIED             35

/* Define a boundary so we can easily and quickly determine
 * if a job abnormally terminated - leave a little room
 * for future expansion
 */
#define PRTE_JOB_STATE_ERROR 50
/* Define specific error code values */
#define PRTE_JOB_STATE_KILLED_BY_CMD            (PRTE_JOB_STATE_ERROR + 1) /* job was killed by PRTE cmd */
#define PRTE_JOB_STATE_ABORTED                  (PRTE_JOB_STATE_ERROR + 2) /* at least one process aborted, causing job to abort */
#define PRTE_JOB_STATE_FAILED_TO_START          (PRTE_JOB_STATE_ERROR + 3) /* at least one process failed to start */
#define PRTE_JOB_STATE_ABORTED_BY_SIG           (PRTE_JOB_STATE_ERROR + 4) /* job was killed by a signal */
#define PRTE_JOB_STATE_ABORTED_WO_SYNC          (PRTE_JOB_STATE_ERROR + 5) /* job was aborted because proc exit'd w/o required sync */
#define PRTE_JOB_STATE_COMM_FAILED              (PRTE_JOB_STATE_ERROR + 6) /* communication has failed */
#define PRTE_JOB_STATE_SENSOR_BOUND_EXCEEDED    (PRTE_JOB_STATE_ERROR + 7) /* job had a process that exceeded a sensor limit */
#define PRTE_JOB_STATE_CALLED_ABORT             (PRTE_JOB_STATE_ERROR + 8) /* at least one process called "errmgr.abort" */
#define PRTE_JOB_STATE_HEARTBEAT_FAILED         (PRTE_JOB_STATE_ERROR + 9) /* heartbeat failed to arrive \
                                                                    */
#define PRTE_JOB_STATE_NEVER_LAUNCHED           (PRTE_JOB_STATE_ERROR + 10) /* the job never even attempted to launch due to \
                                                                             * an error earlier in the                       \
                                                                             * launch procedure                              \
                                                                             */
#define PRTE_JOB_STATE_ABORT_ORDERED            (PRTE_JOB_STATE_ERROR + 11) /* the processes in this job have been ordered to "die",   \
                                                                             * but may not have completed it yet. Don't order it again \
                                                                             */
#define PRTE_JOB_STATE_NON_ZERO_TERM            (PRTE_JOB_STATE_ERROR + 12) /* at least one process exited with non-zero status */
#define PRTE_JOB_STATE_FAILED_TO_LAUNCH         (PRTE_JOB_STATE_ERROR + 13)
#define PRTE_JOB_STATE_FORCED_EXIT              (PRTE_JOB_STATE_ERROR + 14)
#define PRTE_JOB_STATE_SILENT_ABORT             (PRTE_JOB_STATE_ERROR + 16) /* an error occurred and was reported elsewhere, so error out quietly */

#define PRTE_JOB_STATE_REPORT_PROGRESS          (PRTE_JOB_STATE_ERROR + 17) /* report launch progress - not an error */
#define PRTE_JOB_STATE_ALLOC_FAILED             (PRTE_JOB_STATE_ERROR + 18) /* job failed to obtain an allocation */
#define PRTE_JOB_STATE_MAP_FAILED               (PRTE_JOB_STATE_ERROR + 19) /* job failed to map */
#define PRTE_JOB_STATE_CANNOT_LAUNCH            (PRTE_JOB_STATE_ERROR + 20) /* resources were busy and so the job cannot be launched */
#define PRTE_JOB_STATE_FILES_POSN_FAILED        (PRTE_JOB_STATE_ERROR + 21)

#define PRTE_JOB_STATE_FT (PRTE_JOB_STATE_ERROR + 200)
/* define an FT event */
#define PRTE_JOB_STATE_FT_CHECKPOINT            (PRTE_JOB_STATE_FT + 1)
#define PRTE_JOB_STATE_FT_CONTINUE              (PRTE_JOB_STATE_FT + 2)
#define PRTE_JOB_STATE_FT_RESTART               (PRTE_JOB_STATE_FT + 3)

/* Define a boundary so that external developers
 * have a starting point for defining their own
 * job states
 */
#define PRTE_JOB_STATE_DYNAMIC (PRTE_JOB_STATE_ERROR + 2000)

/**
 * Node State, corresponding to the PRTE_NODE_STATE_* #defines,
 * below.  These are #defines instead of an enum because the thought
 * is that we may have lots and lots of entries of these in the
 * registry and by making this an int8_t, it's only 1 byte, whereas an
 * enum defaults to an int (probably 4 bytes).  So it's a bit of a
 * space savings.
 */
typedef int8_t prte_node_state_t;

#define PRTE_NODE_STATE_UNDEF        0 // Node is undefined
#define PRTE_NODE_STATE_UNKNOWN      1 // Node is defined but in an unknown state
#define PRTE_NODE_STATE_DOWN         2 // Node is down
#define PRTE_NODE_STATE_UP           3 // Node is up / available for use
#define PRTE_NODE_STATE_REBOOT       4 // Node is rebooting
#define PRTE_NODE_STATE_DO_NOT_USE   5 // Node is up, but not available for use for the next mapping
#define PRTE_NODE_STATE_NOT_INCLUDED 6 // Node is up, but not part of the node pool for jobs
#define PRTE_NODE_STATE_ADDED        7 // Node was dynamically added to pool

/* Define a boundary so that external developers
 * have a starting point for defining their own
 * node states
 */
#define PRTE_NODE_STATE_DYNAMIC 100

/*
 * PLM commands
 */
typedef uint8_t prte_plm_cmd_flag_t;
#define PRTE_PLM_LAUNCH_JOB_CMD         1
#define PRTE_PLM_UPDATE_PROC_STATE      2
#define PRTE_PLM_REGISTERED_CMD         3
#define PRTE_PLM_ALLOC_JOBID_CMD        4
#define PRTE_PLM_READY_FOR_DEBUG_CMD    5
#define PRTE_PLM_LOCAL_LAUNCH_COMP_CMD  6

END_C_DECLS

#endif

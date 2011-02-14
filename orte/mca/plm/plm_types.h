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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_PLM_TYPES_H
#define ORTE_PLM_TYPES_H

#include "orte_config.h"
#include "orte/types.h"



BEGIN_C_DECLS

/*
 * Process exit codes
 */

typedef int32_t orte_exit_code_t;
#define ORTE_EXIT_CODE_T OPAL_INT32

/*
 * Process state codes
 */

typedef uint32_t orte_proc_state_t;
#define ORTE_PROC_STATE_T   OPAL_UINT32

#define ORTE_PROC_STATE_UNDEF                   0x00000000  /* undefined process state */
#define ORTE_PROC_STATE_INIT                    0x00000001  /* process entry has been created by rmaps */
#define ORTE_PROC_STATE_RESTART                 0x00000002  /* the proc is ready for restart */
#define ORTE_PROC_STATE_LAUNCHED                0x00000004  /* process has been launched */
#define ORTE_PROC_STATE_TERMINATE               0x00000008  /* process is marked for termination */
#define ORTE_PROC_STATE_RUNNING                 0x00000010  /* daemon has locally fork'd process */
#define ORTE_PROC_STATE_REGISTERED              0x00000020  /* process has registered for sync */
/*
 * Define a "boundary" so we can easily and quickly determine
 * if a proc is still running or not - any value less than
 * this one means that we are not terminated
 */
#define ORTE_PROC_STATE_UNTERMINATED            0x00000040

#define ORTE_PROC_STATE_TERMINATED              0x00000080  /* process has terminated and is no longer running */
#define ORTE_PROC_STATE_KILLED_BY_CMD           0x00000100  /* process was killed by ORTE cmd */
#define ORTE_PROC_STATE_ABORTED                 0x00000200  /* process aborted */
#define ORTE_PROC_STATE_FAILED_TO_START         0x00000400  /* process failed to start */
#define ORTE_PROC_STATE_ABORTED_BY_SIG          0x00000800  /* process aborted by signal */
#define ORTE_PROC_STATE_TERM_WO_SYNC            0x00001000  /* process exit'd w/o required sync */
#define ORTE_PROC_STATE_COMM_FAILED             0x00002000  /* process communication has failed */
#define ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED   0x00004000  /* process exceeded a sensor limit */
#define ORTE_PROC_STATE_CALLED_ABORT            0x00008000  /* process called "errmgr.abort" */
#define ORTE_PROC_STATE_HEARTBEAT_FAILED        0x00010000  /* heartbeat failed to arrive */
#define ORTE_PROC_STATE_MIGRATING               0x00020000  /* process is migrating */
#define ORTE_PROC_STATE_CANNOT_RESTART          0x00040000  /* process failed and cannot be restarted */

/*
 * Job state codes
 */

typedef uint32_t orte_job_state_t;
#define ORTE_JOB_STATE_T    OPAL_UINT32

#define ORTE_JOB_STATE_UNDEF                    0x00000000
#define ORTE_JOB_STATE_INIT                     0x00000001  /* job entry has been created by rmaps */
#define ORTE_JOB_STATE_RESTART                  0x00000002  /* the job is ready for restart after one or more procs failed */
#define ORTE_JOB_STATE_LAUNCHED                 0x00000004  /* job has been launched by plm */
#define ORTE_JOB_STATE_RUNNING                  0x00000008  /* all process have been fork'd */
#define ORTE_JOB_STATE_SUSPENDED                0x00000010  /* job has been suspended */
#define ORTE_JOB_STATE_REGISTERED               0x00000020  /* all procs registered for sync */
/*
 * Define a "boundary" so we can easily and quickly determine
 * if a job is still running or not - any value less than
 * this one means that we are not terminated
 */
#define ORTE_JOB_STATE_UNTERMINATED             0x00000040

#define ORTE_JOB_STATE_TERMINATED               0x00000080  /* all processes have terminated and is no longer running */
#define ORTE_JOB_STATE_ABORTED                  0x00000100  /* at least one process aborted, causing job to abort */
#define ORTE_JOB_STATE_FAILED_TO_START          0x00000200  /* at least one process failed to start */
#define ORTE_JOB_STATE_ABORTED_BY_SIG           0x00000400  /* job was killed by a signal */
#define ORTE_JOB_STATE_ABORTED_WO_SYNC          0x00000800  /* job was aborted because proc exit'd w/o required sync */
#define ORTE_JOB_STATE_KILLED_BY_CMD            0x00001000  /* job was killed by ORTE cmd */
#define ORTE_JOB_STATE_COMM_FAILED              0x00002000  /* communication has failed */
#define ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED    0x00004000  /* job had a process that exceeded a sensor limit */
#define ORTE_JOB_STATE_CALLED_ABORT             0x00008000  /* at least one process called "errmgr.abort" */
#define ORTE_JOB_STATE_HEARTBEAT_FAILED         0x00010000  /* heartbeat failed to arrive */
#define ORTE_JOB_STATE_PROCS_MIGRATING          0x00020000  /* procs waiting to migrate */

/* the job never even attempted to launch due to an error earlier in the
 * launch procedure
 */
#define ORTE_JOB_STATE_NEVER_LAUNCHED           0x10000000

/* the processes in this job have been ordered to "die", but may not have completed it yet. Don't order it again */
#define ORTE_JOB_STATE_ABORT_ORDERED            0x20010000


/**
* Node State, corresponding to the ORTE_NODE_STATE_* #defines,
 * below.  These are #defines instead of an enum because the thought
 * is that we may have lots and lots of entries of these in the
 * registry and by making this an int8_t, it's only 1 byte, whereas an
 * enum defaults to an int (probably 4 bytes).  So it's a bit of a
 * space savings.
 */
typedef int8_t orte_node_state_t;
#define ORTE_NODE_STATE_T OPAL_INT8

/** Node is in an unknown state (see orte_node_state_t) */
#define ORTE_NODE_STATE_UNKNOWN        0
/** Node is down (see orte_node_state_t) */
#define ORTE_NODE_STATE_DOWN           1
/** Node is up / available for use (see orte_node_state_t) */
#define ORTE_NODE_STATE_UP             2
/** Node is rebooting (only some systems will support this; see
orte_node_state_t) */
#define ORTE_NODE_STATE_REBOOT         3
/** Node is up, but not available for use for the next mapping */
#define ORTE_NODE_STATE_DO_NOT_USE     4
/** Node is up, but not part of the node pool for jobs */
#define ORTE_NODE_STATE_NOT_INCLUDED   5

/*
 * PLM commands
 */
typedef uint8_t orte_plm_cmd_flag_t;
#define ORTE_PLM_CMD    OPAL_UINT8
#define ORTE_PLM_LAUNCH_JOB_CMD         1
#define ORTE_PLM_UPDATE_PROC_STATE      2
#define ORTE_PLM_INIT_ROUTES_CMD        3

END_C_DECLS

#endif

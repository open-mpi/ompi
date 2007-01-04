/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#ifndef ORTE_SOH_TYPES_H
#define ORTE_SOH_TYPES_H

#include "orte_config.h"

#include "opal/class/opal_list.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Process exit codes
 */

typedef int orte_exit_code_t;

/*
 * Process state codes
 */

typedef uint16_t orte_proc_state_t;
#define ORTE_PROC_STATE_T   ORTE_UINT16

#define ORTE_PROC_STATE_UNDEF               0x0000  /* undefined process state */
#define ORTE_PROC_STATE_INIT                0x0001  /* process entry has been created by rmaps */
#define ORTE_PROC_STATE_LAUNCHED            0x0002  /* process has been launched by pls */
#define ORTE_PROC_STATE_AT_STG1             0x0004  /* process is at Stage Gate 1 barrier in orte_init */
#define ORTE_PROC_STATE_AT_STG2             0x0008  /* process is at Stage Gate 2 barrier in orte_init */
#define ORTE_PROC_STATE_RUNNING             0x0010  /* process has exited orte_init and is running */
#define ORTE_PROC_STATE_AT_STG3             0x0020  /* process is at Stage Gate 3 barrier in orte_finalize */
#define ORTE_PROC_STATE_FINALIZED           0x0040  /* process has completed orte_finalize and is running */
#define ORTE_PROC_STATE_TERMINATED          0x0080  /* process has terminated and is no longer running */
#define ORTE_PROC_STATE_ABORTED             0x0100  /* process aborted */
#define ORTE_PROC_STATE_FAILED_TO_START     0x0200  /* process failed to start */
/* this process has been ordered to "die", but may not have completed it yet. Don't tell it again */
#define ORTE_PROC_STATE_ABORT_ORDERED       0x0400  

/** define some common shorthands for when we want to be alerted */
#define ORTE_PROC_STATE_ALL            0xffff   /* alert on ALL triggers */
#define ORTE_PROC_STAGE_GATES_ONLY     ORTE_PROC_STATE_AT_STG1 | ORTE_PROC_STATE_AT_STG2 | ORTE_PROC_STATE_AT_STG3 | ORTE_PROC_STATE_FINALIZED
#define ORTE_PROC_STATE_NONE           0x0000   /* don't alert on any triggers */

/*
 * Job state codes
 */

typedef uint16_t orte_job_state_t;
#define ORTE_JOB_STATE_T    ORTE_UINT16

#define ORTE_JOB_STATE_INIT                 0x0001  /* job entry has been created by rmaps */
#define ORTE_JOB_STATE_LAUNCHED             0x0002  /* job has been launched by pls */
#define ORTE_JOB_STATE_AT_STG1              0x0004  /* all processes are at Stage Gate 1 barrier in orte_init */
#define ORTE_JOB_STATE_AT_STG2              0x0008  /* all processes are at Stage Gate 2 barrier in orte_init */
#define ORTE_JOB_STATE_RUNNING              0x0010  /* all processes have exited orte_init and is running */
#define ORTE_JOB_STATE_AT_STG3              0x0020  /* all processes are at Stage Gate 3 barrier in orte_finalize */
#define ORTE_JOB_STATE_FINALIZED            0x0040  /* all processes have completed orte_finalize and is running */
#define ORTE_JOB_STATE_TERMINATED           0x0080  /* all processes have terminated and is no longer running */
#define ORTE_JOB_STATE_ABORTED              0x0100  /* at least one process aborted, causing job to abort */
#define ORTE_JOB_STATE_FAILED_TO_START      0x0200  /* at least one process failed to start */
/* the processes in this job have been ordered to "die", but may not have completed it yet. Don't order it again */
#define ORTE_JOB_STATE_ABORT_ORDERED       0x0400  

/**
 * Node State, corresponding to the ORTE_NODE_STATE_* #defines,
 * below.  These are #defines instead of an enum because the thought
 * is that we may have lots and lots of entries of these in the
 * registry and by making this an int8_t, it's only 1 byte, whereas an
 * enum defaults to an int (probably 4 bytes).  So it's a bit of a
 * space savings.
 */
typedef int8_t orte_node_state_t;

/** Node is in an unknown state (see orte_node_state_t) */
#define ORTE_NODE_STATE_UNKNOWN  0x00
/** Node is down (see orte_node_state_t) */
#define ORTE_NODE_STATE_DOWN     0x01
/** Node is up / available for use (see orte_node_state_t) */
#define ORTE_NODE_STATE_UP       0x02
/** Node is rebooting (only some systems will support this; see
    orte_node_state_t) */
#define ORTE_NODE_STATE_REBOOT   0x03

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

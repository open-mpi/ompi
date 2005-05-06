/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef ORTE_SCHEMA_TYPES_H
#define ORTE_SCHEMA_TYPES_H

#include "orte_config.h"
#include "mca/ns/ns_types.h"

/*
 * This macro and array are used to output intelligible error
 * messages. It is disturbing to think that we are still outputing error numbers and
 * expecting users to look them up in the "big book" to find out what they represent.
 * This macro allows the user to output an actual string representation of the error.
 * 
 *  * THE ACTUAL ARRAY IS INSTANTIATED IN runtime/ompi_init.c
 */

#define ORTE_ERROR_NAME(n)  orte_error_strings[-1*n]
extern char *orte_error_strings[];

/*
 * ORTE SEGMENT NAMES
 * There are some predefined segments that are used across the entire ORTE system.
 * These defines establish those names so everyone can access them, and so they
 * can be easily changed if required.
 */
#define ORTE_JOB_SEGMENT        "orte-job"
#define ORTE_NODE_SEGMENT       "orte-node"
#define ORTE_RESOURCE_SEGMENT   "orte-resources"

/*
 * ORTE pre-defined tokens for special containers
 */
#define ORTE_JOB_GLOBALS        "orte-job-globals"

/*
 * ORTE-wide key names for storing/retrieving data from the registry.
 * Subsystem-specific keys will be defined in each=/ subsystem's xxx_types.h file.
 */
#define ORTE_CELLID_KEY             "orte-cellid"
#define ORTE_JOBID_KEY              "orte-jobid"
#define ORTE_VPID_KEY               "orte-vpid"
#define ORTE_NODE_NAME_KEY          "orte-node-name"
#define ORTE_NODE_ARCH_KEY          "orte-node-arch"
#define ORTE_NODE_STATE_KEY         "orte-node-state"
#define ORTE_NODE_SLOTS_KEY         "orte-node-slots"
#define ORTE_NODE_SLOTS_ALLOC_KEY   "orte-node-slots-alloc"
#define ORTE_NODE_SLOTS_MAX_KEY     "orte-node-slots-max"
#define ORTE_NODE_ALLOC_KEY         "orte-node-alloc"
#define ORTE_NODE_BOOTPROXY_KEY     "orte-node-bootproxy"
#define ORTE_JOB_APP_CONTEXT_KEY    "orte-job-app-context"
#define ORTE_JOB_SLOTS_KEY          "orte-job-slots"                /**< number of procs in job */
#define ORTE_JOB_VPID_START_KEY     "orte-job-vpid-start"
#define ORTE_JOB_VPID_RANGE_KEY     "orte-job-vpid-range"
#define ORTE_JOB_IOF_KEY            "orte-job-iof"
#define ORTE_PROC_NAME_KEY          "orte-proc-name"
#define ORTE_PROC_RANK_KEY          "orte-proc-rank"
#define ORTE_PROC_PID_KEY           "orte-proc-pid"
#define ORTE_PROC_LOCAL_PID_KEY     "orte-proc-local-pid"
#define ORTE_PROC_STATE_KEY         "orte-proc-state"
#define ORTE_PROC_APP_CONTEXT_KEY   "orte-proc-app-context"
#define ORTE_PROC_EXIT_CODE_KEY     "orte-proc-exit-code"
#define ORTE_PROC_NUM_ALIVE         "orte-proc-num-alive"
#define ORTE_PROC_NUM_ABORTED       "orte-proc-num-aborted"
#define ORTE_PROC_NUM_AT_STG1       "orte-proc-num-stg1"
#define ORTE_PROC_NUM_AT_STG2       "orte-proc-num-stg2"
#define ORTE_PROC_NUM_AT_STG3       "orte-proc-num-stg3"
#define ORTE_PROC_NUM_FINALIZED     "orte-proc-num-finalized"
#define ORTE_PROC_NUM_TERMINATED    "orte-proc-num-terminated"


#endif

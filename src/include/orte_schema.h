/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_SCHEMA_H
#define ORTE_SCHEMA_H

#include "orte_config.h"
#include "include/orte_types.h"
#include "mca/ns/ns_types.h"

/*
 * ORTE_DATA_NAME macro
 * This macro is provided so that users can output an intelligible name for a data
 * type during debugging. It is called by passing the data type into the macro and
 * outputing the result via some print variant. For example, you can call it as:
 * ompi_output(0, "data type: %s", ORTE_DATA_NAME(keyval.type));
 * 
 * THE ACTUAL ARRAY IS INSTANTIATED IN runtime/ompi_init.c
 */

#define ORTE_DATA_NAME(n) orte_data_strings[n]
extern char *orte_data_strings[];

/*
 * Similar to the above, this macro and array are used to output intelligible error
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
#define ORTE_JOB_APP_CONTEXT_KEY    "orte-job-app-context"
#define ORTE_JOB_SLOTS_KEY          "orte-job-slots"                /**< number of procs in job */
#define ORTE_JOB_VPID_START_KEY     "orte-job-vpid-start"
#define ORTE_JOB_VPID_RANGE_KEY     "orte-job-vpid-range"
#define ORTE_JOB_IOF_KEY            "orte-job-iof"
#define ORTE_PROC_NAME_KEY          "orte-proc-name"
#define ORTE_PROC_RANK_KEY          "orte-proc-rank"
#define ORTE_PROC_PID_KEY           "orte-proc-pid"
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

/*
 * Convenience functions for accessing ORTE data
 */
typedef int (*orte_schema_get_proc_tokens_fn_t)(
    char ***tokens, 
    int32_t* num_tokens, 
    orte_process_name_t *proc);

typedef int (*orte_schema_get_node_tokens_fn_t)(
    char ***tokens, 
    int32_t* num_tokens, 
    orte_cellid_t cellid, 
    char *nodename);

typedef int (*orte_schema_get_cell_tokens_fn_t)(
    char ***tokens, 
    int32_t* num_tokens, 
    orte_cellid_t cellid);

typedef int (*orte_schema_get_job_segment_name_fn_t)(char **name, orte_jobid_t jobid);

typedef int (*orte_schema_extract_jobid_from_segment_name_fn_t)(orte_jobid_t *jobid, char *name);
/*
 * Base structure for the convenience functions
 */
typedef struct {
    orte_schema_get_proc_tokens_fn_t get_proc_tokens;
    orte_schema_get_node_tokens_fn_t get_node_tokens;
    orte_schema_get_cell_tokens_fn_t get_cell_tokens;
    orte_schema_get_job_segment_name_fn_t get_job_segment_name;
    orte_schema_extract_jobid_from_segment_name_fn_t extract_jobid_from_segment_name;
} orte_schema_t;

OMPI_DECLSPEC extern orte_schema_t orte_schema;

/*
 * Open function to ensure orte_schema is loaded
 */
int orte_schema_open(void);

#endif

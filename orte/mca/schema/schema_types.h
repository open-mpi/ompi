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

#ifndef ORTE_SCHEMA_TYPES_H
#define ORTE_SCHEMA_TYPES_H

#include "orte_config.h"
#include "orte/mca/ns/ns_types.h"
#include "opal/util/error.h"

/**
* Standard characters used in ORTE
 */
#define ORTE_SCHEMA_DELIMITER_CHAR      '.'
#define ORTE_SCHEMA_DELIMITER_STRING    "."
#define ORTE_SCHEMA_WILDCARD_CHAR       '*'
#define ORTE_SCHEMA_WILDCARD_STRING     "*"
#define ORTE_SCHEMA_INVALID_CHAR        '$'
#define ORTE_SCHEMA_INVALID_STRING      "$"

/*
 * Standard names used across the system
 */
#define ORTE_DEFAULT_UNIVERSE    "default-universe"

/*
 * ORTE SEGMENT NAMES
 * There are some predefined segments that are used across the entire ORTE system.
 * These defines establish those names so everyone can access them, and so they
 * can be easily changed if required.
 */
#define ORTE_JOB_SEGMENT        "orte-job"
#define ORTE_NODE_SEGMENT       "orte-node"
#define ORTE_JOBINFO_SEGMENT    "orte-active-jobs"
#define ORTE_RESOURCE_SEGMENT   "orte-resources"

/*
 * ORTE pre-defined tokens for special containers
 */
#define ORTE_JOB_GLOBALS        "orte-job-globals"

/*
 * ORTE-wide key names for storing/retrieving data from the registry.
 * Subsystem-specific keys will be defined in each=/ subsystem's xxx_types.h file.
 */
#define ORTE_CELLID_KEY                         "orte-cellid"
#define ORTE_JOBGRP_KEY                         "orte-jobgrp"
#define ORTE_JOBID_KEY                          "orte-jobid"
#define ORTE_VPID_KEY                           "orte-vpid"

/* NODE specific keys */
#define ORTE_NODE_NAME_KEY                      "orte-node-name"
#define ORTE_NODE_LAUNCH_ID_KEY                 "orte-node-launch-id"
#define ORTE_NODE_ARCH_KEY                      "orte-node-arch"
#define ORTE_NODE_STATE_KEY                     "orte-node-state"
#define ORTE_NODE_SLOTS_KEY                     "orte-node-slots"
#define ORTE_NODE_SLOTS_ALLOC_KEY               "orte-node-slots-alloc"
#define ORTE_NODE_SLOTS_IN_USE_KEY              "orte-node-slots-in-use"
#define ORTE_NODE_SLOTS_MAX_KEY                 "orte-node-slots-max"
#define ORTE_NODE_ALLOC_KEY                     "orte-node-alloc"
#define ORTE_NODE_BOOTPROXY_KEY                 "orte-node-bootproxy"
#define ORTE_NODE_USERNAME_KEY                  "orte-node-username"
#define ORTE_NODE_OVERSUBSCRIBED_KEY            "orte-node-oversubscribed"

/* JOB specific keys */
#define ORTE_JOB_APP_CONTEXT_KEY                "orte-job-app-context"
#define ORTE_JOB_SLOTS_KEY                      "orte-job-slots"                /**< number of procs in job */
#define ORTE_JOB_VPID_START_KEY                 "orte-job-vpid-start"
#define ORTE_JOB_VPID_RANGE_KEY                 "orte-job-vpid-range"
#define ORTE_JOB_OVERSUBSCRIBE_OVERRIDE_KEY     "orte-job-override-oversubscribe"
#define ORTE_JOB_TOTAL_SLOTS_ALLOC_KEY          "orte-job-total-slots"
#define ORTE_JOB_IOF_KEY                        "orte-job-iof"
#define ORTE_JOB_STATE_KEY                      "orte-job-state"
#define ORTE_JOB_MAPPING_MODE_KEY               "orte-job-mapping-mode"

/* PROCESS specific keys */
#define ORTE_PROC_NAME_KEY                      "orte-proc-name"
#define ORTE_PROC_RANK_KEY                      "orte-proc-rank"
#define ORTE_PROC_PID_KEY                       "orte-proc-pid"
#define ORTE_PROC_LOCAL_PID_KEY                 "orte-proc-local-pid"
#define ORTE_PROC_STATE_KEY                     "orte-proc-state"
#define ORTE_PROC_APP_CONTEXT_KEY               "orte-proc-app-context"
#define ORTE_PROC_EXIT_CODE_KEY                 "orte-proc-exit-code"
#define ORTE_PROC_NUM_ALIVE                     "orte-proc-num-alive"
#define ORTE_PROC_NUM_ABORTED                   "orte-proc-num-aborted"
#define ORTE_PROC_NUM_FAILED_START              "orte-proc-num-failed-start"
#define ORTE_PROC_NUM_AT_INIT                   "orte-proc-num-init"
#define ORTE_PROC_NUM_LAUNCHED                  "orte-proc-num-launched"
#define ORTE_PROC_NUM_RUNNING                   "orte-proc-num-running"
#define ORTE_PROC_NUM_AT_STG1                   "orte-proc-num-stg1"
#define ORTE_PROC_NUM_AT_STG2                   "orte-proc-num-stg2"
#define ORTE_PROC_NUM_AT_STG3                   "orte-proc-num-stg3"
#define ORTE_PROC_NUM_FINALIZED                 "orte-proc-num-finalized"
#define ORTE_PROC_NUM_TERMINATED                "orte-proc-num-terminated"
#define ORTE_PROC_RML_IP_ADDRESS_KEY            "orte-proc-rml-ip-addr"

/*
 * ORTE-wide names for specific system triggers and subscriptions
 */
#define ORTE_ALL_INIT_TRIGGER           "orte-init-trig"
#define ORTE_ALL_LAUNCHED_TRIGGER       "orte-launch-trig"
#define ORTE_ALL_RUNNING_TRIGGER        "orte-running-trig"
#define ORTE_STG1_TRIGGER               "orte-stage1"
#define ORTE_STG2_TRIGGER               "orte-stage2"
#define ORTE_STG3_TRIGGER               "orte-stage3"
#define ORTE_NUM_FINALIZED_TRIGGER      "orte-num-finalized"
#define ORTE_NUM_ABORTED_TRIGGER        "orte-num-aborted"
#define ORTE_NUM_TERMINATED_TRIGGER     "orte-num-terminated"
#define ORTE_FAILED_TO_START_TRIGGER    "orte-failed-start-trig"

/*
 * ORTED (ORTE DAEMON) TRIGGER DEFINITIONS
 */
#define ORTED_LAUNCH_STAGE_GATE_TRIGGER     "orted-launch-gate"
#define ORTED_LAUNCH_STG_SUB                "orted-launch-sub"
#define ORTED_LAUNCH_STAGE_GATE_CNTR        "orted-num-at-launch-gate"
#define ORTED_NUM_TO_BE_LAUNCHED            "orted-num-to-be-launched"

/*
 * BPROC-SPECIFIC SEGMENT FOR STORING CLUSTER-WIDE NODE STATES
 * This obviously shouldn't be in a general ORTE schema file. However,
 * it is a temporary requirement until we can install the ORTE 2.0
 * schema - and the definition needs to be in some central location
 * where the various bproc components can locate it
 */
#define ORTE_BPROC_NODE_SEGMENT     "orte-node-bproc-segment"

#endif

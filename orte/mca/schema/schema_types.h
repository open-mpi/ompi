/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#define ORTE_PROC_SEGMENT       "orte-proc"
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
#define ORTE_NODE_NUM_PROCS_KEY                 "orte-node-num-procs"

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
#define ORTE_JOB_PARENT_JOBID_KEY               "orte-job-parent-jobid"
#define ORTE_JOB_NUM_NEW_DAEMONS_KEY            "orte-job-num-new-daemons"
#define ORTE_JOB_DAEMON_VPID_START_KEY          "orte-job-daemon-vpid-start"
#define ORTE_JOB_BEING_LAUNCHED_KEY             "orte-job-being-launched"

/* PROCESS specific keys */
#define ORTE_PROC_NAME_KEY                      "orte-proc-name"
#define ORTE_PROC_RANK_KEY                      "orte-proc-rank"
#define ORTE_PROC_MAPED_RANK_KEY                "orte-proc-maped-rank"
#define ORTE_PROC_LOCAL_RANK_KEY                "orte-proc-local-rank"
#define ORTE_PROC_PID_KEY                       "orte-proc-pid"
#define ORTE_PROC_LOCAL_PID_KEY                 "orte-proc-local-pid"
#define ORTE_PROC_STATE_KEY                     "orte-proc-state"
#define ORTE_PROC_APP_CONTEXT_KEY               "orte-proc-app-context"
#define ORTE_PROC_EXIT_CODE_KEY                 "orte-proc-exit-code"
#define ORTE_PROC_NUM_ALIVE                     "orte-proc-num-alive"
#define ORTE_PROC_NUM_ABORTED                   "orte-proc-num-aborted"
#define ORTE_PROC_NUM_FAILED_START              "orte-proc-num-failed-start"
#define ORTE_PROC_NUM_AT_ORTE_STARTUP           "orte-proc-num-orte-startup"
#define ORTE_PROC_NUM_AT_INIT                   "orte-proc-num-init"
#define ORTE_PROC_NUM_LAUNCHED                  "orte-proc-num-launched"
#define ORTE_PROC_NUM_RUNNING                   "orte-proc-num-running"
#define ORTE_PROC_NUM_AT_STG1                   "orte-proc-num-stg1"
#define ORTE_PROC_NUM_AT_STG2                   "orte-proc-num-stg2"
#define ORTE_PROC_NUM_AT_STG3                   "orte-proc-num-stg3"
#define ORTE_PROC_NUM_FINALIZED                 "orte-proc-num-finalized"
#define ORTE_PROC_NUM_TERMINATED                "orte-proc-num-terminated"
#define ORTE_PROC_RML_IP_ADDRESS_KEY            "orte-proc-rml-ip-addr"
#define ORTE_PROC_CPU_LIST_KEY                  "orte-proc-cpu-list"

#define ORTE_JOB_CKPT_STATE_KEY                "orte-job-ckpt-state"
#define ORTE_JOB_CKPT_SNAPSHOT_REF_KEY         "orte-job-ckpt-snapshot-ref"
#define ORTE_JOB_CKPT_SNAPSHOT_LOC_KEY         "orte-job-ckpt-snapshot-loc"

#define ORTE_PROC_CKPT_STATE_KEY                "orte-proc-ckpt-state"
#define ORTE_PROC_CKPT_SNAPSHOT_REF_KEY         "orte-proc-ckpt-snapshot-ref"
#define ORTE_PROC_CKPT_SNAPSHOT_LOC_KEY         "orte-proc-ckpt-snapshot-loc"

/*
 * ORTE-wide names for specific system triggers and subscriptions
 */

/* trigger during orte_init to allow exchange of ORTE info within a job */
#define ORTE_STARTUP_TRIGGER            "orte-startup-trig"
/* subscription to allow exchange of ORTE info between jobs */
#define ORTE_XCONNECT_SUB               "orte-xconnect-sub"
/* standard subscription required for xconnect */
#define ORTE_PARENT_JOBID_SUBSCRIPTION  "orte-parent-jobid"

/* process state triggers - fire when all procs reach corresponding point */
#define ORTE_ALL_INIT_TRIGGER           "orte-init-trig"
#define ORTE_ALL_LAUNCHED_TRIGGER       "orte-launch-trig"
#define ORTE_ALL_RUNNING_TRIGGER        "orte-running-trig"
#define ORTE_STG1_TRIGGER               "orte-stage1"
#define ORTE_STG2_TRIGGER               "orte-stage2"
#define ORTE_STG3_TRIGGER               "orte-stage3"
#define ORTE_ALL_FINALIZED_TRIGGER      "orte-finalized-trig"
#define ORTE_ALL_TERMINATED_TRIGGER     "orte-terminated-trig"

#define ORTE_JOB_CKPT_STATE_TRIGGER     "orte-job-ckpt-trig"
#define ORTE_PROC_CKPT_STATE_TRIGGER    "orte-proc-ckpt-trig"

/* exception triggers - fired when one process meets condition */
#define ORTE_NUM_ABORTED_TRIGGER        "orte-num-aborted"
#define ORTE_FAILED_TO_START_TRIGGER    "orte-failed-start-trig"


/*
 * ORTED (ORTE DAEMON) TRIGGER DEFINITIONS
 */
#define ORTED_LAUNCH_STG_SUB            "orted-launch-sub"
#define ORTED_LAUNCH_TRIGGER            "orted-launch-trig"
#define ORTED_LAUNCH_CNTR               "orted-launch-cntr"

/*
 * BPROC-SPECIFIC SEGMENT FOR STORING CLUSTER-WIDE NODE STATES
 * This obviously shouldn't be in a general ORTE schema file. However,
 * it is a temporary requirement until we can install the ORTE 2.0
 * schema - and the definition needs to be in some central location
 * where the various bproc components can locate it
 */
#define ORTE_BPROC_NODE_SEGMENT     "orte-node-bproc-segment"

#endif

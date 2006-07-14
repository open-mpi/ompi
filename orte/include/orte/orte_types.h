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
/** @file */

#ifndef ORTE_TYPES_H
#define ORTE_TYPES_H

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/**
 * Supported datatypes for messaging and storage operations.
 */

typedef uint8_t orte_data_type_t ;
#define ORTE_DSS_ID_MAX UINT8_MAX

#define    ORTE_UNDEF               (orte_data_type_t)    0 /**< type hasn't been defined yet */
#define    ORTE_BYTE                (orte_data_type_t)    1 /**< a byte of data */
#define    ORTE_BOOL                (orte_data_type_t)    2 /**< boolean */
#define    ORTE_STRING              (orte_data_type_t)    3 /**< a NULL terminated string */
#define    ORTE_SIZE                (orte_data_type_t)    4 /**< the generic size_t */
#define    ORTE_PID                 (orte_data_type_t)    5 /**< process pid */
    /* all the integer flavors */
#define    ORTE_INT                 (orte_data_type_t)    6 /**< generic integer */
#define    ORTE_INT8                (orte_data_type_t)    7 /**< an 8-bit integer */
#define    ORTE_INT16               (orte_data_type_t)    8 /**< a 16-bit integer */
#define    ORTE_INT32               (orte_data_type_t)    9 /**< a 32-bit integer */
#define    ORTE_INT64               (orte_data_type_t)   10 /**< a 64-bit integer */
    /* all the unsigned integer flavors */
#define    ORTE_UINT                (orte_data_type_t)   11 /**< generic unsigned integer */
#define    ORTE_UINT8               (orte_data_type_t)   12 /**< an 8-bit unsigned integer */
#define    ORTE_UINT16              (orte_data_type_t)   13 /**< a 16-bit unsigned integer */
#define    ORTE_UINT32              (orte_data_type_t)   14 /**< a 32-bit unsigned integer */
#define    ORTE_UINT64              (orte_data_type_t)   15 /**< a 64-bit unsigned integer */

    /* we don't support floating point types */
    
    /* orte-specific typedefs - grouped according to the subystem that handles
     * their packing/unpacking */
    /* General types - packing/unpacking handled within DSS */
#define    ORTE_BYTE_OBJECT         (orte_data_type_t)   16 /**< byte object structure */
#define    ORTE_DATA_TYPE           (orte_data_type_t)   17 /**< data type */
#define    ORTE_NULL                (orte_data_type_t)   18 /**< don't interpret data type */
#define    ORTE_DATA_VALUE          (orte_data_type_t)   19 /**< data value */
    /* Name Service types */
#define    ORTE_NAME                (orte_data_type_t)   20 /**< an ompi_process_name_t */
#define    ORTE_VPID                (orte_data_type_t)   21 /**< a vpid */
#define    ORTE_JOBID               (orte_data_type_t)   22 /**< a jobid */
#define    ORTE_JOBGRP              (orte_data_type_t)   23 /**< a job group */
#define    ORTE_CELLID              (orte_data_type_t)   24 /**< a cellid */
    /* SOH types */
#define    ORTE_NODE_STATE          (orte_data_type_t)   25 /**< node status flag */
#define    ORTE_PROC_STATE          (orte_data_type_t)   26 /**< process/resource status */
#define    ORTE_JOB_STATE           (orte_data_type_t)   27 /**< job status flag */
#define    ORTE_EXIT_CODE           (orte_data_type_t)   28 /**< process exit code */
    /* GPR types */
#define    ORTE_GPR_KEYVAL          (orte_data_type_t)   29 /**< registry key-value pair */
#define    ORTE_GPR_NOTIFY_ACTION   (orte_data_type_t)   30 /**< registry notify action */
#define    ORTE_GPR_TRIGGER_ACTION  (orte_data_type_t)   31 /**< registry trigger action */
#define    ORTE_GPR_CMD             (orte_data_type_t)   32 /**< registry command */
#define    ORTE_GPR_SUBSCRIPTION_ID (orte_data_type_t)   33 /**< registry notify id tag */
#define    ORTE_GPR_TRIGGER_ID      (orte_data_type_t)   34 /**< registry notify id tag */
#define    ORTE_GPR_VALUE           (orte_data_type_t)   35 /**< registry return value */
#define    ORTE_GPR_ADDR_MODE       (orte_data_type_t)   36 /**< Addressing mode for registry cmds */
#define    ORTE_GPR_SUBSCRIPTION    (orte_data_type_t)   37 /**< describes data returned by subscription */
#define    ORTE_GPR_TRIGGER         (orte_data_type_t)   38 /**< describes trigger conditions */
#define    ORTE_GPR_NOTIFY_DATA     (orte_data_type_t)   39 /**< data returned from a subscription */
#define    ORTE_GPR_NOTIFY_MSG      (orte_data_type_t)   40 /**< notify message containing notify_data objects */
#define    ORTE_GPR_NOTIFY_MSG_TYPE (orte_data_type_t)   41 /**< notify message type (subscription or trigger) */
    /* Resource Manager types */
#define    ORTE_APP_CONTEXT         (orte_data_type_t)   42 /**< argv and enviro arrays */
#define    ORTE_APP_CONTEXT_MAP     (orte_data_type_t)   43 /**< application context mapping array */
#define    ORTE_RAS_NODE			(orte_data_type_t)	 44 /**< node information */
    /* DAEMON communication type */
#define    ORTE_DAEMON_CMD          (orte_data_type_t)   45 /**< command flag for communicating with the daemon */

/* define the starting point for dynamically assigning data types */
#define ORTE_DSS_ID_DYNAMIC 60

/* define a structure to hold generic byte objects */
typedef struct {
    size_t size;
    uint8_t *bytes;
} orte_byte_object_t;

/* define the orted command flag type */
typedef uint16_t orte_daemon_cmd_flag_t;

#endif

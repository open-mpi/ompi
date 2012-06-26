/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#ifndef ORCA_TYPES_H
#define ORCA_TYPES_H

#include "orca_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/* For Notifier */
#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif

#include "opal/dss/dss_types.h"


/*******************************************
 * Standard counter type
 *******************************************/
typedef int32_t orca_std_cntr_t;
#define ORCA_STD_CNTR_T         OPAL_INT32
#define ORCA_STD_CNTR_MIN       INT32_MIN
#define ORCA_STD_CNTR_MAX       INT32_MAX
#define ORCA_STD_CNTR_INVALID   -1


/*******************************************
 * Rank on node
 * From ORTE:
 * Rank on node, used for both local and node rank. We
 * don't send these around on their own, so don't create
 * dedicated type support for them - we are defining them
 * here solely for readability in the code and so we have
 * one place where any future changes can be made
 *
 *******************************************/
typedef uint16_t orca_node_rank_t;
#define ORCA_NODE_RANK          OPAL_UINT16
#define ORCA_NODE_RANK_MIN      0
#define ORCA_NODE_RANK_MAX      UINT16_MAX-1
#define ORCA_NODE_RANK_INVALID  UINT16_MAX


/*******************************************
 * App. Context Index
 *******************************************/
typedef uint32_t orca_app_idx_t;
#define ORCA_APP_IDX        OPAL_UINT32
#define ORCA_APP_IDX_MIN    0
#define ORCA_APP_IDX_MAX    UINT32_MAX


/*******************************************
 * Process name strcture
 *******************************************/
/**
 * Set the allowed range for ids in each space
 *
 * NOTE: Be sure to update the ORCA_NAME_ARGS #define (above) and all
 * uses of it if these types change to be larger than (long)!  The
 * HTON and NTOH macros below must be updated, as well as the MIN /
 * MAX macros below and the datatype packing representations in
 * orca/mca/plm/base/plm_private.h
 *
 * NOTE: Be sure to keep the jobid and vpid types the same size! Due
 * to padding rules, it won't save anything to have one larger than
 * the other, and it will cause problems in the communication subsystems
 */
typedef uint32_t orca_jobid_t;
#define ORCA_JOBID_T              OPAL_UINT32
#define ORCA_JOBID_MIN            0
#define ORCA_JOBID_MAX            UINT32_MAX-2
#define ORCA_JOBID_WILDCARD       (ORCA_JOBID_MAX + 1)
#define ORCA_JOBID_INVALID        (ORCA_JOBID_MAX + 2)
#define ORCA_LOCAL_JOBID_WILDCARD (ORCA_JOBID_WILDCARD & 0x0000FFFF)
#define ORCA_LOCAL_JOBID_INVALID  (ORCA_JOBID_INVALID  & 0x0000FFFF)

typedef uint32_t orca_vpid_t;
#define ORCA_VPID_T               OPAL_UINT32
#define ORCA_VPID_MIN             0
#define ORCA_VPID_MAX             UINT32_MAX-2
#define ORCA_VPID_WILDCARD        (ORCA_VPID_MAX + 1)
#define ORCA_VPID_INVALID         (ORCA_VPID_MAX + 2)

struct orca_process_name_t {
    orca_jobid_t jobid;     /**< Job number */
    orca_vpid_t  vpid;      /**< Process id - equivalent to rank */
};
typedef struct orca_process_name_t orca_process_name_t;


/*
 * Bitmask for comparing process names
 */
typedef uint8_t  orca_name_cmp_bitmask_t;
#define ORCA_NAME_CMP_NONE       0x00
#define ORCA_NAME_CMP_JOBID      0x02
#define ORCA_NAME_CMP_VPID       0x04
#define ORCA_NAME_CMP_ALL        0x0f
#define ORCA_NAME_CMP_WILD       0x10

/*
 * Accessor operations for orca_process_name_t
 */
static inline orca_jobid_t orca_process_info_get_jobid(const orca_process_name_t* name) {
    return name->jobid;
}
static inline void orca_process_info_set_jobid(orca_process_name_t* name, orca_jobid_t jobid) {
    name->jobid = jobid;
}

static inline orca_vpid_t orca_process_info_get_vpid(const orca_process_name_t* name) {
    return name->vpid;
}
static inline void orca_process_info_set_vpid(orca_process_name_t* name, orca_vpid_t vpid) {
    name->vpid = vpid;
}

#define ORCA_PROCESS_NAME_HTON(n)       \
    do {                                     \
        n.jobid = htonl(n.jobid);            \
        n.vpid  = htonl(n.vpid);             \
    } while (0)

#define ORCA_PROCESS_NAME_NTOH(n)       \
    do {                                     \
        n.jobid = ntohl(n.jobid);            \
        n.vpid  = ntohl(n.vpid);             \
    } while (0)


/*******************************************
 * Process Information
 *******************************************/
struct orca_proc_info_t {
    orca_process_name_t my_name;        /**< My official process name */
    orca_app_idx_t app_num;             /**< our index into the app_context array */
    orca_vpid_t num_procs;              /**< number of processes in this job */
    char *nodename;                          /**< string name for this node */
    pid_t pid;                               /**< Local process ID for this process */
    char *job_session_dir;                   /**< Session directory for job */
    char *proc_session_dir;                  /**< Session directory for the process */
};
typedef struct orca_proc_info_t orca_proc_info_t;


/*******************************************
 * Out-of-Band Communication
 *******************************************/
typedef uint32_t orca_oob_tag_t;
#define ORCA_OOB_TAG_T       OPAL_UINT32
#define ORCA_OOB_TAG_MIN     1
#define ORCA_OOB_TAG_MAX     100
#define ORCA_OOB_TAG_INVALID 0

/*
 * General Masks
 */
#define ORCA_OOB_PERSISTENT       0x00000008

/*
 * Tags
 */
#define ORCA_OOB_TAG_DEBUGGER_RELEASE     32

/*
 * Collective type
 */
typedef uint32_t orca_coll_type_t;
#define ORCA_COLL_TYPE_T       OPAL_UINT32
#define ORCA_COLL_TYPE_MIN     0
#define ORCA_COLL_TYPE_MAX     UINT32_MAX-1
#define ORCA_COLL_TYPE_INVALID UINT32_MAX

#define ORCA_COLL_TYPE_BARRIER_INIT     (ORCA_COLL_TYPE_MIN + 1)
#define ORCA_COLL_TYPE_BARRIER_FINALIZE (ORCA_COLL_TYPE_MIN + 2)
#define ORCA_COLL_TYPE_BARRIER_CR       (ORCA_COLL_TYPE_MIN + 3)


/*******************************************
 * Datatypes for opal pack/unpack
 *******************************************/
/*
 * Provide a starting boundary
 *  - Match with ORTE
 */
#define    ORCA_DSS_ID_MIN          (OPAL_DSS_ID_DYNAMIC + 50)

#define    ORCA_DSS_JOBID           (ORCA_DSS_ID_MIN + 1)
#define    ORCA_DSS_VPID            (ORCA_DSS_ID_MIN + 2)
#define    ORCA_DSS_NAME            (ORCA_DSS_ID_MIN + 3)

/*
 * Provide an ending boundary
 */
#define    ORCA_DSS_ID_DYNAMIC      (ORCA_DSS_ID_MIN + 10)


/*******************************************
 * Notifier
 *******************************************/
typedef enum {
    ORCA_NOTIFIER_EMERG  = LOG_EMERG,
    ORCA_NOTIFIER_ALERT  = LOG_ALERT,
    ORCA_NOTIFIER_CRIT   = LOG_CRIT,
    ORCA_NOTIFIER_ERROR  = LOG_ERR,
    ORCA_NOTIFIER_WARN   = LOG_WARNING,
    ORCA_NOTIFIER_NOTICE = LOG_NOTICE,
    ORCA_NOTIFIER_INFO   = LOG_INFO,
    ORCA_NOTIFIER_DEBUG  = LOG_DEBUG
} orca_notifier_severity_t;

/*******************************************
 * Other...
 *******************************************/

#endif

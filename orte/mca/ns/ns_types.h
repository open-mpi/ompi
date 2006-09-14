/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 *
 * The Open MPI Name Server
 *
 * The Open MPI Name Server provides unique name ranges for processes
 * within the universe. Each universe will have one name server
 * running within the seed daemon.  This is done to prevent the
 * inadvertent duplication of names.
 */

#ifndef ORTE_NS_TYPES_H_
#define ORTE_NS_TYPES_H_

/*
 * includes
 */

#include "orte_config.h"
#include "orte/orte_types.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <limits.h>

#include "opal/types.h"
#include "opal/class/opal_list.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define ORTE_NAME_ARGS(n) \
    (unsigned long) ((NULL == n) ? -1 : (int32_t)(n)->cellid), \
    (unsigned long) ((NULL == n) ? -1 : (int32_t)(n)->jobid), \
    (unsigned long) ((NULL == n) ? -1 : (int32_t)(n)->vpid)


/*
 * useful defines for bit-masks
 */

#define ORTE_NS_CMP_NONE       0x00
#define ORTE_NS_CMP_CELLID     0x01
#define ORTE_NS_CMP_JOBID      0x02
#define ORTE_NS_CMP_VPID       0x04
#define ORTE_NS_CMP_ALL        0Xff

/*
 * general typedefs & structures
 */
/** Set the allowed range for ids in each space
 *
 * NOTE: Be sure to update the ORTE_NAME_ARGS #define (above) and all
 * uses of it if these types change to be larger than (unsigned long)!
 */
typedef orte_std_cntr_t orte_jobid_t;
typedef orte_std_cntr_t orte_cellid_t;
typedef orte_std_cntr_t orte_vpid_t;
typedef uint8_t  orte_ns_cmp_bitmask_t;  /**< Bit mask for comparing process names */
typedef uint8_t orte_ns_cmd_flag_t;

struct orte_process_name_t {
    orte_cellid_t cellid;  /**< Cell number */
    orte_jobid_t jobid; /**< Job number */
    orte_vpid_t vpid;  /**< Process number */
};
typedef struct orte_process_name_t orte_process_name_t;

/*
 * define maximum value for id's in any field
 */
#define ORTE_CELLID_MAX     ORTE_STD_CNTR_MAX
#define ORTE_JOBID_MAX      ORTE_STD_CNTR_MAX
#define ORTE_VPID_MAX       ORTE_STD_CNTR_MAX

/*
 * define invalid values
 */
#define ORTE_CELLID_INVALID     -999
#define ORTE_JOBID_INVALID      -999
#define ORTE_VPID_INVALID       -999

/*
 * define wildcard values
 */
#define ORTE_CELLID_WILDCARD     -1
#define ORTE_JOBID_WILDCARD      -1
#define ORTE_VPID_WILDCARD       -1

ORTE_DECLSPEC extern orte_process_name_t orte_name_all;
#define ORTE_NAME_ALL   &orte_name_all

/**
 * Convert process name from host to network byte order.
 *
 * @param name
 */
#define OMPI_PROCESS_NAME_HTON(n) \
    n.cellid = htonl(n.cellid); \
    n.jobid = htonl(n.jobid); \
    n.vpid = htonl(n.vpid);  

/**
 * Convert process name from network to host byte order.
 *
 * @param name
 */
#define OMPI_PROCESS_NAME_NTOH(n) \
    n.cellid = ntohl(n.cellid); \
    n.jobid = ntohl(n.jobid); \
    n.vpid = ntohl(n.vpid);  


/** List of names for general use
 */
struct orte_namelist_t {
    opal_list_item_t item;     /**< Allows this item to be placed on a list */
    orte_process_name_t *name;  /**< Name of a process */
};
typedef struct orte_namelist_t orte_namelist_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_namelist_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <limits.h>

#include "include/types.h"
#include "class/ompi_list.h"

#define ORTE_NAME_ARGS(n) \
    (unsigned long) ((NULL == n) ? -1 : (ssize_t)(n)->cellid), \
    (unsigned long) ((NULL == n) ? -1 : (ssize_t)(n)->jobid), \
    (unsigned long) ((NULL == n) ? -1 : (ssize_t)(n)->vpid)


/*
 * useful defines for bit-masks
 */

#define ORTE_NS_CMP_NONE       0x00
#define ORTE_NS_CMP_CELLID     0x01
#define ORTE_NS_CMP_JOBID      0x02
#define ORTE_NS_CMP_VPID       0x04
#define ORTE_NS_CMP_ALL        0Xff

/*
 * define maximum value for id's in any field
 */
#define ORTE_CELLID_MAX SIZE_MAX
#define ORTE_JOBID_MAX  SIZE_MAX
#define ORTE_VPID_MAX   SIZE_MAX

/*
 * general typedefs & structures
 */
/** Set the allowed range for ids in each space
 *
 * NOTE: Be sure to update the ORTE_NAME_ARGS #define (above) and all
 * uses of it if these types change to be larger than (unsigned long)!
 */
typedef size_t orte_jobid_t;
typedef size_t orte_cellid_t;
typedef size_t orte_vpid_t;
typedef uint8_t  orte_ns_cmp_bitmask_t;  /**< Bit mask for comparing process names */
typedef uint16_t orte_ns_cmd_flag_t;

struct orte_process_name_t {
    orte_cellid_t cellid;  /**< Cell number */
    orte_jobid_t jobid; /**< Job number */
    orte_vpid_t vpid;  /**< Process number */
};
typedef struct orte_process_name_t orte_process_name_t;

extern orte_process_name_t orte_name_all;
#define ORTE_NAME_ALL   &orte_name_all


/** List of names for general use
 */
struct orte_name_services_namelist_t {
    ompi_list_item_t item;     /**< Allows this item to be placed on a list */
    orte_process_name_t *name;  /**< Name of a process */
};
typedef struct orte_name_services_namelist_t orte_name_services_namelist_t;

OMPI_DECLSPEC    OBJ_CLASS_DECLARATION(orte_name_services_namelist_t);

#endif

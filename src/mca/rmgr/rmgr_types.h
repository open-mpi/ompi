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

#ifndef ORTE_RMGR_TYPES_H
#define ORTE_RMGR_TYPES_H

/*
 * REGISTRY KEY NAMES FOR COMMON DATA
 */
#define ORTE_RMGR_LAUNCHER      "orte-rmgr-launcher"

/*
 * Constants for command values
 */
#define ORTE_RMGR_CMD_QUERY       1
#define ORTE_RMGR_CMD_CREATE      2
#define ORTE_RMGR_CMD_ALLOCATE    3
#define ORTE_RMGR_CMD_DEALLOCATE  4
#define ORTE_RMGR_CMD_MAP         5
#define ORTE_RMGR_CMD_LAUNCH      6
#define ORTE_RMGR_CMD_TERM_JOB    7
#define ORTE_RMGR_CMD_TERM_PROC   8
#define ORTE_RMGR_CMD_SPAWN       9

#define ORTE_RMGR_CMD  ORTE_UINT32
typedef uint32_t orte_rmgr_cmd_t;


#endif

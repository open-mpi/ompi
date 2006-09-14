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
 */

#ifndef ORTE_RMGR_BASE_H
#define ORTE_RMGR_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/class/opal_list.h"
#include "orte/dss/dss.h"
#include "opal/mca/mca.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rmgr/rmgr.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * function definitions
 */
ORTE_DECLSPEC int orte_rmgr_base_open(void);
ORTE_DECLSPEC int orte_rmgr_base_select(void);
ORTE_DECLSPEC int orte_rmgr_base_close(void);

/*
 * globals that might be needed
 */

typedef struct orte_rmgr_base_t {
    int rmgr_output;
    opal_list_t rmgr_components;
} orte_rmgr_base_t;

ORTE_DECLSPEC extern orte_rmgr_base_t orte_rmgr_base;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

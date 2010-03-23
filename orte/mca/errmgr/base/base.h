/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
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
/** @file:
 */

#ifndef ORTE_MCA_ERRMGR_BASE_H
#define ORTE_MCA_ERRMGR_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"

#include "opal/mca/mca.h"
#include "orte/mca/errmgr/errmgr.h"


BEGIN_C_DECLS

/*
 * MCA Framework functions
 */
ORTE_DECLSPEC    int orte_errmgr_base_open(void);
ORTE_DECLSPEC    int orte_errmgr_base_select(void);
ORTE_DECLSPEC    int orte_errmgr_base_close(void);

/**
 * Composite Stack states
 */
#define ORTE_ERRMGR_STACK_STATE_NONE       0x00 /* No actions have been performed */
#define ORTE_ERRMGR_STACK_STATE_STABLIZED  0x01 /* Stabalized the runtime */
#define ORTE_ERRMGR_STACK_STATE_CONTINUE   0x02 /* Continue running without this process */
#define ORTE_ERRMGR_STACK_STATE_RECOVERED  0x04 /* Process has been recovered */
#define ORTE_ERRMGR_STACK_STATE_JOB_ABORT  0x08 /* Abort this job, cannot recover */

/**
 * Output and component variables
 */
ORTE_DECLSPEC extern opal_list_t orte_errmgr_base_components_available;
ORTE_DECLSPEC extern int  orte_errmgr_base_output;
ORTE_DECLSPEC extern bool orte_errmgr_base_shutting_down;
ORTE_DECLSPEC extern bool orte_errmgr_base_enable_recovery;

extern opal_pointer_array_t orte_errmgr_base_modules;
extern bool orte_errmgr_initialized;

/*
 * Additional External API function declared in errmgr.h
 */

END_C_DECLS

#endif

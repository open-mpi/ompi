/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
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

#include "orte/mca/mca.h"
#include "orte/mca/errmgr/errmgr.h"


BEGIN_C_DECLS

/*
 * MCA Framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_errmgr_base_framework;
/* select a component */
ORTE_DECLSPEC    int orte_errmgr_base_select(void);

/**
 * Interfaces for orte-migrate tool
 */
#if OPAL_ENABLE_FT_CR
/**
 * Migrating States
 */
#define ORTE_ERRMGR_MIGRATE_STATE_ERROR          (ORTE_SNAPC_CKPT_MAX + 1)
#define ORTE_ERRMGR_MIGRATE_STATE_ERR_INPROGRESS (ORTE_SNAPC_CKPT_MAX + 2)
#define ORTE_ERRMGR_MIGRATE_STATE_NONE           (ORTE_SNAPC_CKPT_MAX + 3)
#define ORTE_ERRMGR_MIGRATE_STATE_REQUEST        (ORTE_SNAPC_CKPT_MAX + 4)
#define ORTE_ERRMGR_MIGRATE_STATE_RUNNING        (ORTE_SNAPC_CKPT_MAX + 5)
#define ORTE_ERRMGR_MIGRATE_STATE_RUN_CKPT       (ORTE_SNAPC_CKPT_MAX + 6)
#define ORTE_ERRMGR_MIGRATE_STATE_STARTUP        (ORTE_SNAPC_CKPT_MAX + 7)
#define ORTE_ERRMGR_MIGRATE_STATE_FINISH         (ORTE_SNAPC_CKPT_MAX + 8)
#define ORTE_ERRMGR_MIGRATE_MAX                  (ORTE_SNAPC_CKPT_MAX + 9)

/*
 * Commands for command line tool and ErrMgr interaction
 */
typedef uint8_t orte_errmgr_tool_cmd_flag_t;
#define ORTE_ERRMGR_MIGRATE_TOOL_CMD  OPAL_UINT8
#define ORTE_ERRMGR_MIGRATE_TOOL_INIT_CMD    1
#define ORTE_ERRMGR_MIGRATE_TOOL_UPDATE_CMD  2

/*  Initialize/Finalize the orte-migrate communication functionality */
ORTE_DECLSPEC int orte_errmgr_base_tool_init(void);
ORTE_DECLSPEC int orte_errmgr_base_tool_finalize(void);

ORTE_DECLSPEC void orte_errmgr_base_migrate_state_notify(int state);
ORTE_DECLSPEC int orte_errmgr_base_migrate_state_str(char ** state_str, int state);

ORTE_DECLSPEC int orte_errmgr_base_migrate_update(int status);

/* Interface to report process state to the notifier */
ORTE_DECLSPEC void orte_errmgr_base_proc_state_notify(orte_proc_state_t state, orte_process_name_t *proc);

#endif /* OPAL_ENABLE_FT_CR */

END_C_DECLS

#endif

/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_DB_BASE_H
#define MCA_DB_BASE_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_list.h"

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

/*
 * Global functions for MCA overall collective open and close
 */

/**
 * Open the db framework
 */
ORTE_DECLSPEC int orte_db_base_open(void);

/**
 * Select a db module
 */
ORTE_DECLSPEC int orte_db_base_select(void);

/**
 * Close the db framework
 */
ORTE_DECLSPEC int orte_db_base_close(void);

/*
 * The verbose channel for debug output
 */
ORTE_DECLSPEC extern int orte_db_base_output;

ORTE_DECLSPEC extern opal_list_t orte_db_base_components_available;


END_C_DECLS

#endif

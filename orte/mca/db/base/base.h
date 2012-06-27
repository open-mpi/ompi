/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
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
#include "opal/dss/dss.h"

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

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

typedef struct {
    int output;
    opal_list_t available_components;
} orte_db_base_t;
ORTE_DECLSPEC extern orte_db_base_t orte_db_base;

ORTE_DECLSPEC int orte_db_base_send_modex_string(const char* key,
                                                 const void *buffer,
                                                 size_t size);

ORTE_DECLSPEC int orte_db_base_send_modex_key_value(const char* key,
                                                    const void *value,
                                                    opal_data_type_t dtype);

END_C_DECLS

#endif

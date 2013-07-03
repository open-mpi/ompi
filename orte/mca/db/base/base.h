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
#include "opal/mca/base/mca_base_framework.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

/**
 * Select a db module
 */
ORTE_DECLSPEC int orte_db_base_select(void);

extern ORTE_DECLSPEC mca_base_framework_t orte_db_base_framework;

ORTE_DECLSPEC int orte_db_base_send_modex_string(const char* key,
                                                 const void *buffer,
                                                 size_t size);

ORTE_DECLSPEC int orte_db_base_send_modex_key_value(const char* key,
                                                    const void *value,
                                                    opal_data_type_t dtype);

END_C_DECLS

#endif

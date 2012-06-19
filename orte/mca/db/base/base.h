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
#include "opal/mca/event/event.h"

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
    struct timeval timeout;
} orte_db_base_t;
ORTE_DECLSPEC extern orte_db_base_t orte_db_base;

typedef struct {
    opal_list_item_t *super;
    orte_process_name_t name;
    char *key;
    opal_event_t *ev;
    orte_db_fetch_callback_fn_t cbfunc;
    void *cbdata;
} orte_db_fetch_req_t;
OBJ_CLASS_DECLARATION(orte_db_fetch_req_t);

END_C_DECLS

#endif

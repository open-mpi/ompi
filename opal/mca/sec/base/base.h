/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_SEC_BASE_H
#define MCA_SEC_BASE_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"

#include "opal/mca/sec/sec.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern mca_base_framework_t opal_sec_base_framework;
OPAL_DECLSPEC extern opal_list_t opal_sec_base_actives;

/* object for storing active components */
typedef struct {
    opal_list_item_t super;
    int pri;
    opal_sec_base_module_t *module;
    mca_base_component_t *component;
} opal_sec_handle_t;
OBJ_CLASS_DECLARATION(opal_sec_handle_t);

/**
 * Select a sec module
 */
OPAL_DECLSPEC int opal_sec_base_select(void);

/* base stubs */
OPAL_DECLSPEC int opal_sec_base_get_cred(char *method,
                                         int dstorehandle,
                                         opal_process_name_t *my_id,
                                         char **payload, size_t *size);

OPAL_DECLSPEC int opal_sec_base_validate(char *payload, size_t size, char **method);

END_C_DECLS

#endif

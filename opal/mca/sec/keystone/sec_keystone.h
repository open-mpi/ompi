/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_SEC_KEYSTONE_H
#define OPAL_SEC_KEYSTONE_H

#include "opal/mca/sec/sec.h"

BEGIN_C_DECLS

typedef struct {
    opal_sec_base_component_t super;
    char *url;
} mca_sec_keystone_component_t;

OPAL_MODULE_DECLSPEC extern mca_sec_keystone_component_t mca_sec_keystone_component;
OPAL_DECLSPEC extern opal_sec_base_module_t opal_sec_keystone_module;

END_C_DECLS

#endif /* OPAL_SEC_KEYSTONE_H */

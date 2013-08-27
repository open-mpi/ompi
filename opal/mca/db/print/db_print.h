/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_DB_PRINT_H
#define OPAL_DB_PRINT_H

#include "opal/mca/db/db.h"

BEGIN_C_DECLS

typedef struct {
    opal_db_base_component_t super;
    char *filename;
} opal_db_print_component_t;
OPAL_MODULE_DECLSPEC extern opal_db_print_component_t mca_db_print_component;

OPAL_DECLSPEC extern opal_db_base_module_t opal_db_print_module;

END_C_DECLS

#endif /* OPAL_DB_PRINT_H */

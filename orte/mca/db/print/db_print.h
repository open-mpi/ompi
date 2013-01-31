/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_DB_PRINT_H
#define ORTE_DB_PRINT_H

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

typedef struct {
    orte_db_base_component_t super;
    char *filename;
} orte_db_print_component_t;
ORTE_MODULE_DECLSPEC extern orte_db_print_component_t mca_db_print_component;

ORTE_DECLSPEC extern orte_db_base_module_t orte_db_print_module;

END_C_DECLS

#endif /* ORTE_DB_PRINT_H */

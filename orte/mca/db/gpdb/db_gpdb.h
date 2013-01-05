/*
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_DB_GPDB_H
#define ORTE_DB_GPDB_H

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

typedef struct {
    orte_db_base_component_t super;
    int num_worker_threads;
    char *db_file;
} orte_db_gpdb_component_t;
ORTE_MODULE_DECLSPEC extern orte_db_gpdb_component_t mca_db_gpdb_component;

ORTE_DECLSPEC extern orte_db_base_module_t orte_db_gpdb_module;

END_C_DECLS

#endif /* ORTE_DB_GPDB_H */

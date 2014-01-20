/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_DB_POSTGRES_H
#define OPAL_DB_POSTGRES_H

#include "opal/mca/db/db.h"

BEGIN_C_DECLS

typedef struct {
    opal_db_base_component_t super;
    int num_worker_threads;
    char *dbname;
    char *table;
    char *user;
    char *pguri;
    char *pgoptions;
    char *pgtty;
} opal_db_postgres_component_t;
OPAL_MODULE_DECLSPEC extern opal_db_postgres_component_t mca_db_postgres_component;

OPAL_DECLSPEC extern opal_db_base_module_t opal_db_postgres_module;

END_C_DECLS

#endif /* OPAL_DB_POSTGRES_H */

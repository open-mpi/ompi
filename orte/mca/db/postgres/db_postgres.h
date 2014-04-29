/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_DB_POSTGRES_H
#define ORTE_DB_POSTGRES_H

#include "libpq-fe.h"

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

ORTE_MODULE_DECLSPEC extern orte_db_base_component_t mca_db_postgres_component;

typedef struct {
    orte_db_base_module_t api;
    int num_worker_threads;
    char *dbname;
    char *table;
    char *user;
    char *pguri;
    char *pgoptions;
    char *pgtty;
    PGconn *conn;
} mca_db_postgres_module_t;
ORTE_MODULE_DECLSPEC extern mca_db_postgres_module_t mca_db_postgres_module;

END_C_DECLS

#endif /* ORTE_DB_POSTGRES_H */

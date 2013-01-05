/*
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_DB_SQLITE_H
#define ORTE_DB_SQLITE_H

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

typedef struct {
    orte_db_base_component_t super;
    int num_worker_threads;
    char *db_file;
} orte_db_sqlite_component_t;
ORTE_MODULE_DECLSPEC extern orte_db_sqlite_component_t mca_db_sqlite_component;

ORTE_DECLSPEC extern orte_db_base_module_t orte_db_sqlite_module;

/* Macros for manipulating sqlite */
#define ORTE_SQLITE_CMD(f, db, r)                               \
    {                                                           \
        *(r) = sqlite3_ ## f;                                   \
        if (*(r) != SQLITE_OK) {                                \
            opal_output(0, "%s: %s failed with status %d: %s",  \
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),     \
                        #f, *(r), sqlite3_errmsg(db));          \
        }                                                       \
    }                                                           \

#define ORTE_SQLITE_OP(f, x, db, r)                             \
    {                                                           \
        *(r) = sqlite3_ ## f;                                   \
        if (*(r) != SQLITE_ ## x) {                             \
            opal_output(0, "%s: %s failed with status %d: %s",  \
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),     \
                        #f, *(r), sqlite3_errmsg(db));          \
        }                                                       \
    }                                                           \

END_C_DECLS

#endif /* ORTE_DB_SQLITE_H */

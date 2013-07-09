/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_DB_SQLITE_H
#define OPAL_DB_SQLITE_H

#include "opal/mca/db/db.h"

BEGIN_C_DECLS

typedef struct {
    opal_db_base_component_t super;
    int num_worker_threads;
    char *db_file;
} opal_db_sqlite_component_t;
OPAL_MODULE_DECLSPEC extern opal_db_sqlite_component_t mca_db_sqlite_component;

OPAL_DECLSPEC extern opal_db_base_module_t opal_db_sqlite_module;

/* Macros for manipulating sqlite */
#define OPAL_SQLITE_CMD(f, db, r)                               \
    {                                                           \
        *(r) = sqlite3_ ## f;                                   \
        if (*(r) != SQLITE_OK) {                                \
            opal_output(0, "%s failed with status %d: %s",      \
                        #f, *(r), sqlite3_errmsg(db));          \
        }                                                       \
    }                                                           \

#define OPAL_SQLITE_OP(f, x, db, r)                             \
    {                                                           \
        *(r) = sqlite3_ ## f;                                   \
        if (*(r) != SQLITE_ ## x) {                             \
            opal_output(0, "%s failed with status %d: %s",      \
                        #f, *(r), sqlite3_errmsg(db));          \
        }                                                       \
    }                                                           \

END_C_DECLS

#endif /* OPAL_DB_SQLITE_H */

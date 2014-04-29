/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#include <sys/types.h>
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sqlite3.h>

#include "opal/dss/dss_types.h"
#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/malloc.h"
#include "opal/util/basename.h"
#include "opal/mca/pstat/base/base.h"

#include "orte/util/show_help.h"

#include "orte/mca/db/base/base.h"
#include "db_sqlite.h"

static int init(struct orte_db_base_module_t *imod);
static void finalize(struct orte_db_base_module_t *imod);
static int store(struct orte_db_base_module_t *imod,
                 const char *primary_key,
                 opal_list_t *kvs);

mca_db_sqlite_module_t mca_db_sqlite_module = {
    {
        init,
        finalize,
        store,
        NULL,
        NULL,
        NULL
    },
};

static int init(struct orte_db_base_module_t *imod)
{
    mca_db_sqlite_module_t *mod = (mca_db_sqlite_module_t*)imod;
    int i;


    /* get the required number of database handles */
    mod->dbhandles = (sqlite3**)malloc(mod->nthreads * sizeof(sqlite3*));

    /* open the database - this will create the database file if
     * it doesn't already exist
     */
    for (i=0; i < mod->nthreads; i++) {
        if (SQLITE_OK != sqlite3_open(mod->dbfile, &mod->dbhandles[i])) {
            orte_show_help("help-db-sqlite.txt", "cannot-create-sqlite", true, mod->dbfile);
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
    }

    return ORTE_SUCCESS;
}

static void finalize(struct orte_db_base_module_t *imod)
{
    mca_db_sqlite_module_t *mod = (mca_db_sqlite_module_t*)imod;
    int i;

    /* if we are normally terminating, remove the recovery file */
    if (NULL != mod->dbhandles) {
        for (i=0; i < mod->nthreads; i++) {
            if (SQLITE_OK != sqlite3_close(mod->dbhandles[i])) {
                opal_output(0, "sqlite failed to close");
            }
        }
    }
}

static int store(struct orte_db_base_module_t *imod,
                 const char *primary_key,
                 opal_list_t *kvs)
{
    int i, rc;
    char *sql, **cmd = NULL, *tmp;
    sqlite3_stmt *stmt;
    opal_value_t *kv;

    mca_db_sqlite_module_t *mod = (mca_db_sqlite_module_t*)imod;

    /* setup the insert statement */
    for (i=0; i < (int)opal_list_get_size(kvs); i++) {
        opal_argv_append_nosize(&cmd, "?");
    }
    tmp = opal_argv_join(cmd, ',');
    asprintf(&sql, "INSERT INTO %s VALUES (%s)", mod->dbfile, tmp);
    free(tmp);
    opal_argv_free(cmd);
    /* use the next worker thread */
    ORTE_SQLITE_CMD(prepare_v2(mod->dbhandles[mod->active], sql, strlen(sql)+1, &stmt, NULL), mod->dbhandles[mod->active], &rc);
    if (SQLITE_OK != rc) {
        return ORTE_ERROR;
    }

    /* cycle through the provided values and construct
     * an insert command for them - note that the values
     * MUST be in column-order for the database!
     */
    OPAL_LIST_FOREACH(kv, kvs, opal_value_t) {
        switch (kv->type) {
        case OPAL_STRING:
            ORTE_SQLITE_CMD(bind_text(stmt, i, kv->data.string, strlen(kv->data.string), NULL),
                            mod->dbhandles[mod->active], &rc);
            break;
        case OPAL_INT32:
            ORTE_SQLITE_CMD(bind_int(stmt, i, kv->data.int32), mod->dbhandles[mod->active], &rc);
            break;
        case OPAL_INT16:
            ORTE_SQLITE_CMD(bind_int(stmt, i, kv->data.int16), mod->dbhandles[mod->active], &rc);
            break;
        case OPAL_PID:
            ORTE_SQLITE_CMD(bind_int64(stmt, i, kv->data.pid), mod->dbhandles[mod->active], &rc);
            break;
        case OPAL_INT64:
            ORTE_SQLITE_CMD(bind_int64(stmt, i, kv->data.int64), mod->dbhandles[mod->active], &rc);
            break;
        case OPAL_FLOAT:
            ORTE_SQLITE_CMD(bind_double(stmt, i, kv->data.fval), mod->dbhandles[mod->active], &rc);
            break;
        case OPAL_TIMEVAL:
            asprintf(&tmp, "%d.%06d", (int)kv->data.tv.tv_sec, (int)kv->data.tv.tv_usec);
            ORTE_SQLITE_CMD(bind_text(stmt, i, tmp, strlen(tmp), NULL),
                            mod->dbhandles[mod->active], &rc);
            free(tmp);
            break;
        }
        if (SQLITE_OK != rc) {
            return ORTE_ERROR;
        }
    }

    ORTE_SQLITE_OP(step(stmt), DONE, mod->dbhandles[mod->active], &rc);
    if (SQLITE_OK != rc) {
        return ORTE_ERROR;
    }
    opal_output_verbose(2, orte_db_base_framework.framework_output,
                        "INSERTED ROW %d", (int)sqlite3_last_insert_rowid(mod->dbhandles[mod->active]));

    /* cycle to the next worker thread */
    mod->active++;
    if (mod->nthreads < mod->active) {
        mod->active = 0;
    }

    return ORTE_SUCCESS;
}

/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/constants.h"

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

#include "opal/util/show_help.h"

#include "opal/mca/db/base/base.h"
#include "db_sqlite.h"

static int init(void);
static void finalize(void);
static int add_log(const char *table,
                   const opal_value_t *kvs, int nkvs);

opal_db_base_module_t opal_db_sqlite_module = {
    init,
    finalize,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    add_log
};

/* local variables */
static sqlite3 **dbhandles=NULL;
static int nthreads = 1;
static int active = 0;

static int init(void)
{
    int i;

    /* initialize sqlite3 */
    if (SQLITE_OK != sqlite3_initialize()) {
        return OPAL_ERR_UNREACH;
    }

    /* check if sqlite was built thread-safe - if not, we won't
     * use worker threads for thruput
     */
    if (0 == mca_db_sqlite_component.num_worker_threads || 0 != sqlite3_threadsafe()) {
        nthreads = 1;
    } else {
        nthreads = mca_db_sqlite_component.num_worker_threads;
    }

    /* get the required number of database handles */
    dbhandles = (sqlite3**)malloc(nthreads * sizeof(sqlite3*));

    /* open the database - this will create the database file if
     * it doesn't already exist
     */
    for (i=0; i < nthreads; i++) {
        if (SQLITE_OK != sqlite3_open(mca_db_sqlite_component.db_file, &dbhandles[i])) {
            opal_show_help("help-db-sqlite.txt", "cannot-create-sqlite", true, mca_db_sqlite_component.db_file);
            return OPAL_ERR_FILE_OPEN_FAILURE;
        }
    }

    return OPAL_SUCCESS;
}

static void finalize(void)
{
    int i;

    /* if we are normally terminating, remove the recovery file */
    if (NULL != dbhandles) {
        for (i=0; i < nthreads; i++) {
            if (SQLITE_OK != sqlite3_close(dbhandles[i])) {
                opal_output(0, "sqlite failed to close");
            }
        }
    }
}

static int add_log(const char *table,
                   const opal_value_t *kvs, int nkvs)
{
    int i, rc;
    char *sql, **cmd = NULL, *tmp;
    sqlite3_stmt *stmt;

    opal_output_verbose(2, opal_db_base_framework.framework_output,
                        "Logging data for table %s", table);

    /* setup the insert statement */
    for (i=0; i < nkvs; i++) {
        opal_argv_append_nosize(&cmd, "?");
    }
    tmp = opal_argv_join(cmd, ',');
    asprintf(&sql, "INSERT INTO %s VALUES (%s)", table, tmp);
    free(tmp);
    opal_argv_free(cmd);
    /* use the next worker thread */
    OPAL_SQLITE_CMD(prepare_v2(dbhandles[active], sql, strlen(sql)+1, &stmt, NULL), dbhandles[active], &rc);
    if (SQLITE_OK != rc) {
        return OPAL_ERROR;
    }

    /* cycle through the provided values and construct
     * an insert command for them - note that the values
     * MUST be in column-order for the database!
     */
    for (i=0; i < nkvs; i++) {
        switch (kvs[i].type) {
        case OPAL_STRING:
            OPAL_SQLITE_CMD(bind_text(stmt, i, kvs[i].data.string, strlen(kvs[i].data.string), NULL),
                            dbhandles[active], &rc);
            break;
        case OPAL_INT32:
            OPAL_SQLITE_CMD(bind_int(stmt, i, kvs[i].data.int32), dbhandles[active], &rc);
            break;
        case OPAL_INT16:
            OPAL_SQLITE_CMD(bind_int(stmt, i, kvs[i].data.int16), dbhandles[active], &rc);
            break;
        case OPAL_PID:
            OPAL_SQLITE_CMD(bind_int64(stmt, i, kvs[i].data.pid), dbhandles[active], &rc);
            break;
        case OPAL_INT64:
            OPAL_SQLITE_CMD(bind_int64(stmt, i, kvs[i].data.int64), dbhandles[active], &rc);
            break;
        case OPAL_FLOAT:
            OPAL_SQLITE_CMD(bind_double(stmt, i, kvs[i].data.fval), dbhandles[active], &rc);
            break;
        case OPAL_TIMEVAL:
            asprintf(&tmp, "%d.%06d", (int)kvs[i].data.tv.tv_sec, (int)kvs[i].data.tv.tv_usec);
            OPAL_SQLITE_CMD(bind_text(stmt, i, tmp, strlen(tmp), NULL),
                            dbhandles[active], &rc);
            free(tmp);
            break;
        }
        if (SQLITE_OK != rc) {
            return OPAL_ERROR;
        }
    }

    OPAL_SQLITE_OP(step(stmt), DONE, dbhandles[active], &rc);
    if (SQLITE_OK != rc) {
        return OPAL_ERROR;
    }
    opal_output_verbose(2, opal_db_base_framework.framework_output,
                        "INSERTED ROW %d", (int)sqlite3_last_insert_rowid(dbhandles[active]));

    /* cycle to the next worker thread */
    active++;
    if (nthreads < active) {
        active = 0;
    }

    return OPAL_SUCCESS;
}

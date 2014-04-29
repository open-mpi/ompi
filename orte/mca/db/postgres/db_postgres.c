/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
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

#include "libpq-fe.h"

#include "opal_stdint.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"

#include "orte/mca/db/base/base.h"
#include "db_postgres.h"

#define ORTE_PG_MAX_LINE_LENGTH 4096

static int init(struct orte_db_base_module_t *imod);
static void finalize(struct orte_db_base_module_t *imod);
static int store(struct orte_db_base_module_t *imod,
                 const char *primary_key,
                 opal_list_t *kvs);

mca_db_postgres_module_t mca_db_postgres_module = {
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
    mca_db_postgres_module_t *mod = (mca_db_postgres_module_t*)imod;
    char **login=NULL;
    char **connection=NULL;

    /* break the user info into its login parts */
    login = opal_argv_split(mod->user, ':');
    if (2 != opal_argv_count(login)) {
        opal_output(0, "db:postgres: User info is invalid: %s",
                    mod->user);
        opal_argv_free(login);
        return ORTE_ERR_BAD_PARAM;
    }
    /* break the uri */
    connection = opal_argv_split(mod->pguri, ':');
    if (2 != opal_argv_count(connection)) {
        opal_argv_free(login);
        opal_argv_free(connection);
        opal_output(0, "db:postgres: Connection info is invalid: %s",
                    mod->pguri);
        return ORTE_ERR_BAD_PARAM;
    }

    conn = PQsetdbLogin(connection[0], connection[1],
                        mod->pgoptions,
                        mod->pgtty,
                        mod->dbname,
                        login[0], login[1]);
    opal_argv_free(login);
    opal_argv_free(connection);

    if (PQstatus(conn) != CONNECTION_OK) {
        conn = NULL;
        opal_output(0, "***********************************************\n");
        opal_output(0, "db:postgres: Connection failed:\n\tURI: %s\n\tOPTIONS: %s\n\tTTY: %s\n\tDBNAME: %s\n\tUSER: %s",
                    mod->pguri,
                    (NULL == mod->pgoptions) ? "NULL" : mod->pgoptions,
                    (NULL == mod->pgtty) ? "NULL" : mod->pgtty,
                    mod->dbname,
                    mod->user);
        opal_output(0, "\n***********************************************");
        exit(ORTE_ERR_CONNECTION_FAILED);
        return ORTE_ERR_CONNECTION_FAILED;
    }
    opal_output_verbose(5, orte_db_base_framework.framework_output,
                        "db:postgres: Connection established to %s",
                        mod->dbname);

    return ORTE_SUCCESS;
}

static void finalize(struct orte_db_base_module_t *imod)
{
    mca_db_postgres_module_t *mod = (mca_db_postgres_module_t*)imod;
    if (NULL != mod->dbname) {
        free(mod->dbname);
    }
    if (NULL != mod->table) {
        free(mod->table);
    }
    if (NULL != mod->user) {
        free(mod->user);
    }
    if (NULL != mod->pguri) {
        free(mod->pguri);
    }
    if (NULL != mod->pgoptions) {
        free(mod->pgoptions);
    }
    if (NULL != mod->pgtty) {
        free(mod->pgtty);
    }
    if (NULL != mod->conn) {
        PQfinish(mod->conn);
    }
}

static int store(struct orte_db_base_module_t *imod,
                 const char *primary_key,
                 opal_list_t *kvs)
{
    mca_db_postgres_module_t *mod = (mca_db_postgres_module_t*)imod;
    char *query, *vstr;
    PGresult *res;
    char **cmdargs=NULL;
    time_t nowtime;
    struct tm *nowtm;
    char tbuf[1024], buf[64];
    int i;
    opal_value_t *kv;

    /* cycle through the provided values and construct
     * an insert command for them - note that the values
     * MUST be in column-order for the database!
     */
    OPAL_LIST_FOREACH(kv, kvs, opal_value_t) {
        switch (kv->type) {
        case OPAL_STRING:
            snprintf(tbuf, sizeof(tbuf), "%s", kv->data.string);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_SIZE:
            snprintf(tbuf, sizeof(tbuf), "%" PRIsize_t "", kv->data.size);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT:
            snprintf(tbuf, sizeof(tbuf), "%d", kv->data.integer);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT8:
            snprintf(tbuf, sizeof(tbuf), "%" PRIi8 "", kv->data.int8);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT16:
            snprintf(tbuf, sizeof(tbuf), "%" PRIi16 "", kv->data.int16);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT32:
            snprintf(tbuf, sizeof(tbuf), "%" PRIi32 "", kv->data.int32);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT64:
            snprintf(tbuf, sizeof(tbuf), "%" PRIi64 "", kv->data.int64);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT:
            snprintf(tbuf, sizeof(tbuf), "%u", kv->data.uint);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT8:
            snprintf(tbuf, sizeof(tbuf), "%" PRIu8 "", kv->data.uint8);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT16:
            snprintf(tbuf, sizeof(tbuf), "%" PRIu16 "", kv->data.uint16);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT32:
            snprintf(tbuf, sizeof(tbuf), "%" PRIu32 "", kv->data.uint32);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT64:
            snprintf(tbuf, sizeof(tbuf), "%" PRIu64 "", kv->data.uint64);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_PID:
            snprintf(tbuf, sizeof(tbuf), "%lu", (unsigned long)kv->data.pid);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_FLOAT:
            snprintf(tbuf, sizeof(tbuf), "%f", kv->data.fval);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_TIMEVAL:
            /* we only care about seconds */
            nowtime = kv->data.tv.tv_sec;
            (void)localtime_r(&nowtime, &nowtm);
            strftime(tbuf, sizeof(tbuf), "%Y-%m-%d %H:%M:%S", &nowtm);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        default:
            snprintf(tbuf, sizeof(tbuf), "Unsupported type: %s",
                     opal_dss.lookup_data_type(kv->type));
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        }
    }

    /* assemble the value string */
    vstr = opal_argv_join(cmdargs, ',');
    opal_argv_free(cmdargs);

    /* create the query */
    asprintf(&query, "INSERT INTO %s values (%s)", mod->table, vstr);
    free(vstr);

    opal_output_verbose(2, orte_db_base_framework.framework_output,
                        "Executing query %s", query);

    /* execute it */
    res = PQexec(conn, query);
    free(query);

    if ((!res) || (PQresultStatus(res) != PGRES_COMMAND_OK)) {
        opal_output(0, "***********************************************\n");
        opal_output(0, "POSTGRES INSERT COMMAND FAILED - UNABLE TO LOG");
        opal_output(0, "DATA. ABORTING");
        opal_output(0, "\n***********************************************");
        PQclear(res);
        return ORTE_ERROR;
    }

    opal_output_verbose(2, orte_db_base_framework.framework_output,
                        "Query succeeded");

    PQclear(res);
    return OPAL_SUCCESS;
}

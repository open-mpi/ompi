/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
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

#include "libpq-fe.h"

#include "opal_stdint.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"

#include "opal/mca/db/base/base.h"
#include "db_postgres.h"

#define OPAL_PG_MAX_LINE_LENGTH 4096

static int init(void);
static void finalize(void);
static int add_log(const char *table,
                   const opal_value_t *kvs, int nkvs);

opal_db_base_module_t opal_db_postgres_module = {
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

static PGconn *conn = NULL;

/* local variables */
static int init(void)
{
    char **login=NULL;
    char **connection=NULL;

    /* break the user info into its login parts */
    login = opal_argv_split(mca_db_postgres_component.user, ':');
    if (2 != opal_argv_count(login)) {
        opal_output(0, "db:postgres: User info is invalid: %s",
                    mca_db_postgres_component.user);
        opal_argv_free(login);
        return OPAL_ERR_BAD_PARAM;
    }
    /* break the uri */
    connection = opal_argv_split(mca_db_postgres_component.pguri, ':');
    if (2 != opal_argv_count(connection)) {
        opal_argv_free(login);
        opal_argv_free(connection);
        opal_output(0, "db:postgres: Connection info is invalid: %s",
                    mca_db_postgres_component.pguri);
        return OPAL_ERR_BAD_PARAM;
    }

    conn = PQsetdbLogin(connection[0], connection[1],
                        mca_db_postgres_component.pgoptions,
                        mca_db_postgres_component.pgtty,
                        mca_db_postgres_component.dbname,
                        login[0], login[1]);
    opal_argv_free(login);
    opal_argv_free(connection);

    if (PQstatus(conn) != CONNECTION_OK) {
        conn = NULL;
        opal_output(0, "***********************************************\n");
        opal_output(0, "db:postgres: Connection failed:\n\tURI: %s\n\tOPTIONS: %s\n\tTTY: %s\n\tDBNAME: %s\n\tUSER: %s",
                    mca_db_postgres_component.pguri,
                    (NULL == mca_db_postgres_component.pgoptions) ? "NULL" : mca_db_postgres_component.pgoptions,
                    (NULL == mca_db_postgres_component.pgtty) ? "NULL" : mca_db_postgres_component.pgtty,
                    mca_db_postgres_component.dbname,
                    mca_db_postgres_component.user);
        opal_output(0, "\n***********************************************");
        exit(OPAL_ERR_CONNECTION_FAILED);
        return OPAL_ERR_CONNECTION_FAILED;
    }
    opal_output_verbose(5, opal_db_base_framework.framework_output,
                        "db:postgres: Connection established to %s",
                        mca_db_postgres_component.dbname);

    return OPAL_SUCCESS;
}

static void finalize(void)
{
    if (NULL != conn) {
        PQfinish(conn);
    }
}

/* NOTE: at this time, we only support data from the
 * "sigar" source. We'll have to add a mapping from
 * source to table later
 */
static int add_log(const char *source,
                   const opal_value_t *kvs, int nkvs)
{
    char *query, *vstr;
    PGresult *res;
    char **cmdargs=NULL;
    time_t nowtime;
    struct tm *nowtm;
    char tbuf[1024], buf[64];
    int i;

    opal_output_verbose(2, opal_db_base_framework.framework_output,
                        "Logging data for source %s", source);

    if (0 != strcmp(source, "sigar")) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }

    /* cycle through the provided values and construct
     * an insert command for them - note that the values
     * MUST be in column-order for the database!
     */
    for (i=0; i < nkvs; i++) {
        switch (kvs[i].type) {
        case OPAL_STRING:
            snprintf(tbuf, sizeof(tbuf), "\'%s\'", kvs[i].data.string);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT32:
            snprintf(tbuf, sizeof(tbuf), "%d", kvs[i].data.int32);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT16:
            snprintf(tbuf, sizeof(tbuf), "%d", (int)kvs[i].data.int16);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_PID:
            snprintf(tbuf, sizeof(tbuf), "%lu", (unsigned long)kvs[i].data.pid);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT64:
            snprintf(tbuf, sizeof(tbuf), "%" PRIi64 "", kvs[i].data.int64);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT64:
            snprintf(tbuf, sizeof(tbuf), "%" PRIu64 "", kvs[i].data.uint64);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_FLOAT:
            snprintf(tbuf, sizeof(tbuf), "%f", kvs[i].data.fval);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_TIMEVAL:
            /* we only care about seconds */
            nowtime = kvs[i].data.tv.tv_sec;
            nowtm = localtime(&nowtime);
            strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", nowtm);
            snprintf(tbuf, sizeof(tbuf), "\'%s\'", buf);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        default:
            OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
            break;
        }
    }

    /* assemble the value string */
    vstr = opal_argv_join(cmdargs, ',');

    /* create the query */
    asprintf(&query, "INSERT INTO %s values (%s)", mca_db_postgres_component.table, vstr);
    free(vstr);

    opal_output_verbose(2, opal_db_base_framework.framework_output,
                        "Executing query %s", query);

    /* execute it */
    res = PQexec(conn, query);
    free(query);

    if ((!res) || (PQresultStatus(res) != PGRES_COMMAND_OK)) {
        opal_output(0, "***********************************************\n");
        opal_output(0, "POSTGRES INSERT COMMAND FAILED - UNABLE TO LOG");
        opal_output(0, "SENSOR DATA. ABORTING");
        opal_output(0, "\n***********************************************");
        PQclear(res);
        return OPAL_ERROR;
    }

    opal_output_verbose(2, opal_db_base_framework.framework_output,
                        "Query succeeded");

    PQclear(res);
    return OPAL_SUCCESS;
}

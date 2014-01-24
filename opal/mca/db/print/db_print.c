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

#include <time.h>
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

#include "opal/class/opal_pointer_array.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal_stdint.h"

#include "opal/mca/db/base/base.h"
#include "db_print.h"

static int init(void);
static void finalize(void);
static int add_log(const char *table,
                   const opal_value_t *kvs, int nkvs);

opal_db_base_module_t opal_db_print_module = {
    init,
    finalize,
    opal_db_base_set_id,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    add_log
};

static opal_pointer_array_t tables;
static FILE *fpout=NULL;

static int init(void)
{
    OBJ_CONSTRUCT(&tables, opal_pointer_array_t);
    opal_pointer_array_init(&tables, 1, INT_MAX, 1);

    if (0 == strcmp(mca_db_print_component.filename, "-")) {
        fpout = stdout;
    } else if (0 == strcmp(mca_db_print_component.filename, "+")) {
        fpout = stderr;
    } else if (NULL == (fpout = fopen(mca_db_print_component.filename, "w"))) {
        opal_output(0, "ERROR: cannot open log file %s", mca_db_print_component.filename);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static void finalize(void)
{
    int i;
    char *ptr;

    for (i=0; i < tables.size; i++) {
        if (NULL != (ptr = (char*)opal_pointer_array_get_item(&tables, i))) {
            free(ptr);
        }
    }
    OBJ_DESTRUCT(&tables);
    if (NULL != fpout &&
        stdout != fpout &&
        stderr != fpout) {
        fclose(fpout);
        fpout = NULL;
    }
}

static int add_log(const char *table,
                   const opal_value_t *kvs, int nkvs)
{
    char **cmdargs=NULL, *vstr;
    time_t nowtime;
    struct tm nowtm;
    char tbuf[1024];
    int i;
    bool found;

    opal_output_verbose(2, opal_db_base_framework.framework_output,
                        "Logging data for table %s", table);

    found = false;
    for (i=0; i < tables.size; i++) {
        if (NULL == (vstr = (char*)opal_pointer_array_get_item(&tables, i))) {
            continue;
        }
        if (0 == strcmp(vstr, table)) {
            /* already handled this one */
            found = true;
            break;
        }
    }

    if (!found) {
        /* record that we have it */
        vstr = strdup(table);
        opal_pointer_array_add(&tables, vstr);
        /* start with the table name */
        opal_argv_append_nosize(&cmdargs, table);
        /* create and print the column headers */
        for (i=0; i < nkvs; i++) {
            opal_argv_append_nosize(&cmdargs, kvs[i].key);
        }
        vstr = opal_argv_join(cmdargs, '|');
        fprintf(fpout, "%s\n", vstr);
        free(vstr);
        opal_argv_free(cmdargs);
        cmdargs = NULL;
    }

    /* put the table at the start */
    opal_argv_append_nosize(&cmdargs, table);

    /* cycle through the provided values and print them */
    for (i=0; i < nkvs; i++) {
        switch (kvs[i].type) {
        case OPAL_STRING:
            snprintf(tbuf, sizeof(tbuf), "%s", kvs[i].data.string);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_SIZE:
            snprintf(tbuf, sizeof(tbuf), "%" PRIsize_t "", kvs[i].data.size);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT:
            snprintf(tbuf, sizeof(tbuf), "%d", kvs[i].data.integer);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT8:
            snprintf(tbuf, sizeof(tbuf), "%" PRIi8 "", kvs[i].data.int8);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT16:
            snprintf(tbuf, sizeof(tbuf), "%" PRIi16 "", kvs[i].data.int16);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT32:
            snprintf(tbuf, sizeof(tbuf), "%" PRIi32 "", kvs[i].data.int32);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_INT64:
            snprintf(tbuf, sizeof(tbuf), "%" PRIi64 "", kvs[i].data.int64);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT:
            snprintf(tbuf, sizeof(tbuf), "%u", kvs[i].data.uint);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT8:
            snprintf(tbuf, sizeof(tbuf), "%" PRIu8 "", kvs[i].data.uint8);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT16:
            snprintf(tbuf, sizeof(tbuf), "%" PRIu16 "", kvs[i].data.uint16);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT32:
            snprintf(tbuf, sizeof(tbuf), "%" PRIu32 "", kvs[i].data.uint32);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_UINT64:
            snprintf(tbuf, sizeof(tbuf), "%" PRIu64 "", kvs[i].data.uint64);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_PID:
            snprintf(tbuf, sizeof(tbuf), "%lu", (unsigned long)kvs[i].data.pid);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_FLOAT:
            snprintf(tbuf, sizeof(tbuf), "%f", kvs[i].data.fval);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        case OPAL_TIMEVAL:
            /* we only care about seconds */
            nowtime = kvs[i].data.tv.tv_sec;
            (void)localtime_r(&nowtime, &nowtm);
            strftime(tbuf, sizeof(tbuf), "%Y-%m-%d %H:%M:%S", &nowtm);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        default:
            snprintf(tbuf, sizeof(tbuf), "Unsupported type: %s",
                     opal_dss.lookup_data_type(kvs[i].type));
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        }
    }

    /* assemble the value string */
    vstr = opal_argv_join(cmdargs, ',');

    /* print it */
    fprintf(fpout, "%s\n", vstr);
    free(vstr);

    return OPAL_SUCCESS;
}

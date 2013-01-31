/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
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

#include "opal/class/opal_pointer_array.h"
#include "opal/util/argv.h"

#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/db/base/base.h"
#include "db_print.h"

static int init(void);
static void finalize(void);
static int add_log(const char *table,
                   const opal_value_t *kvs, int nkvs);

orte_db_base_module_t orte_db_print_module = {
    init,
    finalize,
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
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
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
    struct tm *nowtm;
    char tbuf[1024];
    int i;
    bool found;

    opal_output_verbose(2, orte_db_base.output,
                        "%s Logging data for table %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), table);

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

    /* cycle through the provided values and print them */
    for (i=0; i < nkvs; i++) {
        switch (kvs[i].type) {
        case OPAL_STRING:
            snprintf(tbuf, sizeof(tbuf), "%s", kvs[i].data.string);
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
            snprintf(tbuf, sizeof(tbuf), "%ld", (long int)kvs[i].data.int64);
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
            strftime(tbuf, sizeof(tbuf), "%Y-%m-%d %H:%M:%S", nowtm);
            opal_argv_append_nosize(&cmdargs, tbuf);
            break;
        }
    }

    /* assemble the value string */
    vstr = opal_argv_join(cmdargs, ',');

    /* print it */
    fprintf(fpout, "%s\n", vstr);
    free(vstr);

    return ORTE_SUCCESS;
}

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

#include "orte/mca/db/base/base.h"
#include "db_print.h"

static int init(struct orte_db_base_module_t *imod);
static void finalize(struct orte_db_base_module_t *imod);
static int store(struct orte_db_base_module_t *imod,
                 const char *primary_key,
                 opal_list_t *kvs);

mca_db_print_module_t mca_db_print_module = {
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
    mca_db_print_module_t *mod = (mca_db_print_module_t*)imod;

    if (0 == strcmp(mod->file, "-")) {
        mod->fp = stdout;
    } else if (0 == strcmp(mod->file, "+")) {
        mod->fp = stderr;
    } else if (NULL == (mod->fp = fopen(mod->file, "w"))) {
        opal_output(0, "ERROR: cannot open log file %s", mod->file);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static void finalize(struct orte_db_base_module_t *imod)
{
    mca_db_print_module_t *mod = (mca_db_print_module_t*)imod;

    if (NULL != mod->fp &&
        stdout != mod->fp &&
        stderr != mod->fp) {
        fclose(mod->fp);
    }
}

static int store(struct orte_db_base_module_t *imod,
                 const char *primary_key,
                 opal_list_t *kvs)
{
    mca_db_print_module_t *mod = (mca_db_print_module_t*)imod;
    char **cmdargs=NULL, *vstr;
    time_t nowtime;
    struct tm nowtm;
    char tbuf[1024];
    opal_value_t *kv;

    /* cycle through the provided values and print them */
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

    /* print it */
    fprintf(mod->fp, "%s\n", vstr);
    free(vstr);
    opal_argv_free(cmdargs);

    return OPAL_SUCCESS;
}

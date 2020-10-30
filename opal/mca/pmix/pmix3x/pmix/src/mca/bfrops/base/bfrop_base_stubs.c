/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"


#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/include/pmix_globals.h"

#include "src/mca/bfrops/base/base.h"

static const char* basic_type_string(pmix_data_type_t type)
{
    switch(type) {
        case PMIX_BOOL:
            return "PMIX_BOOL";
        case PMIX_REGEX:
            return "PMIX_REGEX";
        case PMIX_BYTE:
            return "PMIX_BYTE";
        case PMIX_STRING:
            return "PMIX_STRING";
        case PMIX_SIZE:
            return "PMIX_SIZE";
        case PMIX_PID:
            return "PMIX_PID";
        case PMIX_INT:
            return "PMIX_INT";
        case PMIX_INT8:
            return "PMIX_INT8";
        case PMIX_INT16:
            return "PMIX_INT16";
        case PMIX_INT32:
            return "PMIX_INT32";
        case PMIX_INT64:
            return "PMIX_INT64";
        case PMIX_UINT:
            return "PMIX_UINT";
        case PMIX_UINT8:
            return "PMIX_UINT8";
        case PMIX_UINT16:
            return "PMIX_UINT16";
        case PMIX_UINT32:
            return "PMIX_UINT32";
        case PMIX_UINT64:
            return "PMIX_UINT64";
        case PMIX_FLOAT:
            return "PMIX_FLOAT";
        case PMIX_DOUBLE:
            return "PMIX_DOUBLE";
        case PMIX_TIMEVAL:
            return "PMIX_TIMEVAL";
        case PMIX_TIME:
            return "PMIX_TIME";
        case PMIX_STATUS:
            return "PMIX_STATUS";
        case PMIX_VALUE:
            return "PMIX_VALUE";
        case PMIX_PROC:
            return "PMIX_PROC";
        case PMIX_APP:
            return "PMIX_APP";
        case PMIX_INFO:
            return "PMIX_INFO";
        case PMIX_PDATA:
            return "PMIX_PDATA";
        case PMIX_BUFFER:
            return "PMIX_BUFFER";
        case PMIX_BYTE_OBJECT:
            return "PMIX_BYTE_OBJECT";
        case PMIX_KVAL:
            return "PMIX_KVAL";
        case PMIX_PERSIST:
            return "PMIX_PERSIST";
        case PMIX_POINTER:
            return "PMIX_POINTER";
        case PMIX_SCOPE:
            return "PMIX_SCOPE";
        case PMIX_DATA_RANGE:
            return "PMIX_DATA_RANGE";
        case PMIX_COMMAND:
            return "PMIX_COMMAND";
        case PMIX_INFO_DIRECTIVES:
            return "PMIX_INFO_DIRECTIVES";
        case PMIX_DATA_TYPE:
            return "PMIX_DATA_TYPE";
        case PMIX_PROC_STATE:
            return "PMIX_PROC_STATE";
        case PMIX_PROC_INFO:
            return "PMIX_PROC_INFO";
        case PMIX_DATA_ARRAY:
            return "PMIX_DATA_ARRAY";
        case PMIX_PROC_RANK:
            return "PMIX_PROC_RANK";
        case PMIX_QUERY:
            return "PMIX_QUERY";
        case PMIX_COMPRESSED_STRING:
            return "PMIX_COMPRESSED_STRING";
        case PMIX_ALLOC_DIRECTIVE:
            return "PMIX_ALLOC_DIRECTIVE";
        case PMIX_IOF_CHANNEL:
            return "PMIX_IOF_CHANNEL";
        case PMIX_ENVAR:
            return "PMIX_ENVAR";
        default:
            return "NOT INITIALIZED";
    }
}
PMIX_EXPORT const char* PMIx_Data_type_string(pmix_data_type_t type)
{
    pmix_bfrops_base_active_module_t *active;
    char *reply;

    if (!pmix_bfrops_globals.initialized) {
        return basic_type_string(type);
    }

    PMIX_LIST_FOREACH(active, &pmix_bfrops_globals.actives, pmix_bfrops_base_active_module_t) {
        if (NULL != active->module->data_type_string) {
            if (NULL != (reply = (char*)active->module->data_type_string(type))) {
                return reply;
            }
        }
    }
    return "UNKNOWN";
}

char* pmix_bfrops_base_get_available_modules(void)
{
    pmix_bfrops_base_active_module_t *active;
    char **tmp=NULL, *reply=NULL;

    if (!pmix_bfrops_globals.initialized) {
        return NULL;
    }

    PMIX_LIST_FOREACH(active, &pmix_bfrops_globals.actives, pmix_bfrops_base_active_module_t) {
        pmix_argv_append_nosize(&tmp, active->component->base.pmix_mca_component_name);
    }
    if (NULL != tmp) {
        reply = pmix_argv_join(tmp, ',');
        pmix_argv_free(tmp);
    }
    return reply;
}

pmix_bfrops_module_t* pmix_bfrops_base_assign_module(const char *version)
{
    pmix_bfrops_base_active_module_t *active;
    pmix_bfrops_module_t *mod;
    char **tmp=NULL;
    int i;

    if (!pmix_bfrops_globals.initialized) {
        return NULL;
    }

    if (NULL != version) {
        tmp = pmix_argv_split(version, ',');
    }

    PMIX_LIST_FOREACH(active, &pmix_bfrops_globals.actives, pmix_bfrops_base_active_module_t) {
        if (NULL == tmp) {
            if (NULL != (mod = active->component->assign_module())) {
                return mod;
            }
        } else {
            for (i=0; NULL != tmp[i]; i++) {
                if (0 == strcmp(tmp[i], active->component->base.pmix_mca_component_name)) {
                    if (NULL != (mod = active->component->assign_module())) {
                        pmix_argv_free(tmp);
                        return mod;
                    }
                }
            }
        }
    }

    /* we only get here if nothing was found */
    if (NULL != tmp) {
        pmix_argv_free(tmp);
    }
    return NULL;
}

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <pmix_common.h>
#include "src/include/pmix_globals.h"

#include "src/class/pmix_list.h"
#include "src/util/argv.h"
#include "src/util/error.h"

#include "src/mca/gds/base/base.h"


char* pmix_gds_base_get_available_modules(void)
{
    if (!pmix_gds_globals.initialized) {
        return NULL;
    }

    return strdup(pmix_gds_globals.all_mods);
}

/* Select a gds module per the given directives */
pmix_gds_base_module_t* pmix_gds_base_assign_module(pmix_info_t *info, size_t ninfo)
{
    pmix_gds_base_active_module_t *active;
    pmix_gds_base_module_t *mod = NULL;
    int pri, priority = -1;

    if (!pmix_gds_globals.initialized) {
        return NULL;
    }

    PMIX_LIST_FOREACH(active, &pmix_gds_globals.actives, pmix_gds_base_active_module_t) {
        if (NULL == active->module->assign_module) {
            continue;
        }
        if (PMIX_SUCCESS == active->module->assign_module(info, ninfo, &pri)) {
            if (pri < 0) {
                /* use the default priority from the component */
                pri = active->pri;
            }
            if (priority < pri) {
                mod = active->module;
                priority = pri;
            }
        }
    }

    return mod;
}

pmix_status_t pmix_gds_base_setup_fork(const pmix_proc_t *proc,
                                       char ***env)
{
    pmix_gds_base_active_module_t *active;
    pmix_status_t rc;

    if (!pmix_gds_globals.initialized) {
        return PMIX_ERR_INIT;
    }

    PMIX_LIST_FOREACH(active, &pmix_gds_globals.actives, pmix_gds_base_active_module_t) {
        if (NULL == active->module->setup_fork) {
            continue;
        }
        if (PMIX_SUCCESS != (rc = active->module->setup_fork(proc, env))) {
            return rc;
        }
    }

    return PMIX_SUCCESS;
}

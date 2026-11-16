/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "opal_config.h"

#include <stdlib.h>

#include "opal/constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/mca.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/memory/base/empty.h"
#include "opal/mca/memory/memory.h"
#include "opal/runtime/opal.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/memory/base/static-components.h"

static int empty_query(int *priority)
{
    *priority = 0;
    return OPAL_SUCCESS;
}

/*
 * Local variables
 */
static opal_memory_base_component_2_0_0_t empty_component = {
    /* Empty / safe functions to call if no memory component is selected */
    .memoryc_query = empty_query,
    .memoryc_register = opal_memory_base_component_register_empty,
    .memoryc_deregister = opal_memory_base_component_deregister_empty,
    .memoryc_set_alignment = opal_memory_base_component_set_alignment_empty,
};

/*
 * Globals
 */
opal_memory_base_component_2_0_0_t *opal_memory = &empty_component;

void opal_memory_base_malloc_init_hook(void)
{
    if (opal_memory->memoryc_init_hook) {
        opal_memory->memoryc_init_hook();
    }
}

typedef struct {
    mca_base_component_list_item_t *cli;
    int priority;
} memory_candidate_t;

static bool memory_selection_is_explicit(void)
{
    char **names = NULL;
    bool include_mode = false;

    if (OPAL_SUCCESS
        != mca_base_component_parse_requested(opal_memory_base_framework.framework_selection,
                                              &include_mode, &names)) {
        return false;
    }
    include_mode = include_mode && NULL != names;
    opal_argv_free(names);
    return include_mode;
}

static int memory_component_open(mca_base_component_list_item_t *cli)
{
    const mca_base_component_t *component = cli->cli_component;
    int output = opal_memory_base_framework.framework_output;
    int ret;

    if (NULL == component->mca_open_component) {
        return OPAL_SUCCESS;
    }

    ret = component->mca_open_component();
    if (OPAL_SUCCESS == ret) {
        return OPAL_SUCCESS;
    }

    if (OPAL_ERR_NOT_AVAILABLE != ret
        && mca_base_show_load_errors(component->mca_type_name, component->mca_component_name)) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR, output,
                            "mca: base: components_open: component %s / %s open function failed",
                            component->mca_type_name, component->mca_component_name);
    }
    opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output,
                        "memory: component %s did not open (%d), trying the next one",
                        component->mca_component_name, ret);

    mca_base_component_close(component, output);
    opal_list_remove_item(&opal_memory_base_framework.framework_components, &cli->super);
    OBJ_RELEASE(cli);
    return ret;
}

/*
 * Every memory component that configure found is built in.  Open the
 * ones that the memory MCA variable allows in priority order, and use
 * the first one that opens.  A component whose open fails (e.g.,
 * because a device is missing) leaves the next one to be tried.
 */
static int opal_memory_base_open(mca_base_open_flag_t flags)
{
    opal_list_t *components = &opal_memory_base_framework.framework_components;
    int output = opal_memory_base_framework.framework_output;
    mca_base_component_list_item_t *cli, *next, *selected = NULL;
    memory_candidate_t *candidates;
    size_t count = 0;
    int ret;

    if (flags & MCA_BASE_OPEN_FIND_COMPONENTS) {
        ret = mca_base_component_find(NULL, &opal_memory_base_framework, false,
                                      !(flags & MCA_BASE_OPEN_STATIC_ONLY));
        if (OPAL_SUCCESS != ret) {
            return ret;
        }
    }

    /* drop the components that the memory MCA variable excludes */
    ret = mca_base_components_filter(&opal_memory_base_framework, MCA_BASE_METADATA_PARAM_NONE);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    candidates = calloc(opal_list_get_size(components) + 1, sizeof(*candidates));
    if (NULL == candidates) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    OPAL_LIST_FOREACH (cli, components, mca_base_component_list_item_t) {
        opal_memory_base_component_2_0_0_t *component = (opal_memory_base_component_2_0_0_t *)
                                                            cli->cli_component;
        int priority;

        if (OPAL_SUCCESS == component->memoryc_query(&priority) && priority >= 0) {
            candidates[count++] = (memory_candidate_t){.cli = cli, .priority = priority};
        }
    }

    while (NULL == selected) {
        memory_candidate_t *best = NULL;

        for (size_t i = 0; i < count; ++i) {
            if (NULL != candidates[i].cli
                && (NULL == best || candidates[i].priority > best->priority)) {
                best = &candidates[i];
            }
        }
        if (NULL == best) {
            break;
        }

        if (OPAL_SUCCESS == memory_component_open(best->cli)) {
            selected = best->cli;
        }
        best->cli = NULL;
    }
    free(candidates);

    /* unload the components that were not opened */
    OPAL_LIST_FOREACH_SAFE (cli, next, components, mca_base_component_list_item_t) {
        if (cli != selected) {
            mca_base_component_unload(cli->cli_component, output);
            opal_list_remove_item(components, &cli->super);
            OBJ_RELEASE(cli);
        }
    }

    if (NULL == selected) {
        if (memory_selection_is_explicit()) {
            opal_show_help("help-mca-base.txt", "find-available:not-valid", true,
                           opal_gethostname(), opal_memory_base_framework.framework_name,
                           opal_memory_base_framework.framework_selection);
            return OPAL_ERR_NOT_FOUND;
        }
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output,
                            "memory: no component can be used, memory hooks are disabled");
        return OPAL_SUCCESS;
    }

    opal_memory = (opal_memory_base_component_2_0_0_t *) selected->cli_component;
    opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output, "memory: using component %s",
                        selected->cli_component->mca_component_name);
    return OPAL_SUCCESS;
}

static int opal_memory_base_close(void)
{
    /* mca_base_framework_close() closes the components next, and
       nothing may call into them after that */
    opal_memory = &empty_component;

    return OPAL_SUCCESS;
}

/* Use the default register function */
MCA_BASE_FRAMEWORK_DECLARE(opal, memory, "memory hooks", NULL, opal_memory_base_open,
                           opal_memory_base_close, mca_memory_base_static_components, 0);

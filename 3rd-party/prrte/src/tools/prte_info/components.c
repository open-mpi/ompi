/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <stdlib.h>
#include <string.h>

#include "src/runtime/runtime.h"

#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/include/constants.h"

#include "src/util/pmix_argv.h"
#include "src/util/pmix_cmd_line.h"
#include "src/util/error.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

#include "src/include/prte_frameworks.h"

#include "src/mca/prteinstalldirs/prteinstalldirs.h"

#include "src/mca/base/pmix_mca_base_component_repository.h"
#include "src/tools/prte_info/pinfo.h"

/*
 * Public variables
 */

static void component_map_construct(prte_info_component_map_t *map)
{
    map->type = NULL;
}
static void component_map_destruct(prte_info_component_map_t *map)
{
    if (NULL != map->type) {
        free(map->type);
    }
    /* the type close functions will release the
     * list of components
     */
}
PMIX_CLASS_INSTANCE(prte_info_component_map_t,
                    pmix_list_item_t,
                    component_map_construct,
                    component_map_destruct);

pmix_pointer_array_t prte_component_map = PMIX_POINTER_ARRAY_STATIC_INIT;

/*
 * Private variables
 */

static bool opened_components = false;

static int info_register_framework(pmix_mca_base_framework_t *framework,
                                   pmix_pointer_array_t *component_map)
{
    prte_info_component_map_t *map;
    int rc;

    rc = pmix_mca_base_framework_register(framework, PMIX_MCA_BASE_REGISTER_ALL);
    if (PMIX_SUCCESS != rc && PMIX_ERR_BAD_PARAM != rc) {
        return rc;
    }

    if (NULL != component_map) {
        map = PMIX_NEW(prte_info_component_map_t);
        map->type = strdup(framework->framework_name);
        map->components = &framework->framework_components;
        map->failed_components = &framework->framework_failed_components;
        pmix_pointer_array_add(component_map, map);
    }

    return rc;
}

static int register_project_frameworks(const char *project_name,
                                       pmix_mca_base_framework_t **frameworks,
                                       pmix_pointer_array_t *component_map)
{
    int i, rc = PRTE_SUCCESS;

    for (i = 0; NULL != frameworks[i]; i++) {
        if (PMIX_SUCCESS != (rc = info_register_framework(frameworks[i], component_map))) {
            if (PMIX_ERR_BAD_PARAM == rc) {
                fprintf(stderr,
                        "\nA \"bad parameter\" error was encountered when opening the %s %s "
                        "framework\n",
                        project_name, frameworks[i]->framework_name);
                fprintf(stderr, "The output received from that framework includes the following "
                                "parameters:\n\n");
            } else if (PMIX_ERR_NOT_AVAILABLE != rc) {
                fprintf(stderr, "%s_info_register: %s failed\n", project_name,
                        frameworks[i]->framework_name);
                rc = PRTE_ERROR;
            } else {
                continue;
            }

            break;
        }
    }

    return rc;
}

static int register_framework_params(pmix_pointer_array_t *component_map)
{
    int rc;

    /* Register mca/base parameters */
    if (PMIX_SUCCESS != pmix_mca_base_open(NULL)) {
        pmix_show_help("help-prte_info.txt", "lib-call-fail", true, "mca_base_open", __FILE__,
                       __LINE__);
        return PRTE_ERROR;
    }

    /* Register the PRTE layer's MCA parameters */
    if (PRTE_SUCCESS != (rc = prte_register_params())) {
        fprintf(stderr, "prte_info_register: prte_register_params failed\n");
        return rc;
    }

    return register_project_frameworks("prte", prte_frameworks, component_map);
}

void prte_info_components_open(void)
{
    if (opened_components) {
        return;
    }

    opened_components = true;

    /* init the map */
    PMIX_CONSTRUCT(&prte_component_map, pmix_pointer_array_t);
    pmix_pointer_array_init(&prte_component_map, 256, INT_MAX, 128);

    register_framework_params(&prte_component_map);
}

/*
 * Not to be confused with prte_info_close_components.
 */
void prte_info_components_close(void)
{
    int i;
    prte_info_component_map_t *map;

    if (!opened_components) {
        return;
    }

    for (i = 0; NULL != prte_frameworks[i]; i++) {
        (void) pmix_mca_base_framework_close(prte_frameworks[i]);
    }

    for (i = 0; i < prte_component_map.size; i++) {
        if (NULL
            != (map = (prte_info_component_map_t *) pmix_pointer_array_get_item(&prte_component_map,
                                                                                i))) {
            PMIX_RELEASE(map);
        }
    }

    PMIX_DESTRUCT(&prte_component_map);

    opened_components = false;
}

/*
 *  Copyright (c) 2013      Mellanox Technologies, Inc.
 *                          All rights reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "oshmem/include/oshmem/constants.h"
#include "oshmem/include/oshmem/frameworks.h"

#include "oshmem/runtime/params.h"
#include "oshmem/runtime/runtime.h"

#include "opal/runtime/opal_info_support.h"
#include "ompi/runtime/ompi_info_support.h"
#include "oshmem/runtime/oshmem_info_support.h"
#include "opal/util/show_help.h"
#include "ompi/include/ompi/constants.h"

const char *oshmem_info_type_oshmem = "oshmem";

static bool oshmem_info_registered = false;

void oshmem_info_register_types(opal_pointer_array_t *mca_types)
{
    int i;

    /* add the top-level type */
    opal_pointer_array_add(mca_types, (void *)oshmem_info_type_oshmem);

    /* push all the types found by autogen */
    for (i = 0; NULL != oshmem_frameworks[i]; i++) {
        opal_pointer_array_add(mca_types, oshmem_frameworks[i]->framework_name);
    }
}

int oshmem_info_register_framework_params(opal_pointer_array_t *component_map)
{
    int rc;

    if (oshmem_info_registered) {
        return OSHMEM_SUCCESS;
    }

    oshmem_info_registered = true;

    /* Register the OSHMEM layer's MCA parameters */
    if (OSHMEM_SUCCESS != (rc = oshmem_shmem_register_params())) {
        fprintf(stderr, "oshmem_info_register: oshmem_register_params failed\n");
        return rc;
    }

    /* Do OMPI interface call */
    rc = ompi_info_register_framework_params(component_map);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    return opal_info_register_project_frameworks(oshmem_info_type_oshmem, oshmem_frameworks, component_map);
}

void oshmem_info_close_components(void)
{
    int i;

    for (i = 0; NULL != oshmem_frameworks[i]; i++) {
        (void) mca_base_framework_close(oshmem_frameworks[i]);
    }

    /* Do OMPI interface call */
    ompi_info_close_components();
}

void oshmem_info_show_oshmem_version(const char *scope)
{
    char *tmp, *tmp2;

    if (0 < asprintf(&tmp, "%s:version:full", oshmem_info_type_oshmem)) {
        tmp2 = opal_info_make_version_str(scope,
                OSHMEM_MAJOR_VERSION, OSHMEM_MINOR_VERSION,
                OSHMEM_RELEASE_VERSION,
                OSHMEM_GREEK_VERSION,
                OSHMEM_REPO_REV);
        opal_info_out("Open SHMEM", tmp, tmp2);
        free(tmp);
        free(tmp2);
    }
    if(0 < asprintf(&tmp, "%s:version:repo", oshmem_info_type_oshmem)) {
        opal_info_out("Open SHMEM repo revision", tmp, OSHMEM_REPO_REV);
        free(tmp);
    }
    if (0 < asprintf(&tmp, "%s:version:release_date", oshmem_info_type_oshmem)) {
        opal_info_out("Open SHMEM release date", tmp, OSHMEM_RELEASE_DATE);
        free(tmp);
    }

    /* Do OMPI interface call */
    ompi_info_show_ompi_version(scope);
}

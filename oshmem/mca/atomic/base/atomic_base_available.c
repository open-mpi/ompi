/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "oshmem_config.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "oshmem/constants.h"
#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"

/*
 * Private functions
 */
static int init_query(const mca_base_component_t * ls,
                      bool enable_progress_threads,
                      bool enable_threads);

/*
 * Scan down the list of successfully opened components and query each of
 * them (the opened list will be one or more components.  If the user
 * requested a specific component, it will be the only component in the
 * opened list).  Create and populate the available list of all
 * components who indicate that they want to be considered for selection.
 * Close all components who do not want to be considered for selection,
 * and destroy the opened list.
 *
 * Also find the basic component while we're doing all of this, and save
 * it in a global variable so that we can find it easily later (e.g.,
 * during scope selection).
 */
int mca_atomic_base_find_available(bool enable_progress_threads,
                                   bool enable_threads)
{
    mca_base_component_list_item_t *cli, *next;
    const mca_base_component_t *component;

    OPAL_LIST_FOREACH_SAFE(cli, next, &oshmem_atomic_base_framework.framework_components, mca_base_component_list_item_t) {
        component = cli->cli_component;

        /* Call a subroutine to do the work, because the component may
           represent different versions of the coll MCA. */

        if (OSHMEM_SUCCESS != init_query(component, enable_progress_threads,
                                       enable_threads)) {
            /* If the component doesn't want to run, then close it.
               Now close it out and release it from the DSO repository (if it's there). */
            opal_list_remove_item(&oshmem_atomic_base_framework.framework_components, &cli->super);
            mca_base_component_close(component, oshmem_atomic_base_framework.framework_output);
            OBJ_RELEASE(cli);
        }
    }

    /* If we have no collective components available, it's an error.
       Thanks for playing! */

    if (opal_list_get_size(&oshmem_atomic_base_framework.framework_components) == 0) {
        ATOMIC_VERBOSE(10,
                      "atomic:find_available: no components available!");
        return OSHMEM_ERROR;
    }

    /* All done */

    return mca_atomic_base_select();
}

/*
 * Query a component, see if it wants to run at all.  If it does, save
 * some information.  If it doesn't, close it.
 */
static int init_query(const mca_base_component_t * component,
                      bool enable_progress_threads,
                      bool enable_threads)
{
    int ret;

    ATOMIC_VERBOSE(10,
                   "atomic:find_available: querying atomic component %s",
                   component->mca_component_name);

    /* This component has already been successfully opened.  So now
     query it. */

    if (1 == component->mca_type_major_version
            && 0 == component->mca_type_minor_version
            && 0 == component->mca_type_release_version) {

        mca_atomic_base_component_t *atomic =
                (mca_atomic_base_component_t *) component;

        ret = atomic->atomic_init(enable_progress_threads, enable_threads);
    } else {
        /* Unrecognized coll API version */

        ATOMIC_VERBOSE(10,
                       "atomic:find_available: unrecognized atomic API version (%d.%d.%d, ignored)",
                      component->mca_type_major_version,
                      component->mca_type_minor_version,
                      component->mca_type_release_version);
        return OSHMEM_ERROR;
    }

    /* Query done -- look at the return value to see what happened */

    if (OSHMEM_SUCCESS != ret) {
        ATOMIC_VERBOSE(10,
                       "atomic:find_available: atomic component %s is not available",
                       component->mca_component_name);
        if (NULL != component->mca_close_component) {
            component->mca_close_component();
        }
    } else {
        ATOMIC_VERBOSE(10,
                       "atomic:find_available: atomic component %s is available",
                       component->mca_component_name);
    }

    /* All done */

    return ret;
}

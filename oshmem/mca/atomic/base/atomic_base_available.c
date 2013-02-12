/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
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

#include "orte/util/show_help.h"

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"


/*
 * Global variables
 */
bool mca_atomic_base_components_available_valid = false;
opal_list_t mca_atomic_base_components_available;


/*
 * Private functions
 */
static int init_query(const mca_base_component_t * ls,
                      mca_base_component_priority_list_item_t * entry,
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
    bool found = false;
    mca_base_component_priority_list_item_t *entry;
    opal_list_item_t *p;
    const mca_base_component_t *component;

    /* Initialize the list */

    OBJ_CONSTRUCT(&mca_atomic_base_components_available, opal_list_t);
    mca_atomic_base_components_available_valid = true;

    /* The list of components that we should check has already been
       established in mca_coll_base_open. */

    for (found = false,
         p = opal_list_remove_first(&mca_atomic_base_components_opened);
         p != NULL;
         p = opal_list_remove_first(&mca_atomic_base_components_opened)) {
        component = ((mca_base_component_list_item_t *) p)->cli_component;

        /* Call a subroutine to do the work, because the component may
           represent different versions of the coll MCA. */

        entry = OBJ_NEW(mca_base_component_priority_list_item_t);
        entry->super.cli_component = component;
        entry->cpli_priority = 0;
        if (OSHMEM_SUCCESS == init_query(component, entry,
                                       enable_progress_threads,
                                       enable_threads)) {
            opal_list_append(&mca_atomic_base_components_available,
                             (opal_list_item_t *) entry);
            found = true;
        } else {

            /* If the component doesn't want to run, then close it.
               It's already had its close() method invoked; now close
               it out of the DSO repository (if it's there). */

            mca_base_component_repository_release(component);
            OBJ_RELEASE(entry);
        }

        /* Free the entry from the "opened" list */

        OBJ_RELEASE(p);
    }

    /* The opened list is now no longer useful and we can free it */

    OBJ_DESTRUCT(&mca_atomic_base_components_opened);
    mca_atomic_base_components_opened_valid = false;

    /* If we have no atomic components available, it's an error.
       Thanks for playing! */

    if (!found) {
        /* Need to free all items in the list */
        OBJ_DESTRUCT(&mca_atomic_base_components_available);
        mca_atomic_base_components_available_valid = false;
        ATOMIC_VERBOSE(10,"atomic:find_available: no atomic components available!");
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
                      mca_base_component_priority_list_item_t * entry,
                      bool enable_progress_threads, bool enable_threads)
{
    int ret;

    ATOMIC_VERBOSE(10,"atomic:find_available: querying atomic component %s",
		            component->mca_component_name);

    /* This component has already been successfully opened.  So now
       query it. */

    if (1 == component->mca_type_major_version &&
        0 == component->mca_type_minor_version &&
        0 == component->mca_type_release_version) {

            mca_atomic_base_component_t *atomic =
                (mca_atomic_base_component_t *) component;

            ret = atomic->atomic_init(enable_progress_threads,
                                    enable_threads);
    } else {
        /* Unrecognized coll API version */

        ATOMIC_VERBOSE(10,"atomic:find_available: unrecognized atomic API version (%d.%d.%d, ignored)",
                            component->mca_type_major_version,
                            component->mca_type_minor_version,
                            component->mca_type_release_version);
        return OSHMEM_ERROR;
    }

    /* Query done -- look at the return value to see what happened */

    if (OSHMEM_SUCCESS != ret) {
        ATOMIC_VERBOSE(10,"atomic:find_available: atomic component %s is not available",
                            component->mca_component_name);
        if (NULL != component->mca_close_component) {
            component->mca_close_component();
        }
    } else {
        ATOMIC_VERBOSE(10,"atomic:find_available: atomic component %s is available",
                            component->mca_component_name);
    }

    /* All done */

    return ret;
}

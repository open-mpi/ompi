/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "opal_config.h"

#include "opal/class/opal_list.h"
#include "opal/constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/mca/base/mca_base_vari.h"
#include "opal/mca/mca.h"
#include "opal/util/output.h"

/* Deregister the component's MCA variables and invalidate its performance
   variables.  Once this returns, no MPI_T call is using them (see
   mca_base_var_group_deregister()).  Harmless if they are already
   deregistered. */
static void component_vars_deregister(const mca_base_component_t *component)
{
    int ret;

    /* Hold the lock across the lookup too: the group name table is
       modified under it */
    mca_base_tool_lock();
    ret = mca_base_var_group_find_locked(component->mca_project_name, component->mca_type_name,
                                         component->mca_component_name);
    if (0 <= ret) {
        mca_base_var_group_deregister_locked(ret);
    }
    mca_base_tool_unlock();
}

/* Invalidate the component's performance variables (and events).  Once
   this returns, no MPI_T call is using them (see
   mca_base_var_group_invalidate()). */
static void component_pvars_invalidate(const mca_base_component_t *component)
{
    int ret;

    /* As in component_vars_deregister() */
    mca_base_tool_lock();
    ret = mca_base_var_group_find_locked(component->mca_project_name, component->mca_type_name,
                                         component->mca_component_name);
    if (0 <= ret) {
        mca_base_var_group_invalidate_locked(ret);
    }
    mca_base_tool_unlock();
}

void mca_base_component_unload(const mca_base_component_t *component, int output_id)
{
    /* Unload */
    opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output_id,
                        "mca: base: close: unloading component %s", component->mca_component_name);

    component_vars_deregister(component);

    mca_base_component_repository_release(component);
}

void mca_base_component_close(const mca_base_component_t *component, int output_id)
{
    /* Invalidate the component's performance variables BEFORE its close
       function frees what their callbacks use, as
       mca_base_framework_close() does for a whole framework.

       Its control variables stay registered until it is unloaded: a
       component that refuses to close stays loaded, and may still use
       them.  So the close function must not free or change the storage
       of its control variables; deregistering them (below, at unload)
       frees their string values, and until then an MPI_T call on another
       thread may still read them.

       The performance variables of a component that refuses to close
       stay invalid.  Its caller drops it from the framework anyway, and
       its close function may have freed some of what their callbacks use
       before it failed.  Opening the framework again registers them
       again, as new, valid variables under the same names. */
    component_pvars_invalidate(component);

    /* Close */
    if (NULL != component->mca_close_component) {
        if (OPAL_SUCCESS == component->mca_close_component()) {
            opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output_id,
                                "mca: base: close: component %s closed",
                                component->mca_component_name);
        } else {
            opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, output_id,
                                "mca: base: close: component %s refused to close [drop it]",
                                component->mca_component_name);
            return;
        }
    }

    mca_base_component_unload(component, output_id);
}

int mca_base_framework_components_close(mca_base_framework_t *framework,
                                        const mca_base_component_t *skip)
{
    return mca_base_components_close(framework->framework_output, &framework->framework_components,
                                     skip);
}

int mca_base_components_close(int output_id, opal_list_t *components,
                              const mca_base_component_t *skip)
{
    mca_base_component_list_item_t *cli, *next;

    /* Close and unload all components in the available list, except the
       "skip" item.  This is handy to close out all non-selected
       components.  It's easier to simply remove the entire list and
       then simply re-add the skip entry when done. */

    OPAL_LIST_FOREACH_SAFE (cli, next, components, mca_base_component_list_item_t) {
        if (skip == cli->cli_component) {
            continue;
        }

        mca_base_component_close(cli->cli_component, output_id);
        opal_list_remove_item(components, &cli->super);

        OBJ_RELEASE(cli);
    }

    /* All done */
    return OPAL_SUCCESS;
}

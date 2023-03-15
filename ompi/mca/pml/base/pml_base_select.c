/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2020-2022 Amazon.com, Inc. or its affiliates.  All Rights
 * Copyright (c) 2018-2020 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal_progress.h"
#include "ompi/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/mca/pmix/pmix-internal.h"

#include "ompi/constants.h"
#include "ompi/instance/instance.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/proc/proc.h"

typedef struct opened_component_t {
  opal_list_item_t super;
  mca_pml_base_component_t *om_component;
} opened_component_t;


static int mca_pml_base_finalize (void) {
  if (NULL != mca_pml_base_selected_component.pmlm_finalize) {
      return mca_pml_base_selected_component.pmlm_finalize();
  }

  return OMPI_SUCCESS;
}

/**
 * Function for selecting one component from all those that are
 * available.
 *
 * Call the init function on all available components and get their
 * priorities.  Select the component with the highest priority.  All
 * other components will be closed and unloaded.  The selected component
 * will have all of its function pointers saved and returned to the
 * caller.
 */
int mca_pml_base_select(bool enable_progress_threads,
                        bool enable_mpi_threads)
{
    int i, priority = 0, best_priority = -1, ret = 0;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    mca_pml_base_component_t *component = NULL, *best_component = NULL;
    mca_pml_base_module_t *module = NULL, *best_module = NULL;
    opal_list_t opened;
    opened_component_t *om = NULL;
    bool found_pml;

    /* Traverse the list of available components; call their init
       functions. */

    OBJ_CONSTRUCT(&opened, opal_list_t);
    OPAL_LIST_FOREACH(cli, &ompi_pml_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (mca_pml_base_component_t *) cli->cli_component;

        /* if there is an include list - item must be in the list to be included */
        found_pml = false;
        for( i = 0; i < opal_pointer_array_get_size(&mca_pml_base_pml); i++) {
            char * tmp_val = NULL;
            tmp_val = (char *) opal_pointer_array_get_item(&mca_pml_base_pml, i);
            if( NULL == tmp_val) {
                continue;
            }

            if(0 == strncmp(component->pmlm_version.mca_component_name,
                            tmp_val, strlen(component->pmlm_version.mca_component_name)) ) {
                found_pml = true;
                break;
            }
        }

        if(!found_pml && opal_pointer_array_get_size(&mca_pml_base_pml)) {
            opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                                     "select: component %s not in the include list",
                                     component->pmlm_version.mca_component_name );

            continue;
        }

        /* if there is no init function - ignore it */
        if (NULL == component->pmlm_init) {
            opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                                 "select: no init function; ignoring component %s",
                                 component->pmlm_version.mca_component_name );
            continue;
        }

        /* Init component to get its priority */
        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                             "select: initializing %s component %s",
                             component->pmlm_version.mca_type_name,
                             component->pmlm_version.mca_component_name );
        priority = best_priority;
        module = component->pmlm_init(&priority, enable_progress_threads,
                                      enable_mpi_threads);
        if (NULL == module) {
            opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                                 "select: init returned failure for component %s",
                                 component->pmlm_version.mca_component_name );
            continue;
        }

        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                             "select: init returned priority %d", priority );
        if (priority > best_priority) {
            best_priority = priority;
            best_component = component;
            best_module = module;
        }

        om = (opened_component_t*)malloc(sizeof(opened_component_t));
        if (NULL == om) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        OBJ_CONSTRUCT(om, opal_list_item_t);
        om->om_component = component;
        opal_list_append(&opened, (opal_list_item_t*) om);
    }

    /* Finished querying all components.  Check for the bozo case. */

    if( NULL == best_component ) {
        opal_show_help("help-mca-base.txt", "find-available:none found",
                       true, "pml",
                       opal_process_info.nodename,
                       "pml");
        for( i = 0; i < opal_pointer_array_get_size(&mca_pml_base_pml); i++) {
            char * tmp_val = NULL;
            tmp_val = (char *) opal_pointer_array_get_item(&mca_pml_base_pml, i);
            if( NULL == tmp_val) {
                continue;
            }
            ompi_rte_abort(1, "PML %s cannot be selected", tmp_val);
        }
        if(0 == i) {
            ompi_rte_abort(2, "No pml component available.  This shouldn't happen.");
        }
    }

    opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                         "selected %s best priority %d\n",
                         best_component->pmlm_version.mca_component_name, best_priority);

    /* Save the winner */

    mca_pml_base_selected_component = *best_component;
    mca_pml = *best_module;
    opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                         "select: component %s selected",
                         mca_pml_base_selected_component.pmlm_version.mca_component_name );

    /* Finalize all non-selected components */

    for (item = opal_list_remove_first(&opened);
         NULL != item;
         item = opal_list_remove_first(&opened)) {
        om = (opened_component_t *) item;

        if (om->om_component != best_component ) {
            /* Finalize */

            if (NULL != om->om_component->pmlm_finalize) {

                /* Blatantly ignore the return code (what would we do to
                   recover, anyway?  This component is going away, so errors
                   don't matter anymore) */

                om->om_component->pmlm_finalize();
                opal_output_verbose(10, ompi_pml_base_framework.framework_output,
                                    "select: component %s not selected / finalized",
                                    om->om_component->pmlm_version.mca_component_name);
            }
        }
        OBJ_DESTRUCT( om );
        free(om);
    }
    OBJ_DESTRUCT( &opened );

    /* This base function closes, unloads, and removes from the
       available list all unselected components.  The available list will
       contain only the selected component. */

    mca_base_components_close(ompi_pml_base_framework.framework_output,
                              &ompi_pml_base_framework.framework_components,
                              (mca_base_component_t *) best_component);

    /* register the winner's callback */
    if( NULL != mca_pml.pml_progress ) {
        opal_progress_register(mca_pml.pml_progress);
    }

#if OPAL_ENABLE_FT_MPI
    if( NULL == mca_pml.pml_revoke_comm ) {
        /* do not crash when calling a not implemented function after a failure is
         * reported, return a NOT_IMPLEMENTED error */
        mca_pml.pml_revoke_comm = mca_pml_base_revoke_comm;
    }
#endif /* OPAL_ENABLE_FT_MPI */

    /* register winner in the modex */
    ret = mca_pml_base_pml_selected(best_component->pmlm_version.mca_component_name);

    /* All done */
    ompi_mpi_instance_append_finalize (mca_pml_base_finalize);

    return ret;
}

/* need a "commonly" named PML structure so everything ends up in the
   same modex field */
static mca_base_component_t pml_base_component = {
    OMPI_MCA_BASE_VERSION_2_1_0("pml", 2, 0, 0),
    .mca_component_name = "base",
    .mca_component_major_version = 2,
    .mca_component_minor_version = 0,
    .mca_component_release_version = 0,
};


/*
 * If direct modex, then publish PML for all procs. If full modex then
 * publish PML for rank 0 only. This information is used during add_procs
 * to perform PML check.
 * During PML check, for direct modex, compare our PML with the peer's
 * PML for all procs in the add_procs call. This does not change the
 * connection complexity of modex transfers, since adding the proc is
 * going to get the peer information in the MTL/PML/BTL anyway.
 * For full modex, compare our PML with rank 0.
 * Direct Modex is performed when collect_all_data is false, as we do
 * not perform a fence operation during MPI_Init if async_modex is true.
 * If async_modex is false and collect_all_data is false then we do a
 * zero-byte barrier and we would still require direct modex during
 * add_procs
 */
int
mca_pml_base_pml_selected(const char *name)
{
    int rc = 0;

    if (!opal_pmix_collect_all_data || 0 == OMPI_PROC_MY_NAME->vpid) {
        OPAL_MODEX_SEND(rc, PMIX_GLOBAL, &pml_base_component, name,
                        strlen(name) + 1);
    }
    return rc;
}

static int
mca_pml_base_pml_check_selected_impl(const char *my_pml,
                                     opal_process_name_t proc_name)
{
    size_t size;
    int ret = 0;
    char *remote_pml;

    /* if we are proc_name=OMPI_PROC_MY_NAME, then we can also assume success */
    if (0 == opal_compare_proc(ompi_proc_local()->super.proc_name, proc_name)) {
        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                            "check:select: PML check not necessary on self");
        return OMPI_SUCCESS;
    }
    OPAL_MODEX_RECV_STRING(ret,
                           mca_base_component_to_string(&pml_base_component),
                           &proc_name, (void**) &remote_pml, &size);
    if (PMIX_ERR_NOT_FOUND == ret) {
        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                            "check:select: PML modex for process %s not found",
                            OMPI_NAME_PRINT(&proc_name));
        return OMPI_ERR_NOT_FOUND;
    }

    /* the remote pml returned should never be NULL if an error
     * wasn't returned, but just to be safe, and since the check
     * is fast...let's be sure
     */
    if (NULL == remote_pml) {
        opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                            "check:select: got a NULL pml from process %s",
                            OMPI_NAME_PRINT(&proc_name));
        return OMPI_ERR_UNREACH;
    }

    opal_output_verbose( 10, ompi_pml_base_framework.framework_output,
                        "check:select: checking my pml %s against process %s"
                        " pml %s", my_pml, OMPI_NAME_PRINT(&proc_name),
                        remote_pml);

    /* if that module doesn't match my own, return an error */
    if ((size != strlen(my_pml) + 1) ||
        (0 != strcmp(my_pml, remote_pml))) {
        char *errhost = NULL;
        OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, PMIX_HOSTNAME, &proc_name,
                                       &(errhost), PMIX_STRING);
        opal_output(0, "%s selected pml %s, but peer %s on %s selected pml %s",
                    OMPI_NAME_PRINT(&ompi_proc_local()->super.proc_name),
                    my_pml, OMPI_NAME_PRINT(&proc_name),
                    (NULL == errhost) ? "unknown" : errhost,
                    remote_pml);
        free(remote_pml);
        free(errhost);
         /* cleanup before returning */
        return OMPI_ERR_UNREACH;
    }

    free(remote_pml);
    return OMPI_SUCCESS;
}

int
mca_pml_base_pml_check_selected(const char *my_pml,
                                ompi_proc_t **procs,
                                size_t nprocs)
{
    int ret = 0;
    size_t i;

    if (!ompi_pml_base_check_pml) {
        return OMPI_SUCCESS;
    }

    if (!opal_pmix_collect_all_data) {
        /*
         * If direct modex, then compare our PML with the peer's PML
         * for all procs
         */
        for (i = 0; i < nprocs; i++) {
            ret = mca_pml_base_pml_check_selected_impl(
                                                 my_pml,
                                                 procs[i]->super.proc_name);
            if (ret) {
                return ret;
            }
        }
    } else {
        /* else if full modex compare our PML with rank 0 */
        opal_process_name_t proc_name = {
                           .jobid = ompi_proc_local()->super.proc_name.jobid,
                           .vpid = 0
        };
        ret = mca_pml_base_pml_check_selected_impl(
                                                 my_pml,
                                                 proc_name);
    }

    return ret;
}

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/util/argv.h"
#include "runtime/runtime.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"

/**
 * Function for weeding out ptl components that don't want to run.
 *
 * Call the init function on all available components to find out if
 * they want to run.  Select all components that don't fail.  Failing
 * components will be closed and unloaded.  The selected modules will
 * be returned to the caller in a opal_list_t.
 */
int mca_ptl_base_select(bool enable_progress_threads,
                        bool enable_mpi_threads)
{
    int i, num_ptls;
    opal_list_item_t *item;
    opal_list_t* useless = OBJ_NEW(opal_list_t);
    mca_base_component_list_item_t *cli;
    mca_ptl_base_component_t *component;
    mca_ptl_base_module_t **modules;
    mca_ptl_base_selected_module_t *sm;

    char** include = opal_argv_split(mca_ptl_base_include, ',');
    char** exclude = opal_argv_split(mca_ptl_base_exclude, ',');

    /* Traverse the list of opened modules; call their init
       functions. */

    while( NULL != (item = opal_list_remove_first(&mca_ptl_base_components_opened)) ) {
        bool keep_me = false;

        cli = (mca_base_component_list_item_t *) item;
        component = (mca_ptl_base_component_t *) cli->cli_component;

        /* if there is an include list - item must be in the list to be included */
        if ( NULL != include ) {
            char** argv = include; 
            while(argv && *argv) {
                if(strcmp(component->ptlm_version.mca_component_name,*argv) == 0) {
                    keep_me = true;
                    break;
                }
                argv++;
            }
            /* otherwise - check the exclude list to see if this item has been specifically excluded */
        } else if ( NULL != exclude ) {
            char** argv = exclude;
            keep_me = true;
            while(argv && *argv) {
                if(strcmp(component->ptlm_version.mca_component_name,*argv) == 0) {
                    keep_me = false;
                    break;
                }
                argv++;
            }
        }
        if( keep_me == false) {
            opal_list_append( useless, item );
            continue;
        }

        opal_output_verbose(10, mca_ptl_base_output, 
                            "select: initializing %s component %s",
                            component->ptlm_version.mca_type_name,
                            component->ptlm_version.mca_component_name);
        if (NULL == component->ptlm_init) {
            opal_output_verbose(10, mca_ptl_base_output,
                                "select: no init function; ignoring component");
            opal_list_append( useless, item );
            continue;
        } else {
            modules = component->ptlm_init(&num_ptls, enable_progress_threads,
                                           enable_mpi_threads);

            /* If the component didn't initialize, remove it from the opened
               list and remove it from the component repository */

            if (NULL == modules) {
                opal_output_verbose( 10, mca_ptl_base_output,
                                     "select: %s PTL init returned failure",
                                     component->ptlm_version.mca_component_name);
                opal_list_append( useless, item );
                continue;
            } 

            /* Otherwise, it initialized properly.  Save it. */

            else {
                opal_output_verbose(10, mca_ptl_base_output,
                                    "select: init returned success");
                opal_list_append( &mca_ptl_base_components_initialized, item );
                for (i = 0; i < num_ptls; ++i) {
                    sm = malloc(sizeof(mca_ptl_base_selected_module_t));
                    if (NULL == sm) {
                        return OMPI_ERR_OUT_OF_RESOURCE;
                    }
                    OBJ_CONSTRUCT(sm, opal_list_item_t);
                    sm->pbsm_component = component;
                    sm->pbsm_module = modules[i];
                    opal_list_append(&mca_ptl_base_modules_initialized,
                                     (opal_list_item_t*) sm);
                }
                free(modules);
            }
        }
    }

    /* All useless components have to be cleanly removed */
    mca_base_components_close( mca_ptl_base_output, useless, NULL );
    OBJ_RELEASE( useless );

    /* Finished querying all components.  Check for the bozo case. */

    if (0 == opal_list_get_size(&mca_ptl_base_modules_initialized)) {
        /* JMS Replace with show_help */
        orte_abort(1, "No ptl components available.  This shouldn't happen.");
    }

    /* All done */
    return OMPI_SUCCESS;
}

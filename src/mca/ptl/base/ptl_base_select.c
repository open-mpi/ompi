/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "util/argv.h"
#include "runtime/runtime.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"

/**
 * Function for weeding out ptl components that don't want to run.
 *
 * Call the init function on all available components to find out if
 * they want to run.  Select all components that don't fail.  Failing
 * components will be closed and unloaded.  The selected modules will
 * be returned to the caller in a ompi_list_t.
 */
int mca_ptl_base_select(bool *allow_multi_user_threads, 
                        bool *have_hidden_threads)
{
  int i, num_ptls;
  bool user_threads, hidden_threads;
  ompi_list_item_t *item;
  mca_base_component_list_item_t *cli;
  mca_ptl_base_component_t *component;
  mca_ptl_base_module_t **modules;
  mca_ptl_base_selected_module_t *sm;

  char** include = ompi_argv_split(mca_ptl_base_include, ',');
  char** exclude = ompi_argv_split(mca_ptl_base_exclude, ',');

  /* Traverse the list of opened modules; call their init
     functions. */

  item  = ompi_list_get_first(&mca_ptl_base_components_opened);
  while(item != ompi_list_get_end(&mca_ptl_base_components_opened)) {
    ompi_list_item_t *next = ompi_list_get_next(item);
    cli = (mca_base_component_list_item_t *) item;

    component = (mca_ptl_base_component_t *) cli->cli_component;

    /* if there is an include list - item must be in the list to be included */
    if ( NULL != include ) {
        char** argv = include; 
        bool found = false;
        while(argv && *argv) {
            if(strcmp(component->ptlm_version.mca_component_name,*argv) == 0) {
                found = true;
                break;
            }
            argv++;
        }
        if(found == false) {
            item = next;
            continue;
        }

    /* otherwise - check the exclude list to see if this item has been specifically excluded */
    } else if ( NULL != exclude ) {
        char** argv = exclude; 
        bool found = false;
        while(argv && *argv) {
            if(strcmp(component->ptlm_version.mca_component_name,*argv) == 0) {
                found = true;
                break;
            }
            argv++;
        }
        if(found == true) {
            item = next;
            continue;
        }
    }

    ompi_output_verbose(10, mca_ptl_base_output, 
                       "select: initializing %s component %s",
                       component->ptlm_version.mca_type_name,
                       component->ptlm_version.mca_component_name);
    if (NULL == component->ptlm_init) {
      ompi_output_verbose(10, mca_ptl_base_output,
                         "select: no init function; ignoring component");
    } else {
      modules = component->ptlm_init(&num_ptls, &user_threads,
                                     &hidden_threads);

      /* If the component didn't initialize, remove it from the opened
         list and remove it from the component repository */

      if (NULL == modules) {
        ompi_output_verbose(10, mca_ptl_base_output,
                           "select: init returned failure");
        ompi_output_verbose(10, mca_ptl_base_output,
                            "select: module %s unloaded",
                            component->ptlm_version.mca_component_name);

        mca_base_component_repository_release((mca_base_component_t *) component);
        ompi_list_remove_item(&mca_ptl_base_components_opened, item);
      } 

      /* Otherwise, it initialized properly.  Save it. */

      else {
        *allow_multi_user_threads &= user_threads;
        *have_hidden_threads |= hidden_threads;

        ompi_output_verbose(10, mca_ptl_base_output,
                           "select: init returned success");

        for (i = 0; i < num_ptls; ++i) {
          sm = malloc(sizeof(mca_ptl_base_selected_module_t));
          if (NULL == sm) {
            return OMPI_ERR_OUT_OF_RESOURCE;
          }
          OBJ_CONSTRUCT(sm, ompi_list_item_t);
          sm->pbsm_component = component;
          sm->pbsm_module = modules[i];
          ompi_list_append(&mca_ptl_base_modules_initialized,
                          (ompi_list_item_t*) sm);
        }
        free(modules);
      }
    }
    item = next;
  }

  /* Finished querying all components.  Check for the bozo case. */

  if (0 == ompi_list_get_size(&mca_ptl_base_modules_initialized)) {
    /* JMS Replace with show_help */
    orte_abort(1, "No ptl components available.  This shouldn't happen.");
  }

  /* All done */

  return OMPI_SUCCESS;
}

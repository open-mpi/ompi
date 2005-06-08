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

#include "util/argv.h"
#include "runtime/runtime.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "mca/bmi/base/base.h"

OBJ_CLASS_INSTANCE(
   mca_bmi_base_selected_module_t,
   ompi_list_item_t,
   NULL,
   NULL);

/**
 * Function for weeding out bmi components that don't want to run.
 *
 * Call the init function on all available components to find out if
 * they want to run.  Select all components that don't fail.  Failing
 * components will be closed and unloaded.  The selected modules will
 * be returned to the caller in a ompi_list_t.
 */
int mca_bmi_base_select(bool enable_progress_threads,
                        bool enable_mpi_threads)
{
  int i, num_bmis;
  ompi_list_item_t *item;
  mca_base_component_list_item_t *cli;
  mca_bmi_base_component_t *component;
  mca_bmi_base_module_t **modules;
  mca_bmi_base_selected_module_t *sm;

  char** include = ompi_argv_split(mca_bmi_base_include, ',');
  char** exclude = ompi_argv_split(mca_bmi_base_exclude, ',');

  /* Traverse the list of opened modules; call their init
     functions. */

  item  = ompi_list_get_first(&mca_bmi_base_components_opened);
  while(item != ompi_list_get_end(&mca_bmi_base_components_opened)) {
    ompi_list_item_t *next = ompi_list_get_next(item);
    cli = (mca_base_component_list_item_t *) item;

    component = (mca_bmi_base_component_t *) cli->cli_component;

    /* if there is an include list - item must be in the list to be included */
    if ( NULL != include ) {
        char** argv = include; 
        bool found = false;
        while(argv && *argv) {
            if(strcmp(component->bmi_version.mca_component_name,*argv) == 0) {
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
            if(strcmp(component->bmi_version.mca_component_name,*argv) == 0) {
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

    ompi_output_verbose(10, mca_bmi_base_output, 
                       "select: initializing %s component %s",
                       component->bmi_version.mca_type_name,
                       component->bmi_version.mca_component_name);
    if (NULL == component->bmi_init) {
      ompi_output_verbose(10, mca_bmi_base_output,
                         "select: no init function; ignoring component");
    } else {
      modules = component->bmi_init(&num_bmis, enable_progress_threads,
                                     enable_mpi_threads);

      /* If the component didn't initialize, remove it from the opened
         list and remove it from the component repository */

      if (NULL == modules) {
        ompi_output_verbose(10, mca_bmi_base_output,
                           "select: init returned failure");
        ompi_output_verbose(10, mca_bmi_base_output,
                            "select: module %s unloaded",
                            component->bmi_version.mca_component_name);

        mca_base_component_repository_release((mca_base_component_t *) component);
        ompi_list_remove_item(&mca_bmi_base_components_opened, item);
      } 

      /* Otherwise, it initialized properly.  Save it. */

      else {
        ompi_output_verbose(10, mca_bmi_base_output,
                           "select: init returned success");

        for (i = 0; i < num_bmis; ++i) {
          sm = OBJ_NEW(mca_bmi_base_selected_module_t);
          if (NULL == sm) {
            return OMPI_ERR_OUT_OF_RESOURCE;
          }
          sm->bmi_component = component;
          sm->bmi_module = modules[i];
          ompi_list_append(&mca_bmi_base_modules_initialized,
                          (ompi_list_item_t*) sm);
        }
        free(modules);
      }
    }
    item = next;
  }

  /* Finished querying all components.  Check for the bozo case. */

  if (0 == ompi_list_get_size(&mca_bmi_base_modules_initialized)) {
    /* JMS Replace with show_help */
    orte_abort(1, "No bmi components available.  This shouldn't happen.");
  }
  return OMPI_SUCCESS;
}

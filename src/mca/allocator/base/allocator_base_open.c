/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/allocator/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/allocator/base/static-modules.h"


/*
 * Global variables
 */
ompi_list_t mca_allocator_base_components;
int mca_allocator_base_output = -1;

/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_allocator_base_open(void)
{
  /* Open up all available modules */
  return mca_base_modules_open("allocator", 0, mca_allocator_base_static_modules, 
                            &mca_allocator_base_components);
}


mca_allocator_base_module_t* mca_allocator_component_lookup(const char* name)
{
    /* Traverse the list of available modules; call their init functions. */
    ompi_list_item_t* item;
    for (item = ompi_list_get_first(&mca_allocator_base_components);
         item != ompi_list_get_end(&mca_allocator_base_components);
         item = ompi_list_get_next(item)) {
         mca_base_module_list_item_t *mli = (mca_base_module_list_item_t *) item;
         mca_allocator_base_module_t* component = (mca_allocator_base_module_t *) mli->mli_module;
         if(strcmp(component->allocator_version.mca_module_name,name) == 0) {
             return component;
         }
    }
    return NULL;
}



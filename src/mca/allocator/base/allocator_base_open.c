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
 * component's public mca_base_component_t struct.
 */

#include "mca/allocator/base/static-components.h"


/*
 * Global variables
 */
ompi_list_t mca_allocator_base_components;
int mca_allocator_base_output = -1;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_allocator_base_open(void)
{
  /* Open up all available components */

  return mca_base_components_open("allocator", 0, 
                                  mca_allocator_base_static_components, 
                                  &mca_allocator_base_components);
}

/**
 * Traverses through the list of available components, calling their init
 * functions until it finds the component that has the specified name. It
 * then returns the found component.
 *
 * @param name the name of the component that is being searched for.
 * @retval mca_allocator_base_component_t* pointer to the requested component
 * @retval NULL if the requested component is not found
 */
mca_allocator_base_component_t* mca_allocator_component_lookup(const char* name)
{
    /* Traverse the list of available components; call their init functions. */
    ompi_list_item_t* item;
    for (item = ompi_list_get_first(&mca_allocator_base_components);
         item != ompi_list_get_end(&mca_allocator_base_components);
         item = ompi_list_get_next(item)) {
         mca_base_component_list_item_t *cli = (mca_base_component_list_item_t *) item;
         mca_allocator_base_component_t* component = (mca_allocator_base_component_t *) cli->cli_component;
         if(strcmp(component->allocator_version.mca_component_name,
                   name) == 0) {
             return component;
         }
    }
    return NULL;
}



/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"

mca_mpool_base_component_t* mca_mpool_component_lookup(const char* name)
{
    /* Traverse the list of available modules; call their init functions. */
    ompi_list_item_t* item;
    for (item = ompi_list_get_first(&mca_mpool_base_components);
         item != ompi_list_get_end(&mca_mpool_base_components);
         item = ompi_list_get_next(item)) {
         mca_base_module_list_item_t *mli = (mca_base_module_list_item_t *) item;
         mca_mpool_base_component_t* component = (mca_mpool_base_component_t *) mli->mli_module;
         if(strcmp(component->mpool_version.mca_module_name,name) == 0) {
             return component;
         }
    }
    return NULL;
}


mca_mpool_t* mca_mpool_module_lookup(const char* name)
{
    /* Finalize all the mpool modules and free their list items */
    ompi_list_item_t *item;
    for(item =  ompi_list_get_first(&mca_mpool_base_modules);
        item != ompi_list_get_end(&mca_mpool_base_modules);
        item = ompi_list_get_next(item)) {
        mca_mpool_base_selected_module_t *sm = (mca_mpool_base_selected_module_t *) item;
        if(strcmp(sm->mpool_component->mpool_version.mca_module_name,name) == 0) {
             return sm->mpool_module;
        }
    }
    return NULL;
}


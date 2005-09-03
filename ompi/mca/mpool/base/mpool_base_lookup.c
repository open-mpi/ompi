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

#include <stdio.h>
#include <string.h>

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/mpool/base/base.h"


mca_mpool_base_component_t* mca_mpool_base_component_lookup(const char* name)
{
    /* Traverse the list of available modules; call their init functions. */
    opal_list_item_t* item;
    for (item = opal_list_get_first(&mca_mpool_base_components);
         item != opal_list_get_end(&mca_mpool_base_components);
         item = opal_list_get_next(item)) {
         mca_base_component_list_item_t *cli = 
           (mca_base_component_list_item_t *) item;
         mca_mpool_base_component_t* component = 
           (mca_mpool_base_component_t *) cli->cli_component;
         if(strcmp(component->mpool_version.mca_component_name, name) == 0) {
             return component;
         }
    }
    return NULL;
}


mca_mpool_base_module_t* mca_mpool_base_module_create(
    const char* name, 
    void* user_data,
    struct mca_mpool_base_resources_t* resources) 
{
    
    mca_mpool_base_component_t* component = NULL; 
    mca_mpool_base_module_t* module = NULL; 
    opal_list_item_t* item;
    mca_mpool_base_selected_module_t *sm;

    for (item = opal_list_get_first(&mca_mpool_base_components);
         item != opal_list_get_end(&mca_mpool_base_components);
         item = opal_list_get_next(item)) {
         mca_base_component_list_item_t *cli = 
           (mca_base_component_list_item_t *) item;
         component = 
           (mca_mpool_base_component_t *) cli->cli_component;
         if(0 == strcmp(component->mpool_version.mca_component_name, name)) {
             break;
         }
    }

    if (NULL == component) {
        return NULL;
    }
    module = component->mpool_init(resources); 
    sm = OBJ_NEW(mca_mpool_base_selected_module_t); 
    sm->mpool_component = component; 
    sm->mpool_module = module; 
    sm->user_data = user_data;
    sm->mpool_resources = resources;
    opal_list_append(&mca_mpool_base_modules, (opal_list_item_t*) sm); 
    return module; 
}


mca_mpool_base_module_t* mca_mpool_base_module_lookup(const char* name)
{
    opal_list_item_t* item;

    for (item = opal_list_get_first(&mca_mpool_base_modules);
         item != opal_list_get_end(&mca_mpool_base_modules);
         item = opal_list_get_next(item)) {
        mca_mpool_base_selected_module_t *mli =
            (mca_mpool_base_selected_module_t *) item;
        if(0 == strcmp(mli->mpool_component->mpool_version.mca_component_name,
                       name)) {
            return mli->mpool_module;
        }
    }

    return NULL;
}


int mca_mpool_base_module_destroy(mca_mpool_base_module_t *module)
{
    opal_list_item_t* item;
    mca_mpool_base_selected_module_t *sm;

    for (item = opal_list_remove_first(&mca_mpool_base_modules);
         NULL != item; 
         item = opal_list_remove_first(&mca_mpool_base_modules)) {
        sm = (mca_mpool_base_selected_module_t *) item;
        if (module == sm->mpool_module) {
            if (NULL != sm->mpool_module->mpool_finalize) {
                sm->mpool_module->mpool_finalize(sm->mpool_module);
            }
            OBJ_RELEASE(sm);
            return OMPI_SUCCESS;
        }
    }

    return OMPI_ERR_NOT_FOUND;
}

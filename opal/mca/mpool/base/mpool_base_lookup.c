/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#define OPAL_DISABLE_ENABLE_MEM_DEBUG 1
#include "opal_config.h"

#include <stdio.h>
#include <string.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/show_help.h"
#include "opal/util/proc.h"
#include "opal/runtime/opal_params.h"

#include "opal/mca/mpool/mpool.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "mpool_base_mem_cb.h"

mca_mpool_base_component_t* mca_mpool_base_component_lookup(const char* name)
{
    /* Traverse the list of available modules; call their init functions. */
    opal_list_item_t* item;
    for (item = opal_list_get_first(&opal_mpool_base_framework.framework_components);
         item != opal_list_get_end(&opal_mpool_base_framework.framework_components);
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
    mca_base_component_list_item_t *cli;
    mca_mpool_base_selected_module_t *sm;

    OPAL_LIST_FOREACH(cli, &opal_mpool_base_framework.framework_components, mca_base_component_list_item_t) {
         component = (mca_mpool_base_component_t *) cli->cli_component;
         if(0 == strcmp(component->mpool_version.mca_component_name, name)) {
             module = component->mpool_init(resources);
             break;
         }
    }

    if ( NULL == module ) {
        return NULL;
    }
    sm = OBJ_NEW(mca_mpool_base_selected_module_t); 
    sm->mpool_component = component; 
    sm->mpool_module = module; 
    sm->user_data = user_data;
    sm->mpool_resources = resources;
    opal_list_append(&mca_mpool_base_modules, (opal_list_item_t*) sm); 
    /* on the very first creation of a module we init the memory
       callback */
    if (opal_list_get_size(&mca_mpool_base_modules) == 1) { 
        /* Default to not using memory hooks */
        int use_mem_hooks = 0;

        /* Use the memory hooks if leave_pinned or
           leave_pinned_pipeline is enabled (note that either of these
           leave_pinned variables may have been set by a user MCA
           param or elsewhere in the code base).  Yes, we could have
           coded this more succinctly, but this is more clear. Do not
           check memory hooks if the mpool explicity asked us not to. */
        if ((opal_leave_pinned > 0 || opal_leave_pinned_pipeline) &&
            !(module->flags & MCA_MPOOL_FLAGS_NO_HOOKS)) {
            use_mem_hooks = 1;
        }

        if (use_mem_hooks) {
            if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
                ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) & 
                 opal_mem_hooks_support_level())) {
                opal_mem_hooks_register_release(mca_mpool_base_mem_cb, NULL);
            } else {
                opal_show_help("help-mpool-base.txt", "leave pinned failed",
                               true, name, OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                               opal_proc_local_get()->proc_hostname);
                return NULL;
            }

            /* Set this to true so that mpool_base_close knows to
               cleanup */
            mca_mpool_base_used_mem_hooks = 1;
        }
    }
    return module; 
}


mca_mpool_base_module_t* mca_mpool_base_module_lookup(const char* name)
{
    mca_mpool_base_selected_module_t *mli;

    OPAL_LIST_FOREACH(mli, &mca_mpool_base_modules, mca_mpool_base_selected_module_t) {
        if(0 == strcmp(mli->mpool_component->mpool_version.mca_component_name,
                       name)) {
            return mli->mpool_module;
        }
    }

    return NULL;
}


int mca_mpool_base_module_destroy(mca_mpool_base_module_t *module)
{
    mca_mpool_base_selected_module_t *sm, *next;

    OPAL_LIST_FOREACH_SAFE(sm, next, &mca_mpool_base_modules, mca_mpool_base_selected_module_t) {
        if (module == sm->mpool_module) {
            opal_list_remove_item(&mca_mpool_base_modules, (opal_list_item_t*)sm);
            if (NULL != sm->mpool_module->mpool_finalize) {
                sm->mpool_module->mpool_finalize(sm->mpool_module);
            }
            OBJ_RELEASE(sm);
            return OPAL_SUCCESS;
        }
    }

    return OPAL_ERR_NOT_FOUND;
}

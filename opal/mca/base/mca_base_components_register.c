/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2012 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/class/opal_list.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/constants.h"

/*
 * Local functions
 */
static int register_components(const char *project_name, const char *type_name,
                               int output_id, opal_list_t *src, opal_list_t *dest);
/**
 * Function for finding and opening either all MCA components, or the
 * one that was specifically requested via a MCA parameter.
 */
int mca_base_framework_components_register (mca_base_framework_t *framework,
                                            mca_base_register_flag_t flags)
{
    bool open_dso_components = !(flags & MCA_BASE_REGISTER_STATIC_ONLY);
    bool ignore_requested = !!(flags & MCA_BASE_REGISTER_ALL);
    opal_list_t components_found;
    int ret;

    /* Find and load requested components */
    ret = mca_base_component_find(NULL, framework->framework_name,
                                  framework->framework_static_components,
                                  ignore_requested ? NULL : framework->framework_selection,
                                  &components_found, open_dso_components);

    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* Register all remaining components */
    ret = register_components(framework->framework_project, framework->framework_name,
                              framework->framework_output, &components_found,
                              &framework->framework_components);

    OBJ_DESTRUCT(&components_found);

    /* All done */
    return ret;
}

/*
 * Traverse the entire list of found components (a list of
 * mca_base_component_t instances).  If the requested_component_names
 * array is empty, or the name of each component in the list of found
 * components is in the requested_components_array, try to open it.
 * If it opens, add it to the components_available list.
 */
static int register_components(const char *project_name, const char *type_name,
                               int output_id, opal_list_t *src, opal_list_t *dest)
{
    int ret;
    opal_list_item_t *item;
    mca_base_component_t *component;
    mca_base_component_list_item_t *cli;
    
    /* Announce */
    opal_output_verbose(10, output_id,
                        "mca: base: components_register: registering %s components",
                        type_name);
    
    /* Traverse the list of found components */
    
    OBJ_CONSTRUCT(dest, opal_list_t);
    while (NULL != (item = opal_list_remove_first (src))) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_base_component_t *)cli->cli_component;

        opal_output_verbose(10, output_id, 
                            "mca: base: components_register: found loaded component %s",
                            component->mca_component_name);

        /* Call the component's MCA parameter registration function (or open if register doesn't exist) */
        if (NULL == component->mca_register_component_params) {
            opal_output_verbose(10, output_id, 
                                "mca: base: components_register: "
                                "component %s has no register or open function",
                                component->mca_component_name);
            ret = OPAL_SUCCESS;
        } else {
            ret = component->mca_register_component_params();
        }

        if (OPAL_SUCCESS != ret) {
            if (OPAL_ERR_NOT_AVAILABLE != ret) {
                /* If the component returns OPAL_ERR_NOT_AVAILABLE,
                   it's a cue to "silently ignore me" -- it's not a
                   failure, it's just a way for the component to say
                   "nope!".  
 
                   Otherwise, however, display an error.  We may end
                   up displaying this twice, but it may go to separate
                   streams.  So better to be redundant than to not
                   display the error in the stream where it was
                   expected. */
                
                if (mca_base_component_show_load_errors) {
                    opal_output(0, "mca: base: components_register: "
                                "component %s / %s register function failed",
                                component->mca_type_name,
                                component->mca_component_name);
                }

                opal_output_verbose(10, output_id, 
                                    "mca: base: components_register: "
                                    "component %s register function failed",
                                    component->mca_component_name);
            }

            mca_base_component_unload (component, output_id);

            /* Release this list item */
            OBJ_RELEASE(cli);
            continue;
        }

        if (NULL != component->mca_register_component_params) {
            opal_output_verbose (10, output_id, "mca: base: components_register: "
                                 "component %s register function successful",
                                 component->mca_component_name);
        }

        /* Register this component's version */
        mca_base_component_var_register (component, "major_version", NULL, MCA_BASE_VAR_TYPE_INT, NULL,
                                         0, MCA_BASE_VAR_FLAG_DEFAULT_ONLY | MCA_BASE_VAR_FLAG_INTERNAL,
                                         OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_CONSTANT,
                                         &component->mca_component_major_version);
        mca_base_component_var_register (component, "minor_version", NULL, MCA_BASE_VAR_TYPE_INT, NULL,
                                         0, MCA_BASE_VAR_FLAG_DEFAULT_ONLY | MCA_BASE_VAR_FLAG_INTERNAL,
                                         OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_CONSTANT,
                                         &component->mca_component_minor_version);
        mca_base_component_var_register (component, "release_version", NULL, MCA_BASE_VAR_TYPE_INT, NULL,
                                         0, MCA_BASE_VAR_FLAG_DEFAULT_ONLY | MCA_BASE_VAR_FLAG_INTERNAL,
                                         OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_CONSTANT,
                                         &component->mca_component_release_version);
        
        opal_list_append(dest, item);
    }
    
    /* All done */
    
    return OPAL_SUCCESS;
}

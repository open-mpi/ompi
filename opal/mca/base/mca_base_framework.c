/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal/include/opal_config.h"

#include "opal/include/opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "mca_base_framework.h"
#include "mca_base_var.h"
#include "opal/mca/base/base.h"

bool mca_base_framework_is_registered (struct mca_base_framework_t *framework)
{
    return !!(framework->framework_flags & MCA_BASE_FRAMEWORK_FLAG_REGISTERED);
}

bool mca_base_framework_is_open (struct mca_base_framework_t *framework)
{
    return !!(framework->framework_flags & MCA_BASE_FRAMEWORK_FLAG_OPEN);
}

static void framework_open_output (struct mca_base_framework_t *framework)
{
    if (0 < framework->framework_verbose) {
        if (-1 == framework->framework_output) {
            framework->framework_output = opal_output_open (NULL);
        }
        opal_output_set_verbosity(framework->framework_output,
                                  framework->framework_verbose);
    } else if (-1 != framework->framework_output) {
        opal_output_close (framework->framework_output);
        framework->framework_output = -1;
    }
}

static void framework_close_output (struct mca_base_framework_t *framework)
{
    if (-1 != framework->framework_output) {
        opal_output_close (framework->framework_output);
        framework->framework_output = -1;
    }
}

static int mca_base_framework_parse_aliases (mca_base_framework_t *framework)
{
    char **aliases = opal_argv_split (framework->framework_component_aliases, ',');
    int alias_count;

    if (NULL == aliases) {
        /* nothing to do */
        return OPAL_SUCCESS;
    }

    alias_count = opal_argv_count (aliases);

    framework->framework_alias_array = calloc (alias_count + 1, sizeof (framework->framework_alias_array[0]));
    if (NULL == framework->framework_alias_array) {
        opal_argv_free (aliases);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    for (int i = 0, j = 0 ; i < alias_count ; ++i) {
        char *sep = strchr (aliases[i], ':');
        if (NULL == sep) {
            free (aliases[i]);
            continue;
        }

        *sep = '\0';
        framework->framework_alias_array[j].original = aliases[i];
        framework->framework_alias_array[j].alias = sep + 1;
        ++j;
    }

    free (aliases);
    return OPAL_SUCCESS;
}

static void mca_base_framework_free_aliases (mca_base_framework_t *framework)
{
    if (NULL == framework->framework_alias_array) {
        return;
    }

    for (int i = 0 ; framework->framework_alias_array[i].original ; ++i) {
        free ((void *) framework->framework_alias_array[i].original);
    }

    free (framework->framework_alias_array);
}

int mca_base_framework_register (struct mca_base_framework_t *framework,
                                 mca_base_register_flag_t flags)
{
    char *desc;
    int ret;

    assert (NULL != framework);

    framework->framework_refcnt++;

    if (mca_base_framework_is_registered (framework)) {
        return OPAL_SUCCESS;
    }

    OBJ_CONSTRUCT(&framework->framework_components, opal_list_t);

    if (framework->framework_flags & MCA_BASE_FRAMEWORK_FLAG_NO_DSO) {
        flags |= MCA_BASE_REGISTER_STATIC_ONLY;
    }

    /* create group aliases */
    if (framework->framework_component_aliases) {
        mca_base_framework_parse_aliases (framework);

        /* the framework array ends with an alias */
        for (int i = 0 ; framework->framework_alias_array[i].original ; ++i) {
            int group_id = mca_base_var_group_register (framework->framework_project,
                                                        framework->framework_name,
                                                        framework->framework_alias_array[i].original, NULL);
            int alias_id = mca_base_var_group_register (framework->framework_project,
                                                        framework->framework_name,
                                                        framework->framework_alias_array[i].alias, NULL);
            mca_base_var_group_alias (group_id, alias_id, false);
        }
    }


    if (!(MCA_BASE_FRAMEWORK_FLAG_NOREGISTER & framework->framework_flags)) {
        /* register this framework with the MCA variable system */
        ret = mca_base_var_group_register (framework->framework_project,
                                           framework->framework_name,
                                           NULL, framework->framework_description);
        if (0 > ret) {
            return ret;
        }

        asprintf (&desc, "Default selection set of components for the %s framework (<none>"
                  " means use all components that can be found)", framework->framework_name);
        ret = mca_base_var_register (framework->framework_project, framework->framework_name,
                                     NULL, NULL, desc, MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                     MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_2,
                                     MCA_BASE_VAR_SCOPE_ALL_EQ, &framework->framework_selection);
        free (desc);
        if (0 > ret) {
            return ret;
        }

        /* register a verbosity variable for this framework */
        ret = asprintf (&desc, "Verbosity level for the %s framework (default: 0)",
                        framework->framework_name);
        if (0 > ret) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        framework->framework_verbose = MCA_BASE_VERBOSE_ERROR;
        ret = mca_base_framework_var_register (framework, "verbose", desc,
                                               MCA_BASE_VAR_TYPE_INT,
                                               &mca_base_var_enum_verbose, 0,
                                               MCA_BASE_VAR_FLAG_SETTABLE,
                                               OPAL_INFO_LVL_8,
                                               MCA_BASE_VAR_SCOPE_LOCAL,
                                               &framework->framework_verbose);
        free(desc);
        if (0 > ret) {
            return ret;
        }

        /* check the initial verbosity and open the output if necessary. we
           will recheck this on open */
        framework_open_output (framework);

        /* register framework variables */
        if (NULL != framework->framework_register) {
            ret = framework->framework_register (flags);
            if (OPAL_SUCCESS != ret) {
                return ret;
            }
        }

        /* register components variables */
        ret = mca_base_framework_components_register (framework, flags);
        if (OPAL_SUCCESS != ret) {
            return ret;
        }
    }

    framework->framework_flags |= MCA_BASE_FRAMEWORK_FLAG_REGISTERED;

    /* framework did not provide a register function */
    return OPAL_SUCCESS;
}

int mca_base_framework_open (struct mca_base_framework_t *framework,
                             mca_base_open_flag_t flags) {
    int ret;

    assert (NULL != framework);

    /* register this framework before opening it */
    ret = mca_base_framework_register (framework, MCA_BASE_REGISTER_DEFAULT);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* check if this framework is already open */
    if (mca_base_framework_is_open (framework)) {
        return OPAL_SUCCESS;
    }

    if (MCA_BASE_FRAMEWORK_FLAG_NOREGISTER & framework->framework_flags) {
        flags |= MCA_BASE_OPEN_FIND_COMPONENTS;

        if (MCA_BASE_FRAMEWORK_FLAG_NO_DSO & framework->framework_flags) {
            flags |= MCA_BASE_OPEN_STATIC_ONLY;
        }
    }

    /* lock all of this frameworks's variables */
    ret = mca_base_var_group_find (framework->framework_project,
                                   framework->framework_name,
                                   NULL);
    mca_base_var_group_set_var_flag (ret, MCA_BASE_VAR_FLAG_SETTABLE, false);

    /* check the verbosity level and open (or close) the output */
    framework_open_output (framework);

    if (NULL != framework->framework_open) {
        ret = framework->framework_open (flags);
    } else {
        ret = mca_base_framework_components_open (framework, flags);
    }

    if (OPAL_SUCCESS != ret) {
        framework->framework_refcnt--;
    } else {
        framework->framework_flags |= MCA_BASE_FRAMEWORK_FLAG_OPEN;
    }

    return ret;
}

int mca_base_framework_close (struct mca_base_framework_t *framework) {
    bool is_open = mca_base_framework_is_open (framework);
    bool is_registered = mca_base_framework_is_registered (framework);
    int ret, group_id;

    assert (NULL != framework);

    if (!(is_open || is_registered)) {
        return OPAL_SUCCESS;
    }

    assert (framework->framework_refcnt);
    if (--framework->framework_refcnt) {
        return OPAL_SUCCESS;
    }

    /* find and deregister all component groups and variables */
    group_id = mca_base_var_group_find (framework->framework_project,
                                        framework->framework_name, NULL);
    if (0 <= group_id) {
        (void) mca_base_var_group_deregister (group_id);
    }

    /* close the framework and all of its components */
    if (is_open) {
        if (NULL != framework->framework_close) {
            ret = framework->framework_close ();
        } else {
            ret = mca_base_framework_components_close (framework, NULL);
        }

        if (OPAL_SUCCESS != ret) {
            return ret;
        }
    } else {
        opal_list_item_t *item;
        while (NULL != (item = opal_list_remove_first (&framework->framework_components))) {
            mca_base_component_list_item_t *cli;
            cli = (mca_base_component_list_item_t*) item;
            mca_base_component_unload(cli->cli_component,
                                      framework->framework_output);
            OBJ_RELEASE(item);
        }
        ret = OPAL_SUCCESS;
    }

    framework->framework_flags &= ~(MCA_BASE_FRAMEWORK_FLAG_REGISTERED | MCA_BASE_FRAMEWORK_FLAG_OPEN);

    OBJ_DESTRUCT(&framework->framework_components);

    framework_close_output (framework);
    mca_base_framework_free_aliases (framework);

    return ret;
}

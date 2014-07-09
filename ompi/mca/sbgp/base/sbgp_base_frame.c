/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012-2014 Los Alamos National Security, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNIST_H */
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "ompi/mca/sbgp/sbgp.h"
#include "ompi/mca/sbgp/base/base.h"
#include "ompi/include/ompi/constants.h"
#include "opal/util/argv.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/sbgp/base/static-components.h"

/*
**  * Global variables
**   */
opal_list_t mca_sbgp_base_components_in_use;
int mca_sbgp_base_components_in_use_inited=0;
OMPI_DECLSPEC char *ompi_sbgp_subgroups_string;

static void mca_sbgp_base_destruct (mca_sbgp_base_module_t *module)
{
   /* free the list of ranks */
   if(module->group_list ) {
       free(module->group_list);
       module->group_list=NULL;
   }
}

OBJ_CLASS_INSTANCE(mca_sbgp_base_module_t,
        opal_object_t,
        NULL,
        mca_sbgp_base_destruct);

OBJ_CLASS_INSTANCE(sbgp_base_component_keyval_t,
        mca_base_component_list_item_t,
        NULL,
        NULL);

/* get list of subgrouping coponents to use */
static int ompi_sbgp_set_components_to_use(opal_list_t *sbgp_components_avail,
        opal_list_t *sbgp_components_in_use)
{
    /* local variables */
    const mca_base_component_t *component;
    mca_base_component_list_item_t *cli;
    sbgp_base_component_keyval_t *clj;
    char **subgroups_requested = NULL, **sbgp_string = NULL;
    char *sbgp_component, *sbgp_key;
    const char *component_name;
    int i, sbgp_size = 0,
        sbgp_string_size = 0,
        rc = OMPI_SUCCESS;

    /* split the list of requested subgroups */
    subgroups_requested = opal_argv_split(ompi_sbgp_subgroups_string, ',');
    if(NULL == subgroups_requested) {
        return OMPI_ERROR;
    }
    sbgp_size = opal_argv_count (subgroups_requested);

    /* Initialize list */
    OBJ_CONSTRUCT(sbgp_components_in_use, opal_list_t);

    /* loop over list of components requested */
    for (i = 0; i < sbgp_size; i++) {
        /* get key-value */
        sbgp_string = opal_argv_split(subgroups_requested[i], ':');
        if (NULL == sbgp_string) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            break;
        }

        sbgp_string_size = opal_argv_count (sbgp_string);
        if (sbgp_string_size < 1 || sbgp_string_size > 2) {
            opal_output(ompi_sbgp_base_framework.framework_output,
                        "Requested SBGP configuration is illegal %s",
                        subgroups_requested[i]);
            opal_argv_free (sbgp_string);
            rc = OMPI_ERROR;
            break;
        }

        /* it is garanteed that sbgp_string[1] will either be NULL (count = 1) or a string */
        sbgp_key = sbgp_string[1];
        sbgp_component = sbgp_string[0];

        /* loop over discovered components */
	OPAL_LIST_FOREACH(cli, sbgp_components_avail, mca_base_component_list_item_t) {
            component = cli->cli_component;
            component_name = component->mca_component_name;

            /* key_value[0] has the component name, and key_value[1], if
            ** it is not NULL, has the key_value associated with this
            ** instance of the compoenent
            */

            if (0 == strcmp (component_name, sbgp_component)) {
                 /* found selected component */
                 clj = OBJ_NEW(sbgp_base_component_keyval_t);
                 if (NULL == clj) {
                     rc = OPAL_ERR_OUT_OF_RESOURCE;
                     opal_argv_free (sbgp_string);
                     goto exit_ERROR;
                 }
                 /* fprintf(stderr,"sbgp selecting %s %s\n", sbgp_component, component_name); */

                 clj->component.cli_component = component;
                 if (NULL != sbgp_key) {
                     clj->key_value = strdup(sbgp_key);
                 } else {
                     clj->key_value = NULL;
                 }
                 opal_list_append(sbgp_components_in_use, (opal_list_item_t *)clj);
                 break;
            }
        }

        opal_argv_free (sbgp_string);
    }

    /* Note: Need to add error checking to make sure all requested functions
    ** were found */

    /*
    ** release resources
    ** */
 exit_ERROR:
    opal_argv_free (subgroups_requested);

    return rc;
}

static int mca_sbgp_base_register(mca_base_register_flag_t flags)
{
    /* get list of sub-grouping functions to use */
    ompi_sbgp_subgroups_string = "basesmsocket,basesmuma,ibnet,p2p";
    (void) mca_base_var_register("ompi", "sbgp", "base", "subgroups_string",
                                 "Default set of subgroup operations to apply ",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_LOCAL,
                                 &ompi_sbgp_subgroups_string);

    return OMPI_SUCCESS;
}

static int mca_sbgp_base_close(void)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (&mca_sbgp_base_components_in_use))) {
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&mca_sbgp_base_components_in_use);

    return mca_base_framework_components_close(&ompi_sbgp_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int mca_sbgp_base_open(mca_base_open_flag_t flags)
{
    int ret;

    if (OMPI_SUCCESS != (ret = mca_base_framework_components_open(&ompi_sbgp_base_framework, flags))) {
        return ret;
    }

    ret = ompi_sbgp_set_components_to_use(&ompi_sbgp_base_framework.framework_components,
            &mca_sbgp_base_components_in_use);

    return ret;
}

MCA_BASE_FRAMEWORK_DECLARE(ompi, sbgp, "OMPI Subgroup Subsystem", mca_sbgp_base_register,
                           mca_sbgp_base_open, mca_sbgp_base_close,
                           mca_sbgp_base_static_components, 0);


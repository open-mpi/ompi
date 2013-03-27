/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/if/if.h"
#include "opal/mca/if/base/base.h"
#include "opal/mca/if/base/static-components.h"

int opal_if_base_output=-1;
opal_list_t opal_if_components;
static bool already_done = false;

/* instantiate the global list of interfaces */
opal_list_t opal_if_list;
bool opal_if_do_not_resolve;
bool opal_if_retain_loopback;

static int opal_if_base_verbose = -1;

/* instance the opal_if_t object */
OBJ_CLASS_INSTANCE(opal_if_t, opal_list_item_t, NULL, NULL);

static int opal_if_base_register(int flags)
{
    int var_id;

    opal_if_base_verbose = -1;
    (void) mca_base_var_register("opal", "if", "base", "verbose",
                                 "Provide verbose output if greater than 0",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_if_base_verbose);

    opal_if_do_not_resolve = false;
    var_id = mca_base_var_register("opal", "if", "base", "do_not_resolve",
                                   "If nonzero, do not attempt to resolve interfaces",
                                   MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                   OPAL_INFO_LVL_9,
                                   MCA_BASE_VAR_SCOPE_READONLY,
                                   &opal_if_do_not_resolve);
    (void) mca_base_var_register_synonym(var_id, "opal", "if", NULL, "do_not_resolve", 0);

    opal_if_retain_loopback = false;
    var_id = mca_base_var_register("opal", "if", "base", "retain_loopback",
                                   "If nonzero, retain loopback interfaces",
                                   MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                   OPAL_INFO_LVL_9,
                                   MCA_BASE_VAR_SCOPE_READONLY,
                                   &opal_if_retain_loopback);
    (void) mca_base_var_register_synonym(var_id, "opal", "if", NULL, "retain_loopback", 0);

    return OPAL_SUCCESS;
}

int opal_if_base_open(void)
{
    int i, ret;
    mca_base_component_list_item_t *cli;

    if (already_done) {
        return OPAL_SUCCESS;
    }
    already_done = true;

    (void) opal_if_base_register(0);

    /* setup the global list */
    OBJ_CONSTRUCT(&opal_if_list, opal_list_t);

    if (0 < opal_if_base_verbose) {
        opal_if_base_output = opal_output_open(NULL);
        opal_output_set_verbosity(opal_if_base_output, opal_if_base_verbose);
    }

    OBJ_CONSTRUCT(&opal_if_components, opal_list_t);
    for (i = 0 ; mca_if_base_static_components[i] != NULL ; ++i) {
        opal_if_base_component_t *component = 
            (opal_if_base_component_t*) 
            mca_if_base_static_components[i];

        /* Save it in a global list for ompi_info */
        cli = OBJ_NEW(mca_base_component_list_item_t);
        cli->cli_component = mca_if_base_static_components[i];
        opal_list_append(&opal_if_components, &cli->super);

        opal_output_verbose(5, opal_if_base_output,
                            "if:base: opening component %s",
                            component->component.mca_component_name);

        if (NULL != component->component.mca_open_component) {
            ret = component->component.mca_open_component();
            if (OPAL_SUCCESS != ret) continue;
        }

        if (NULL !=  mca_if_base_static_components[i]->mca_close_component) {
            mca_if_base_static_components[i]->mca_close_component();
        }
    }

    return OPAL_SUCCESS;
}


int opal_if_base_close(void)
{
    opal_list_item_t *item;

    if (!already_done) {
        return OPAL_SUCCESS;
    }
    already_done = false;

    for (item = opal_list_remove_first(&opal_if_list);
         NULL != item; 
         item = opal_list_remove_first(&opal_if_list)) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&opal_if_list);

    for (item = opal_list_remove_first(&opal_if_components);
         NULL != item; 
         item = opal_list_remove_first(&opal_if_components)) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&opal_if_components);

    /* Close the framework output */
    opal_output_close (opal_if_base_output);
    opal_if_base_output = -1;

    return OPAL_SUCCESS;
}


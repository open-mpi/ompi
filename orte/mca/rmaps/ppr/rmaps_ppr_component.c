/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/show_help.h"

#include "orte/mca/rmaps/base/base.h"
#include "rmaps_ppr.h"

/*
 * Local functions
 */

static int orte_rmaps_ppr_open(void);
static int orte_rmaps_ppr_close(void);
static int orte_rmaps_ppr_query(mca_base_module_t **module, int *priority);

orte_rmaps_base_component_t mca_rmaps_ppr_component = {
    {
        ORTE_RMAPS_BASE_VERSION_2_0_0,
        
        "ppr", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rmaps_ppr_open,  /* component open  */
        orte_rmaps_ppr_close, /* component close */
        orte_rmaps_ppr_query  /* component query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


/**
  * component open/close/init function
  */
static int orte_rmaps_ppr_open(void)
{
    int tmp, value;
    mca_base_component_t *c=&mca_rmaps_ppr_component.base_version;

    /* check for pernode, npernode, and npersocket directives - reqd for backward compatibility */
    tmp = mca_base_param_reg_int(c, "pernode",
                                 "Launch one ppn as directed",
                                 false, false, (int)false, NULL);
    mca_base_param_reg_syn_name(tmp, "rmaps", "base_pernode", false);
    mca_base_param_lookup_int(tmp, &value);
    if (value) {
        if (ORTE_MAPPING_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
            /* if a non-default mapping is already specified, then we
             * have an error
             */
            orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                           "PERNODE", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
            ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_CONFLICTED);
            return ORTE_ERR_SILENT;
        }
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_PPR);
        ORTE_SET_MAPPING_POLICY(orte_rmaps_base.mapping, ORTE_MAPPING_BYNODE);
        orte_rmaps_base.ppr = strdup("1:node");
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_GIVEN);
    }
 
    tmp = mca_base_param_reg_int(c, "n_pernode",
                                 "Launch n procs/node",
                                 false, false, (int)false, NULL);
    mca_base_param_reg_syn_name(tmp, "rmaps", "base_n_pernode", false);
    mca_base_param_lookup_int(tmp, &value);
    if (value) {
        if (ORTE_MAPPING_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
            /* if a non-default mapping is already specified, then we
             * have an error
             */
            orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                           "NPERNODE", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
            ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_CONFLICTED);
            return ORTE_ERR_SILENT;
        }
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_PPR);
        ORTE_SET_MAPPING_POLICY(orte_rmaps_base.mapping, ORTE_MAPPING_BYNODE);
        asprintf(&orte_rmaps_base.ppr, "%d:node", value);
        ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_GIVEN);
    }

#if OPAL_HAVE_HWLOC
    {
        char *ppr;

        tmp = mca_base_param_reg_int(c, "n_persocket",
                                     "Launch n procs/socket",
                                     false, false, (int)false, NULL);
        mca_base_param_reg_syn_name(tmp, "rmaps", "base_n_persocket", false);
        mca_base_param_lookup_int(tmp, &value);
        if (value) {
            if (ORTE_MAPPING_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
                /* if a non-default mapping is already specified, then we
                 * have an error
                 */
                orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                               "NPERSOCKET", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
                ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_CONFLICTED);
                return ORTE_ERR_SILENT;
            }
            ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_PPR);
            ORTE_SET_MAPPING_POLICY(orte_rmaps_base.mapping, ORTE_MAPPING_BYSOCKET);
            /* this implies binding to the sockets, unless otherwise directed */
            if (!OPAL_BINDING_POLICY_IS_SET(opal_hwloc_binding_policy)) {
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_SOCKET);
                opal_hwloc_binding_policy |= OPAL_BIND_GIVEN;
            }
            asprintf(&orte_rmaps_base.ppr, "%d:socket", value);
            ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_GIVEN);
        }

        mca_base_param_reg_string(c, "pattern",
                                  "Comma-separated list of number of processes on a given resource type [default: none]",
                                  false, false, NULL, &ppr);
        if (NULL != ppr) {
            if (ORTE_MAPPING_GIVEN & ORTE_GET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping)) {
                /* if a non-default mapping is already specified, then we
                 * have an error
                 */
                orte_show_help("help-orte-rmaps-base.txt", "redefining-policy", true, "mapping",
                               "PPR", orte_rmaps_base_print_mapping(orte_rmaps_base.mapping));
                ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_CONFLICTED);
                return ORTE_ERR_SILENT;
            }
            ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_PPR);
            /* since we don't know what pattern was given, leave the policy undefined
             * for now - we will assign it when we analyze the pattern later
             */
            orte_rmaps_base.ppr = ppr;
            ORTE_SET_MAPPING_DIRECTIVE(orte_rmaps_base.mapping, ORTE_MAPPING_GIVEN);
        }
    }
#endif

    return ORTE_SUCCESS;
}


static int orte_rmaps_ppr_query(mca_base_module_t **module, int *priority)
{
    *priority = 90;
    *module = (mca_base_module_t *)&orte_rmaps_ppr_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_ppr_close(void)
{
    return ORTE_SUCCESS;
}



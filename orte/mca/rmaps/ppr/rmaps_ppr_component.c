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

orte_rmaps_ppr_component_t mca_rmaps_ppr_component = {
    {
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
    }
};


/**
  * component open/close/init function
  */
static int orte_rmaps_ppr_open(void)
{
    char **ppr, *ctmp, **ck;
    int i, n;
    size_t value;
    opal_hwloc_level_t start=OPAL_HWLOC_NODE_LEVEL;

    /* initialize */
    mca_rmaps_ppr_component.selected = false;
    mca_rmaps_ppr_component.pruning_reqd = false;
    memset(mca_rmaps_ppr_component.ppr, 0, OPAL_HWLOC_HWTHREAD_LEVEL * sizeof(opal_hwloc_level_t));
    n=0;

    mca_base_param_reg_string(&mca_rmaps_ppr_component.super.base_version,
                              "pattern",
                              "Comma-separate list of number of processes on a given resource type [default: none]",
                              false, false, NULL, &mca_rmaps_ppr_component.given_ppr);
    ctmp = mca_rmaps_ppr_component.given_ppr;
    if (NULL != ctmp) {
        ppr = opal_argv_split(ctmp, ',');

        /* check validity of mppr spec */
        for (i=0; NULL != ppr[i]; i++) {
            /* split on the colon */
            ck = opal_argv_split(ppr[i], ':');
            if (2 != opal_argv_count(ck)) {
                /* must provide a specification */
                orte_show_help("help-orte-rmaps-ppr.txt", "invalid-ppr", true, ctmp);
                opal_argv_free(ppr);
                opal_argv_free(ck);
                free(ctmp);
                return ORTE_ERR_SILENT;
            }
            value = strlen(ck[1]);
            if (0 == strncasecmp(ck[1], "hwthread", value) ||
                0 == strncasecmp(ck[1], "thread", value)) {
                mca_rmaps_ppr_component.ppr[OPAL_HWLOC_HWTHREAD_LEVEL] = strtol(ck[0], NULL, 10);
                start = OPAL_HWLOC_HWTHREAD_LEVEL;
                n++;
            } else if (0 == strncasecmp(ck[1], "core", value)) {
                mca_rmaps_ppr_component.ppr[OPAL_HWLOC_CORE_LEVEL] = strtol(ck[0], NULL, 10);
                if (start < OPAL_HWLOC_CORE_LEVEL) {
                    start = OPAL_HWLOC_CORE_LEVEL;
                }
                n++;
            } else if (0 == strncasecmp(ck[1], "socket", value) ||
                       0 == strncasecmp(ck[1], "skt", value)) {
                mca_rmaps_ppr_component.ppr[OPAL_HWLOC_SOCKET_LEVEL] = strtol(ck[0], NULL, 10);
                if (start < OPAL_HWLOC_SOCKET_LEVEL) {
                    start = OPAL_HWLOC_SOCKET_LEVEL;
                }
                n++;
            } else if (0 == strncasecmp(ck[1], "l1cache", value)) {
                mca_rmaps_ppr_component.ppr[OPAL_HWLOC_L1CACHE_LEVEL] = strtol(ck[0], NULL, 10);
                if (start < OPAL_HWLOC_L1CACHE_LEVEL) {
                    start = OPAL_HWLOC_L1CACHE_LEVEL;
                }
                n++;
            } else if (0 == strncasecmp(ck[1], "l2cache", value)) {
                mca_rmaps_ppr_component.ppr[OPAL_HWLOC_L2CACHE_LEVEL] = strtol(ck[0], NULL, 10);
                if (start < OPAL_HWLOC_L2CACHE_LEVEL) {
                    start = OPAL_HWLOC_L2CACHE_LEVEL;
                }
                n++;
            } else if (0 == strncasecmp(ck[1], "l3cache", value)) {
                mca_rmaps_ppr_component.ppr[OPAL_HWLOC_L3CACHE_LEVEL] = strtol(ck[0], NULL, 10);
                if (start < OPAL_HWLOC_L3CACHE_LEVEL) {
                    start = OPAL_HWLOC_L3CACHE_LEVEL;
                }
                n++;
            } else if (0 == strncasecmp(ck[1], "numa", value)) {
                mca_rmaps_ppr_component.ppr[OPAL_HWLOC_NUMA_LEVEL] = strtol(ck[0], NULL, 10);
                if (start < OPAL_HWLOC_NUMA_LEVEL) {
                    start = OPAL_HWLOC_NUMA_LEVEL;
                }
                n++;
            } else if (0 == strncasecmp(ck[1], "node", value)) {
                mca_rmaps_ppr_component.ppr[OPAL_HWLOC_NODE_LEVEL] = strtol(ck[0], NULL, 10);
                n++;
            } else {
                /* unknown spec */
                orte_show_help("help-orte-rmaps-ppr.txt", "unrecognized-ppr-option", true, ck[1], ctmp);
                opal_argv_free(ppr);
                opal_argv_free(ck);
                free(ctmp);
                return ORTE_ERR_SILENT;
            }
            opal_argv_free(ck);
        }
        opal_argv_free(ppr);
        mca_rmaps_ppr_component.selected = true;
        mca_rmaps_ppr_component.start = start;
        /* if more than one level was specified, then pruning will be reqd */
        if (1 < n) {
            mca_rmaps_ppr_component.pruning_reqd = true;
        }
    }

    return ORTE_SUCCESS;
}


static int orte_rmaps_ppr_query(mca_base_module_t **module, int *priority)
{
    if (mca_rmaps_ppr_component.selected) {
        *priority = 1000;
        *module = (mca_base_module_t *)&orte_rmaps_ppr_module;
        return ORTE_SUCCESS;
    }

    /* cannot run without ppr spec */
    *priority = 0;
    *module = NULL;
    return ORTE_ERROR;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_ppr_close(void)
{
    if (NULL != mca_rmaps_ppr_component.given_ppr) {
        free(mca_rmaps_ppr_component.given_ppr);
    }
    return ORTE_SUCCESS;
}



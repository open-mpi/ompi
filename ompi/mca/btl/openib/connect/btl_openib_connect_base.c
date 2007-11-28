/*
 * Copyright (c) 2007 Cisco, Inc.  All rights reserved.
 * Copyright (c) 2007 Mellanox Technologies, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_openib.h"
#include "connect/base.h"
#include "connect/btl_openib_connect_oob.h"
#include "connect/btl_openib_connect_xoob.h"
#include "connect/btl_openib_connect_rdma_cm.h"

#include "opal/util/argv.h"
#include "opal/util/show_help.h"

/*
 * Global variable with the selected function pointers in it
 */
ompi_btl_openib_connect_base_funcs_t ompi_btl_openib_connect = {
    "",
    /* Compiler fills in the rest with NULL */
};

/*
 * Array of all possible connection functions
 */
static ompi_btl_openib_connect_base_funcs_t *all[] = {
    &ompi_btl_openib_connect_oob,
    &ompi_btl_openib_connect_xoob,
    &ompi_btl_openib_connect_rdma_cm,
    NULL
};

/*
 * MCA parameter value
 */
static char *param = NULL;

/*
 * Register MCA parameters
 */
int ompi_btl_openib_connect_base_open(void)
{
    int i;
    char **temp, *a, *b;

    /* Make an MCA parameter to select which connect module to use */
    temp = NULL;
    for (i = 0; NULL != all[i]; ++i) {
        opal_argv_append_nosize(&temp, all[i]->bcf_name);
    }
    a = opal_argv_join(temp, ',');
    opal_argv_free(temp);
    asprintf(&b,
             "Method used to make OpenFabrics connections (valid values: %s)",
             a);

    mca_base_param_reg_string(&mca_btl_openib_component.super.btl_version,
                              "btl_openib_connect",
                              b, false, false,
                              "oob", &param);

    /* For XRC qps we must to use XOOB connection manager */
    if (mca_btl_openib_component.num_xrc_qps > 0 && 0 == strcmp("oob", param)) {
        opal_show_help("help-mpi-btl-openib.txt",
                "XRC with OOB", true,
                orte_system_info.nodename, 
                mca_btl_openib_component.num_xrc_qps);
        return OMPI_ERROR;
    }

    /* XOOB connection manager may be used only with XRC qps */
    if ((mca_btl_openib_component.num_srq_qps > 0 || mca_btl_openib_component.num_pp_qps > 0) 
            && 0 == strcmp("xoob", param)) {
        opal_show_help("help-mpi-btl-openib.txt",
                "SRQ or PP with XOOB", true,
                orte_system_info.nodename, 
                mca_btl_openib_component.num_srq_qps,
                mca_btl_openib_component.num_pp_qps);
        return OMPI_ERROR;
    }

    /* Call the open function on all the connect modules */
    for (i = 0; NULL != all[i]; ++i) {
        if (NULL != all[i]->bcf_open) {
            all[i]->bcf_open();
        }
    }

    return OMPI_SUCCESS;
}


int ompi_btl_openib_connect_base_select(void)
{
    int i;

    /* Go through all the pseudo-components; if the btl_openib_connect
       param is empty, then take the first one that returns
       OMPI_SUCCESS from its init function.  If
       btl_openib_connect_param is not empty, find that one and ensure
       that its init function returns OMPI_SUCCESS. */
    if (NULL != param && '\0' == param[0]) {
        param = NULL;
    }
    for (i = 0; NULL != all[i]; ++i) {
        if ((NULL != param && 0 == strcmp(all[i]->bcf_name, param)) ||
            (NULL == param)) {
            if (NULL != all[i]->bcf_init && 
                OMPI_SUCCESS == all[i]->bcf_init()) {
                ompi_btl_openib_connect = *(all[i]);
                break;
            }
        }
    }
    if (NULL == all[i]) {
        /* JMS opal_show_help */
        return OMPI_ERR_NOT_FOUND;
    }

    return OMPI_SUCCESS;
}

/*
 * Copyright (c) 2007 Cisco, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_openib_endpoint.h"
#include "connect/connect.h"

static int ibcm_open(void);
static int ibcm_init(void);
static int ibcm_connect(mca_btl_base_endpoint_t *e);
static int ibcm_finalize(void);

ompi_btl_openib_connect_base_funcs_t ompi_btl_openib_connect_ibcm = {
    "ibcm",
    ibcm_open,
    ibcm_init,
    ibcm_connect,
    ibcm_finalize,
};

static int ibcm_open(void)
{
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "btl_openib_connect_ibcm_foo",
                           "A dummy help message", false, false,
                           17, NULL);

    return OMPI_SUCCESS;
}

static int ibcm_init(void)
{
    printf("ibcm init\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int ibcm_connect(mca_btl_base_endpoint_t *e)
{
    printf("ibcm connect\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int ibcm_finalize(void)
{
    printf("ibcm finalize\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}


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

static int rdma_cm_open(void);
static int rdma_cm_init(void);
static int rdma_cm_connect(mca_btl_base_endpoint_t *e);
static int rdma_cm_finalize(void);

ompi_btl_openib_connect_base_funcs_t ompi_btl_openib_connect_rdma_cm = {
    "rdma_cm",
    rdma_cm_open,
    rdma_cm_init,
    rdma_cm_connect,
    rdma_cm_finalize,
};

static int rdma_cm_open(void)
{
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "btl_openib_connect_rdma_cm_foo",
                           "A dummy help message", false, false,
                           17, NULL);

    return OMPI_SUCCESS;
}

static int rdma_cm_init(void)
{
    printf("rdma cm init\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int rdma_cm_connect(mca_btl_base_endpoint_t *e)
{
    printf("rdma cm connect\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int rdma_cm_finalize(void)
{
    printf("rdma cm finalize\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}


/*
 * Copyright (c) 2007-2008 Cisco, Inc.  All rights reserved.
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

static void rdma_cm_open(void);
static int rdma_cm_init(void);
static int rdma_cm_connect(mca_btl_base_endpoint_t *e);
static int rdma_cm_query(mca_btl_openib_hca_t *hca);
static int rdma_cm_finalize(void);

ompi_btl_openib_connect_base_funcs_t ompi_btl_openib_connect_rdma_cm = {
    "rdma_cm",
    rdma_cm_open,
    rdma_cm_init,
    rdma_cm_connect,
    rdma_cm_query,
    rdma_cm_finalize,
};

static int rdma_cm_priority = -1;

/* Open - this functions sets up any rdma_cm specific commandline params */
static void rdma_cm_open(void)
{
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_rdma_cm_priority",
                           "The selection method priority for rdma_cm",
                           false, false, rdma_cm_priority, &rdma_cm_priority);

    if (rdma_cm_priority > 100) {
        rdma_cm_priority = 100;
    } else if (rdma_cm_priority < -1) {
        rdma_cm_priority = -1;
    }
}

static int rdma_cm_init(void)
{
    BTL_ERROR(("rdma cm init"));
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int rdma_cm_connect(mca_btl_base_endpoint_t *e)
{
    BTL_ERROR(("rdma cm connect"));
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int rdma_cm_query(mca_btl_openib_hca_t *hca)
{
    /* JMS need something better than this */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (IBV_TRANSPORT_IWARP == hca->ib_dev->transport_type) {
        BTL_ERROR(("rdma cm Not currently supported"));
        return rdma_cm_priority;
    }
#endif

    return -1;
}

static int rdma_cm_finalize(void)
{
    BTL_ERROR(("rdma cm finalize"));
    return OMPI_ERR_NOT_IMPLEMENTED;
}


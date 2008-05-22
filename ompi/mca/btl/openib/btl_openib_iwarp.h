/*
 * Copyright (c) 2008 Chelsio
 * Copyright (c) 2008 Cisco Systems, Inc.  All rights reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_BTL_OPENIB_IWARP_H
#define MCA_BTL_OPENIB_IWARP_H

#include "ompi_config.h"

BEGIN_C_DECLS

/**
 * Get an iWARP equivalent of a subnet ID.
 */
extern uint64_t mca_btl_openib_get_iwarp_subnet_id(struct ibv_device *ib_dev);

/**
 * ?
 */
extern uint32_t mca_btl_openib_rdma_get_ipv4addr(struct ibv_context *verbs, 
                                                 uint8_t port);

/**
 * ?
 */
extern int mca_btl_openib_build_rdma_addr_list(void);

/**
 * ?
 */
extern void mca_btl_openib_free_rdma_addr_list(void);

END_C_DECLS

#endif

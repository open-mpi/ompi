/*
 * Copyright (c) 2008 Chelsio, Inc. All rights reserved.
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
 *
 * @param ib_dev (IN) IB/iWARP device
 * @return            Value of the IPv4 Address bitwise-and'ed with the Netmask
 */
extern uint64_t mca_btl_openib_get_iwarp_subnet_id(struct ibv_device *ib_dev,
                                                   uint8_t port);

/**
 * Get the IPv4 address of the specified HCA/RNIC device and physical port.
 *
 * @param verbs (IN)   cm_id verbs of the IB/iWARP device
 * @param port (IN)    physical port of the IB/iWARP device
 * @return             IPv4 Address
 */
extern uint32_t mca_btl_openib_rdma_get_ipv4addr(struct ibv_context *verbs, 
                                                 uint8_t port);

/**
 * Create a list of all available IB and iWARP devices and each device's
 * relevant information.  This is necessary for
 * mca_btl_openib_rdma_get_ipv4addr to work.
 *
 * @return OMPI_SUCCESS or failure status
 */
extern int mca_btl_openib_build_rdma_addr_list(void);

/**
 * Free the list of all available IB and iWARP devices created by
 * mca_btl_openib_build_rdma_addr_list.
 */
extern void mca_btl_openib_free_rdma_addr_list(void);

END_C_DECLS

#endif

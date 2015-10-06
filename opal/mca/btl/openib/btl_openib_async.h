/*
 * Copyright (c) 2007-2008 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         received.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef MCA_BTL_OPENIB_ASYNC_H
#define MCA_BTL_OPENIB_ASYNC_H
#include "btl_openib_endpoint.h"

void       mca_btl_openib_load_apm(struct ibv_qp *qp, mca_btl_openib_endpoint_t *ep);
#if OPAL_HAVE_CONNECTX_XRC
void       mca_btl_openib_load_apm_xrc_rcv(uint32_t qp_num, mca_btl_openib_endpoint_t *ep);
#endif

#define APM_ENABLED (0 != mca_btl_openib_component.apm_lmc || 0 != mca_btl_openib_component.apm_ports)

/**
 * Initialize the async event base
 */
int mca_btl_openib_async_init (void);

/**
 * Finalize the async event base
 */
void mca_btl_openib_async_fini (void);

/**
 * Register a device with the async event base
 *
 * @param[in] device     device to register
 */
void mca_btl_openib_async_add_device (mca_btl_openib_device_t *device);

/**
 * Deregister a device with the async event base
 *
 * @param[in] device     device to deregister
 */
void mca_btl_openib_async_rem_device (mca_btl_openib_device_t *device);

/**
 * Ignore error events on a queue pair
 *
 * @param[in] qp         queue pair to ignore
 */
void mca_btl_openib_async_add_qp_ignore (struct ibv_qp *qp);

#endif

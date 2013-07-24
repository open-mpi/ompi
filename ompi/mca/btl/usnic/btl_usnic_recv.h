/*
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_RECV_H
#define BTL_USNIC_RECV_H

#include <infiniband/verbs.h>

#include "btl_usnic.h"
#include "btl_usnic_frag.h"


void ompi_btl_usnic_recv(ompi_btl_usnic_module_t *module,
                           ompi_btl_usnic_recv_segment_t *rseg,
                           struct ibv_recv_wr **repost_recv_head);

#endif /* BTL_USNIC_RECV_H */

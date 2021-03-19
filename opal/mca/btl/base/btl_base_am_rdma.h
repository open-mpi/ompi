/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * This file provides support for active-message (send/recv) based RDMA.
 * It can be used with any btl that provides a minimum of send support but
 * can also be used with partial-RDMA BTLs (put only, get only, etc). It
 * will provide support for any RDMA or atomic operation not currently
 * supported by the supplied BTL. For more info see the description of
 * mca_btl_base_am_rdma_init.
 */

#include "opal_config.h"
#include "opal/mca/btl/btl.h"

#if !defined(OPAL_MCA_BTL_BASE_AM_RDMA_H)
#    define OPAL_MCA_BTL_BASE_AM_RDMA_H

/**
 * @brief initialize active-message RDMA/atomic support
 *
 * @inout btl   btl module to augment
 *
 * This function adds functionality to the btl for any missing RDMA/atomic
 * operation. Atomic operations are entirely emulated using send/recv and
 * work best with a btl that also has async-progress enabled. Put/get
 * support will use either send/recv or get (for put)/put (for get) (if
 * available).
 */
int mca_btl_base_am_rdma_init(mca_btl_base_module_t *btl);

#endif /* OPAL_MCA_BTL_BASE_AM_RDMA_H */

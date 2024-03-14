/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2012-2013 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2017-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_rdma.h"
#include "osc_rdma_frag.h"
#include "osc_rdma_btl_comm.h"

#include "opal/mca/btl/base/base.h"

void ompi_osc_rdma_atomic_complete (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint,
                                    void *local_address, mca_btl_base_registration_handle_t *local_handle,
                                    void *context, void *data, int status)
{
    ompi_osc_rdma_pending_op_t *pending_op = (ompi_osc_rdma_pending_op_t *) context;

    OSC_RDMA_VERBOSE(MCA_BASE_VERBOSE_INFO, "pending atomic %p complete with status %d", (void*)pending_op, status);

    if (pending_op->op_result) {
        osc_rdma_accelerator_mem_move (pending_op->op_result, pending_op->op_buffer, pending_op->op_size);
    }

    if (NULL != pending_op->cbfunc) {
        pending_op->cbfunc (pending_op->cbdata, pending_op->cbcontext, status);
    }

    if (NULL != pending_op->op_frag) {
        ompi_osc_rdma_frag_complete (pending_op->op_frag);
        pending_op->op_frag = NULL;
    }

    pending_op->op_complete = true;
    OBJ_RELEASE(pending_op);
}

/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/* Implements a progress engine based accelerator asynchronous copy implementation */

#ifndef OMPI_PML_OB1_ACCELERATOR_H
#define OMPI_PML_OB1_ACCELERATOR_H

#include "opal/constants.h"
#include "opal/sys/atomic.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/btl/btl.h"

OPAL_DECLSPEC int mca_pml_ob1_record_event(char *msg, struct mca_btl_base_descriptor_t *frag,
                                           opal_accelerator_stream_t *stream);
OPAL_DECLSPEC opal_accelerator_stream_t *mca_pml_ob1_get_dtoh_stream(void);
OPAL_DECLSPEC opal_accelerator_stream_t *mca_pml_ob1_get_htod_stream(void);
OPAL_DECLSPEC int mca_pml_ob1_progress_one_event(struct mca_btl_base_descriptor_t **);
OPAL_DECLSPEC int mca_pml_ob1_accelerator_init(void);
OPAL_DECLSPEC void mca_pml_ob1_accelerator_fini(void);

/* Set to true once the accelerator streams have been created (defined in
 * pml_ob1_accelerator.c).  Read on the hot path by the inline below. */
OPAL_DECLSPEC extern bool mca_pml_ob1_accelerator_streams_initialized;

/* Slow path for mca_pml_ob1_accelerator_ensure_init(): create the streams on
 * first device buffer use.  Call it through the inline wrapper, never directly. */
OPAL_DECLSPEC int mca_pml_ob1_accelerator_create_streams(void);

/* Ensure the accelerator streams exist before first use of a device buffer.  The
 * common, already-initialized case is a lock-free inline flag check; only the
 * first use per process falls through to the out-of-line slow path. */
static inline int mca_pml_ob1_accelerator_ensure_init(void)
{
    if (OPAL_LIKELY(mca_pml_ob1_accelerator_streams_initialized)) {
        /* Acquire: pair with the release (wmb) in
         * mca_pml_ob1_accelerator_create_streams() so the stream pointers read
         * after this point are visible and non-stale. */
        opal_atomic_rmb();
        return OPAL_SUCCESS;
    }
    return mca_pml_ob1_accelerator_create_streams();
}

#endif /* OMPI_PML_OB1_ACCELERATOR_H */

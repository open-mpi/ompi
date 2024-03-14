/*
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Implements a progress engine based accelerator asynchronous copy implementation */

#ifndef MCA_BTL_SMCUDA_ACCELERATOR_H
#define MCA_BTL_SMCUDA_ACCELERATOR_H

#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/btl/btl.h"

OPAL_DECLSPEC int mca_btl_smcuda_accelerator_init(void);
OPAL_DECLSPEC int mca_btl_smcuda_progress_one_ipc_event(struct mca_btl_base_descriptor_t **frag);
OPAL_DECLSPEC int mca_btl_smcuda_memcpy(void *dst, void *src, size_t amount, char *msg,
                           struct mca_btl_base_descriptor_t *frag);
OPAL_DECLSPEC void mca_btl_smcuda_accelerator_fini(void);

#endif /* MCA_BTL_SMCUDA_ACCELERATOR_H */

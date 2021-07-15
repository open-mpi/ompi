/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_SMSC_CMA_SMSC_CMA_H
#define OPAL_MCA_SMSC_CMA_SMSC_CMA_H

#include "opal_config.h"

#include "opal/mca/smsc/smsc.h"

mca_smsc_endpoint_t *mca_smsc_cma_get_endpoint(opal_proc_t *peer_proc);
void mca_smsc_cma_return_endpoint(mca_smsc_endpoint_t *endpoint);

int mca_smsc_cma_copy_to(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                         size_t size, void *reg_handle);
int mca_smsc_cma_copy_from(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                           size_t size, void *reg_handle);

/* unsupported interfaces defined to support MCA direct */
void *mca_smsc_cma_map_peer_region(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                   void *remote_address, size_t size, void **local_mapping);
void mca_smsc_cma_unmap_peer_region(void *ctx);
void *mca_smsc_cma_register_region(void *local_address, size_t size);
void mca_smsc_cma_deregister_region(void *reg_data);

#endif /* OPAL_MCA_SMSC_CMA_SMSC_CMA_H */

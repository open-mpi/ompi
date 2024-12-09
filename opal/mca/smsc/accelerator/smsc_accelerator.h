/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_SMSC_ACCELERATOR_H
#define OPAL_MCA_SMSC_ACCELERATOR_H

#include "opal_config.h"

#include "opal/mca/smsc/smsc.h"

mca_smsc_endpoint_t *mca_smsc_accelerator_get_endpoint(opal_proc_t *peer_proc);
void mca_smsc_accelerator_return_endpoint(mca_smsc_endpoint_t *endpoint);

int mca_smsc_accelerator_copy_to(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                           size_t size, void *reg_handle);

int mca_smsc_accelerator_copy_from(mca_smsc_endpoint_t *endpoint, void *local_address,
                             void *remote_address, size_t size, void *reg_handle);

void *mca_smsc_accelerator_map_peer_region(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                     void *remote_ptr, size_t size, void **local_ptr);
void mca_smsc_accelerator_unmap_peer_region(void *ctx);

void *mca_smsc_accelerator_register_region(void *local_address, size_t size);
void mca_smsc_accelerator_deregister_region(void *reg_data);

#endif /* OPAL_MCA_SMSC_ACCELERATOR__H */

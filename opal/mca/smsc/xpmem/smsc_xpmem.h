/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_SMSC_XPMEM_SMSC_XPMEM_H
#define OPAL_MCA_SMSC_XPMEM_SMSC_XPMEM_H

#include "opal_config.h"

#include "opal/mca/smsc/smsc.h"

mca_smsc_endpoint_t *mca_smsc_xpmem_get_endpoint(opal_proc_t *peer_proc);
void mca_smsc_xpmem_return_endpoint(mca_smsc_endpoint_t *endpoint);

int mca_smsc_xpmem_copy_to(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                           size_t size, void *reg_handle);

int mca_smsc_xpmem_copy_from(mca_smsc_endpoint_t *endpoint, void *local_address,
                             void *remote_address, size_t size, void *reg_handle);

/**
 * @brief Map a peer memory region into this processes address space.
 *
 * See the description in smsc.h.
 *
 * Caveats: XPMEM does not support futex operations within the region. Attempts to wake the
 * process owning the mutex will result in an EFAULT error code.
 */
void *mca_smsc_xpmem_map_peer_region(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                     void *remote_ptr, size_t size, void **local_ptr);
void mca_smsc_xpmem_unmap_peer_region(void *ctx);

/* unsupported interfaces defined to support MCA direct */
void *mca_smsc_xpmem_register_region(void *local_address, size_t size);
void mca_smsc_xpmem_deregister_region(void *reg_data);

#endif /* OPAL_MCA_SMSC_XPMEM_SMSC_XPMEM_H */

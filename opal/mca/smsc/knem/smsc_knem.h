/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_SMSC_KNEM_SMSC_KNEM_H
#define OPAL_MCA_SMSC_KNEM_SMSC_KNEM_H

#include "opal_config.h"

#include "opal/mca/smsc/smsc.h"

mca_smsc_endpoint_t *mca_smsc_knem_get_endpoint(opal_proc_t *peer_proc);
void mca_smsc_knem_return_endpoint(mca_smsc_endpoint_t *endpoint);

int mca_smsc_knem_copy_to(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                          size_t size, void *reg_data);
int mca_smsc_knem_copy_from(mca_smsc_endpoint_t *endpoint, void *local_address,
                            void *remote_address, size_t size, void *reg_data);

void *mca_smsc_knem_register_region(void *local_address, size_t size);
void mca_smsc_knem_deregister_region(void *reg_data);

/* unsupported interfaces defined to support MCA direct */
void *mca_smsc_knem_map_peer_region(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                    void *remote_address, size_t size, void **local_mapping);
void mca_smsc_knem_unmap_peer_region(void *ctx);

#endif /* OPAL_MCA_SMSC_KNEM_SMSC_KNEM_H */

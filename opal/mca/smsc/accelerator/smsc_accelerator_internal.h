/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_SMSC_ACCELERATOR_INTERNAL_H
#define OPAL_MCA_SMSC_ACCELERATOR_INTERNAL_H

#include "opal_config.h"

#include "opal/mca/rcache/base/base.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/smsc/accelerator/smsc_accelerator.h"
#include "opal/mca/accelerator/accelerator.h"

#define SMSC_ACCELERATOR_HANDLE_SIZE IPC_MAX_HANDLE_SIZE
struct mca_smsc_accelerator_registration_data_t {
    uint64_t base_addr;
    union {
        uint8_t accelerator[SMSC_ACCELERATOR_HANDLE_SIZE];
        void* host;
    } handle;
};
typedef struct mca_smsc_accelerator_registration_data_t mca_smsc_accelerator_registration_data_t;

struct mca_smsc_accelerator_registration_handle_t {
    mca_rcache_base_registration_t base;
    mca_smsc_accelerator_registration_data_t data;
    opal_accelerator_ipc_handle_t ipc_handle;
};
typedef struct mca_smsc_accelerator_registration_handle_t mca_smsc_accelerator_registration_handle_t;
OBJ_CLASS_DECLARATION(mca_smsc_accelerator_registration_handle_t);

#define MCA_SMSC_ACCELERATOR_REG_DATA_TO_HANDLE(data_ptr)                                            \
    ((mca_smsc_accelerator_registration_handle_t *) ((uintptr_t) data_ptr                            \
                                              - offsetof(mca_smsc_accelerator_registration_handle_t, \
                                                         data)))


struct mca_smsc_accelerator_endpoint_t {
    mca_smsc_endpoint_t super;
    mca_smsc_endpoint_t *prev_endpoint;
    mca_rcache_base_module_t *rcache;
};
typedef struct mca_smsc_accelerator_endpoint_t mca_smsc_accelerator_endpoint_t;
OBJ_CLASS_DECLARATION(mca_smsc_accelerator_endpoint_t);

struct mca_smsc_accelerator_component_t {
    mca_smsc_component_t super;
};
typedef struct mca_smsc_accelerator_component_t mca_smsc_accelerator_component_t;

struct mca_smsc_accelerator_module_t {
    mca_smsc_module_t super;
    mca_smsc_module_t *prev_smsc;
    mca_rcache_base_module_t *rcache;
    int device_id;
};
typedef struct mca_smsc_accelerator_module_t mca_smsc_accelerator_module_t;

extern mca_smsc_accelerator_module_t mca_smsc_accelerator_module;
extern mca_smsc_accelerator_component_t mca_smsc_accelerator_component;

#endif /* OPAL_MCA_SMSC_ACCELERATOR_INTERNAL_H */

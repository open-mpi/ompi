/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_SMSC_KNEM_SMSC_KNEM_INTERNAL_H
#define OPAL_MCA_SMSC_KNEM_SMSC_KNEM_INTERNAL_H

#include "opal_config.h"

#include "opal/mca/rcache/base/base.h"
#include "opal/mca/rcache/rcache.h"
#include "opal/mca/smsc/knem/smsc_knem.h"

#include <sys/mman.h>

#include <knem_io.h>

struct mca_smsc_knem_registration_data_t {
    uint64_t cookie;
    intptr_t base_addr;
};

typedef struct mca_smsc_knem_registration_data_t mca_smsc_knem_registration_data_t;

struct mca_smsc_knem_registration_handle_t {
    mca_rcache_base_registration_t base;
    mca_smsc_knem_registration_data_t data;
};

typedef struct mca_smsc_knem_registration_handle_t mca_smsc_knem_registration_handle_t;

#define MCA_SMSC_KNEM_REG_HANDLE_TO_DATA(handle) (&(handle)->data)
#define MCA_SMSC_KNEM_REG_DATA_TO_HANDLE(data_ptr)                                            \
    ((mca_smsc_knem_registration_handle_t *) ((uintptr_t) data_ptr                            \
                                              - offsetof(mca_smsc_knem_registration_handle_t, \
                                                         data)))

struct mca_smsc_knem_endpoint_t {
    mca_smsc_endpoint_t super;
};

typedef struct mca_smsc_knem_endpoint_t mca_smsc_knem_endpoint_t;

OBJ_CLASS_DECLARATION(mca_smsc_knem_endpoint_t);

struct mca_smsc_knem_component_t {
    mca_smsc_component_t super;

    int knem_fd;
    unsigned int dma_min;
};

typedef struct mca_smsc_knem_component_t mca_smsc_knem_component_t;

struct mca_smsc_knem_module_t {
    mca_smsc_module_t super;

    /** cache of knem attachments. this cache holds attachments for all peers. the registrations
     * are differentiated by the alloc_base which is set to the endpoint. */
    mca_rcache_base_module_t *rcache;
};

typedef struct mca_smsc_knem_module_t mca_smsc_knem_module_t;

extern mca_smsc_knem_module_t mca_smsc_knem_module;
extern mca_smsc_knem_component_t mca_smsc_knem_component;

#endif /* OPAL_MCA_SMSC_KNEM_SMSC_KNEM_INTERNAL_H */

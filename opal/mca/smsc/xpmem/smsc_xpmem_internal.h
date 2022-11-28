/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * Copyright (c) 2022      Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_SMSC_XPMEM_SMSC_XPMEM_INTERNAL_H
#define OPAL_MCA_SMSC_XPMEM_SMSC_XPMEM_INTERNAL_H

#include "opal/mca/smsc/xpmem/smsc_xpmem.h"

#include "opal/mca/rcache/base/rcache_base_vma.h"
#if defined(HAVE_XPMEM_H)
#    include <xpmem.h>

typedef struct xpmem_addr xpmem_addr_t;
#elif defined(HAVE_SN_XPMEM_H)
#    include <sn/xpmem.h>

typedef int64_t xpmem_segid_t;
typedef int64_t xpmem_apid_t;
#endif

typedef struct xpmem_addr xpmem_addr_t;

struct mca_smsc_xpmem_modex_t {
    /** XPMEM segment id for this peer */
    xpmem_segid_t seg_id;
    /** maximum address we can attach to on this peer */
    uintptr_t address_max;
};

typedef struct mca_smsc_xpmem_modex_t mca_smsc_xpmem_modex_t;

struct mca_smsc_xpmem_endpoint_t {
    mca_smsc_endpoint_t super;
    /** XPMEM apid for this peer */
    xpmem_apid_t apid;
    /** maximum address we can attach to on this peer */
    uintptr_t address_max;
    /** cache of xpmem attachments created using this endpoint */
    mca_rcache_base_vma_module_t *vma_module;
};

typedef struct mca_smsc_xpmem_endpoint_t mca_smsc_xpmem_endpoint_t;

OBJ_CLASS_DECLARATION(mca_smsc_xpmem_endpoint_t);

struct mca_smsc_xpmem_component_t {
    mca_smsc_component_t super;

    /** maximum attachment address for this process. attempts to attach past this value may fail. */
    uintptr_t my_address_max;
    /** XPMEM segment id for this process */
    xpmem_segid_t my_seg_id;
    /** log base 2 of the attachment alignment. this controls how big the smallest attachment is. a
     * larger value will produce fewer entries in the cache but will increase attachment time. */
    unsigned int log_attach_align;
    /** maximum size that will be used with a single memcpy call. on some systems we see better
     * performance if we chunk the copy into multiple memcpy calls. */
    uint64_t memcpy_chunk_size;
};

typedef struct mca_smsc_xpmem_component_t mca_smsc_xpmem_component_t;

struct mca_smsc_xpmem_module_t {
    mca_smsc_module_t super;
};

typedef struct mca_smsc_xpmem_module_t mca_smsc_xpmem_module_t;

extern mca_smsc_xpmem_module_t mca_smsc_xpmem_module;
extern mca_smsc_xpmem_component_t mca_smsc_xpmem_component;

#endif /* OPAL_MCA_SMSC_XPMEM_SMSC_XPMEM_INTERNAL_H */

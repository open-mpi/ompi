/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_OFI_TYPES_H_HAS_BEEN_INCLUDED
#define MTL_OFI_TYPES_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/communicator/communicator.h"

#include <rdma/fabric.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>

#include "mtl_ofi_endpoint.h"

BEGIN_C_DECLS

/**
 * MTL Module Interface
 */
typedef struct mca_mtl_ofi_module_t {
    mca_mtl_base_module_t base;

    /** Fabric Domain handle */
    struct fid_fabric *fabric;

    /** Access Domain handle */
    struct fid_domain *domain;

    /** Address vector handle */
    struct fid_av *av;

    /** Completion queue handle */
    struct fid_cq *cq;

    /** Memory region handle */
    struct fid_mr *mr;

    /** Endpoint to communicate on */
    struct fid_ep *ep;

    /** Endpoint name length */
    size_t epnamelen;

    /** "Any source" address */
    fi_addr_t any_addr;

    /** List of free messages for matched probe */
    opal_free_list_t free_messages;

    /** Optional user-specified OFI provider name */
    char *provider_name;

} mca_mtl_ofi_module_t;

extern mca_mtl_ofi_module_t ompi_mtl_ofi;

typedef struct mca_mtl_ofi_component_t {
    /** Base MTL component */
    mca_mtl_base_component_2_0_0_t super;
} mca_mtl_ofi_component_t;


/* match/ignore bit manipulation
 *
 * 0 123 4567 01234567 01234567 01234567 01234567 01234567 01234567 01234567
 *  |   |             |                 |
 *  |   | context id  |      source     |            message tag
 * ^| ^ |             |                 |
 * |  |
 * |  +- protocol
 * +---- ACK flag
 */

#define MTL_OFI_PROTOCOL_HEADER_MASK (0xF000000000000000ULL)
#define MTL_OFI_PROTOCOL_MASK        (0x7000000000000000ULL)
#define MTL_OFI_CONTEXT_MASK         (0x0FFF000000000000ULL)
#define MTL_OFI_SOURCE_MASK          (0x0000FFFF00000000ULL)
#define MTL_OFI_TAG_MASK             (0x00000000FFFFFFFFULL)

#define MTL_OFI_SYNC_SEND            (0x1000000000000000ULL)
#define MTL_OFI_SYNC_SEND_ACK        (0x9000000000000000ULL)

/* send posting */
#define MTL_OFI_SET_SEND_BITS(match_bits, contextid, source, tag, type) \
    {                                                                   \
        match_bits = contextid;                                         \
        match_bits = (match_bits << 16);                                \
        match_bits |= source;                                           \
        match_bits = (match_bits << 32);                                \
        match_bits |= (MTL_OFI_TAG_MASK & tag) | type;                  \
    }

/* receive posting */
/* Special tags are used for collective operations.
 * MPI_ANY_TAG should not match these special tags.
 * See ompi/mca/coll/base/coll_tags.h
 */
#define MTL_OFI_SET_RECV_BITS(match_bits, mask_bits, contextid, source, tag) \
    {                                                                   \
        match_bits = 0;                                                 \
        mask_bits  = MTL_OFI_PROTOCOL_MASK;                             \
                                                                        \
        match_bits = contextid;                                         \
        match_bits = (match_bits << 16);                                \
                                                                        \
        if (MPI_ANY_SOURCE == source) {                                 \
            match_bits = (match_bits << 32);                            \
            mask_bits |= MTL_OFI_SOURCE_MASK;                           \
        } else {                                                        \
            match_bits |= source;                                       \
            match_bits = (match_bits << 32);                            \
        }                                                               \
                                                                        \
        if (MPI_ANY_TAG == tag) {                                       \
            mask_bits |= 0x000000007FFFFFFFULL;                         \
        } else {                                                        \
            match_bits |= (MTL_OFI_TAG_MASK & tag);                     \
        }                                                               \
    }

#define MTL_OFI_IS_SYNC_SEND(match_bits)           \
    (MTL_OFI_SYNC_SEND == (MTL_OFI_PROTOCOL_HEADER_MASK & match_bits))
#define MTL_OFI_IS_SYNC_SEND_ACK(match_bits)           \
    (MTL_OFI_SYNC_SEND_ACK == (MTL_OFI_PROTOCOL_HEADER_MASK & match_bits))

#define MTL_OFI_GET_TAG(match_bits)                \
    ((int)(match_bits & MTL_OFI_TAG_MASK))
#define MTL_OFI_GET_SOURCE(match_bits)             \
    ((int)((match_bits & MTL_OFI_SOURCE_MASK) >> 32))

END_C_DECLS

#endif /* MTL_OFI_TYPES_H_HAS_BEEN_INCLUDED */

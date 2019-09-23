/*
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved
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

#include "mtl_ofi.h"

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

    /** Endpoint to communicate on */
    struct fid_ep *ep;

    /** Endpoint name length */
    size_t epnamelen;

    /** "Any source" address */
    fi_addr_t any_addr;

    /** Optional user-specified OFI provider name */
    char *provider_name;

    /** Maximum inject size */
    size_t max_inject_size;

    /** Largest message that can be sent in a single send. */
    size_t max_msg_size;

    /** Maximum number of CQ events to read in OFI Progress */
    int ofi_progress_event_count;

    /** Use FI_REMOTE_CQ_DATA*/
    bool fi_cq_data;

    /** Info used to create the OFI tag **/
    unsigned long long source_rank_tag_mask;
    int num_bits_source_rank;
    unsigned long long source_rank_mask;
    unsigned long long mpi_tag_mask;
    int num_bits_mpi_tag;

    /** Synchronous protocol tag bits */
    unsigned long long sync_send;
    unsigned long long sync_send_ack;
    unsigned long long sync_proto_mask;

} mca_mtl_ofi_module_t;

extern mca_mtl_ofi_module_t ompi_mtl_ofi;

typedef struct mca_mtl_ofi_component_t {
    /** Base MTL component */
    mca_mtl_base_component_2_0_0_t super;
} mca_mtl_ofi_component_t;

/*OFI TAG:
 * Define 3 different OFI tag distributions:
 * 1) Support FI_REMOTE_CQ_DATA: No need for source rank in the tag
 * 2) ofi_tag_1: fallback when no FI_REMOTE_CQ_DATA is supported
 * 3) ofi_tag_2: Alternative tag when no FI_REMOTE_CQ_DATA is supported
 *    with more bits for the communicator ID.
 * More details of the tags are in the README file (mtl_ofi_tag_mode).
*/

/* Support FI_REMOTE_CQ_DATA, send the source rank in the CQ data (4 Bytes is the minimum)
 *  01234567 01234567 01234567 012345  67  01234567 01234567 01234567 01234567
 *                                   |    |
 *           context_id              |prot|          message tag
 */
#define MTL_OFI_PROTO_BIT_COUNT         (2)

#define MTL_OFI_CID_MASK_DATA           (0xFFFFFFFC00000000ULL)
#define MTL_OFI_CID_BIT_COUNT_DATA      (30)
#define MTL_OFI_TAG_MASK_DATA           (0x00000000FFFFFFFFULL)
#define MTL_OFI_TAG_BIT_COUNT_DATA      (32)
#define MTL_OFI_PROTO_MASK_DATA         (0x0000000300000000ULL)
#define MTL_OFI_SYNC_SEND_DATA          (0x0000000100000000ULL)
#define MTL_OFI_SYNC_SEND_ACK_DATA      (0x0000000200000000ULL)

/* Send tag with CQ_DATA */
__opal_attribute_always_inline__ static inline uint64_t
mtl_ofi_create_send_tag_CQD(int comm_id, int tag)
{
    uint64_t  match_bits = comm_id;
    match_bits = (match_bits << (MTL_OFI_TAG_BIT_COUNT_DATA
                                + MTL_OFI_PROTO_BIT_COUNT));
    match_bits |= (tag & MTL_OFI_TAG_MASK_DATA);
    return match_bits;
}

/* Receive tag with CQ_DATA */
__opal_attribute_always_inline__ static inline void
mtl_ofi_create_recv_tag_CQD(uint64_t *match_bits, uint64_t *mask_bits,
                            int comm_id, int tag)
{
    *mask_bits  = ompi_mtl_ofi.sync_proto_mask;
    *match_bits = (uint64_t) comm_id;
    *match_bits = (*match_bits << (MTL_OFI_PROTO_BIT_COUNT
                                +  MTL_OFI_TAG_BIT_COUNT_DATA));
    if (MPI_ANY_TAG == tag) {
        /* Special negative tags are used for collective operations.
         * MPI_ANY_TAG should not match these special tags.
         * See ompi/mca/coll/base/coll_tags.h
         */
        *mask_bits  |= (ompi_mtl_ofi.mpi_tag_mask>>1);
    } else {
        *match_bits |= (ompi_mtl_ofi.mpi_tag_mask & tag);
    }
}

/*
* ofi_tag_1: fallback when no FI_REMOTE_CQ_DATA is supported
*
*  01234567 0123 4567 01234567 012345   67   01234567 01234567 01234567 01234567
*               |                     |    |
*    Comm id    |     source          |prot|           message tag
*/

#define MTL_OFI_CID_BIT_COUNT_1         (12)
#define MTL_OFI_SOURCE_TAG_MASK_1       (0x000FFFFC00000000ULL)
#define MTL_OFI_SOURCE_BIT_COUNT_1      (18)
#define MTL_OFI_SOURCE_MASK_1           (0x000000000003FFFFULL)
#define MTL_OFI_TAG_MASK_1              (0x00000000FFFFFFFFULL)
#define MTL_OFI_TAG_BIT_COUNT_1         (32)
#define MTL_OFI_PROTO_MASK_1            (0x0000000300000000ULL)
#define MTL_OFI_SYNC_SEND_1             (0x0000000100000000ULL)
#define MTL_OFI_SYNC_SEND_ACK_1         (0x0000000200000000ULL)

/*
* ofi_tag_2: Alternative tag when no FI_REMOTE_CQ_DATA is supported
*
*  01234567 01234567 01234567 01234567 01234567 01  23   4567 01234567 01234567
*                            |                    |    |
*                Comm id     |     source         |prot|     message tag
*/

#define MTL_OFI_CID_BIT_COUNT_2         (24)
#define MTL_OFI_SOURCE_TAG_MASK_2       (0x000000FFFFC00000ULL)
#define MTL_OFI_SOURCE_BIT_COUNT_2      (18)
#define MTL_OFI_SOURCE_MASK_2           (0x000000000003FFFFULL)
#define MTL_OFI_TAG_MASK_2              (0x00000000000FFFFFULL)
#define MTL_OFI_TAG_BIT_COUNT_2         (20)
#define MTL_OFI_PROTO_MASK_2            (0x0000000000300000ULL)
#define MTL_OFI_SYNC_SEND_2             (0x0000000000100000ULL)
#define MTL_OFI_SYNC_SEND_ACK_2         (0x0000000000200000ULL)

/* Send tag */
__opal_attribute_always_inline__ static inline uint64_t
mtl_ofi_create_send_tag(int comm_id, int source, int tag)
{
    uint64_t  match_bits = comm_id;
    match_bits = (match_bits << ompi_mtl_ofi.num_bits_source_rank);
    match_bits |= (uint64_t)(source & ompi_mtl_ofi.source_rank_mask);
    match_bits = (match_bits << (ompi_mtl_ofi.num_bits_mpi_tag
                                 + MTL_OFI_PROTO_BIT_COUNT));
    match_bits |= (tag & ompi_mtl_ofi.mpi_tag_mask);
    return match_bits;
}

/* Receive tag*/
__opal_attribute_always_inline__ static inline void
mtl_ofi_create_recv_tag(uint64_t *match_bits, uint64_t *mask_bits,
                            int comm_id, int source, int tag)
{
    *mask_bits  = ompi_mtl_ofi.sync_proto_mask;
    *match_bits = comm_id;
    *match_bits = (*match_bits << ompi_mtl_ofi.num_bits_source_rank);

    if (MPI_ANY_SOURCE == source) {
        *match_bits = (*match_bits << (ompi_mtl_ofi.num_bits_mpi_tag
                                    + MTL_OFI_PROTO_BIT_COUNT));
        *mask_bits |= ompi_mtl_ofi.source_rank_tag_mask;
    } else {
        *match_bits |= (uint64_t)(source & ompi_mtl_ofi.source_rank_mask);
        *match_bits = (*match_bits << (ompi_mtl_ofi.num_bits_mpi_tag
                                 + MTL_OFI_PROTO_BIT_COUNT));
    }

    if (MPI_ANY_TAG == tag) {
        /* Special negative tags are used for collective operations.
         * MPI_ANY_TAG should not match these special tags.
         * See ompi/mca/coll/base/coll_tags.h
         */
          *mask_bits  |= (ompi_mtl_ofi.mpi_tag_mask>>1);
    } else {
        *match_bits |= (ompi_mtl_ofi.mpi_tag_mask & tag);
    }
}

#define MTL_OFI_SET_SYNC_SEND(match_bits)          \
        match_bits |= ompi_mtl_ofi.sync_send

#define MTL_OFI_IS_SYNC_SEND(match_bits)           \
    (ompi_mtl_ofi.sync_send == (ompi_mtl_ofi.sync_proto_mask & match_bits))

#define MTL_OFI_IS_SYNC_SEND_ACK(match_bits)       \
    (ompi_mtl_ofi.sync_send_ack == (ompi_mtl_ofi.sync_proto_mask & match_bits))

#define MTL_OFI_GET_TAG(match_bits)                \
    ((int)(match_bits & ompi_mtl_ofi.mpi_tag_mask))

__opal_attribute_always_inline__ static inline int
mtl_ofi_get_source(struct fi_cq_tagged_entry *wc)
{
    int src;
    if (ompi_mtl_ofi.fi_cq_data) {
        src = (int) wc->data;
    }
    else {
        src = (int)((wc->tag >> (MTL_OFI_PROTO_BIT_COUNT +
                    ompi_mtl_ofi.num_bits_mpi_tag)) & ompi_mtl_ofi.source_rank_mask);
    }

    return src;
}
END_C_DECLS

#endif /* MTL_OFI_TYPES_H_HAS_BEEN_INCLUDED */

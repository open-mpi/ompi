/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_PORTALS_H_HAS_BEEN_INCLUDED
#define MTL_PORTALS_H_HAS_BEEN_INCLUDED

#include <portals4.h>

#include "opal/class/opal_free_list.h"
#include "opal/class/opal_list.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"

#include "mtl_portals4_flowctl.h"

BEGIN_C_DECLS

struct mca_mtl_portals4_send_request_t;

struct mca_mtl_portals4_module_t {
    mca_mtl_base_module_t base;

    /** Eager limit; messages greater than this use a rendezvous protocol */
    unsigned long long eager_limit;
    /** Size of short message blocks */
    unsigned long long recv_short_size;
    /** Number of short message blocks which should be created during startup */
    int recv_short_num;
    /** Length of the send event queues */
    int send_queue_size;
    /** Length of the receive event queues */
    int recv_queue_size;
    /** Protocol for long message transfer */
    enum { eager, rndv } protocol;

    /* free list of message for matched probe */
    opal_free_list_t fl_message;

    /** Network interface handle for matched interface */
    ptl_handle_ni_t ni_h;
    /** Uid for current user */
    ptl_uid_t uid;

    /** portals index for message matching */
    ptl_pt_index_t recv_idx;
    /** portals index for long message rendezvous */
    ptl_pt_index_t read_idx;
    /** portals index for flow control recovery */
    ptl_pt_index_t flowctl_idx;

    /** Event queue handles.  See send_eq_h and recv_eq_h defines for
        usage.  Array for PtlEQPoll */
    ptl_handle_eq_t eqs_h[2];

    /** MD handle for sending ACKS */
    ptl_handle_md_t zero_md_h;

    /** Send MD handle(s).  Use ompi_mtl_portals4_get_md() to get the right md */
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    ptl_handle_md_t *send_md_hs;
#else
    ptl_handle_md_t send_md_h;
#endif

    /** long message receive overflow ME.  Persistent ME, first in
        overflow list on the recv_idx portal table. */
    ptl_handle_me_t long_overflow_me_h;

    /** List of active short receive blocks.  Active means that the ME
        was posted to the overflow list and the UNLINK event has not
        yet been received. */
    opal_list_t active_recv_short_blocks;

    /** List of short receive blocks waiting for FREE event.  Blocks
        are added to this list when the UNLINK event has been
        received and removed when the FREE event is received. */
    opal_list_t waiting_recv_short_blocks;

    /** number of send-side operations started */
    uint64_t opcount;

#if OPAL_ENABLE_DEBUG
    /** number of receive-side operations started.  Used only for
        debugging */
    uint64_t recv_opcount;
#endif

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    ompi_mtl_portals4_flowctl_t flowctl;
#endif
};
typedef struct mca_mtl_portals4_module_t mca_mtl_portals4_module_t;

#define send_eq_h eqs_h[0]
#define recv_eq_h eqs_h[1]

extern mca_mtl_portals4_module_t ompi_mtl_portals4;

#define REQ_RECV_TABLE_ID    12
#define REQ_READ_TABLE_ID    13
#define REQ_FLOWCTL_TABLE_ID 14

#define MTL_PORTALS4_FLOWCTL_TRIGGER 0x01
#define MTL_PORTALS4_FLOWCTL_ALERT   0x02
#define MTL_PORTALS4_FLOWCTL_FANIN   0x03
#define MTL_PORTALS4_FLOWCTL_FANOUT  0x04

/* match/ignore bit manipulation
 *
 * 0123 4567 01234567 01234567 01234567 01234567 01234567 01234567 01234567
 *     |             |                 |
 * ^   | context id  |      source     |            message tag
 * |   |             |                 |
 * +---- protocol
 */

#define MTL_PORTALS4_PROTOCOL_MASK 0xF000000000000000ULL
#define MTL_PORTALS4_CONTEXT_MASK  0x0FFF000000000000ULL
#define MTL_PORTALS4_SOURCE_MASK   0x0000FFFF00000000ULL
#define MTL_PORTALS4_TAG_MASK      0x00000000FFFFFFFFULL

#define MTL_PORTALS4_PROTOCOL_IGNR MTL_PORTALS4_PROTOCOL_MASK
#define MTL_PORTALS4_CONTEXT_IGNR  MTL_PORTALS4_CONTEXT_MASK
#define MTL_PORTALS4_SOURCE_IGNR   MTL_PORTALS4_SOURCE_MASK
#define MTL_PORTALS4_TAG_IGNR      0x000000007FFFFFFFULL

#define MTL_PORTALS4_SHORT_MSG      0x1000000000000000ULL
#define MTL_PORTALS4_LONG_MSG       0x2000000000000000ULL

/* send posting */
#define MTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, source, tag, type) \
    {                                                                   \
        match_bits = contextid;                                         \
        match_bits = (match_bits << 16);                                \
        match_bits |= source;                                           \
        match_bits = (match_bits << 32);                                \
        match_bits |= (MTL_PORTALS4_TAG_MASK & tag) | type;             \
    }

/* receive posting */
#define MTL_PORTALS4_SET_RECV_BITS(match_bits, ignore_bits, contextid, source, tag) \
    {                                                                   \
        match_bits = 0;                                                 \
        ignore_bits = MTL_PORTALS4_PROTOCOL_IGNR;                       \
                                                                        \
        match_bits = contextid;                                         \
        match_bits = (match_bits << 16);                                \
                                                                        \
        if (MPI_ANY_SOURCE == source) {                                 \
            match_bits = (match_bits << 32);                            \
            ignore_bits |= MTL_PORTALS4_SOURCE_IGNR;                    \
        } else {                                                        \
            match_bits |= source;                                       \
            match_bits = (match_bits << 32);                            \
        }                                                               \
                                                                        \
        if (MPI_ANY_TAG == tag) {                                       \
            ignore_bits |= MTL_PORTALS4_TAG_IGNR;                       \
        } else {                                                        \
            match_bits |= (MTL_PORTALS4_TAG_MASK & tag);                \
        }                                                               \
    }

#define MTL_PORTALS4_IS_SHORT_MSG(match_bits)           \
    (0 != (MTL_PORTALS4_SHORT_MSG & match_bits))
#define MTL_PORTALS4_IS_LONG_MSG(match_bits)            \
    (0 != (MTL_PORTALS4_LONG_MSG & match_bits))
#define MTL_PORTALS4_IS_READY_MSG(match_bits)           \
    (0 != (MTL_PORTALS4_READY_MSG & match_bits))

#define MTL_PORTALS4_GET_TAG(match_bits)                \
    ((int)(match_bits & MTL_PORTALS4_TAG_MASK))
#define MTL_PORTALS4_GET_SOURCE(match_bits)             \
    ((int)((match_bits & MTL_PORTALS4_SOURCE_MASK) >> 32))


#define MTL_PORTALS4_SYNC_MSG       0x8000000000000000ULL

#define MTL_PORTALS4_SET_HDR_DATA(hdr_data, opcount, length, sync)   \
    {                                                                \
        hdr_data = (sync) ? 1 : 0;                                   \
        hdr_data = (hdr_data << 15);                                 \
        hdr_data |= opcount & 0x7FFFULL;                             \
        hdr_data = (hdr_data << 48);                                 \
        hdr_data |= (length & 0xFFFFFFFFFFFFULL);                    \
    }

#define MTL_PORTALS4_GET_LENGTH(hdr_data) ((size_t)(hdr_data & 0xFFFFFFFFFFFFULL))
#define MTL_PORTALS4_IS_SYNC_MSG(hdr_data)            \
    (0 != (MTL_PORTALS4_SYNC_MSG & hdr_data))


/*
 * Not all implementations of Portals 4 support binding a memory
 * descriptor which covers all of memory, but all support covering a
 * large fraction of memory.  Therefore, rather than working around
 * the issue by pinning per message, we use a number of memory
 * descriptors to cover all of memory.  As long as the maximum memory
 * descriptor is a large fraction of the user virtual address space
 * (like 46 bit MDs on a platform with 47 bits of user virtual address
 * space), this works fine.
 *
 * Our scheme is to create N memory descriptors which contiguously
 * cover the entire user address space, then another N-1 contiguous
 * memory descriptors offset by 1/2 the size of the MD, then a final
 * memory descriptor of 1/2 the size of the other MDs covering the top
 * of the memory space, to avoid if statements in the critical path.  This
 * scheme allows for a maximum message size of 1/2 the size of the MD
 * without ever crossing an MD boundary.  Also, because MD sizes are
 * always on a power of 2 in this scheme, computing the offsets and MD
 * selection are quick, using only bit shift and mask.q
 *
 * ompi_mtl_portals4_get_md() relies heavily on compiler constant folding.
 * "mask" can be constant folded into a constant.  "which" compiler folds 
 * into a bit shift of a register a constant number of times, then masked
 * by a constant (the input is, unfortunately, not constant).
 *
 * In the case where an MD can cover all of memory,
 * ompi_mtl_portals4_get_md() will be compiled into two assignments.
 * Assuming the function inlines (and it certainly should be), the two
 * assignments should be optimized into register assignments for the
 * Portals call relatively easily.
 */
static inline void
ompi_mtl_portals4_get_md(const void *ptr, ptl_handle_md_t *md_h, void **base_ptr)
{
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    int mask = (1ULL << (OMPI_PORTALS4_MAX_VA_SIZE - OMPI_PORTALS4_MAX_MD_SIZE + 1)) - 1;
    int which = (((uintptr_t) ptr) >> (OMPI_PORTALS4_MAX_MD_SIZE - 1)) & mask;
    *md_h = ompi_mtl_portals4.send_md_hs[which];
    *base_ptr = (void*) (which * (1ULL << (OMPI_PORTALS4_MAX_MD_SIZE - 1)));
#else
    *md_h = ompi_mtl_portals4.send_md_h;
    *base_ptr = 0;
#endif
}


static inline int
ompi_mtl_portals4_get_num_mds(void)
{
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    return (1 << (OMPI_PORTALS4_MAX_VA_SIZE - OMPI_PORTALS4_MAX_MD_SIZE + 1));
#else
    return 1;
#endif
}


/* MTL interface functions */
extern int ompi_mtl_portals4_finalize(struct mca_mtl_base_module_t *mtl);

extern int ompi_mtl_portals4_add_procs(struct mca_mtl_base_module_t* mtl, 
                                       size_t nprocs,
                                       struct ompi_proc_t** procs);

extern int ompi_mtl_portals4_del_procs(struct mca_mtl_base_module_t* mtl, 
                                       size_t nprocs,
                                       struct ompi_proc_t** procs);

extern int ompi_mtl_portals4_send(struct mca_mtl_base_module_t* mtl,
                                  struct ompi_communicator_t* comm,
                                  int dest,
                                  int tag,
                                  struct opal_convertor_t *convertor,
                                  mca_pml_base_send_mode_t mode);

extern int ompi_mtl_portals4_isend(struct mca_mtl_base_module_t* mtl,
                                   struct ompi_communicator_t* comm,
                                   int dest,
                                   int tag,
                                   struct opal_convertor_t *convertor,
                                   mca_pml_base_send_mode_t mode,
                                   bool blocking,
                                   mca_mtl_request_t *mtl_request);

extern int ompi_mtl_portals4_irecv(struct mca_mtl_base_module_t* mtl,
                                   struct ompi_communicator_t *comm,
                                   int src,
                                   int tag,
                                   struct opal_convertor_t *convertor,
                                   mca_mtl_request_t *mtl_request);

extern int ompi_mtl_portals4_iprobe(struct mca_mtl_base_module_t* mtl,
                                    struct ompi_communicator_t *comm,
                                    int src,
                                    int tag,
                                    int *flag,
                                    struct ompi_status_public_t *status);

extern int ompi_mtl_portals4_imrecv(struct mca_mtl_base_module_t* mtl,
                                    struct opal_convertor_t *convertor,
                                    struct ompi_message_t **message,
                                    struct mca_mtl_request_t *mtl_request);

extern int ompi_mtl_portals4_improbe(struct mca_mtl_base_module_t *mtl,
                                     struct ompi_communicator_t *comm,
                                     int src,
                                     int tag,
                                     int *matched,
                                     struct ompi_message_t **message,
                                     struct ompi_status_public_t *status);

extern int ompi_mtl_portals4_cancel(struct mca_mtl_base_module_t* mtl,
                                    mca_mtl_request_t *mtl_request,
                                    int flag);

extern int ompi_mtl_portals4_add_comm(struct mca_mtl_base_module_t *mtl,
                                      struct ompi_communicator_t *comm);

extern int ompi_mtl_portals4_del_comm(struct mca_mtl_base_module_t *mtl,
                                      struct ompi_communicator_t *comm);

extern int ompi_mtl_portals4_progress(void);

extern int ompi_mtl_portals4_get_error(int ptl_error);

END_C_DECLS

#endif  /* MTL_PORTALS_H_HAS_BEEN_INCLUDED */

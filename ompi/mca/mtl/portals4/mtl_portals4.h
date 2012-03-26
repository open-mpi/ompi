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

#include "mtl_portals4_request.h"
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
#include "mtl_portals4_flowctl.h"
#endif

BEGIN_C_DECLS

struct mca_mtl_portals4_send_request_t;

struct mca_mtl_portals4_module_t {
    mca_mtl_base_module_t base;

    /** Eager limit; messages greater than this use a rendezvous protocol */
    size_t eager_limit;
    /** Size of short message blocks */
    size_t recv_short_size;
    /** Number of short message blocks which should be created during startup */
    int recv_short_num;
    /** Length of both the receive and send event queues */
    int queue_size;
    /** Protocol for long message transfer */
    enum { eager, rndv } protocol;

    /* free list of message for matched probe */
    opal_free_list_t fl_message;

    /** Network interface handle for matched interface */
    ptl_handle_ni_t ni_h;

    /** portals index for message matching */
    ptl_pt_index_t send_idx;
    /** portals index for long message rendezvous */
    ptl_pt_index_t read_idx;
    /** portals index for flow control recovery */
    ptl_pt_index_t flowctl_idx;
    /** portals index for flow control recovery operatings which
        generate full events */
    ptl_pt_index_t flowctl_event_idx;

    /** Event queue handles.  See send_eq_h and recv_eq_h defines for
        usage.  Array for PtlEQPoll */
    ptl_handle_eq_t eqs_h[2];

    /** MD for zero-length sends and acks.  Optimization, can be
        reused anywhere a 0-byte ping is necessary */
    ptl_handle_md_t zero_md_h;

    /** long message receive overflow ME.  Persistent ME, first in
        overflow list on the send_idx portal table. */
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
    uint32_t opcount;

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    /** Number of event slots available for send side operations.
        Note that this is slightly smaller than the event queue size
        to allow space for flow control related events. */
    uint32_t send_queue_slots;

    /** free list of send items */
    opal_free_list_t send_fl;
    /** list of sends which are pending (either due to needing to
       retransmit or because we're in flow control recovery */
    opal_list_t pending_sends;
    /** list of sends which are currently active */
    opal_list_t active_sends;

    ompi_mtl_portals4_flowctl_t flowctl;
#endif

#if OPAL_ENABLE_DEBUG
    /** number of receive-side operations started.  Used only for
        debugging */
    uint32_t recv_opcount;
#endif
};
typedef struct mca_mtl_portals4_module_t mca_mtl_portals4_module_t;

#define send_eq_h eqs_h[0]
#define recv_eq_h eqs_h[1]

extern mca_mtl_portals4_module_t ompi_mtl_portals4;

#define REQ_SEND_TABLE_ID    2
#define REQ_READ_TABLE_ID    3
#define REQ_FLOWCTL_TABLE_ID 4

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
#define MTL_PORTALS4_FLOWCTL_TRIGGER 0x01
#define MTL_PORTALS4_FLOWCTL_ALERT   0x02
#define MTL_PORTALS4_FLOWCTL_FANIN   0x03
#define MTL_PORTALS4_FLOWCTL_FANOUT  0x04
#endif

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
#define MTL_PORTALS4_READY_MSG      0x4000000000000000ULL

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

/* MTL interface functions */
extern int ompi_mtl_portals4_finalize(struct mca_mtl_base_module_t *mtl);

extern int ompi_mtl_portals4_add_procs(struct mca_mtl_base_module_t* mtl, 
                                       size_t nprocs,
                                       struct ompi_proc_t** procs, 
                                       struct mca_mtl_base_endpoint_t **mtl_peer_data);

extern int ompi_mtl_portals4_del_procs(struct mca_mtl_base_module_t* mtl, 
                                       size_t nprocs,
                                       struct ompi_proc_t** procs, 
                                       struct mca_mtl_base_endpoint_t **mtl_peer_data);

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

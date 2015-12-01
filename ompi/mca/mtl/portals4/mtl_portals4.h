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

#include "opal/include/opal_config.h"
#include "opal/class/opal_free_list.h"
#include "opal/class/opal_list.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"

#include "ompi/communicator/communicator.h"

#include "mtl_portals4_flowctl.h"

BEGIN_C_DECLS

struct mca_mtl_portals4_send_request_t;

struct mca_mtl_portals4_module_t {
    mca_mtl_base_module_t base;

    /* add_procs() can get called multiple times.  this prevents multiple calls to portals4_init_interface(). */
    int need_init;

    /* Use the logical to physical table to accelerate portals4 adressing: 1 (true) : 0 (false) */
    int use_logical;
    /* Use flow control: 1 (true) : 0 (false) */
    int use_flowctl;

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

    /** Send MD handle */
    ptl_handle_md_t send_md_h;

    /** long message receive overflow ME.  Persistent ME, first in
        overflow list on the recv_idx portal table. */
    ptl_handle_me_t long_overflow_me_h;

    /** List of short receive blocks. */
    opal_list_t recv_short_blocks;

    /** Number of active short receive blocks. Active means that the ME
        was posted to the overflow list, the LINK event has been received but the UNLINK or the FREE event has not
        yet been received. */
    uint32_t active_recv_short_blocks;

    /** Mutex to protect opal_list */
    opal_mutex_t short_block_mutex;

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
 *     |             |                          |
 * ^   | context id  |          source          |        message tag
 * |   |             |                          |
 * +---- protocol
 */

#define MTL_PORTALS4_MAX_TAG       ((1UL << 24) -1)

#define MTL_PORTALS4_PROTOCOL_MASK 0xF000000000000000ULL
#define MTL_PORTALS4_CONTEXT_MASK  0x0FFF000000000000ULL
#define MTL_PORTALS4_SOURCE_MASK   0x0000FFFFFF000000ULL
#define MTL_PORTALS4_TAG_MASK      0x0000000000FFFFFFULL

#define MTL_PORTALS4_PROTOCOL_IGNR MTL_PORTALS4_PROTOCOL_MASK
#define MTL_PORTALS4_CONTEXT_IGNR  MTL_PORTALS4_CONTEXT_MASK
#define MTL_PORTALS4_SOURCE_IGNR   MTL_PORTALS4_SOURCE_MASK
#define MTL_PORTALS4_TAG_IGNR      0x00000000007FFFFFULL

#define MTL_PORTALS4_SHORT_MSG      0x1000000000000000ULL
#define MTL_PORTALS4_LONG_MSG       0x2000000000000000ULL

/* send posting */
#define MTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, source, tag, type) \
    {                                                                   \
        match_bits = contextid;                                         \
        match_bits = (match_bits << 24);                                \
        match_bits |= source;                                           \
        match_bits = (match_bits << 24);                                \
        match_bits |= (MTL_PORTALS4_TAG_MASK & tag) | type;             \
    }

/* receive posting */
#define MTL_PORTALS4_SET_RECV_BITS(match_bits, ignore_bits, contextid, source, tag) \
    {                                                                   \
        match_bits = 0;                                                 \
        ignore_bits = MTL_PORTALS4_PROTOCOL_IGNR;                       \
                                                                        \
        match_bits = contextid;                                         \
        match_bits = (match_bits << 24);                                \
                                                                        \
        if (MPI_ANY_SOURCE == source) {                                 \
            match_bits = (match_bits << 24);                            \
            ignore_bits |= MTL_PORTALS4_SOURCE_IGNR;                    \
        } else {                                                        \
            match_bits |= source;                                       \
            match_bits = (match_bits << 24);                            \
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
    ((int)((match_bits & MTL_PORTALS4_SOURCE_MASK) >> 24))


/* hda_data bit manipulation
 *
 * 0 1234567 01234567 01234567 0123 4567 01234567 01234567 01234567 01234567
 *  |                              |             |
 * ^|                              | context id  |        message tag
 * ||                              |             |
 * +---- is_sync
 */

#define MTL_PORTALS4_SYNC_MSG       0x8000000000000000ULL

#define MTL_PORTALS4_SET_HDR_DATA(hdr_data, tag, contextid, sync)    \
    {                                                                \
        hdr_data = (sync) ? 1 : 0;                                   \
        hdr_data = (hdr_data << 39);                                 \
        hdr_data |= contextid;                                       \
        hdr_data = (hdr_data << 24);                                 \
        hdr_data |= (MTL_PORTALS4_TAG_MASK & tag);                   \
    }

#define MTL_PORTALS4_IS_SYNC_MSG(hdr_data)            \
    (0 != (MTL_PORTALS4_SYNC_MSG & hdr_data))

/* mtl-portals4 helpers */
OMPI_DECLSPEC ompi_proc_t *
ompi_mtl_portals4_get_proc_group(struct ompi_group_t *group, int rank);

static inline ptl_process_t
ompi_mtl_portals4_get_peer_group(struct ompi_group_t *group, int rank)
{
    return *((ptl_process_t*)(ompi_mtl_portals4_get_proc_group(group, rank)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]));
}

static inline ompi_proc_t *
ompi_mtl_portals4_get_proc(struct ompi_communicator_t *comm, int rank)
{
    return ompi_mtl_portals4_get_proc_group(comm->c_remote_group, rank);
}

static inline ptl_process_t
ompi_mtl_portals4_get_peer(struct ompi_communicator_t *comm, int rank)
{
    return *((ptl_process_t*)(ompi_mtl_portals4_get_proc(comm, rank)->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]));
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

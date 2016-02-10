/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2013-2016 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_BTL_USNIC_ENDPOINT_H
#define OPAL_BTL_USNIC_ENDPOINT_H

#include <rdma/fabric.h>

#include "opal/class/opal_list.h"
#include "opal/class/opal_hotel.h"
#include "opal/mca/event/event.h"

#include "btl_usnic.h"

BEGIN_C_DECLS

/*
 * Forward declarations to avoid include loops
 */
struct opal_btl_usnic_module_t;
struct opal_btl_usnic_send_segment_t;

/*
 * Have the window size as a compile-time constant that is a power of
 * two so that we can take advantage of fast bit operations.
 */
#define WINDOW_SIZE 4096
#define WINDOW_SIZE_MOD(a) (((a) & (WINDOW_SIZE - 1)))
#define WINDOW_OPEN(E) (SEQ_LT((E)->endpoint_next_seq_to_send, \
        ((E)->endpoint_ack_seq_rcvd + WINDOW_SIZE)))
#define WINDOW_EMPTY(E) ((E)->endpoint_ack_seq_rcvd == \
        ((E)->endpoint_next_seq_to_send-1))

/*
 * Returns true when an endpoint has nothing left to send
 */
#define ENDPOINT_DRAINED(E) (WINDOW_EMPTY(E) && \
        opal_list_is_empty(&(E)->endpoint_frag_send_queue))

/*
 * Channel IDs
 */
typedef enum opal_btl_usnic_channel_id_t {
    USNIC_PRIORITY_CHANNEL,
    USNIC_DATA_CHANNEL,
    USNIC_NUM_CHANNELS
} opal_btl_usnic_channel_id_t;

typedef struct opal_btl_usnic_modex_t {
    /* Stored in network order */
    uint32_t ipv4_addr;
    /* Stored in host order */
    uint32_t ports[USNIC_NUM_CHANNELS];
    /* Stored in network order */
    uint32_t netmask;
    /* Stored in host order */
    uint32_t connectivity_udp_port;
    uint32_t link_speed_mbps;
    uint16_t max_msg_size;
    opal_btl_usnic_seq_t isn;
    uint32_t protocol;
} opal_btl_usnic_modex_t;

struct opal_btl_usnic_send_segment_t;
struct opal_btl_usnic_proc_t;

/*
 * This is a descriptor for an incoming fragment that is broken
 * into chunks.  When the first reference to this frag_id is seen,
 * memory is allocated for it.  When the last byte arrives, the assembled
 * fragment is passed to the PML.
 *
 * The endpoint structure has space for WINDOW_SIZE/2 simultaneous fragments.
 * This is the largest number of fragments that can possibly be in-flight
 * to us from a particular endpoint because eash chunked fragment will occupy
 * at least two segments, and only WINDOW_SIZE segments can be in flight.
 * OK, so there is an extremely pathological case where we could see
 * (WINDOW_SIZE/2)+1 "in flight" at once, but just dropping that last one
 * and waiting for retrans is just fine in this hypothetical hyper-pathological
 * case, which is what we'll do.
 */
#define MAX_ACTIVE_FRAGS (WINDOW_SIZE/2)
typedef struct opal_btl_usnic_rx_frag_info_t {
    uint32_t    rfi_frag_id;    /* ID for this fragment */
    uint32_t    rfi_frag_size;  /* bytes in this fragment */
    uint32_t    rfi_bytes_left; /* bytes remaining to RX in fragment */
    bool        rfi_data_in_pool; /* data in data_pool if true, else malloced */
    int         rfi_data_pool;  /* if <0, data malloced, else rx buf pool */
    char       *rfi_data;       /* pointer to assembly area */
    opal_free_list_item_t *rfi_fl_elt; /* free list elemement from buf pool
                                          when rfi_data_pool is nonzero */
} opal_btl_usnic_rx_frag_info_t;

/**
 * An abstraction that represents a connection to a remote process.
 * An instance of mca_btl_base_endpoint_t is associated with each
 * (btl_usnic_proc_t, btl_usnic_module_t) tuple and address
 * information is exchanged at startup.  The usnic BTL is
 * connectionless, so no connection is ever established.
 */
typedef struct mca_btl_base_endpoint_t {
    opal_list_item_t super;

    /** BTL module that created this connection */
    struct opal_btl_usnic_module_t *endpoint_module;

    /** proc that owns this endpoint */
    struct opal_btl_usnic_proc_t *endpoint_proc;
    int endpoint_proc_index;    /* index in owning proc's endpoint array */

    /** True when proc has been deleted, but still have sends that need ACKs */
    bool endpoint_exiting;

    /** List item for linking into module "all_endpoints" */
    opal_list_item_t endpoint_endpoint_li;

    /** List item for linking into "need ack" */
    opal_list_item_t endpoint_ack_li;

    /** Remote address information */
    opal_btl_usnic_modex_t endpoint_remote_modex;

    /** Remote address handle. Need one for each
        channel because each remote channel has different dest port */
    fi_addr_t endpoint_remote_addrs[USNIC_NUM_CHANNELS];

    /** Send-related data */
    bool endpoint_ready_to_send;
    opal_list_t endpoint_frag_send_queue;
    int32_t endpoint_send_credits;
    uint32_t endpoint_next_frag_id;

    /** Receive-related data */
    struct opal_btl_usnic_rx_frag_info_t *endpoint_rx_frag_info;

    /** OPAL hotel to track outstanding stends */
    opal_hotel_t endpoint_hotel;

    /** Sliding window parameters for this peer */
    /* Values for the current proc to send to this endpoint on the
       peer proc */
    opal_btl_usnic_seq_t endpoint_next_seq_to_send; /* n_t */
    opal_btl_usnic_seq_t endpoint_ack_seq_rcvd; /* n_a */

    struct opal_btl_usnic_send_segment_t *endpoint_sent_segs[WINDOW_SIZE];

    /* Values for the current proc to receive from this endpoint on
       the peer proc */
    bool endpoint_ack_needed;

    /* When we receive a packet that needs an ACK, set this
     * to delay the ACK to allow for piggybacking
     */
    uint64_t endpoint_acktime;

    opal_btl_usnic_seq_t endpoint_next_contig_seq_to_recv; /* n_r */
    opal_btl_usnic_seq_t endpoint_highest_seq_rcvd; /* n_s */

    bool endpoint_rcvd_segs[WINDOW_SIZE];
    uint32_t endpoint_rfstart;

    bool endpoint_connectivity_checked;
    bool endpoint_on_all_endpoints;
} mca_btl_base_endpoint_t;

typedef mca_btl_base_endpoint_t opal_btl_usnic_endpoint_t;
OBJ_CLASS_DECLARATION(opal_btl_usnic_endpoint_t);

/*
 * Helper struct for the asynchornous creation of fi_addr array
 */
typedef struct {
    opal_btl_usnic_endpoint_t *endpoint;
    opal_btl_usnic_channel_id_t channel_id;
} opal_btl_usnic_addr_context_t;

/*
 * Flush all pending sends and resends from and endpoint
 */
void
opal_btl_usnic_flush_endpoint(
    opal_btl_usnic_endpoint_t *endpoint);

END_C_DECLS
#endif

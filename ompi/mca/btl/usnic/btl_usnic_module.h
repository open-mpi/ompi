/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef OMPI_BTL_USNIC_MODULE_H
#define OMPI_BTL_USNIC_MODULE_H

#include "opal/class/opal_pointer_array.h"

#include "ompi/mca/common/verbs/common_verbs.h"

#include "btl_usnic_endpoint.h"
#include "btl_usnic_stats.h"

/*
 * Default limits.
 *
 * These values obtained from empirical testing on Intel E5-2690
 * machines with Sereno/Lexington cards through an N3546 switch.
 */
#define USNIC_DFLT_EAGER_LIMIT_1DEVICE (150 * 1024)
#define USNIC_DFLT_EAGER_LIMIT_NDEVICES (25 * 1024)
#define USNIC_DFLT_RNDV_EAGER_LIMIT 500
#define USNIC_DFLT_PACK_LAZY_THRESHOLD (16 * 1024)

BEGIN_C_DECLS

/*
 * Forward declarations to avoid include loops
 */
struct ompi_btl_usnic_send_segment_t;
struct ompi_btl_usnic_recv_segment_t;

/*
 * Abstraction of a set of IB queues
 */
typedef struct ompi_btl_usnic_channel_t {
    int chan_index;

    struct ibv_cq *cq;

    int chan_mtu;
    int chan_rd_num;
    int chan_sd_num;

    /** available send WQ entries */
    int32_t sd_wqe;

    /* fastsend enabled if sd_wqe >= fastsend_wqe_thresh */
    int fastsend_wqe_thresh;

    /* pointer to receive segment whose bookkeeping has been deferred */
    struct ompi_btl_usnic_recv_segment_t *chan_deferred_recv;

    /** queue pair */
    struct ibv_qp* qp;

    struct ibv_recv_wr *repost_recv_head;

    /** receive segments & buffers */
    ompi_free_list_t recv_segs;

    bool chan_error;    /* set when error detected on channel */

    /* statistics */
    uint32_t num_channel_sends;
} ompi_btl_usnic_channel_t;

/**
 * usNIC verbs BTL interface
 */
typedef struct ompi_btl_usnic_module_t {
    mca_btl_base_module_t super;

    /* Cache for use during component_init to associate a module with
       the ompi_common_verbs_port_item_t that it came from. */
    ompi_common_verbs_port_item_t *port;

    mca_btl_base_module_error_cb_fn_t pml_error_callback;

    /* Information about the usNIC verbs device */
    uint8_t port_num;
    struct ibv_device *device;
    struct ibv_context *device_context;
    struct event device_async_event;
    bool device_async_event_active;
    struct ibv_pd *pd;
    int numa_distance; /* hwloc NUMA distance from this process */

    /* Information about the IP interface corresponding to this USNIC
       interface */
    char if_name[64];
    uint32_t if_ipv4_addr; /* in network byte order */
    uint32_t if_cidrmask; /* X in "/X" CIDR addr fmt, host byte order */
    uint8_t if_mac[6];
    int if_mtu;

    /** desired send, receive, and completion queue entries (from MCA
        params; cached here on the component because the MCA param
        might == 0, which means "max supported on that device") */
    int sd_num;
    int rd_num;
    int cq_num;
    int prio_sd_num;
    int prio_rd_num;

    /* 
     * Fragments larger than max_frag_payload will be broken up into
     * multiple chunks.  The amount that can be held in a single chunk
     * segment is slightly less than what can be held in frag segment due
     * to fragment reassembly info.
     */
    size_t tiny_mtu;
    size_t max_frag_payload;    /* most that fits in a frag segment */
    size_t max_chunk_payload;   /* most that can fit in chunk segment */
    size_t max_tiny_payload;    /* threshold for using inline send */

    /** Hash table to keep track of senders */
    opal_hash_table_t senders;

    /** local address information */
    struct ompi_btl_usnic_addr_t local_addr;

    /** list of all endpoints.  Note that the main application thread
        reads and writes to this list, and the connectivity agent
        reads from it.  So all access to the list (but not the items
        in the list) must be protected by a lock.  Also, have a flag
        that indicates that the list has been constructed.  Probably
        overkill, but you can't be too safe with multi-threaded
        programming in non-performance-critical code paths... */
    opal_list_t all_endpoints;
    opal_mutex_t all_endpoints_lock;
    bool all_endpoints_constructed;

    /** array of procs used by this module (can't use a list because a
        proc can be used by multiple modules) */
    opal_pointer_array_t all_procs;

    /** send fragments & buffers */
    ompi_free_list_t small_send_frags;
    ompi_free_list_t large_send_frags;
    ompi_free_list_t put_dest_frags;
    ompi_free_list_t chunk_segs;

    /** receive buffer pools */
    int first_pool;
    int last_pool;
    ompi_free_list_t *module_recv_buffers;

    /** list of endpoints with data to send */
    /* this list uses base endpoint ptr */
    opal_list_t endpoints_with_sends;

    /** list of send frags that are waiting to be resent (they
        previously deferred because of lack of resources) */
    opal_list_t pending_resend_segs;

    /** ack segments */
    ompi_free_list_t ack_segs;

    /** list of endpoints to which we need to send ACKs */
    /* this list uses endpoint->endpoint_ack_li */
    opal_list_t endpoints_that_need_acks;

    /* abstract queue-pairs into channels */
    ompi_btl_usnic_channel_t mod_channels[USNIC_NUM_CHANNELS];

    uint32_t qp_max_inline;
    uint32_t num_short_packets;

    /* Performance / debugging statistics */
    ompi_btl_usnic_module_stats_t stats;
} ompi_btl_usnic_module_t;

struct ompi_btl_usnic_frag_t;
extern ompi_btl_usnic_module_t ompi_btl_usnic_module_template;

/*
 * Manipulate the "endpoints_that_need_acks" list
 */

/* get first endpoint needing ACK */
static inline ompi_btl_usnic_endpoint_t *
ompi_btl_usnic_get_first_endpoint_needing_ack(
    ompi_btl_usnic_module_t *module)
{
    opal_list_item_t *item;
    ompi_btl_usnic_endpoint_t *endpoint;

    item = opal_list_get_first(&module->endpoints_that_need_acks);
    if (item != opal_list_get_end(&module->endpoints_that_need_acks)) {
        endpoint = container_of(item, mca_btl_base_endpoint_t, endpoint_ack_li);
        return endpoint;
    } else {
        return NULL;
    }
}

/* get next item in chain */
static inline ompi_btl_usnic_endpoint_t *
ompi_btl_usnic_get_next_endpoint_needing_ack(
    ompi_btl_usnic_endpoint_t *endpoint)
{
    opal_list_item_t *item;
    ompi_btl_usnic_module_t *module;

    module = endpoint->endpoint_module;

    item = opal_list_get_next(&(endpoint->endpoint_ack_li));
    if (item != opal_list_get_end(&module->endpoints_that_need_acks)) {
        endpoint = container_of(item, mca_btl_base_endpoint_t, endpoint_ack_li);
        return endpoint;
    } else {
        return NULL;
    }
}

static inline void
ompi_btl_usnic_remove_from_endpoints_needing_ack(
    ompi_btl_usnic_endpoint_t *endpoint)
{
    opal_list_remove_item(
            &(endpoint->endpoint_module->endpoints_that_need_acks),
            &endpoint->endpoint_ack_li);
    endpoint->endpoint_ack_needed = false;
    endpoint->endpoint_acktime = 0;
#if MSGDEBUG1
    opal_output(0, "clear ack_needed on %p\n", (void*)endpoint);
#endif
}

static inline void
ompi_btl_usnic_add_to_endpoints_needing_ack(
    ompi_btl_usnic_endpoint_t *endpoint)
{
    opal_list_append(&(endpoint->endpoint_module->endpoints_that_need_acks),
            &endpoint->endpoint_ack_li);
    endpoint->endpoint_ack_needed = true;
#if MSGDEBUG1
    opal_output(0, "set ack_needed on %p\n", (void*)endpoint);
#endif
}

/*
 * Initialize a module
 */
int ompi_btl_usnic_module_init(ompi_btl_usnic_module_t* module);


/*
 * Progress pending sends on a module
 */
void ompi_btl_usnic_module_progress_sends(ompi_btl_usnic_module_t *module);

/* opal_output statistics that are useful for debugging */
void ompi_btl_usnic_print_stats(
    ompi_btl_usnic_module_t *module,
    const char *prefix,
    bool reset_stats);

END_C_DECLS
#endif

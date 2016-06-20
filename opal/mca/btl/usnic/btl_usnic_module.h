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
 * Copyright (c) 2011-2016 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef OPAL_BTL_USNIC_MODULE_H
#define OPAL_BTL_USNIC_MODULE_H

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_eq.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_ext_usnic.h>

#include "opal/class/opal_pointer_array.h"

#include "btl_usnic_endpoint.h"
#include "btl_usnic_stats.h"
#include "btl_usnic_util.h"

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
struct opal_btl_usnic_send_segment_t;
struct opal_btl_usnic_recv_segment_t;

/*
 * Abstraction of a set of endpoints
 */
typedef struct opal_btl_usnic_channel_t {
    int chan_index;

    struct fid_cq *cq;

    int chan_max_msg_size;
    int chan_rd_num;
    int chan_sd_num;

    int credits;  /* RFXXX until libfab credits fixed */
    uint32_t rx_post_cnt;

    /* fastsend enabled if num_credits_available >= fastsend_wqe_thresh */
    unsigned fastsend_wqe_thresh;

    /** pointer to receive segment whose bookkeeping has been deferred */
    struct opal_btl_usnic_recv_segment_t *chan_deferred_recv;

    /** queue pair and attributes */
    struct fi_info *info;
    struct fid_ep *ep;

    struct opal_btl_usnic_recv_segment_t *repost_recv_head;

    /** receive segments & buffers */
    opal_free_list_t recv_segs;

    bool chan_error;    /* set when error detected on channel */

    /* statistics */
    uint32_t num_channel_sends;
} opal_btl_usnic_channel_t;

/**
 * usnic BTL module
 */
typedef struct opal_btl_usnic_module_t {
    mca_btl_base_module_t super;

    /* Cache for use during component_init to associate a module with
       the libfabric device that it came from. */
    struct fid_fabric *fabric;
    struct fid_domain *domain;
    struct fi_info *fabric_info;
    struct fi_usnic_ops_fabric *usnic_fabric_ops;
    struct fi_usnic_ops_av *usnic_av_ops;
    struct fi_usnic_info usnic_info;
    struct fid_eq *dom_eq;
    struct fid_eq *av_eq;
    struct fid_av *av;

    size_t av_eq_size;

    mca_btl_base_module_error_cb_fn_t pml_error_callback;

    /* Information about the events */
    struct event device_async_event;
    bool device_async_event_active;
    int numa_distance; /* hwloc NUMA distance from this process */

    /** local address information */
    struct opal_btl_usnic_modex_t local_modex;
    char if_ipv4_addr_str[IPV4STRADDRLEN];

    /** desired send, receive, and completion queue entries (from MCA
        params; cached here on the component because the MCA param
        might == 0, which means "max supported on that device") */
    int sd_num;
    int rd_num;
    int cq_num;
    int av_eq_num;
    int prio_sd_num;
    int prio_rd_num;

    /*
     * Fragments larger than max_frag_payload will be broken up into
     * multiple chunks.  The amount that can be held in a single chunk
     * segment is slightly less than what can be held in frag segment due
     * to fragment reassembly info.
     */
    size_t max_tiny_msg_size;
    size_t max_frag_payload;    /* most that fits in a frag segment */
    size_t max_chunk_payload;   /* most that can fit in chunk segment */
    size_t max_tiny_payload;    /* threshold for using inline send */

    /** Hash table to keep track of senders */
    opal_hash_table_t senders;

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
    opal_free_list_t small_send_frags;
    opal_free_list_t large_send_frags;
    opal_free_list_t put_dest_frags;
    opal_free_list_t chunk_segs;

    /** receive buffer pools */
    int first_pool;
    int last_pool;
    opal_free_list_t *module_recv_buffers;

    /** list of endpoints with data to send */
    /* this list uses base endpoint ptr */
    opal_list_t endpoints_with_sends;

    /** list of send frags that are waiting to be resent (they
        previously deferred because of lack of resources) */
    opal_list_t pending_resend_segs;

    /** ack segments */
    opal_free_list_t ack_segs;

    /** list of endpoints to which we need to send ACKs */
    /* this list uses endpoint->endpoint_ack_li */
    opal_list_t endpoints_that_need_acks;

    /* abstract queue-pairs into channels */
    opal_btl_usnic_channel_t mod_channels[USNIC_NUM_CHANNELS];

    /* Number of short/erroneous packets we've receive on this
       interface */
    uint32_t num_short_packets;

    /* Performance / debugging statistics */
    opal_btl_usnic_module_stats_t stats;
} opal_btl_usnic_module_t;

struct opal_btl_usnic_frag_t;
extern opal_btl_usnic_module_t opal_btl_usnic_module_template;

/*
 * Manipulate the "endpoints_that_need_acks" list
 */

/* get first endpoint needing ACK */
static inline opal_btl_usnic_endpoint_t *
opal_btl_usnic_get_first_endpoint_needing_ack(
    opal_btl_usnic_module_t *module)
{
    opal_list_item_t *item;
    opal_btl_usnic_endpoint_t *endpoint;

    item = opal_list_get_first(&module->endpoints_that_need_acks);
    if (item != opal_list_get_end(&module->endpoints_that_need_acks)) {
        endpoint = container_of(item, mca_btl_base_endpoint_t, endpoint_ack_li);
        return endpoint;
    } else {
        return NULL;
    }
}

/* get next item in chain */
static inline opal_btl_usnic_endpoint_t *
opal_btl_usnic_get_next_endpoint_needing_ack(
    opal_btl_usnic_endpoint_t *endpoint)
{
    opal_list_item_t *item;
    opal_btl_usnic_module_t *module;

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
opal_btl_usnic_remove_from_endpoints_needing_ack(
    opal_btl_usnic_endpoint_t *endpoint)
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
opal_btl_usnic_add_to_endpoints_needing_ack(
    opal_btl_usnic_endpoint_t *endpoint)
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
int opal_btl_usnic_module_init(opal_btl_usnic_module_t* module);


/*
 * Progress pending sends on a module
 */
void opal_btl_usnic_module_progress_sends(opal_btl_usnic_module_t *module);

/* opal_output statistics that are useful for debugging */
void opal_btl_usnic_print_stats(
    opal_btl_usnic_module_t *module,
    const char *prefix,
    bool reset_stats);

END_C_DECLS
#endif

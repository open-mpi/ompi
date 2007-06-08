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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _IOF_BASE_ENDPOINT_
#define _IOF_BASE_ENDPOINT_

#include "orte_config.h"
#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/iof_base_header.h"

BEGIN_C_DECLS

/**
 * Structure store callbacks
 */

struct orte_iof_base_callback_t {
    opal_list_item_t super;
    orte_iof_base_callback_fn_t cb_func;
    void* cb_data;
};
typedef struct orte_iof_base_callback_t orte_iof_base_callback_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_iof_base_callback_t);

/**
 *  Structure that represents a published endpoint.
 */

struct orte_iof_base_endpoint_t {
    /** Parent */
    opal_list_item_t super;
    /** ORTE_IOF_SOURCE or ORTE_IOF_SINK */
    orte_iof_base_mode_t ep_mode;
    /** The origin process for this endpoint.  Will either by myself
        (i.e., it's an fd that represents a source or a sink in my
        process) or another process (i.e., this process is acting as a
        proxy for another process and [typically] has a pipe/fd optn
        to that process to get their stdin, stdout, or stderr). */
    orte_process_name_t ep_origin;
    /** Predefined tags: ORTE_IOF_ANY, ORTE_IOF_STDIN, ORTE_IOF_STDOUT,
        ORTE_IOF_STDERR */
    int ep_tag;
    /** File descriptor to read or write from (or -1 if it has been
        closed */
    int ep_fd;
    /** Rollover byte count of what has been forwarded from the fd to
        other targets */
    uint32_t ep_seq;
    /** Minimum byte count of what has been ACK'ed from all the targets
        that are listening to this endpoint */
    uint32_t ep_ack;
    /** Event library event for this file descriptor */
    opal_event_t ep_event;
    /** Special event library event for the case of stdin */
    opal_event_t ep_stdin_event;
    /** The list for fragments that are in-flight from a SOURCE 
        endpoint */
    opal_list_t ep_source_frags;
    /** The list for fragments that are in-flight from a SINK
        endpoint */
    opal_list_t ep_sink_frags;
    /** List of callbacks for subscriptions */
    opal_list_t ep_callbacks;
};
typedef struct orte_iof_base_endpoint_t orte_iof_base_endpoint_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_iof_base_endpoint_t);

/*
 * Diff between two sequence numbers allowing for rollover
 */

#define ORTE_IOF_BASE_SEQDIFF(s1,s2)  \
    ((s1 >= s2) ? (s1 - s2) : (s1 + (ULONG_MAX - s2)))


/**
 * Create a local endpoint.
 * 
 * @param name  Origin process name corresponding to endpoint.
 * @param mode  Source or sink of data (exclusive).
 * @param tag   Logical tag for matching.
 * @param fd Local file descriptor corresponding to endpoint.  If the
 * endpoint originates in this process, it'll be an fd in this
 * process.  If this process is acting as a proxy for another process,
 * then the fd will be a pipe to that other process (e.g., the origin
 * process' stdin, stdout, or stderr).
 */

ORTE_DECLSPEC int orte_iof_base_endpoint_create(
   const orte_process_name_t* name,
   orte_iof_base_mode_t mode,
   int tag,
   int fd);

/**
 * Associate a callback on receipt of data.
 * 
 * @param name    Process name corresponding to endpoint.
 * @param cbfunc  Logical tag for matching.
 * @aram  cbdata  Local file descriptor corresponding to endpoint.
 */

ORTE_DECLSPEC int orte_iof_base_callback_create(
    const orte_process_name_t *name,
    int tag,
    orte_iof_base_callback_fn_t cbfunc,
    void* cbdata);

ORTE_DECLSPEC int orte_iof_base_callback_delete(
    const orte_process_name_t *name,
    int tag);


/**
 * Delete all local endpoints matching the specified origin / mask /
 * tag parameters.
 *
 * @paran name  Origin process name corresponding to one or more endpoint(s).
 * @param mask  Mask used for name comparisons.
 * @param tag   Tag for matching endpoints.
 */

ORTE_DECLSPEC int orte_iof_base_endpoint_delete(
   const orte_process_name_t* name,
   orte_ns_cmp_bitmask_t mask,
   int tag);

/**
 * Disable forwarding through the specified endpoint.
 */

ORTE_DECLSPEC int orte_iof_base_endpoint_close(
    orte_iof_base_endpoint_t* endpoint);

/**
 * Attempt to match an endpoint based on the origin process name /
 * mask / tag.
 */

ORTE_DECLSPEC orte_iof_base_endpoint_t* orte_iof_base_endpoint_match(
    const orte_process_name_t* target_name,
    orte_ns_cmp_bitmask_t target_mask,
    int target_tag);

/**
 * Forward the specified message out the endpoint.
 */

ORTE_DECLSPEC int orte_iof_base_endpoint_forward(
    orte_iof_base_endpoint_t* endpoint,
    const orte_process_name_t* origin,
    orte_iof_base_msg_header_t* hdr,
    const unsigned char* data);

/*
 * Close the file descriptor associated with an endpoint and perform
 * any necessary cleanup.
 */

ORTE_DECLSPEC void orte_iof_base_endpoint_closed(
    orte_iof_base_endpoint_t* endpoint);

/**
 * Callback when the next set of bytes has been acknowledged.
 */

ORTE_DECLSPEC int orte_iof_base_endpoint_ack(
    orte_iof_base_endpoint_t* endpoint,
    uint32_t seq);

/**
 * Simple check for whether we have any frags "in flight".
 *
 * Return "true" for SOURCEs if source_frags is not empty, indicating
 * that there are frags in-flight via the RML.
 *
 * Return "true" for SINKs if sink_frags is not empty, indicating that
 * there are pending frags for the fd that are either partially
 * written or have not yet been written (because writing to the fd
 * would have blocked).
 */
bool orte_iof_base_endpoint_have_pending_frags(
    orte_iof_base_endpoint_t* endpoint);

/**
 * Simple check for whether we have all the ACKs that we expect.
 *
 * Return "true" for SOURCEs if ep_seq == ep_ack.
 *
 * Return "true" for SINKs always; SINK endpoints don't receive ACKs.
 */
bool orte_iof_base_endpoint_have_pending_acks(
    orte_iof_base_endpoint_t* endpoint);

END_C_DECLS

#endif


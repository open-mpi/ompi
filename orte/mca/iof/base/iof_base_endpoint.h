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

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

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
    opal_list_item_t super;
    orte_iof_base_mode_t ep_mode;
    orte_process_name_t ep_name;
    int ep_tag;
    int ep_fd;
    uint32_t ep_seq;
    uint32_t ep_ack;
    opal_event_t ep_event;
    opal_event_t ep_stdin_event;
    opal_list_t ep_frags;
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
 * @param name  Process name corresponding to endpoint.
 * @param mode  Source or sink of data (exclusive).
 * @param tag   Logical tag for matching.
 * @aram  fd    Local file descriptor corresponding to endpoint.
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
 * Delete all local endpoints matching the specified 
 * name/mask/tag parameters.
 *
 * @paran name  Process name corresponding to one or more endpoint(s).
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
 * Attempt to match an endpoint based on the destination
 * process name/mask/tag.
 */

ORTE_DECLSPEC orte_iof_base_endpoint_t* orte_iof_base_endpoint_match(
    const orte_process_name_t* dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    int dst_tag);

/**
 * Forward the specified message out the endpoint.
 */

ORTE_DECLSPEC int orte_iof_base_endpoint_forward(
    orte_iof_base_endpoint_t* endpoint,
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr,
    const unsigned char* data);

/*
 * Callback when peer has closed endpoint.
 */

ORTE_DECLSPEC void orte_iof_base_endpoint_closed(
    orte_iof_base_endpoint_t* endpoint);

/**
 * Callback when the specified sequence has been 
 * acknowledged.
 */

ORTE_DECLSPEC int orte_iof_base_endpoint_ack(
    orte_iof_base_endpoint_t* endpoint,
    uint32_t seq);

/**
 * Check for pending I/O
 */

static inline bool orte_iof_base_endpoint_pending(
    orte_iof_base_endpoint_t* endpoint)
{
    return opal_list_get_size(&endpoint->ep_frags) || (endpoint->ep_seq != endpoint->ep_ack);
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif


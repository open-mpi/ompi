/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * The svc IOF component is used in HNP processes only.  It is the
 * "hub" for all IOF activity, meaning that *all* IOF traffic is
 * routed to the svc component, and this component figures out where
 * it is supposed to go from there.  Specifically: there is *no*
 * direct proxy-to-proxy IOF communication.  If a proxy/orted wants to
 * get a stream from another proxy/orted, the stream will go
 * proxy/orted -> svc/HNP -> proxy/orted.
 *
 * The svc IOF component does two things: 1. forward fragments between
 * file descriptors and streams, and 2. maintain forwarding tables to
 * "route" incomding fragments to outgoing destinations (both file
 * descriptors and other published streams).
 *
 * The svc IOF component maintains tables of all publications and all
 * subscriptions.  Subscriptions can have a list of publications
 * and/or endpoints to forward incoming fragments to.
 *
 * 
 *
 * Important: this component is designed to work with the proxy IOF
 * component only.  If we ever do a different IOF implementation
 * scheme, it is likely that only some of this component will be
 * useful for cannibalisation (if any at all).
 */
#ifndef ORTE_IOF_SVC_H
#define ORTE_IOF_SVC_H

#include "orte/mca/iof/iof.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif  /* HAVE_SYS_UIO_H */
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif  /* HAVE_NET_UIO_H */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/**
 * Publish a local file descriptor as an endpoint that is logically
 * associated with the specified process name (e.g. master side of a
 * pipe/pty connected to a child process)
 *
 * @param name
 * @param mode
 * @param tag
 * @param fd
 *
 */

int orte_iof_svc_publish(
    const orte_process_name_t* name,
    orte_iof_base_mode_t mode,
    orte_iof_base_tag_t tag,
    int fd
);

/**
 * Remove all registrations matching the specified process
 * name, mask and tag values.
 *
 * @param name
 * @param mask
 * @param tag
 *
 */

int orte_iof_svc_unpublish(
    const orte_process_name_t* name,
    orte_ns_cmp_bitmask_t mask,
    orte_iof_base_tag_t tag
);

/**
 * Explicitly push data from the specified file descriptor
 * to the indicated set of peers.
 * 
 * @param dst_name  Name used to qualify set of peers.
 * @param dst_mask  Mask that specified how name is interpreted.
 * @param dst_tag   Match a specific peer endpoint.
 * @param fd        Local file descriptor.
 */

int orte_iof_svc_push(
    const orte_process_name_t* dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag,
    int fd
);

/**
 * Explicitly pull data from the specified set of peers
 * and dump to the indicated file descriptor.
 * 
 * @param dst_name  Name used to qualify set of peers.
 * @param dst_mask  Mask that specified how name is interpreted.
 * @param dst_tag   Match a specific peer endpoint.
 * @param fd        Local file descriptor.
 */

int orte_iof_svc_pull(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    int fd
);

/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of peers.
 */

int orte_iof_svc_subscribe(
    const orte_process_name_t* src_name,  
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    orte_iof_base_callback_fn_t cb,
    void* cbdata
);

int orte_iof_svc_unsubscribe(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag
);

int orte_iof_svc_finalize(void);

/**
 * IOF svc Component 
 */
struct orte_iof_svc_component_t { 
    orte_iof_base_component_t super;
    opal_list_t svc_published;
    opal_list_t svc_subscribed;
    opal_mutex_t svc_lock;
    struct iovec svc_iov[1];
};
typedef struct orte_iof_svc_component_t orte_iof_svc_component_t;

ORTE_MODULE_DECLSPEC extern orte_iof_svc_component_t mca_iof_svc_component;
extern orte_iof_base_module_t orte_iof_svc_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

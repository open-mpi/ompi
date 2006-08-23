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
/**
 * @file
 */
#ifndef ORTE_IOF_PROXY_H
#define ORTE_IOF_PROXY_H

#include "orte/mca/iof/iof.h"

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

int orte_iof_proxy_publish(
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

int orte_iof_proxy_unpublish(
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

int orte_iof_proxy_push(
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

int orte_iof_proxy_pull(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    int fd
);

/**
 * Setup buffering for a specified set of endpoints.
 */

int orte_iof_proxy_buffer(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    size_t buffer_size
);

/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of peers.
 */

int orte_iof_proxy_subscribe(
    const orte_process_name_t* src_name,  
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    orte_iof_base_callback_fn_t cb,
    void* cbdata
);

int orte_iof_proxy_unsubscribe(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag
);

/**
 * IOF proxy Component 
 */
struct orte_iof_proxy_component_t {
    orte_iof_base_component_t super;
    int proxy_debug;
    struct iovec proxy_iov[1];
};
typedef struct orte_iof_proxy_component_t orte_iof_proxy_component_t;

ORTE_MODULE_DECLSPEC extern orte_iof_proxy_component_t mca_iof_proxy_component;
extern orte_iof_base_module_t orte_iof_proxy_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

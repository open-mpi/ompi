/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_IOF_PROXY_H
#define MCA_IOF_PROXY_H

#include "mca/iof/iof.h"
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

int mca_iof_proxy_publish(
    const ompi_process_name_t* name,
    mca_iof_base_mode_t mode,
    mca_iof_base_tag_t tag,
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

int mca_iof_proxy_unpublish(
    const ompi_process_name_t* name,
    ompi_ns_cmp_bitmask_t mask,
    mca_iof_base_tag_t tag
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

int mca_iof_proxy_push(
    const ompi_process_name_t* dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    mca_iof_base_tag_t dst_tag,
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

int mca_iof_proxy_pull(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    int fd
);

/**
 * Setup buffering for a specified set of endpoints.
 */

int mca_iof_proxy_buffer(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    size_t buffer_size
);

/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of peers.
 */

int mca_iof_proxy_subscribe(
    const ompi_process_name_t* src_name,  
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    mca_iof_base_callback_fn_t cb,
    void* cbdata
);

int mca_iof_proxy_unsubscribe(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag
);

/**
 * IOF proxy Component 
 */
struct mca_iof_proxy_component_t {
    mca_iof_base_component_t super;
};
typedef struct mca_iof_proxy_component_t mca_iof_proxy_component_t;

OMPI_COMP_EXPORT extern mca_iof_proxy_component_t mca_iof_proxy_component;
OMPI_COMP_EXPORT extern mca_iof_base_module_t mca_iof_proxy_module;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

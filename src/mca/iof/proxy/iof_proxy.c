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
#include "ompi_config.h"
#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "include/constants.h"
#include "util/output.h"
#include "iof_proxy.h"


mca_iof_base_module_t mca_iof_proxy_module = {
    mca_iof_proxy_publish,
    mca_iof_proxy_unpublish,
    mca_iof_proxy_push,
    mca_iof_proxy_pull,
    mca_iof_proxy_subscribe,
    mca_iof_proxy_unsubscribe
};


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
    int fd)
{
    return OMPI_ERROR;
}


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
    mca_iof_base_tag_t tag)
{
    return OMPI_ERROR;
}


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
    int fd)
{
    return OMPI_ERROR;
}


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
    int fd)
{
    return OMPI_ERROR;
}


/**
 * Setup buffering for a specified set of endpoints.
 */

int mca_iof_proxy_buffer(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    size_t buffer_size)
{
    return OMPI_ERROR;
}


/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of peers.
 */

int mca_iof_proxy_subscribe(
    const ompi_process_name_t* src_name,  
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    mca_iof_base_callback_fn_t cb,
    void* cbdata)
{
    return OMPI_ERROR;
}

int mca_iof_proxy_unsubscribe(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag)
{
    return OMPI_ERROR;
}


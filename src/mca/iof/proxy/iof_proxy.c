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
#include "mca/iof/iof.h"
#include "mca/oob/oob.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "iof_proxy.h"
#include "iof_proxy_svc.h"


mca_iof_base_module_t mca_iof_proxy_module = {
    mca_iof_proxy_publish,
    mca_iof_proxy_unpublish,
    mca_iof_proxy_push,
    mca_iof_proxy_pull,
    mca_iof_proxy_subscribe,
    mca_iof_proxy_unsubscribe,
    mca_iof_base_flush
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
    int rc;

    /* publish to server */
    if(mode == MCA_IOF_SINK) {
        rc = mca_iof_proxy_svc_publish(name,tag);
        if(rc != OMPI_SUCCESS)
            return rc;
    }

    /* setup a local endpoint to reflect registration */
    rc = mca_iof_base_endpoint_create(
        name,
        mode,
        tag,
        fd);
    return rc;
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
    int rc;

    /* cleanup server */
    mca_iof_proxy_svc_unpublish(
        name,
        mask,
        tag);

    /* setup a local endpoint to reflect registration */
    rc = mca_iof_base_endpoint_delete(
        name,
        mask,
        tag);
    return rc;
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
    int rc;

    /* send a subscription to server on behalf of the destination */
    rc = mca_iof_proxy_svc_subscribe(
        MCA_OOB_NAME_SELF,
        OMPI_NS_CMP_ALL,
        dst_tag,
        dst_name,
        dst_mask,
        dst_tag
        );
    if(rc != OMPI_SUCCESS)
        return rc;
                                                                                                             
    /* setup a local endpoint to reflect registration */
    rc = mca_iof_base_endpoint_create(
        MCA_OOB_NAME_SELF,
        MCA_IOF_SOURCE,
        dst_tag,
        fd);
 
    return rc;
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
    /* setup a local endpoint */
    int rc;
    rc = mca_iof_base_endpoint_create(
        MCA_OOB_NAME_SELF,
        MCA_IOF_SOURCE,
        src_tag,
        fd);
    if(rc != OMPI_SUCCESS)
        return rc;

    /* send a subscription message to the server */
    rc = mca_iof_proxy_svc_subscribe(
        src_name,
        src_mask,
        src_tag,
        MCA_OOB_NAME_SELF,
        OMPI_NS_CMP_ALL,
        src_tag);
    return rc;
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
    int rc;

    /* create a local registration to reflect the callback */

    /* send a subscription message to the service */
    rc = mca_iof_proxy_svc_subscribe(
        src_name,
        src_mask,
        src_tag,
        MCA_OOB_NAME_SELF,
        OMPI_NS_CMP_ALL,
        src_tag);
    return rc;
}

int mca_iof_proxy_unsubscribe(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag)
{
    int rc;

    /* send an unsubscribe message to the service */
    rc = mca_iof_proxy_svc_unsubscribe(
        src_name,
        src_mask,
        src_tag,
        MCA_OOB_NAME_SELF,
        OMPI_NS_CMP_ALL,
        src_tag);

    /* remove local callback */
    return OMPI_ERROR;
}


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
#include "mca/oob/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "iof_svc.h"
#include "iof_svc_publish.h"
#include "iof_svc_subscript.h"


mca_iof_base_module_t mca_iof_svc_module = {
    mca_iof_svc_publish,
    mca_iof_svc_unpublish,
    mca_iof_svc_push,
    mca_iof_svc_pull,
    mca_iof_svc_subscribe,
    mca_iof_svc_unsubscribe
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

int mca_iof_svc_publish(
    const ompi_process_name_t* name,
    mca_iof_base_mode_t mode,
    mca_iof_base_tag_t tag,
    int fd)
{
    int rc;

    /* setup a local endpoint to reflect registration */
    rc = mca_iof_base_endpoint_create(
        name,
        mode,
        tag,
        fd);
    if(rc != OMPI_SUCCESS) {
        return rc;
    }

    /* publish endpoint */
    if(mode == MCA_IOF_SINK) {
        rc = mca_iof_svc_publish_create(
            name,
            &mca_oob_name_self,
            OMPI_NS_CMP_ALL,
            tag);
    }
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

int mca_iof_svc_unpublish(
    const ompi_process_name_t* name,
    ompi_ns_cmp_bitmask_t mask,
    mca_iof_base_tag_t tag)
{
    int rc;
    rc = mca_iof_svc_publish_delete(
        name,
        MCA_OOB_NAME_SELF,
        mask,
        tag);
    if(rc != OMPI_SUCCESS)
        return rc;

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

int mca_iof_svc_push(
    const ompi_process_name_t* dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    mca_iof_base_tag_t dst_tag,
    int fd)
{
    int rc;

    /* setup a subscription */
    rc = mca_iof_svc_subscript_create(
        MCA_OOB_NAME_SELF,
        OMPI_NS_CMP_ALL,
        dst_tag,
        dst_name,
        dst_mask,
        dst_tag);
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

int mca_iof_svc_pull(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    int fd)
{
    int rc;

    /* setup a local endpoint */
    rc = mca_iof_base_endpoint_create(
        MCA_OOB_NAME_SELF, 
        MCA_IOF_SINK, 
        src_tag,
        fd);
    if(rc != OMPI_SUCCESS)
        return rc;

    /* create a subscription */
    rc = mca_iof_svc_subscript_create(
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

int mca_iof_svc_buffer(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    size_t buffer_size)
{
    /* send a message to the server indicating this set of connections should be buffered */
    return OMPI_ERROR;
}


/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of peers.
 */

int mca_iof_svc_subscribe(
    const ompi_process_name_t* src_name,  
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    mca_iof_base_callback_fn_t cb,
    void* cbdata)
{
    /* setup local callback on receipt of data */
    /* setup local subscription */
    return OMPI_ERROR;
}

int mca_iof_svc_unsubscribe(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag)
{
    /* cleanup any local resouces associated with this subscription */
    return OMPI_ERROR;
}


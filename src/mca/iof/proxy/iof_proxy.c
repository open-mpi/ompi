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
#include "mca/rml/rml.h"
#include "mca/rml/rml_types.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "iof_proxy.h"
#include "iof_proxy_svc.h"


orte_iof_base_module_t orte_iof_proxy_module = {
    orte_iof_proxy_publish,
    orte_iof_proxy_unpublish,
    orte_iof_proxy_push,
    orte_iof_proxy_pull,
    orte_iof_proxy_subscribe,
    orte_iof_proxy_unsubscribe,
    orte_iof_base_flush
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

int orte_iof_proxy_publish(
    const orte_process_name_t* name,
    orte_iof_base_mode_t mode,
    orte_iof_base_tag_t tag,
    int fd)
{
    int rc;

    if(mca_iof_proxy_component.proxy_debug > 1) {
        char* name_str;
        orte_ns.get_proc_name_string(&name_str, name);
        ompi_output(0, "orte_iof_proxy_publish(%s,%d,%d,%d)\n", name_str, mode, tag, fd);
        free(name_str);
    }

    /* publish to server */
    if(mode == ORTE_IOF_SINK) {
        rc = orte_iof_proxy_svc_publish(name,tag);
        if(rc != OMPI_SUCCESS)
            return rc;
    }

    /* setup a local endpoint to reflect registration */
    rc = orte_iof_base_endpoint_create(
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

int orte_iof_proxy_unpublish(
    const orte_process_name_t* name,
    orte_ns_cmp_bitmask_t mask,
    orte_iof_base_tag_t tag)
{
    int rc;

    /* cleanup server */
    orte_iof_proxy_svc_unpublish(
        name,
        mask,
        tag);

    /* setup a local endpoint to reflect registration */
    rc = orte_iof_base_endpoint_delete(
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

int orte_iof_proxy_push(
    const orte_process_name_t* dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag,
    int fd)
{
    int rc;

    /* send a subscription to server on behalf of the destination */
    rc = orte_iof_proxy_svc_subscribe(
        ORTE_RML_NAME_SELF,
        ORTE_NS_CMP_ALL,
        dst_tag,
        dst_name,
        dst_mask,
        dst_tag
        );
    if(rc != OMPI_SUCCESS)
        return rc;
                                                                                                             
    /* setup a local endpoint to reflect registration */
    rc = orte_iof_base_endpoint_create(
        ORTE_RML_NAME_SELF,
        ORTE_IOF_SOURCE,
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

int orte_iof_proxy_pull(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    int fd)
{
    /* setup a local endpoint */
    int rc;
    rc = orte_iof_base_endpoint_create(
        ORTE_RML_NAME_SELF,
        ORTE_IOF_SOURCE,
        src_tag,
        fd);
    if(rc != OMPI_SUCCESS)
        return rc;

    /* send a subscription message to the server */
    rc = orte_iof_proxy_svc_subscribe(
        src_name,
        src_mask,
        src_tag,
        ORTE_RML_NAME_SELF,
        ORTE_NS_CMP_ALL,
        src_tag);
    return rc;
}

/**
 * Setup buffering for a specified set of endpoints.
 */

int orte_iof_proxy_buffer(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    size_t buffer_size)
{
    return OMPI_ERROR;
}


/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of peers.
 */

int orte_iof_proxy_subscribe(
    const orte_process_name_t* src_name,  
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    orte_iof_base_callback_fn_t cb,
    void* cbdata)
{
    int rc;

    /* create a local registration to reflect the callback */

    /* send a subscription message to the service */
    rc = orte_iof_proxy_svc_subscribe(
        src_name,
        src_mask,
        src_tag,
        ORTE_RML_NAME_SELF,
        ORTE_NS_CMP_ALL,
        src_tag);
    return rc;
}

int orte_iof_proxy_unsubscribe(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag)
{
    int rc;

    /* send an unsubscribe message to the service */
    rc = orte_iof_proxy_svc_unsubscribe(
        src_name,
        src_mask,
        src_tag,
        ORTE_RML_NAME_SELF,
        ORTE_NS_CMP_ALL,
        src_tag);

    /* remove local callback */
    return OMPI_ERROR;
}


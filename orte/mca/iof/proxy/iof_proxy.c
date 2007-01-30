/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
#include "orte_config.h"
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "orte/mca/errmgr/errmgr.h"
#include "iof_proxy.h"
#include "iof_proxy_svc.h"


orte_iof_base_module_t orte_iof_proxy_module = {
    orte_iof_proxy_publish,
    orte_iof_proxy_unpublish,
    orte_iof_proxy_push,
    orte_iof_proxy_pull,
    orte_iof_proxy_subscribe,
    orte_iof_proxy_unsubscribe,
    orte_iof_base_flush,
    NULL
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
        opal_output(0, "orte_iof_proxy_publish(%s,%d,%d,%d)\n", name_str, mode, tag, fd);
        free(name_str);
    }

    /* publish to server */
    if(mode == ORTE_IOF_SINK) {
        rc = orte_iof_proxy_svc_publish(name,tag);
        if(rc != ORTE_SUCCESS)
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
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        dst_tag,
        dst_name,
        dst_mask,
        dst_tag
        );
    if(rc != ORTE_SUCCESS)
        return rc;
                                                                                                             
    /* setup a local endpoint to reflect registration */
    rc = orte_iof_base_endpoint_create(
        ORTE_PROC_MY_NAME,
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
        ORTE_PROC_MY_NAME,
        ORTE_IOF_SINK,
        src_tag,
        fd);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    } 

    /* publish this endpoint */
    rc = orte_iof_proxy_svc_publish(
        ORTE_PROC_MY_NAME,
        src_tag);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* subscribe to peer */
    rc = orte_iof_proxy_svc_subscribe(
        src_name,
        src_mask,
        src_tag,
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        src_tag);
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

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
    return ORTE_ERROR;
}


/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of peers.
 */

int orte_iof_proxy_subscribe(
    const orte_process_name_t* src_name,  
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    orte_iof_base_callback_fn_t cbfunc,
    void* cbdata)
{
    int rc;

    /* create a local registration to reflect the callback */
    rc = orte_iof_base_callback_create(ORTE_PROC_MY_NAME,src_tag,cbfunc,cbdata);
    if(rc != ORTE_SUCCESS)
        return rc;

    /* send a subscription message to the service */
    rc = orte_iof_proxy_svc_subscribe(
        src_name,
        src_mask,
        src_tag,
        ORTE_PROC_MY_NAME,
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
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        src_tag);
    if(rc != ORTE_SUCCESS)
        return rc;
  
    /* remove local callback */
    return orte_iof_base_callback_delete(ORTE_PROC_MY_NAME,src_tag);
}


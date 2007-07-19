/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
    orte_iof_proxy_subscribe,
    orte_iof_proxy_unsubscribe,
    orte_iof_proxy_push,
    orte_iof_proxy_pull,
    orte_iof_base_flush,
    orte_iof_proxy_finalize,
};

/*
 * Finalize module; nothing to do
 */

int orte_iof_proxy_finalize(void ) 
{
    return ORTE_SUCCESS;
}

/**
 * Create an endpoint for a local file descriptor and "publish" it
 * under the name of the origin process.  If the publish mode is a
 * SINK, then create a publication entry for it so that incoming
 * messages can be forwarded to it.
 *
 * SOURCEs do not need to create publication records because a) the
 * endpoint will automatically wake up the event engine and read off
 * the fd whenever there is data available, and b) this data is then
 * automatically sent to the iof svc component for possible
 * forwarding.
 */

int orte_iof_proxy_publish(
    const orte_process_name_t* origin,
    orte_iof_base_mode_t mode,
    orte_iof_base_tag_t tag,
    int fd)
{
    int rc;

    if (orte_iof_base.iof_output >= 0) {
        char* name_str;
        orte_ns.get_proc_name_string(&name_str, origin);
        opal_output(orte_iof_base.iof_output,
                    "orte_iof_proxy_publish(%s,%d,%d,%d)\n", 
                    name_str, mode, tag, fd);
        free(name_str);
    }

    rc = orte_iof_base_endpoint_create(
        origin,
        mode,
        tag,
        fd);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* publish to server */
    if (ORTE_IOF_SINK == mode) {
        rc = orte_iof_proxy_svc_publish(origin, tag);
        if (rc != ORTE_SUCCESS) {
            return rc;
        }
    }

    return ORTE_SUCCESS;
}


/**
 * Remove all registrations matching the specified origin process
 * name, mask and tag values.
 */

int orte_iof_proxy_unpublish(
    const orte_process_name_t* origin,
    orte_ns_cmp_bitmask_t mask,
    orte_iof_base_tag_t tag)
{
    int rc;

#if 0
        {
            int i = 0;
            opal_output(orte_iof_base.iof_output, "[%lu,%lu,%lu] orted: ******** ABOUT TO IOF PROXY UNPUBLISH, %d", ORTE_NAME_ARGS(orte_process_info.my_name), getpid());
            fflush(stderr);
            while (0 == i) sleep(5);
        }
#endif

    /* cleanup server */
    orte_iof_proxy_svc_unpublish(
        origin,
        mask,
        tag);

    /* delete local endpoint.  Note that the endpoint may have already
       been deleted (e.g., if some entity noticed that the fd closed
       and called orte_iof_base_endpoint_delete on the corresopnding
       endpoint already).  So if we get NOT_FOUND, ignore that error
       -- the end result is what we want: the endpoint is deleted when
       we return. */
    rc = orte_iof_base_endpoint_delete(
        origin,
        mask,
        tag);
    if (ORTE_ERR_NOT_FOUND == rc || ORTE_SUCCESS == rc) {
        return ORTE_SUCCESS;
    } else {
        return rc;
    }
}


/**
 * Explicitly push data from the specified file descriptor
 * to the indicated SINK set of peers.
 */

int orte_iof_proxy_push(
    const orte_process_name_t* sink_name,
    orte_ns_cmp_bitmask_t sink_mask,
    orte_iof_base_tag_t sink_tag,
    int fd)
{
    int rc;

    /* setup a local endpoint to reflect registration.  Do this before
       we send the subscription to the server in case a callback
       occurs *while* we are sending the subscription request. */
    rc = orte_iof_base_endpoint_create(
        ORTE_PROC_MY_NAME,
        ORTE_IOF_SOURCE,
        sink_tag,
        fd);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

     /* send a subscription to server on behalf of the destination */
    rc = orte_iof_proxy_svc_subscribe(
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        sink_tag,
        sink_name,
        sink_mask,
        sink_tag
        );
   return rc;
}


/**
 * Explicitly pull data from the specified set of SOURCE peers and
 * dump to the indicated file descriptor.
 */

int orte_iof_proxy_pull(
    const orte_process_name_t* source_name,
    orte_ns_cmp_bitmask_t source_mask,
    orte_iof_base_tag_t source_tag,
    int fd)
{
    /* setup a local endpoint */
    int rc;
    rc = orte_iof_base_endpoint_create(
        ORTE_PROC_MY_NAME,
        ORTE_IOF_SINK,
        source_tag,
        fd);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    } 

    /* publish this endpoint */
    rc = orte_iof_proxy_svc_publish(
        ORTE_PROC_MY_NAME,
        source_tag);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* subscribe to peer */
    rc = orte_iof_proxy_svc_subscribe(
        source_name,
        source_mask,
        source_tag,
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        source_tag);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return rc;
}

/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of origin peers.
 */

int orte_iof_proxy_subscribe(
    const orte_process_name_t* origin_name,  
    orte_ns_cmp_bitmask_t origin_mask,
    orte_iof_base_tag_t origin_tag,
    orte_iof_base_callback_fn_t cbfunc,
    void* cbdata)
{
    int rc;

    /* create a local registration to reflect the callback */
    rc = orte_iof_base_callback_create(ORTE_PROC_MY_NAME,origin_tag,cbfunc,cbdata);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* send a subscription message to the service */
    rc = orte_iof_proxy_svc_subscribe(
        origin_name,
        origin_mask,
        origin_tag,
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        origin_tag);
    return rc;
}

/*
 * Remove a subscription
 */

int orte_iof_proxy_unsubscribe(
    const orte_process_name_t* origin_name,
    orte_ns_cmp_bitmask_t origin_mask,
    orte_iof_base_tag_t origin_tag)
{
    int rc;

    /* send an unsubscribe message to the service */
    rc = orte_iof_proxy_svc_unsubscribe(
        origin_name,
        origin_mask,
        origin_tag,
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        origin_tag);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }
  
    /* remove local callback */
    return orte_iof_base_callback_delete(ORTE_PROC_MY_NAME,origin_tag);
}

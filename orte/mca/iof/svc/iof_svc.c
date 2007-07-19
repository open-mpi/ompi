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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
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
#include "orte/mca/oob/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "iof_svc.h"
#include "iof_svc_pub.h"
#include "iof_svc_sub.h"


orte_iof_base_module_t orte_iof_svc_module = {
    orte_iof_svc_publish,
    orte_iof_svc_unpublish,
    orte_iof_svc_subscribe,
    orte_iof_svc_unsubscribe,
    orte_iof_svc_push,
    orte_iof_svc_pull,
    orte_iof_base_flush,
    orte_iof_svc_finalize,
};


/*
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

int orte_iof_svc_publish(
    const orte_process_name_t* origin,
    orte_iof_base_mode_t mode,
    orte_iof_base_tag_t tag,
    int fd)
{
    int rc;

    /* setup a local endpoint to reflect registration */
    rc = orte_iof_base_endpoint_create(
        origin,
        mode,
        tag,
        fd);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* publish endpoint */
    if (ORTE_IOF_SINK == mode) {
        rc = orte_iof_svc_pub_create(
            origin,
            ORTE_PROC_MY_NAME,
            ORTE_NS_CMP_ALL,
            tag);
    }
    return rc;
}


/*
 * Remove all registrations matching the specified origin process
 * name, mask and tag values (where, here in the svc component, origin
 * should usually be just this process -- ths svc component is
 * unlikely to act as an IOF proxy for any other processes like the
 * orted does).
 */

int orte_iof_svc_unpublish(
    const orte_process_name_t* origin,
    orte_ns_cmp_bitmask_t mask,
    orte_iof_base_tag_t tag)
{
    int rc;

    /* Delete the corresponding publish.  Note that it may have
       already been deleted by some other entity (e.g., message
       arriving saying to unpublish), so we may get a NOT_FOUND.
       That's ok/not an error -- the only end result that we want is
       that there is no corresponding publish. */
    rc = orte_iof_svc_pub_delete(
        origin,
        ORTE_PROC_MY_NAME,
        mask,
        tag);
    if (ORTE_SUCCESS != rc && ORTE_ERR_NOT_FOUND != rc) {
        return rc;
    }

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
 * to the indicated set of SINK peers.
 */

int orte_iof_svc_push(
    const orte_process_name_t* sink_name,
    orte_ns_cmp_bitmask_t sink_mask,
    orte_iof_base_tag_t sink_tag,
    int fd)
{
    int rc;

    /* Setup a subscription.  This will be matched against a publish
       of a SINK from a remote process. */
    rc = orte_iof_svc_sub_create(
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        sink_tag,
        sink_name,
        sink_mask,
        sink_tag);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* Setup a local endpoint to reflect registration.  This will
       enter the fd into the event engine and wakeup when there is
       data to read.  The data will be put in an IOF fragment and RML
       send to iof_svc_proxy_recv() (i.e., in this module!) for
       handling (i.e., matching and forwarding to the publish(es) that
       was(were) matched to the above subscription).  

       Create this endpoint *after* we make the above subscription so
       that it is not found and attached to the subscription.
       Instead, data that is consumed by the event engine callbacks
       will be RML-sent to iof_svc_proxy_recv(), as described
       above. */
    rc = orte_iof_base_endpoint_create(
        ORTE_PROC_MY_NAME, 
        ORTE_IOF_SOURCE, 
        sink_tag,
        fd);
    return rc;
}


/*
 * Explicitly pull data from the specified set of SOURCE peers
 * and dump to the indicated file descriptor.
 */

int orte_iof_svc_pull(
    const orte_process_name_t* source_name,
    orte_ns_cmp_bitmask_t source_mask,
    orte_iof_base_tag_t source_tag,
    int fd)
{
    int rc;

    /* setup a local endpoint -- *before* we create the subscription
       so that the subscription will find the endpoint and attach it
       to the subscription */
    rc = orte_iof_base_endpoint_create(
        ORTE_PROC_MY_NAME, 
        ORTE_IOF_SINK, 
        source_tag,
        fd);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* create a subscription */
    rc = orte_iof_svc_sub_create(
        source_name,
        source_mask,
        source_tag,
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        source_tag);
    return rc;
}


/*
 * Subscribe to receive a callback on receipt of data
 * from a specified set of origin peers.
 */

int orte_iof_svc_subscribe(
    const orte_process_name_t* origin_name,
    orte_ns_cmp_bitmask_t origin_mask,
    orte_iof_base_tag_t origin_tag,
    orte_iof_base_callback_fn_t cbfunc,
    void* cbdata)
{
    int rc;

    /* create a local registration to reflect the callback */
    rc = orte_iof_base_callback_create(ORTE_PROC_MY_NAME, origin_tag, 
                                       cbfunc, cbdata);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* setup local subscription */
    rc = orte_iof_svc_sub_create(
        origin_name,
        origin_mask,
        origin_tag,
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        origin_tag);
    return rc;
}

int orte_iof_svc_unsubscribe(
    const orte_process_name_t* origin_name,
    orte_ns_cmp_bitmask_t origin_mask,
    orte_iof_base_tag_t origin_tag)
{
    int rc;

    /* delete local subscription */
    rc = orte_iof_svc_sub_delete(
        origin_name,
        origin_mask,
        origin_tag,
        ORTE_PROC_MY_NAME,
        ORTE_NS_CMP_ALL,
        origin_tag);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* cleanup any locally registered callback */
    return orte_iof_base_callback_delete(ORTE_PROC_MY_NAME, origin_tag);
}

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

#ifndef ORTE_IOF_SVC_PUBLISH_H
#define ORTE_IOF_SVC_PUBLISH_H

#include "orte_config.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "iof_svc.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 *  Endpoints that are sinks of data are published by the
 *  processes that is acting as a proxy for the destination
 *  application. The published endpoints are matched against
 *  subscriptions to determine the sources of data that are
 *  forwarded to the endpoint.
 */

struct orte_iof_svc_pub_t {
    opal_list_item_t          super;
    orte_process_name_t       pub_name;
    orte_process_name_t       pub_proxy;
    orte_ns_cmp_bitmask_t     pub_mask;
    orte_iof_base_tag_t       pub_tag;
    orte_iof_base_endpoint_t* pub_endpoint;
};
typedef struct orte_iof_svc_pub_t orte_iof_svc_pub_t;

OBJ_CLASS_DECLARATION(orte_iof_svc_pub_t);


/**
 * Create a new entry.
 */

int orte_iof_svc_pub_create(
    const orte_process_name_t* pub_name,
    const orte_process_name_t* pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag);

/**
 * Lookup an existing entry.
 */

orte_iof_svc_pub_t* orte_iof_svc_pub_lookup(
    const orte_process_name_t *pub_name,
    const orte_process_name_t *pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag);

/**
 * Cleanup an existing entry.
 */

int orte_iof_svc_pub_delete(
    const orte_process_name_t* pub_name,
    const orte_process_name_t* pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag);

/**
 * Remove all entries matching a specified process name.
 */

void orte_iof_svc_pub_delete_all(
    const orte_process_name_t* name);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif


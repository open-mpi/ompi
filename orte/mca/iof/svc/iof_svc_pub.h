#ifndef ORTE_IOF_SVC_PUBLISH_H
#define ORTE_IOF_SVC_PUBLISH_H

#include "ompi_config.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "iof_svc.h"



/**
 *  Endpoints that are sinks of data are published by the
 *  processes that is acting as a proxy for the destination
 *  application. The published endpoints are matched against
 *  subscriptions to determine the sources of data that are
 *  forwarded to the endpoint.
 */

struct orte_iof_svc_pub_t {
    ompi_list_item_t          super;
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


#endif


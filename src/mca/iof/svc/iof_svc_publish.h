#ifndef ORTE_IOF_SVC_PUBLISH_H
#define ORTE_IOF_SVC_PUBLISH_H

#include "ompi_config.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "iof_svc.h"

/**
 *
 */

struct orte_iof_svc_publish_t {
    ompi_list_item_t         super;
    orte_process_name_t      pub_name;
    orte_process_name_t      pub_proxy;
    orte_ns_cmp_bitmask_t    pub_mask;
    orte_iof_base_tag_t       pub_tag;
    orte_iof_base_endpoint_t* pub_endpoint;
};
typedef struct orte_iof_svc_publish_t orte_iof_svc_publish_t;

OBJ_CLASS_DECLARATION(orte_iof_svc_publish_t);


/**
 *
 */

int orte_iof_svc_publish_create(
    const orte_process_name_t* pub_name,
    const orte_process_name_t* pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag);

/**
 *
 */

int orte_iof_svc_publish_delete(
    const orte_process_name_t* pub_name,
    const orte_process_name_t* pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag);



#endif


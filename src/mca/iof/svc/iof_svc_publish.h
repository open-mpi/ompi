#ifndef MCA_IOF_SVC_PUBLISH_H
#define MCA_IOF_SVC_PUBLISH_H

#include "ompi_config.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "iof_svc.h"

/**
 *
 */

struct mca_iof_svc_publish_t {
    ompi_list_item_t         super;
    ompi_process_name_t      pub_name;
    ompi_process_name_t      pub_proxy;
    ompi_ns_cmp_bitmask_t    pub_mask;
    mca_iof_base_tag_t       pub_tag;
    mca_iof_base_endpoint_t* pub_endpoint;
};
typedef struct mca_iof_svc_publish_t mca_iof_svc_publish_t;

OBJ_CLASS_DECLARATION(mca_iof_svc_publish_t);


/**
 *
 */

int mca_iof_svc_publish_create(
    const ompi_process_name_t* pub_name,
    const ompi_process_name_t* pub_proxy,
    ompi_ns_cmp_bitmask_t pub_mask,
    mca_iof_base_tag_t pub_tag);

/**
 *
 */

int mca_iof_svc_publish_delete(
    const ompi_process_name_t* pub_name,
    const ompi_process_name_t* pub_proxy,
    ompi_ns_cmp_bitmask_t pub_mask,
    mca_iof_base_tag_t pub_tag);



#endif


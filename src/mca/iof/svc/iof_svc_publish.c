#include "ompi_config.h"
#include "util/output.h"
#include "mca/oob/oob.h"
#include "mca/iof/base/iof_base_header.h"
#include "iof_svc.h"
#include "iof_svc_proxy.h"
#include "iof_svc_publish.h"



static void orte_iof_svc_publish_construct(orte_iof_svc_publish_t* publish)
{
}


static void orte_iof_svc_publish_destruct(orte_iof_svc_publish_t* publish)
{
}


OBJ_CLASS_INSTANCE(
    orte_iof_svc_publish_t,
    ompi_list_item_t,
    orte_iof_svc_publish_construct,
    orte_iof_svc_publish_destruct);


/**
 *
 */
                                                                                                           
int orte_iof_svc_publish_create(
    const orte_process_name_t *pub_name,
    const orte_process_name_t *pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag)
{
    orte_iof_svc_publish_t* pub = OBJ_NEW(orte_iof_svc_publish_t);
    pub->pub_name = *pub_name;
    pub->pub_proxy = *pub_proxy;
    pub->pub_mask = pub_mask;
    pub->pub_tag = pub_tag;
    pub->pub_endpoint = orte_iof_base_endpoint_match(pub_name,pub_mask,pub_tag);
    ompi_list_append(&mca_iof_svc_component.svc_published, &pub->super);
    return OMPI_SUCCESS;
}

                                                                                                           
/**
 *
 */
                                                                                                           
int orte_iof_svc_publish_delete(
    const orte_process_name_t *pub_name,
    const orte_process_name_t *pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag)
{
    return OMPI_SUCCESS;
}
                                                                                                           



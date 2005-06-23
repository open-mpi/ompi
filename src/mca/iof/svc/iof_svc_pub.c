#include "ompi_config.h"
#include "util/output.h"
#include "mca/oob/oob.h"
#include "mca/iof/base/iof_base_header.h"
#include "iof_svc.h"
#include "iof_svc_proxy.h"
#include "iof_svc_pub.h"
#include "iof_svc_sub.h"



static void orte_iof_svc_pub_construct(orte_iof_svc_pub_t* publish)
{
}


static void orte_iof_svc_pub_destruct(orte_iof_svc_pub_t* publish)
{
}


OBJ_CLASS_INSTANCE(
    orte_iof_svc_pub_t,
    ompi_list_item_t,
    orte_iof_svc_pub_construct,
    orte_iof_svc_pub_destruct);


/**
 *  (1) Create an entry to represent the published endpoint 
 *  (2) Lookup any subscriptions that match and install on the
 *      subscription as a destination endpoint.
 */
                                                                                                           
int orte_iof_svc_pub_create(
    const orte_process_name_t *pub_name,
    const orte_process_name_t *pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag)
{
    orte_iof_svc_pub_t* pub;
    ompi_list_item_t* item;

    OMPI_THREAD_LOCK(&mca_iof_svc_component.svc_lock);

    /* has this endpoint already been published */
    for(item  = ompi_list_get_first(&mca_iof_svc_component.svc_published);
        item != ompi_list_get_end(&mca_iof_svc_component.svc_published);
        item =  ompi_list_get_next(item)) {
        pub = (orte_iof_svc_pub_t*)item;
        if(orte_ns.compare(pub_mask,pub_name,&pub->pub_name) == 0 &&
           orte_ns.compare(ORTE_NS_CMP_ALL,pub_proxy,&pub->pub_proxy) == 0 &&
           pub_tag == pub->pub_tag) {
           OMPI_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
           return OMPI_SUCCESS;
        }
    }

    /* create a new entry for this endponit */
    pub = OBJ_NEW(orte_iof_svc_pub_t);
    pub->pub_name = *pub_name;
    pub->pub_proxy = *pub_proxy;
    pub->pub_mask = pub_mask;
    pub->pub_tag = pub_tag;
    pub->pub_endpoint = orte_iof_base_endpoint_match(pub_name,pub_mask,pub_tag);

    /* append this published endpoint to any matching subscription */
    for(item  = ompi_list_get_first(&mca_iof_svc_component.svc_subscribed);
        item != ompi_list_get_end(&mca_iof_svc_component.svc_subscribed);
        item =  ompi_list_get_next(item)) {
        orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)item;
        if(orte_iof_svc_fwd_match(sub,pub)) {
            orte_iof_svc_fwd_create(sub,pub);
        }
    }

    /* append this published endpoint to the global list */
    ompi_list_append(&mca_iof_svc_component.svc_published, &pub->super);
    OMPI_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
    return ORTE_SUCCESS;
}

                                                                                                           
/**
 *  Look for a matching endpoint.
 */

orte_iof_svc_pub_t* orte_iof_svc_pub_lookup(
    const orte_process_name_t *pub_name,
    const orte_process_name_t *pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag)
{
    ompi_list_item_t* item;
    for(item  = ompi_list_get_first(&mca_iof_svc_component.svc_published);
        item != ompi_list_get_end(&mca_iof_svc_component.svc_published);
        item =  ompi_list_get_next(item)) {
        orte_iof_svc_pub_t* pub = (orte_iof_svc_pub_t*)item;
        if (orte_ns.compare(ORTE_NS_CMP_ALL, &pub->pub_name,pub_name) == 0 &&
            orte_ns.compare(ORTE_NS_CMP_ALL, &pub->pub_proxy,pub_proxy) == 0 &&
            pub->pub_mask == pub_mask &&
            pub->pub_tag == pub_tag) {
            return pub;
        }
    }
    return NULL;
}
    
/**
 * Remove the published endpoint and cleanup any associated
 * forwarding entries.
 */
                                                                                                           
int orte_iof_svc_pub_delete(
    const orte_process_name_t *pub_name,
    const orte_process_name_t *pub_proxy,
    orte_ns_cmp_bitmask_t pub_mask,
    orte_iof_base_tag_t pub_tag)
{
    ompi_list_item_t* item;
    orte_iof_svc_pub_t* pub;

    OMPI_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    pub = orte_iof_svc_pub_lookup(pub_name,pub_proxy,pub_mask,pub_tag);
    if(NULL == pub) {
        OMPI_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
        return ORTE_ERR_NOT_FOUND;
    }

    for(item  = ompi_list_get_first(&mca_iof_svc_component.svc_subscribed);
        item != ompi_list_get_end(&mca_iof_svc_component.svc_subscribed);
        item =  ompi_list_get_next(item)) {
        orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)item;
        if(orte_iof_svc_fwd_match(sub,pub)) {
            orte_iof_svc_fwd_delete(sub,pub);
        }
    }
    ompi_list_remove_item(&mca_iof_svc_component.svc_published, &pub->super);
    OBJ_RELEASE(pub);
    OMPI_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
    return ORTE_SUCCESS;
}
                                                                                                           



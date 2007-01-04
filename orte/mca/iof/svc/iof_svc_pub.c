#include "orte_config.h"
#include "opal/util/output.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/iof/base/iof_base_header.h"
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
    opal_list_item_t,
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
    opal_list_item_t* item;

    OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);

    /* has this endpoint already been published */
    for(item  = opal_list_get_first(&mca_iof_svc_component.svc_published);
        item != opal_list_get_end(&mca_iof_svc_component.svc_published);
        item =  opal_list_get_next(item)) {
        pub = (orte_iof_svc_pub_t*)item;
        if(orte_ns.compare_fields(pub_mask,pub_name,&pub->pub_name) == 0 &&
           orte_ns.compare_fields(ORTE_NS_CMP_ALL,pub_proxy,&pub->pub_proxy) == 0 &&
           pub_tag == pub->pub_tag) {
           OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
           return ORTE_SUCCESS;
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
    for(item  = opal_list_get_first(&mca_iof_svc_component.svc_subscribed);
        item != opal_list_get_end(&mca_iof_svc_component.svc_subscribed);
        item =  opal_list_get_next(item)) {
        orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)item;
        if(orte_iof_svc_fwd_match(sub,pub)) {
            orte_iof_svc_fwd_create(sub,pub);
        }
    }

    /* append this published endpoint to the global list */
    opal_list_append(&mca_iof_svc_component.svc_published, &pub->super);
    OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
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
    opal_list_item_t* item;
    for(item  = opal_list_get_first(&mca_iof_svc_component.svc_published);
        item != opal_list_get_end(&mca_iof_svc_component.svc_published);
        item =  opal_list_get_next(item)) {
        orte_iof_svc_pub_t* pub = (orte_iof_svc_pub_t*)item;
        if (orte_ns.compare_fields(ORTE_NS_CMP_ALL, &pub->pub_name,pub_name) == 0 &&
            orte_ns.compare_fields(ORTE_NS_CMP_ALL, &pub->pub_proxy,pub_proxy) == 0 &&
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
    opal_list_item_t* item;
    orte_iof_svc_pub_t* pub;

    OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    pub = orte_iof_svc_pub_lookup(pub_name,pub_proxy,pub_mask,pub_tag);
    if(NULL == pub) {
        OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
        return ORTE_ERR_NOT_FOUND;
    }

    for(item  = opal_list_get_first(&mca_iof_svc_component.svc_subscribed);
        item != opal_list_get_end(&mca_iof_svc_component.svc_subscribed);
        item =  opal_list_get_next(item)) {
        orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)item;
        if(orte_iof_svc_fwd_match(sub,pub)) {
            orte_iof_svc_fwd_delete(sub,pub);
        }
    }
    opal_list_remove_item(&mca_iof_svc_component.svc_published, &pub->super);
    OBJ_RELEASE(pub);
    OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
    return ORTE_SUCCESS;
}
                                                                                                           

/*
 * Remove all publications associated w/ the given process name.
 */

void orte_iof_svc_pub_delete_all(
    const orte_process_name_t* name)
{
    opal_list_item_t* p_item;

    OPAL_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    p_item  = opal_list_get_first(&mca_iof_svc_component.svc_published);
    while(p_item != opal_list_get_end(&mca_iof_svc_component.svc_published)) {
        opal_list_item_t* p_next = opal_list_get_next(p_item);
        orte_iof_svc_pub_t* pub = (orte_iof_svc_pub_t*)p_item;

        if (orte_ns.compare_fields(ORTE_NS_CMP_ALL, &pub->pub_name,name) == 0 ||
            orte_ns.compare_fields(ORTE_NS_CMP_ALL, &pub->pub_proxy,name) == 0) {

            opal_list_item_t* s_item;
            for(s_item  = opal_list_get_first(&mca_iof_svc_component.svc_subscribed);
                s_item != opal_list_get_end(&mca_iof_svc_component.svc_subscribed);
                s_item =  opal_list_get_next(s_item)) {
                orte_iof_svc_sub_t* sub = (orte_iof_svc_sub_t*)s_item;
                if(orte_iof_svc_fwd_match(sub,pub)) {
                    orte_iof_svc_fwd_delete(sub,pub);
                }
            }
            opal_list_remove_item(&mca_iof_svc_component.svc_published, p_item);
            OBJ_RELEASE(pub);
        }
        p_item = p_next;
    }
    OPAL_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
}


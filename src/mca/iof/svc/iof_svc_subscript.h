#ifndef MCA_IOF_SVC_SUBSCRIPT_H
#define MCA_IOF_SVC_SUBSCRIPT_H


/**
 * A subscription maps data from a specified set
 * of source endpoints to one or more destination(s).
 */

struct orte_iof_svc_subscript_t {
    ompi_list_item_t super;
    orte_process_name_t           src_name;
    orte_ns_cmp_bitmask_t         src_mask;
    orte_iof_base_tag_t           src_tag;
    orte_process_name_t           dst_name;
    orte_ns_cmp_bitmask_t         dst_mask;
    orte_iof_base_tag_t           dst_tag;
    orte_iof_base_endpoint_t*     sub_endpoint;
};
typedef struct orte_iof_svc_subscript_t orte_iof_svc_subscript_t;

OBJ_CLASS_DECLARATION(orte_iof_svc_subscript_t);


/**
 *
 */

orte_iof_svc_subscript_t* orte_iof_svc_subscript_lookup(
    const orte_process_name_t* src
);

/**
 * 
 */

int orte_iof_svc_subscript_create(
    const orte_process_name_t *src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    const orte_process_name_t *dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag);

/**
 * 
 */

int orte_iof_svc_subscript_delete(
    const orte_process_name_t *src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    const orte_process_name_t *dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag);

/**
 *
 */

int orte_iof_svc_subscript_forward(
    orte_iof_svc_subscript_t* subscript,
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr,
    const unsigned char* data);

#endif


#ifndef MCA_IOF_SVC_SUBSCRIPT_H
#define MCA_IOF_SVC_SUBSCRIPT_H


/**
 *
 */

struct mca_iof_svc_subscript_t {
    ompi_list_item_t super;
    ompi_process_name_t          src_name;
    ompi_ns_cmp_bitmask_t        src_mask;
    mca_iof_base_tag_t           src_tag;
    ompi_process_name_t          dst_name;
    ompi_ns_cmp_bitmask_t        dst_mask;
    mca_iof_base_tag_t           dst_tag;
    mca_iof_base_endpoint_t*     sub_endpoint;
};
typedef struct mca_iof_svc_subscript_t mca_iof_svc_subscript_t;

OBJ_CLASS_DECLARATION(mca_iof_svc_subscript_t);


/**
 *
 */

mca_iof_svc_subscript_t* mca_iof_svc_subscript_lookup(
    const ompi_process_name_t* src
);

/**
 * 
 */

int mca_iof_svc_subscript_create(
    const ompi_process_name_t *src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    const ompi_process_name_t *dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    mca_iof_base_tag_t dst_tag);

/**
 * 
 */

int mca_iof_svc_subscript_delete(
    const ompi_process_name_t *src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    mca_iof_base_tag_t src_tag,
    const ompi_process_name_t *dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    mca_iof_base_tag_t dst_tag);

/**
 *
 */

int mca_iof_svc_subscript_forward(
    mca_iof_svc_subscript_t* subscript,
    const ompi_process_name_t* src,
    mca_iof_base_msg_header_t* hdr,
    const unsigned char* data);

#endif


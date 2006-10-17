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

#ifndef MCA_IOF_SVC_SUBSCRIPT_H
#define MCA_IOF_SVC_SUBSCRIPT_H

/**
 * A subscription routes data from a specified set
 * of source endpoints to one or more destination
 * endpoints.
 */

#include "opal/class/opal_hash_table.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct orte_iof_svc_fwd_t {
    opal_list_item_t super;
    orte_iof_svc_pub_t* fwd_pub;
    opal_hash_table_t fwd_seq;
};
typedef struct orte_iof_svc_fwd_t orte_iof_svc_fwd_t;


OBJ_CLASS_DECLARATION(orte_iof_svc_fwd_t);

struct orte_iof_svc_sub_t {
    opal_list_item_t super;
    orte_process_name_t           src_name;
    orte_ns_cmp_bitmask_t         src_mask;
    orte_iof_base_tag_t           src_tag;
    orte_process_name_t           dst_name;
    orte_ns_cmp_bitmask_t         dst_mask;
    orte_iof_base_tag_t           dst_tag;
    orte_iof_base_endpoint_t*     sub_endpoint;
    opal_list_t                   sub_forward;
};
typedef struct orte_iof_svc_sub_t orte_iof_svc_sub_t;

OBJ_CLASS_DECLARATION(orte_iof_svc_sub_t);


/**
 *  Lookup an existing subscription.
 */

orte_iof_svc_sub_t* orte_iof_svc_sub_lookup(
    const orte_process_name_t* src
);

/**
 *  Create a subscription
 */

int orte_iof_svc_sub_create(
    const orte_process_name_t *src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    const orte_process_name_t *dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag);

/**
 * Cleanup/remove a subscription
 */

int orte_iof_svc_sub_delete(
    const orte_process_name_t *src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    const orte_process_name_t *dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag);

int orte_iof_svc_sub_delete_all(
    const orte_process_name_t *dst_name);

/**
 * Forward message to any endpoints that
 * match the subscription.
 */

int orte_iof_svc_sub_forward(
    orte_iof_svc_sub_t* sub,
    const orte_process_name_t* src,
    orte_iof_base_msg_header_t* hdr,
    const unsigned char* data,
    bool *forward);

/**
 * Check to see if the published endpoint matches
 * the subscription.
 */

bool orte_iof_svc_fwd_match(
    orte_iof_svc_sub_t* sub,
    orte_iof_svc_pub_t* pub);

/**
 * Create or remove a forwarding entry on the 
 * current subscription.
 */

int orte_iof_svc_fwd_create(
    orte_iof_svc_sub_t* sub,
    orte_iof_svc_pub_t* pub);

int orte_iof_svc_fwd_delete(
    orte_iof_svc_sub_t* sub,
    orte_iof_svc_pub_t* pub);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif


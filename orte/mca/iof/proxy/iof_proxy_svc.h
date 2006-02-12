/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
/**
 * @file
 */
#ifndef MCA_IOF_PROXY_SVC_H
#define MCA_IOF_PROXY_SVC_H

#include "orte_config.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/ns/ns.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 *  Publish endpoint to forwarding service.
 */

int orte_iof_proxy_svc_publish(
    const orte_process_name_t* name,
    int tag
    );

int orte_iof_proxy_svc_unpublish(
    const orte_process_name_t* name,
    orte_ns_cmp_bitmask_t mask,
    int tag
    );

/**
 * Subscribe one/more destination processes as
 * specified by the process name/mask to one/more
 * source processes.
 */

int orte_iof_proxy_svc_subscribe(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    int src_tag,
    const orte_process_name_t* dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    int dst_tag
    );

/**
 * Remove subscription from forwarding
 * service.
 */

int orte_iof_proxy_svc_unsubscribe(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    int src_tag,
    const orte_process_name_t* dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    int dst_tag
    );

/**
 * Dispatch messages received from forwarding
 * service.
 */

void orte_iof_proxy_svc_recv(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    void* cbdata);


#if defined(c_plusplus) || defined(__cplusplus)
};
#endif
#endif


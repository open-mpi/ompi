/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
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

#include "ompi_config.h"
#include "mca/iof/iof.h"
#include "mca/ns/ns.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * 
 */

int mca_iof_proxy_svc_publish(
    const ompi_process_name_t* name,
    int tag
    );

int mca_iof_proxy_svc_unpublish(
    const ompi_process_name_t* name,
    ompi_ns_cmp_bitmask_t mask,
    int tag
    );

/**
 * 
 */

int mca_iof_proxy_svc_subscribe(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    int src_tag,
    const ompi_process_name_t* dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    int dst_tag
    );

/**
 * 
 */

int mca_iof_proxy_svc_unsubscribe(
    const ompi_process_name_t* src_name,
    ompi_ns_cmp_bitmask_t src_mask,
    int src_tag,
    const ompi_process_name_t* dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    int dst_tag
    );

/**
 * 
 */

void mca_iof_proxy_svc_recv(
    int status,
    ompi_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    void* cbdata);


#if defined(c_plusplus) || defined(__cplusplus)
};
#endif
#endif


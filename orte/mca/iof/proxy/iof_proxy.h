/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.   All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * The proxy IOF component is used in non-HNP processes.  It is used
 * to proxy all IOF actions back to the "svc" IOF component (i.e., the
 * IOF component that runs in the HNP).  The proxy IOF component is
 * typically loaded in an orted and then tied to the stdin, stdout,
 * and stderr streams of created child processes via pipes.  The proxy
 * IOF component in the orted then acts as the delay between the
 * stdin/stdout/stderr pipes and the svc IOF component in the HNP.
 * This design allows us to manipulate stdin/stdout/stderr from before
 * main() in the child process.
 *
 * Publish actions for SINKs are pushed back to the svc/HNP.  Publish
 * actions for SOURCEs are not pushed back to SINKs because all data
 * fragments from SOURCEs are automatically sent back to the svc/HNP.
 * 
 * All unpublish actions are pushed back to the svc/HNP (I'm not sure
 * why -- perhaps this is a bug?).
 *
 * Push and pull actions are essentially implemented in terms of
 * subscribe / unsubscribe.
 *
 * Subscribe / unsubscribe actions are fairly straightforward.
 *
 * Much of the intelligence of this component is actually contained in
 * iof_base_endpoint.c (reading and writing to local file descriptors,
 * setting up events based on file descriptors, etc.).  
 *
 * A non-blocking OOB receive is posted at the initializtion of this
 * component to receive all messages from the svc/HNP (e.g., data
 * fragments from streams, ACKs to fragments).
 *
 * Flow control is employed on a per-stream basis to ensure that
 * SOURCEs don't overwhelm SINK resources (E.g., send an entire input
 * file to an orted before the target process has read any of it).
 *
 * Important: this component is designed to work with the svc IOF
 * component only.  If we ever do a different IOF implementation
 * scheme, it is likely that only some of this component will be
 * useful for cannibalisation (if any at all).
 */
#ifndef ORTE_IOF_PROXY_H
#define ORTE_IOF_PROXY_H

#include "orte/mca/iof/iof.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Module publish
 */

int orte_iof_proxy_publish(
    const orte_process_name_t* name,
    orte_iof_base_mode_t mode,
    orte_iof_base_tag_t tag,
    int fd
);

/**
 * Module unpublish
 */

int orte_iof_proxy_unpublish(
    const orte_process_name_t* name,
    orte_ns_cmp_bitmask_t mask,
    orte_iof_base_tag_t tag
);

/**
 * Module push
 */

int orte_iof_proxy_push(
    const orte_process_name_t* dst_name,
    orte_ns_cmp_bitmask_t dst_mask,
    orte_iof_base_tag_t dst_tag,
    int fd
);

/**
 * Module pull
 */

int orte_iof_proxy_pull(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    int fd
);

/**
 * Module subscribe
 */

int orte_iof_proxy_subscribe(
    const orte_process_name_t* src_name,  
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag,
    orte_iof_base_callback_fn_t cb,
    void* cbdata
);

/**
 * Module unsubscribe
 */

int orte_iof_proxy_unsubscribe(
    const orte_process_name_t* src_name,
    orte_ns_cmp_bitmask_t src_mask,
    orte_iof_base_tag_t src_tag
);

/**
 * Module finalize
 */

int orte_iof_proxy_finalize( void );

/**
 * IOF proxy Component 
 */
struct orte_iof_proxy_component_t {
    orte_iof_base_component_t super;
    struct iovec proxy_iov[1];
};
typedef struct orte_iof_proxy_component_t orte_iof_proxy_component_t;

ORTE_MODULE_DECLSPEC extern orte_iof_proxy_component_t mca_iof_proxy_component;
extern orte_iof_base_module_t orte_iof_proxy_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

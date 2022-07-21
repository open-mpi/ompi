/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2019 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* This header contains macros to help minimize usnic BTL differences
 * between v1.7/v1.8, v1.9/v2.0, and v2.0/v2.1. */

#ifndef BTL_USNIC_COMPAT_H
#define BTL_USNIC_COMPAT_H

#include "opal/mca/btl/btl.h"
#include "opal/mca/rcache/rcache.h"

/************************************************************************/

/* OMPI_ERROR_LOG and friends */
#include "opal/util/error.h"

/* PMIX / modex stuff */
#include "opal/mca/pmix/pmix-internal.h"

/* Proc stuff */
#include "opal/util/proc.h"

/* Free lists are unified into OPAL free lists */
#include "opal/class/opal_free_list.h"

/* Include the progress thread stuff */
#include "opal/runtime/opal_progress_threads.h"

#define USNIC_OUT opal_btl_base_framework.framework_output
/* JMS Really want to be able to get the job size somehow...  But for
   now, so that we can compile, just set it to a constant :-( */
#define USNIC_MCW_SIZE                  2
#define proc_bound()                    (NULL != opal_process_info.cpuset ? 1 : 0)
#define USNIC_BTL_DEFAULT_VERSION(name) MCA_BTL_DEFAULT_VERSION(name)

#define USNIC_SEND_LOCAL        des_segments
#define USNIC_SEND_LOCAL_COUNT  des_segment_count
#define USNIC_SEND_REMOTE       des_segments
#define USNIC_SEND_REMOTE_COUNT des_segment_count

#define USNIC_RECV_LOCAL        des_segments
#define USNIC_RECV_LOCAL_COUNT  des_segment_count
#define USNIC_RECV_REMOTE       des_segments
#define USNIC_RECV_REMOTE_COUNT des_segment_count

#define USNIC_PUT_LOCAL        des_segments
#define USNIC_PUT_LOCAL_COUNT  des_segment_count
#define USNIC_PUT_REMOTE       des_segments
#define USNIC_PUT_REMOTE_COUNT des_segments_count

#define USNIC_COMPAT_FREE_LIST_GET(list, item)    (item) = opal_free_list_get((list))
#define USNIC_COMPAT_FREE_LIST_RETURN(list, item) opal_free_list_return((list), (item))

#define usnic_compat_free_list_init opal_free_list_init

/*
 * Performance critical; needs to be inline
 */
static inline int usnic_compat_proc_name_compare(opal_process_name_t a, opal_process_name_t b)
{
    return (bool) (a.jobid == b.jobid && a.vpid == b.vpid);
}

/************************************************************************/

/* Forward declare to avoid #include ordering complications */
struct opal_btl_usnic_modex_t;

void usnic_compat_modex_send(int *rc, mca_base_component_t *component,
                             struct opal_btl_usnic_modex_t *modexes, size_t size);

void usnic_compat_modex_recv(int *rc, mca_base_component_t *component, opal_proc_t *proc,
                             struct opal_btl_usnic_modex_t **modexes, size_t *size);

uint64_t usnic_compat_rte_hash_name(opal_process_name_t *pname);
const char *usnic_compat_proc_name_print(opal_process_name_t *pname);

/************************************************************************/

struct mca_btl_base_module_t;
struct mca_btl_base_endpoint_t;

struct mca_btl_base_descriptor_t *
opal_btl_usnic_prepare_src(struct mca_btl_base_module_t *base_module,
                           struct mca_btl_base_endpoint_t *endpoint,
                           struct opal_convertor_t *convertor, uint8_t order, size_t reserve,
                           size_t *size, uint32_t flags);

int opal_btl_usnic_put(struct mca_btl_base_module_t *base_module,
                       struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                       uint64_t remote_address,
                       struct mca_btl_base_registration_handle_t *local_handle,
                       struct mca_btl_base_registration_handle_t *remote_handle, size_t size,
                       int flags, int order, mca_btl_base_rdma_completion_fn_t cbfunc,
                       void *cbcontext, void *cbdata);

#endif /* BTL_USNIC_COMPAT_H */

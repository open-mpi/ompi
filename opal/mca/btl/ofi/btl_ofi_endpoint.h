/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2017-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Intel, Inc, All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_OFI_ENDPOINT_H
#define MCA_BTL_OFI_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/mca/event/event.h"

#include "btl_ofi.h"

BEGIN_C_DECLS

#if OPAL_HAVE_THREAD_LOCAL
extern opal_thread_local  mca_btl_ofi_context_t *my_context;
#endif /* OPAL_HAVE_THREAD_LOCAL */

struct mca_btl_base_endpoint_t {
    opal_list_item_t super;

    struct fid_ep *ofi_endpoint;
    fi_addr_t peer_addr;

    /** endpoint proc */
    opal_proc_t *ep_proc;

    /** mutex to protect this structure */
    opal_mutex_t ep_lock;
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t mca_btl_ofi_endpoint_t;
OBJ_CLASS_DECLARATION(mca_btl_ofi_endpoint_t);

int init_context_freelists(mca_btl_ofi_context_t *context);

mca_btl_base_endpoint_t *mca_btl_ofi_endpoint_create (opal_proc_t *proc, struct fid_ep *ep);

/* contexts */
mca_btl_ofi_context_t *mca_btl_ofi_context_alloc_scalable(struct fi_info *info,
                                                          struct fid_domain *domain,
                                                          struct fid_ep *sep,
                                                          struct fid_av *av,
                                                          size_t num_contexts);

mca_btl_ofi_context_t *mca_btl_ofi_context_alloc_normal(struct fi_info *info,
                                                        struct fid_domain *domain,
                                                        struct fid_ep *ep,
                                                        struct fid_av *av);
void mca_btl_ofi_context_finalize(mca_btl_ofi_context_t *context, bool scalable_ep);

mca_btl_ofi_context_t *get_ofi_context(mca_btl_ofi_module_t *btl);
mca_btl_ofi_context_t *get_ofi_context_rr(mca_btl_ofi_module_t *btl);

END_C_DECLS
#endif

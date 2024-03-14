/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Intel, Inc, All rights reserved
 *
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * Copyright (c) 2022-2023 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/class/opal_bitmap.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/mpool.h"
#include "opal/util/printf.h"
#include <string.h>

#include "btl_ofi.h"
#include "btl_ofi_endpoint.h"
#include "btl_ofi_frag.h"

static int mca_btl_ofi_add_procs(mca_btl_base_module_t *btl, size_t nprocs,
                                 opal_proc_t **opal_procs, mca_btl_base_endpoint_t **peers,
                                 opal_bitmap_t *reachable)
{
    int rc;
    int count;
    char *ep_name = NULL;
    size_t namelen = mca_btl_ofi_component.namelen;

    opal_proc_t *proc;
    mca_btl_base_endpoint_t *ep;

    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;

    for (size_t i = 0; i < nprocs; ++i) {

        proc = opal_procs[i];

        /* See if we already have an endpoint for this proc. */
        rc = opal_hash_table_get_value_uint64(&ofi_btl->id_to_endpoint, (intptr_t) proc,
                                              (void **) &ep);

        if (OPAL_SUCCESS == rc) {
            BTL_VERBOSE(
                ("returning existing endpoint for proc %s", OPAL_NAME_PRINT(proc->proc_name)));
            peers[i] = ep;

        } else {
            /* We don't have this endpoint yet, create one */
            peers[i] = mca_btl_ofi_endpoint_create(proc, ofi_btl->ofi_endpoint);
            BTL_VERBOSE(("creating peer %p", (void *) peers[i]));

            if (OPAL_UNLIKELY(NULL == peers[i])) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }

            /* Add this endpoint to the lookup table */
            (void) opal_hash_table_set_value_uint64(&ofi_btl->id_to_endpoint, (intptr_t) proc,
                                                    (void **) &ep);
        }

        OPAL_MODEX_RECV(rc, &mca_btl_ofi_component.super.btl_version, &peers[i]->ep_proc->proc_name,
                        (void **) &ep_name, &namelen);
        if (OPAL_SUCCESS != rc) {
            BTL_ERROR(("error receiving modex"));
            MCA_BTL_OFI_ABORT();
        }

        /* get peer fi_addr */
        count = fi_av_insert(ofi_btl->av,          /* Address vector to insert */
                             ep_name,              /* peer name */
                             1,                    /* amount to insert */
                             &peers[i]->peer_addr, /* return peer address here */
                             0,                    /* flags */
                             NULL);                /* context */

        /* if succeed, add this proc and mark reachable */
        if (count == 1) { /* we inserted 1 address. */
            opal_list_append(&ofi_btl->endpoints, &peers[i]->super);
            opal_bitmap_set_bit(reachable, i);
        } else {
            BTL_VERBOSE(("fi_av_insert failed with rc = %d", count));
            MCA_BTL_OFI_ABORT();
        }
    }

    return OPAL_SUCCESS;
}

static int mca_btl_ofi_del_procs(mca_btl_base_module_t *btl, size_t nprocs, opal_proc_t **procs,
                                 mca_btl_base_endpoint_t **peers)
{
    int rc;
    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;
    mca_btl_base_endpoint_t *ep;

    for (size_t i = 0; i < nprocs; ++i) {
        if (peers[i]) {
            rc = opal_hash_table_get_value_uint64(&ofi_btl->id_to_endpoint, (intptr_t) procs[i],
                                                  (void **) &ep);

            if (OPAL_SUCCESS == rc) {
                /* remove the address from AV. */
                rc = fi_av_remove(ofi_btl->av, &peers[i]->peer_addr, 1, 0);
                if (rc < 0) {
                    /* remove failed. this should not happen. */
                    /* Lets not crash because we failed to remove an address. */
                    BTL_ERROR(("fi_av_remove failed with error %d:%s", rc, fi_strerror(-rc)));
                }

                /* remove and free MPI endpoint from the list. */
                opal_list_remove_item(&ofi_btl->endpoints, &peers[i]->super);
                (void) opal_hash_table_remove_value_uint64(&ofi_btl->id_to_endpoint,
                                                           (intptr_t) procs[i]);
                OBJ_RELEASE(peers[i]);
            }
        }
    }

    return OPAL_SUCCESS;
}

void mca_btl_ofi_rcache_init(mca_btl_ofi_module_t *module)
{
    if (!module->initialized) {
        mca_rcache_base_resources_t rcache_resources;
        char *tmp;

        (void) opal_asprintf(&tmp, "ofi.%s", module->linux_device_name);

        rcache_resources.cache_name = tmp;
        rcache_resources.reg_data = (void *) module;
        rcache_resources.sizeof_reg = sizeof(mca_btl_ofi_reg_t);
        rcache_resources.register_mem = mca_btl_ofi_reg_mem;
        rcache_resources.deregister_mem = mca_btl_ofi_dereg_mem;

        module->rcache = mca_rcache_base_module_create("grdma", module, &rcache_resources);
        free(tmp);

        if (NULL == module->rcache) {
            /* something when horribly wrong */
            BTL_ERROR(("cannot create rcache"));
            MCA_BTL_OFI_ABORT();
        }

        module->initialized = true;
    }
}

/**
 * @brief Register a memory region for put/get/atomic operations.
 *
 * @param btl (IN)         BTL module
 * @param endpoint(IN)     BTL addressing information (or NULL for all endpoints)
 * @param base (IN)        Pointer to start of region
 * @param size (IN)        Size of region
 * @param flags (IN)       Flags indicating what memory access level is requested.
 *                         See opal/mca/rcache/rcache.h for valid access flags.
 *
 * @returns a memory registration handle valid for both local and remote operations
 * @returns NULL if the region could not be registered
 *
 * This function registers the specified region with the hardware for use with
 * the btl_put, btl_get, btl_atomic_cas, btl_atomic_op, and btl_atomic_fop
 * functions. Care should be taken to not hold an excessive number of registrations
 * as they may use limited system/NIC resources.
 */
static struct mca_btl_base_registration_handle_t *
mca_btl_ofi_register_mem(struct mca_btl_base_module_t *btl,
                         struct mca_btl_base_endpoint_t *endpoint, void *base, size_t size,
                         uint32_t flags)
{
    mca_btl_ofi_module_t *ofi_module = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_reg_t *reg;
    int access_flags = flags & MCA_BTL_REG_FLAG_ACCESS_ANY;
    int rc;
    uint32_t cache_flags = 0;
    if (ofi_module->bypass_cache) {
	   cache_flags |= MCA_RCACHE_FLAGS_CACHE_BYPASS;
    }

    rc = ofi_module->rcache->rcache_register(ofi_module->rcache, base, size, cache_flags, access_flags,
                                             (mca_rcache_base_registration_t **) &reg);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return NULL;
    }

    return &reg->handle;
}

/**
 * @brief Deregister a memory region
 *
 * @param btl (IN)         BTL module region was registered with
 * @param handle (IN)      BTL registration handle to deregister
 *
 * This function deregisters the memory region associated with the specified handle. Care
 * should be taken to not perform any RDMA or atomic operation on this memory region
 * after it is deregistered. It is erroneous to specify a memory handle associated with
 * a remote node.
 */
static int mca_btl_ofi_deregister_mem(mca_btl_base_module_t *btl,
                                      mca_btl_base_registration_handle_t *handle)
{
    mca_btl_ofi_module_t *ofi_module = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_reg_t *reg = (mca_btl_ofi_reg_t *) ((intptr_t) handle
                                                    - offsetof(mca_btl_ofi_reg_t, handle));

    (void) ofi_module->rcache->rcache_deregister(ofi_module->rcache, &reg->base);

    return OPAL_SUCCESS;
}

int mca_btl_ofi_reg_mem(void *reg_data, void *base, size_t size,
                        mca_rcache_base_registration_t *reg)
{
    int rc, dev_id;
    uint64_t flags;
    static uint64_t access_flags = FI_REMOTE_WRITE | FI_REMOTE_READ | FI_READ | FI_WRITE;
    struct fi_mr_attr attr = {0};
    struct iovec iov = {0};

    mca_btl_ofi_module_t *btl = (mca_btl_ofi_module_t *) reg_data;
    mca_btl_ofi_reg_t *ur = (mca_btl_ofi_reg_t *) reg;

    iov.iov_base = base;
    iov.iov_len = size;
    attr.mr_iov = &iov;
    attr.iov_count = 1;
    attr.access = access_flags;
    attr.offset = 0;
    attr.context = NULL;
    attr.requested_key = (uint64_t) reg;

#if  OPAL_OFI_HAVE_FI_MR_IFACE
    if (OPAL_LIKELY(NULL != base)) {
        rc = opal_accelerator.check_addr(base, &dev_id, &flags);
        if (rc < 0) {
            return rc;
        } else if (rc > 0 ) {
            if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "cuda")) {
                attr.iface = FI_HMEM_CUDA;
                opal_accelerator.get_device(&attr.device.cuda);
#if OPAL_OFI_HAVE_FI_HMEM_ROCR
	    } else if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "rocm")) {
                attr.iface = FI_HMEM_ROCR;
                opal_accelerator.get_device(&attr.device.cuda);
#endif
#if OPAL_OFI_HAVE_FI_HMEM_ZE
            } else if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "ze")) {
                attr.iface = FI_HMEM_ZE;
                opal_accelerator.get_device(&attr.device.ze);
#endif
            } else {
                return OPAL_ERROR;
            }
        }
    }
#endif

    rc = fi_mr_regattr(btl->domain, &attr, 0, &ur->ur_mr);
    if (0 != rc) {
        ur->ur_mr = NULL;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (btl->use_fi_mr_bind) {
        BTL_VERBOSE(("binding mr to endpoint"));
        rc = fi_mr_bind(ur->ur_mr, &btl->ofi_endpoint->fid, 0ULL);
        if (FI_SUCCESS != rc) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        rc = fi_mr_enable(ur->ur_mr);
        if (FI_SUCCESS != rc) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }

    ur->handle.rkey = fi_mr_key(ur->ur_mr);
    ur->handle.desc = fi_mr_desc(ur->ur_mr);

    /* In case the provider doesn't support FI_MR_VIRT_ADDR,
     * we need to reference the remote address by the distance from base registered
     * address. We keep this information to use in rdma/atomic operations. */
    if (btl->use_virt_addr) {
        ur->handle.base_addr = 0;
    } else {
        ur->handle.base_addr = base;
    }

    return OPAL_SUCCESS;
}

int mca_btl_ofi_dereg_mem(void *reg_data, mca_rcache_base_registration_t *reg)
{
    mca_btl_ofi_reg_t *ur = (mca_btl_ofi_reg_t *) reg;

    if (ur->ur_mr != NULL) {
        if (0 != fi_close(&ur->ur_mr->fid)) {
            BTL_ERROR(("%s: error unpinning memory mr=%p: %s", __func__, (void *) ur->ur_mr,
                       strerror(errno)));
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}

/*
 * Cleanup/release module resources.
 */

int mca_btl_ofi_finalize(mca_btl_base_module_t *btl)
{
    int i;
    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_endpoint_t *endpoint, *next;

    assert(btl);

    /* clear the rcache */
    if (ofi_btl->rcache) {
        mca_rcache_base_module_destroy(ofi_btl->rcache);
        ofi_btl->rcache = NULL;
    }

    /* Close basic ep before closing its attached resources. */
    if (NULL != ofi_btl->ofi_endpoint && !ofi_btl->is_scalable_ep) {
        fi_close(&ofi_btl->ofi_endpoint->fid);
        ofi_btl->ofi_endpoint = NULL;
    }

    /* loop over all the contexts */
    for (i = 0; i < ofi_btl->num_contexts; i++) {
        mca_btl_ofi_context_finalize(&ofi_btl->contexts[i], ofi_btl->is_scalable_ep);
    }
    free(ofi_btl->contexts);

    if (NULL != ofi_btl->ofi_endpoint) {
        fi_close(&ofi_btl->ofi_endpoint->fid);
    }

    /* close ep before closing av */
    if (NULL != ofi_btl->av) {
        fi_close(&ofi_btl->av->fid);
    }

    if (NULL != ofi_btl->domain) {
        fi_close(&ofi_btl->domain->fid);
    }

    if (NULL != ofi_btl->fabric) {
        fi_close(&ofi_btl->fabric->fid);
    }

    if (NULL != ofi_btl->fabric_info) {
        fi_freeinfo(ofi_btl->fabric_info);
    }

    /* clean up any leftover endpoints */
    OPAL_LIST_FOREACH_SAFE (endpoint, next, &ofi_btl->endpoints, mca_btl_ofi_endpoint_t) {
        opal_list_remove_item(&ofi_btl->endpoints, &endpoint->super);
        OBJ_RELEASE(endpoint);
    }

    OBJ_DESTRUCT(&ofi_btl->endpoints);
    OBJ_DESTRUCT(&ofi_btl->id_to_endpoint);
    OBJ_DESTRUCT(&ofi_btl->module_lock);

    free(btl);

    return OPAL_SUCCESS;
}

/* Post wildcard recvs on the rx context. */
int mca_btl_ofi_post_recvs(mca_btl_base_module_t *module, mca_btl_ofi_context_t *context, int count)
{
    int i;
    int rc;
    mca_btl_ofi_base_frag_t *frag;
    mca_btl_ofi_frag_completion_t *comp;

    for (i = 0; i < count; i++) {
        frag = (mca_btl_ofi_base_frag_t *) mca_btl_ofi_alloc(module, NULL, 0, MCA_BTL_OFI_FRAG_SIZE,
                                                             MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if (NULL == frag) {
            BTL_ERROR(("cannot allocate recv frag."));
            return OPAL_ERROR;
        }

        comp = mca_btl_ofi_frag_completion_alloc(module, context, frag, MCA_BTL_OFI_TYPE_RECV);

        rc = fi_recv(context->rx_ctx, &frag->hdr, MCA_BTL_OFI_RECV_SIZE, NULL, FI_ADDR_UNSPEC,
                     &comp->comp_ctx);

        if (FI_SUCCESS != rc) {
            BTL_ERROR(("cannot post recvs"));
            return OPAL_ERROR;
        }
    }
    return OPAL_SUCCESS;
}

/* Allocate and fill out the module capabilities according to operation mode. */
mca_btl_ofi_module_t *mca_btl_ofi_module_alloc(int mode)
{
    mca_btl_ofi_module_t *module;

    /* allocate module */
    module = (mca_btl_ofi_module_t *) calloc(1, sizeof(mca_btl_ofi_module_t));
    if (NULL == module) {
        return NULL;
    }

    /* fill in the defaults */
    *module = mca_btl_ofi_module_template;

    if (mode == MCA_BTL_OFI_MODE_ONE_SIDED || mode == MCA_BTL_OFI_MODE_FULL_SUPPORT) {

        module->super.btl_put = mca_btl_ofi_put;
        module->super.btl_get = mca_btl_ofi_get;
        module->super.btl_atomic_op = mca_btl_ofi_aop;
        module->super.btl_atomic_fop = mca_btl_ofi_afop;
        module->super.btl_atomic_cswap = mca_btl_ofi_acswap;
        module->super.btl_flush = mca_btl_ofi_flush;

        module->super.btl_register_mem = mca_btl_ofi_register_mem;
        module->super.btl_deregister_mem = mca_btl_ofi_deregister_mem;

        /* btl/ofi support remote completion because it required FI_DELIVERY_COMPLETE capability
         */
        module->super.btl_flags |= MCA_BTL_FLAGS_ATOMIC_FOPS | MCA_BTL_FLAGS_ATOMIC_OPS
                                   | MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_RDMA_REMOTE_COMPLETION;

        module->super.btl_atomic_flags = MCA_BTL_ATOMIC_SUPPORTS_ADD | MCA_BTL_ATOMIC_SUPPORTS_SWAP
                                         | MCA_BTL_ATOMIC_SUPPORTS_CSWAP
                                         | MCA_BTL_ATOMIC_SUPPORTS_32BIT;

        module->super.btl_put_limit = 1 << 23;
        module->super.btl_put_alignment = 0;

        module->super.btl_get_limit = 1 << 23;
        module->super.btl_get_alignment = 0;

        module->super.btl_registration_handle_size = sizeof(mca_btl_base_registration_handle_t);
    }

    if (mode == MCA_BTL_OFI_MODE_TWO_SIDED || mode == MCA_BTL_OFI_MODE_FULL_SUPPORT) {

        module->super.btl_alloc = mca_btl_ofi_alloc;
        module->super.btl_free = mca_btl_ofi_free;
        module->super.btl_prepare_src = mca_btl_ofi_prepare_src;

        module->super.btl_send = mca_btl_ofi_send;

        module->super.btl_flags |= MCA_BTL_FLAGS_SEND;
        module->super.btl_eager_limit = MCA_BTL_OFI_FRAG_SIZE;
        module->super.btl_max_send_size = MCA_BTL_OFI_FRAG_SIZE;
        module->super.btl_rndv_eager_limit = MCA_BTL_OFI_FRAG_SIZE;

        /* If two sided is enabled, we expected that the user knows exactly what
         * they want. We bump the priority to maximum, making this BTL the default. */
        module->super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;
    }

    if (mode == MCA_BTL_OFI_MODE_FULL_SUPPORT) {
        module->super.btl_rdma_pipeline_frag_size = 4 * 1024 * 1024;
        module->super.btl_rdma_pipeline_send_length = 8 * 1024;
    }

    return module;
}

mca_btl_ofi_module_t mca_btl_ofi_module_template = {
    .super = {
        .btl_component = &mca_btl_ofi_component.super,
        .btl_add_procs = mca_btl_ofi_add_procs,
        .btl_del_procs = mca_btl_ofi_del_procs,
        .btl_finalize = mca_btl_ofi_finalize,
    }};

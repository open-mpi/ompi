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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <string.h>
#include "opal/class/opal_bitmap.h"
#include "opal/mca/btl/btl.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/mpool.h"

#include "btl_ofi.h"
#include "btl_ofi_endpoint.h"

static int mca_btl_ofi_add_procs (mca_btl_base_module_t *btl,
                                  size_t nprocs, opal_proc_t **opal_procs,
                                  mca_btl_base_endpoint_t **peers,
                                  opal_bitmap_t *reachable)
{
    int rc;
    int count;
    char *ep_name = NULL;
    size_t namelen = mca_btl_ofi_component.namelen;

    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;

    for (size_t i = 0 ; i < nprocs ; ++i) {
        peers[i] = mca_btl_ofi_endpoint_create (opal_procs[i], ofi_btl->ofi_endpoint);
        if (OPAL_UNLIKELY(NULL == peers[i])) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        OPAL_MODEX_RECV(rc, &mca_btl_ofi_component.super.btl_version,
                        &peers[i]->ep_proc->proc_name, (void **)&ep_name, &namelen);
        if (OPAL_SUCCESS != rc) {
            BTL_ERROR(("error receiving modex"));
            MCA_BTL_OFI_ABORT();
        }

        /* get peer fi_addr */
        count = fi_av_insert(ofi_btl->av,      /* Address vector to insert */
                             ep_name,          /* peer name */
                             1,                /* amount to insert */
                             &peers[i]->peer_addr, /* return peer address here */
                             0,                /* flags */
                             NULL);            /* context */

        /* if succeed, add this proc and mark reachable */
        if (count == 1) { /* we inserted 1 address. */
            opal_list_append (&ofi_btl->endpoints, &peers[i]->super);
            opal_bitmap_set_bit(reachable, i);
        } else {
            BTL_VERBOSE(("fi_av_insert failed with rc = %d", count));
            MCA_BTL_OFI_ABORT();
        }
    }

    return OPAL_SUCCESS;
}

static int mca_btl_ofi_del_procs (mca_btl_base_module_t *btl, size_t nprocs,
                                  opal_proc_t **procs, mca_btl_base_endpoint_t **peers)
{
    int ret;
    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;

    for (size_t i = 0 ; i < nprocs ; ++i) {
        if (peers[i]) {

            /* remove the address from AV. */
            ret = fi_av_remove(ofi_btl->av, &peers[i]->peer_addr, 1, 0);
            if (ret < 0) {
                /* remove failed. this should not happen. */
                /* Lets not crash because we failed to remove an address. */
                BTL_ERROR(("fi_av_remove failed with error %d:%s",
                                ret, fi_strerror(-ret)));
            }

            /* remove and free MPI endpoint from the list. */
            opal_list_remove_item (&ofi_btl->endpoints, &peers[i]->super);
            OBJ_RELEASE(peers[i]);
        }
    }

    return OPAL_SUCCESS;
}

void mca_btl_ofi_rcache_init (mca_btl_ofi_module_t *module)
{
    if (!module->initialized) {
        mca_rcache_base_resources_t rcache_resources;
        char *tmp;

        (void) asprintf (&tmp, "ofi.%s", module->linux_device_name);

        rcache_resources.cache_name     = tmp;
        rcache_resources.reg_data       = (void *) module;
        rcache_resources.sizeof_reg     = sizeof (mca_btl_ofi_reg_t);
        rcache_resources.register_mem   = mca_btl_ofi_reg_mem;
        rcache_resources.deregister_mem = mca_btl_ofi_dereg_mem;

        module->rcache = mca_rcache_base_module_create ("grdma", module, &rcache_resources);
        free (tmp);

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
 * @param flags (IN)       Flags indicating what operation will be performed. Valid
 *                         values are MCA_BTL_DES_FLAGS_PUT, MCA_BTL_DES_FLAGS_GET,
 *                         and MCA_BTL_DES_FLAGS_ATOMIC
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
mca_btl_ofi_register_mem (struct mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *base,
                          size_t size, uint32_t flags)
{
    mca_btl_ofi_module_t *ofi_module = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_reg_t *reg;
    int access_flags = flags & MCA_BTL_REG_FLAG_ACCESS_ANY;
    int rc;

    rc = ofi_module->rcache->rcache_register (ofi_module->rcache, base, size, 0, access_flags,
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
static int mca_btl_ofi_deregister_mem (mca_btl_base_module_t *btl, mca_btl_base_registration_handle_t *handle)
{
    mca_btl_ofi_module_t *ofi_module = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_reg_t *reg =
        (mca_btl_ofi_reg_t *)((intptr_t) handle - offsetof (mca_btl_ofi_reg_t, handle));

    (void) ofi_module->rcache->rcache_deregister (ofi_module->rcache, &reg->base);

    return OPAL_SUCCESS;
}

int mca_btl_ofi_reg_mem (void *reg_data, void *base, size_t size, mca_rcache_base_registration_t *reg)
{
    int rc;
    static uint64_t access_flags = FI_REMOTE_WRITE | FI_REMOTE_READ | FI_READ | FI_WRITE;

    mca_btl_ofi_module_t *btl = (mca_btl_ofi_module_t*) reg_data;
    mca_btl_ofi_reg_t *ur = (mca_btl_ofi_reg_t*) reg;

    rc = fi_mr_reg(btl->domain, base, size, access_flags, 0,
                   (uint64_t) reg, 0, &ur->ur_mr, NULL);
    if (0 != rc) {
        return OPAL_ERR_OUT_OF_RESOURCE;
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

int mca_btl_ofi_dereg_mem (void *reg_data, mca_rcache_base_registration_t *reg)
{
    mca_btl_ofi_reg_t *ur = (mca_btl_ofi_reg_t*)reg;

    if (ur->ur_mr != NULL) {
        if (0 != fi_close(&ur->ur_mr->fid)) {
            BTL_ERROR(("%s: error unpinning memory mr=%p: %s",
                       __func__, (void*) ur->ur_mr, strerror(errno)));
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}

/*
 * Cleanup/release module resources.
 */

int mca_btl_ofi_finalize (mca_btl_base_module_t* btl)
{
    int i;
    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_endpoint_t *endpoint, *next;

    assert(btl);

    /* loop over all the contexts */
    for (i=0; i < ofi_btl->num_contexts; i++) {
        mca_btl_ofi_context_finalize(&ofi_btl->contexts[i], ofi_btl->is_scalable_ep);
    }
    free(ofi_btl->contexts);

    if (NULL != ofi_btl->av) {
        fi_close(&ofi_btl->av->fid);
    }

    if (NULL != ofi_btl->ofi_endpoint) {
        fi_close(&ofi_btl->ofi_endpoint->fid);
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
    OPAL_LIST_FOREACH_SAFE(endpoint, next, &ofi_btl->endpoints, mca_btl_ofi_endpoint_t) {
        opal_list_remove_item (&ofi_btl->endpoints, &endpoint->super);
        OBJ_RELEASE(endpoint);
    }

    OBJ_DESTRUCT(&ofi_btl->endpoints);

    if (ofi_btl->rcache) {
        mca_rcache_base_module_destroy (ofi_btl->rcache);
    }

    free (btl);

    return OPAL_SUCCESS;
}

mca_btl_ofi_module_t mca_btl_ofi_module_template = {
    .super = {
        /* initialize functions. this btl only support RDMA and atomics
         * for now so it does not provide prepare_src, alloc, free, or send */
        .btl_component      = &mca_btl_ofi_component.super,
        .btl_add_procs      = mca_btl_ofi_add_procs,
        .btl_del_procs      = mca_btl_ofi_del_procs,
        .btl_finalize       = mca_btl_ofi_finalize,
        .btl_put            = mca_btl_ofi_put,
        .btl_get            = mca_btl_ofi_get,
        .btl_register_mem   = mca_btl_ofi_register_mem,
        .btl_deregister_mem = mca_btl_ofi_deregister_mem,
        .btl_atomic_op      = mca_btl_ofi_aop,
        .btl_atomic_fop     = mca_btl_ofi_afop,
        .btl_atomic_cswap   = mca_btl_ofi_acswap,
        .btl_flush          = mca_btl_ofi_flush,

        /* set the default flags for this btl. ofi provides us with rdma and both
         * fetching and non-fetching atomics (though limited to add and cswap) */
        .btl_flags          = MCA_BTL_FLAGS_RDMA |
                              MCA_BTL_FLAGS_ATOMIC_FOPS |
                              MCA_BTL_FLAGS_ATOMIC_OPS,

        .btl_atomic_flags   = MCA_BTL_ATOMIC_SUPPORTS_ADD  |
                              MCA_BTL_ATOMIC_SUPPORTS_SWAP |
                              MCA_BTL_ATOMIC_SUPPORTS_CSWAP |
                              MCA_BTL_ATOMIC_SUPPORTS_32BIT,

        /* set the default limits on put and get */
        .btl_registration_handle_size = sizeof(mca_btl_base_registration_handle_t),
        .btl_put_limit      = 1 << 23,
        .btl_put_alignment  = 0,
        .btl_get_limit      = 1 << 23,
        .btl_get_alignment  = 0,
   }
};

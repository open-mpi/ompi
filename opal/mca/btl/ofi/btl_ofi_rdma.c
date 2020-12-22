/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
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

#include "btl_ofi_rdma.h"

OBJ_CLASS_INSTANCE(mca_btl_ofi_rdma_completion_t,
                   opal_free_list_item_t,
                   NULL,
                   NULL);

mca_btl_ofi_rdma_completion_t *mca_btl_ofi_rdma_completion_alloc (
                                         mca_btl_base_module_t *btl,
                                         mca_btl_base_endpoint_t *endpoint,
                                         mca_btl_ofi_context_t *ofi_context,
                                         void *local_address,
                                         mca_btl_base_registration_handle_t *local_handle,
                                         mca_btl_base_rdma_completion_fn_t cbfunc,
                                         void *cbcontext, void *cbdata,
                                         int type)
{
    assert(btl);
    assert(endpoint);
    assert(ofi_context);

    mca_btl_ofi_rdma_completion_t *comp;

    comp = (mca_btl_ofi_rdma_completion_t*) opal_free_list_get(&ofi_context->rdma_comp_list);
    assert(comp);

    comp->base.btl = btl;
    comp->base.endpoint = endpoint;
    comp->base.my_context = ofi_context;
    comp->base.my_list = &ofi_context->rdma_comp_list;
    comp->base.type = type;

    comp->local_address = local_address;
    comp->local_handle = local_handle;
    comp->cbfunc = cbfunc;
    comp->cbcontext = cbcontext;
    comp->cbdata = cbdata;

    comp->comp_ctx.comp = comp;

    return comp;
}

int mca_btl_ofi_get (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{

    int rc;
    mca_btl_ofi_rdma_completion_t *comp;

    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_endpoint_t *btl_endpoint = (mca_btl_ofi_endpoint_t*) endpoint;
    mca_btl_ofi_context_t *ofi_context;

    ofi_context = get_ofi_context(ofi_btl);

    /* create completion context */
    comp = mca_btl_ofi_rdma_completion_alloc(btl, endpoint,
                                             ofi_context,
                                             local_address,
                                             local_handle,
                                             cbfunc, cbcontext, cbdata,
                                             MCA_BTL_OFI_TYPE_GET);

    remote_address = (remote_address - (uint64_t) remote_handle->base_addr);

    /* Remote write data across the wire */
    rc = fi_read(ofi_context->tx_ctx,
                local_address, size,   /* payload */
                local_handle->desc,
                btl_endpoint->peer_addr,
                remote_address, remote_handle->rkey,
                &comp->comp_ctx);       /* completion context */

    if (-FI_EAGAIN == rc) {
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t*) comp);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (0 != rc) {
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t*) comp);
        BTL_ERROR(("fi_read failed with %d:%s", rc, fi_strerror(-rc)));
        MCA_BTL_OFI_ABORT();
    }

    MCA_BTL_OFI_NUM_RDMA_INC(ofi_btl);

    return OPAL_SUCCESS;
}

int mca_btl_ofi_put (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, void *local_address,
                      uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                      mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                      int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    int rc;
    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;
    mca_btl_ofi_endpoint_t *btl_endpoint = (mca_btl_ofi_endpoint_t*) endpoint;
    mca_btl_ofi_context_t *ofi_context;

    ofi_context = get_ofi_context(ofi_btl);

    /* create completion context */
    mca_btl_ofi_rdma_completion_t *comp;
    comp = mca_btl_ofi_rdma_completion_alloc(btl, endpoint,
                                             ofi_context,
                                             local_address,
                                             local_handle,
                                             cbfunc, cbcontext, cbdata,
                                             MCA_BTL_OFI_TYPE_PUT);

    remote_address = (remote_address - (uint64_t) remote_handle->base_addr);

    /* Remote write data across the wire */
    rc = fi_write(ofi_context->tx_ctx,
                  local_address, size,   /* payload */
                  local_handle->desc,
                  btl_endpoint->peer_addr,
                  remote_address, remote_handle->rkey,
                  &comp->comp_ctx);       /* completion context */

    if (-FI_EAGAIN == rc) {
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t*) comp);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (0 != rc) {
        opal_free_list_return(comp->base.my_list, (opal_free_list_item_t*) comp);
        BTL_ERROR(("fi_write failed with %d:%s", rc, fi_strerror(-rc)));
        MCA_BTL_OFI_ABORT();
    }

    MCA_BTL_OFI_NUM_RDMA_INC(ofi_btl);

    return OPAL_SUCCESS;

}

int mca_btl_ofi_flush (mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_ofi_module_t *ofi_btl = (mca_btl_ofi_module_t *) btl;

    while(ofi_btl->outstanding_rdma > 0) {
        (void) mca_btl_ofi_component.super.btl_progress();
    }

    return OPAL_SUCCESS;
}

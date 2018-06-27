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

#include "btl_ofi.h"
#include "btl_ofi_endpoint.h"
#include "opal/util/proc.h"

#if OPAL_HAVE_THREAD_LOCAL
opal_thread_local mca_btl_ofi_context_t *my_context = NULL;
#endif /* OPAL_HAVE_THREAD_LOCAL */

static void mca_btl_ofi_endpoint_construct (mca_btl_ofi_endpoint_t *endpoint)
{
    endpoint->peer_addr = 0;
    OBJ_CONSTRUCT(&endpoint->ep_lock, opal_mutex_t);
}

static void mca_btl_ofi_endpoint_destruct (mca_btl_ofi_endpoint_t *endpoint)
{
    endpoint->peer_addr = 0;

    /* set to null, we will free ofi endpoint in module */
    endpoint->ofi_endpoint = NULL;

    OBJ_DESTRUCT(&endpoint->ep_lock);
}

OBJ_CLASS_INSTANCE(mca_btl_ofi_endpoint_t, opal_list_item_t,
                   mca_btl_ofi_endpoint_construct,
                   mca_btl_ofi_endpoint_destruct);

mca_btl_base_endpoint_t *mca_btl_ofi_endpoint_create (opal_proc_t *proc, struct fid_ep *ep)
{
    mca_btl_ofi_endpoint_t *endpoint = OBJ_NEW(mca_btl_ofi_endpoint_t);

    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return NULL;
    }

    endpoint->ep_proc = proc;
    endpoint->ofi_endpoint = ep;

    return (mca_btl_base_endpoint_t *) endpoint;
}

int ofi_comp_list_init(opal_free_list_t *comp_list)
{
    int rc;
    OBJ_CONSTRUCT(comp_list, opal_free_list_t);
    rc = opal_free_list_init(comp_list,
                             sizeof(mca_btl_ofi_completion_t),
                             opal_cache_line_size,
                             OBJ_CLASS(mca_btl_ofi_completion_t),
                             0,
                             0,
                             128,
                             -1,
                             128,
                             NULL,
                             0,
                             NULL,
                             NULL,
                             NULL);
    if (rc != OPAL_SUCCESS) {
        BTL_VERBOSE(("cannot allocate completion freelist"));
    }
    return rc;
}

/* mca_btl_ofi_context_alloc_normal()
 *
 * This function will allocate an ofi_context, map the endpoint to tx/rx context,
 * bind CQ,AV to the endpoint and initialize all the structure.
 * USE WITH NORMAL ENDPOINT ONLY */
mca_btl_ofi_context_t *mca_btl_ofi_context_alloc_normal(struct fi_info *info,
                                                        struct fid_domain *domain,
                                                        struct fid_ep *ep,
                                                        struct fid_av *av)
{
    int rc;
    uint32_t cq_flags = FI_TRANSMIT;
    char *linux_device_name = info->domain_attr->name;

    struct fi_cq_attr cq_attr = {0};

    mca_btl_ofi_context_t *context;

    context = (mca_btl_ofi_context_t*) calloc(1, sizeof(*context));
    if (NULL == context) {
        BTL_VERBOSE(("cannot allocate context"));
        return NULL;
    }

    /* Don't really need to check, just avoiding compiler warning because
     * BTL_VERBOSE is a no op in performance build and the compiler will
     * complain about unused variable. */
    if (NULL == linux_device_name) {
        BTL_VERBOSE(("linux device name is NULL. This shouldn't happen."));
        goto single_fail;
    }

    cq_attr.format = FI_CQ_FORMAT_CONTEXT;
    cq_attr.wait_obj = FI_WAIT_NONE;
    rc = fi_cq_open(domain, &cq_attr, &context->cq, NULL);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_cq_open with err=%s",
                        linux_device_name,
                        fi_strerror(-rc)
                        ));
        goto single_fail;
    }

    rc = fi_ep_bind(ep, (fid_t)av, 0);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_ep_bind with err=%s",
                        linux_device_name,
                        fi_strerror(-rc)
                        ));
        goto single_fail;
    }

    rc = fi_ep_bind(ep, (fid_t)context->cq, cq_flags);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_scalable_ep_bind with err=%s",
                        linux_device_name,
                        fi_strerror(-rc)
                        ));
        goto single_fail;
    }

    rc = ofi_comp_list_init(&context->comp_list);
    if (rc != OPAL_SUCCESS) {
        goto single_fail;
    }

    context->tx_ctx = ep;
    context->rx_ctx = ep;
    context->context_id = 0;

    return context;

single_fail:
    mca_btl_ofi_context_finalize(context, false);
    return NULL;
}

/* mca_btl_ofi_context_alloc_scalable()
 *
 * This function allocate communication contexts and return the pointer
 * to the first btl context. It also take care of all the bindings needed.
 * USE WITH SCALABLE ENDPOINT ONLY */
mca_btl_ofi_context_t *mca_btl_ofi_context_alloc_scalable(struct fi_info *info,
                                                          struct fid_domain *domain,
                                                          struct fid_ep *sep,
                                                          struct fid_av *av,
                                                          size_t num_contexts)
{
    BTL_VERBOSE(("creating %zu contexts", num_contexts));

    int rc;
    size_t i;
    char *linux_device_name = info->domain_attr->name;

    struct fi_cq_attr cq_attr = {0};
    struct fi_tx_attr tx_attr = {0};
    struct fi_rx_attr rx_attr = {0};

    mca_btl_ofi_context_t *contexts;
    tx_attr.op_flags = FI_DELIVERY_COMPLETE;

    contexts = (mca_btl_ofi_context_t*) calloc(num_contexts, sizeof(*contexts));
    if (NULL == contexts) {
        BTL_VERBOSE(("cannot allocate communication contexts."));
        return NULL;
    }

    /* Don't really need to check, just avoiding compiler warning because
     * BTL_VERBOSE is a no op in performance build and the compiler will
     * complain about unused variable. */
    if (NULL == linux_device_name) {
        BTL_VERBOSE(("linux device name is NULL. This shouldn't happen."));
        goto scalable_fail;
    }

     /* bind AV to endpoint */
    rc = fi_scalable_ep_bind(sep, (fid_t)av, 0);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_scalable_ep_bind with err=%s",
                        linux_device_name,
                        fi_strerror(-rc)
                        ));
        goto scalable_fail;
    }

    for (i=0; i < num_contexts; i++) {
        rc = fi_tx_context(sep, i, &tx_attr, &contexts[i].tx_ctx, NULL);
        if (0 != rc) {
            BTL_VERBOSE(("%s failed fi_tx_context with err=%s",
                            linux_device_name,
                            fi_strerror(-rc)
                            ));
            goto scalable_fail;
        }

        /* We don't actually need a receiving context as we only do one-sided.
         * However, sockets provider will hang if we dont have one. It is
         * also nice to have equal number of tx/rx context. */
        rc = fi_rx_context(sep, i, &rx_attr, &contexts[i].rx_ctx, NULL);
        if (0 != rc) {
            BTL_VERBOSE(("%s failed fi_rx_context with err=%s",
                            linux_device_name,
                            fi_strerror(-rc)
                            ));
            goto scalable_fail;
        }

        /* create CQ */
        cq_attr.format = FI_CQ_FORMAT_CONTEXT;
        cq_attr.wait_obj = FI_WAIT_NONE;
        rc = fi_cq_open(domain, &cq_attr, &contexts[i].cq, NULL);
        if (0 != rc) {
            BTL_VERBOSE(("%s failed fi_cq_open with err=%s",
                            linux_device_name,
                            fi_strerror(-rc)
                            ));
            goto scalable_fail;
        }

        /* bind cq to transmit context */
        uint32_t cq_flags = (FI_TRANSMIT);
        rc = fi_ep_bind(contexts[i].tx_ctx, (fid_t)contexts[i].cq, cq_flags);
        if (0 != rc) {
            BTL_VERBOSE(("%s failed fi_ep_bind with err=%s",
                            linux_device_name,
                            fi_strerror(-rc)
                            ));
            goto scalable_fail;
        }

        /* enable the context. */
        rc = fi_enable(contexts[i].tx_ctx);
        if (0 != rc) {
            BTL_VERBOSE(("%s failed fi_enable with err=%s",
                            linux_device_name,
                            fi_strerror(-rc)
                            ));
            goto scalable_fail;
        }

        rc = fi_enable(contexts[i].rx_ctx);
        if (0 != rc) {
            BTL_VERBOSE(("%s failed fi_enable with err=%s",
                            linux_device_name,
                            fi_strerror(-rc)
                            ));
            goto scalable_fail;
        }

        /* initialize completion freelist. */
        rc = ofi_comp_list_init(&contexts[i].comp_list);
        if (rc != OPAL_SUCCESS) {
            goto scalable_fail;
        }

         /* assign the id */
        contexts[i].context_id = i;
    }

    return contexts;

scalable_fail:
    /* close and free */
    for(i=0; i < num_contexts; i++) {
        mca_btl_ofi_context_finalize(&contexts[i], true);
    }
    free(contexts);

    return NULL;
}

void mca_btl_ofi_context_finalize(mca_btl_ofi_context_t *context, bool scalable_ep) {

    /* if it is a scalable ep, we have to close all contexts. */
    if (scalable_ep) {
        if (NULL != context->tx_ctx) {
            fi_close(&context->tx_ctx->fid);
        }

        if (NULL != context->rx_ctx) {
            fi_close(&context->rx_ctx->fid);
        }
    }

    if( NULL != context->cq) {
        fi_close(&context->cq->fid);
    }

    /* Can we destruct the object that hasn't been constructed? */
    OBJ_DESTRUCT(&context->comp_list);
}

/* Get a context to use for communication.
 * If TLS is supported, it will use the cached endpoint.
 * If not, it will invoke the normal round-robin assignment. */
mca_btl_ofi_context_t *get_ofi_context(mca_btl_ofi_module_t *btl)
{
#if OPAL_HAVE_THREAD_LOCAL
    /* With TLS, we cache the context we use. */
    static volatile int64_t cur_num = 0;

    if (OPAL_UNLIKELY(my_context == NULL)) {
        OPAL_THREAD_LOCK(&btl->module_lock);

        my_context = &btl->contexts[cur_num];
        cur_num = (cur_num + 1) %btl->num_contexts;

        OPAL_THREAD_UNLOCK(&btl->module_lock);
    }

    assert (my_context);
    return my_context;
#else
    return get_ofi_context_rr(btl);
#endif
}

/* return the context in a round-robin. */
/* There is no need for atomics here as it might hurt the performance. */
mca_btl_ofi_context_t *get_ofi_context_rr(mca_btl_ofi_module_t *btl)
{
    static volatile uint64_t rr_num = 0;
    return &btl->contexts[rr_num++%btl->num_contexts];
}

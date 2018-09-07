/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * $COPYRIGHT$
 * Copyright (c) 2018      Intel Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ofi.h"
#include "btl_ofi_frag.h"
#include "btl_ofi_rdma.h"

#if OPAL_HAVE_THREAD_LOCAL
opal_thread_local mca_btl_ofi_context_t *my_context = NULL;
#endif /* OPAL_HAVE_THREAD_LOCAL */

int init_context_freelists(mca_btl_ofi_context_t *context)
{
    int rc;
    OBJ_CONSTRUCT(&context->rdma_comp_list, opal_free_list_t);
    rc = opal_free_list_init(&context->rdma_comp_list,
                             sizeof(mca_btl_ofi_rdma_completion_t),
                             opal_cache_line_size,
                             OBJ_CLASS(mca_btl_ofi_rdma_completion_t),
                             0,
                             0,
                             512,
                             -1,
                             512,
                             NULL,
                             0,
                             NULL,
                             NULL,
                             NULL);
    if (rc != OPAL_SUCCESS) {
        BTL_VERBOSE(("cannot allocate completion freelist"));
        return rc;
    }

    if (TWO_SIDED_ENABLED) {
        OBJ_CONSTRUCT(&context->frag_comp_list, opal_free_list_t);
        rc = opal_free_list_init(&context->frag_comp_list,
                                 sizeof(mca_btl_ofi_frag_completion_t),
                                 opal_cache_line_size,
                                 OBJ_CLASS(mca_btl_ofi_frag_completion_t),
                                 0,
                                 0,
                                 512,
                                 -1,
                                 512,
                                 NULL,
                                 0,
                                 NULL,
                                 NULL,
                                 NULL);
        if (rc != OPAL_SUCCESS) {
            BTL_VERBOSE(("cannot allocate completion freelist"));
            return rc;
        }

        /* Initialize frag pool */
        OBJ_CONSTRUCT(&context->frag_list, opal_free_list_t);
        rc = opal_free_list_init(&context->frag_list,
                                 sizeof(mca_btl_ofi_base_frag_t) +
                                    MCA_BTL_OFI_FRAG_SIZE,
                                 opal_cache_line_size,
                                 OBJ_CLASS(mca_btl_ofi_base_frag_t),
                                 0,
                                 0,
                                 1024,
                                 -1,
                                 1024,
                                 NULL,
                                 0,
                                 NULL,
                                 NULL,
                                 NULL);
        if (OPAL_SUCCESS != rc) {
            BTL_VERBOSE(("failed to init frag pool (free_list)"));
        }
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
    uint32_t cq_flags = FI_TRANSMIT | FI_SEND | FI_RECV;
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

    rc = init_context_freelists(context);
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
        rc = fi_ep_bind(contexts[i].tx_ctx, (fid_t)contexts[i].cq, FI_TRANSMIT);
        if (0 != rc) {
            BTL_VERBOSE(("%s failed fi_ep_bind with err=%s",
                            linux_device_name,
                            fi_strerror(-rc)
                            ));
            goto scalable_fail;
        }

        /* bind cq to receiving  context */
        if (TWO_SIDED_ENABLED) {
            rc = fi_ep_bind(contexts[i].rx_ctx, (fid_t)contexts[i].cq, FI_RECV);
            if (0 != rc) {
                BTL_VERBOSE(("%s failed fi_ep_bind with err=%s",
                                linux_device_name,
                                fi_strerror(-rc)
                                ));
                goto scalable_fail;
            }
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

        /* initialize freelists. */
        rc = init_context_freelists(&contexts[i]);
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
    OBJ_DESTRUCT(&context->rdma_comp_list);

    if (TWO_SIDED_ENABLED) {
        OBJ_DESTRUCT(&context->frag_comp_list);
        OBJ_DESTRUCT(&context->frag_list);
    }
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

int mca_btl_ofi_context_progress(mca_btl_ofi_context_t *context) {

    int ret = 0;
    int events_read;
    int events = 0;
    struct fi_cq_entry cq_entry[MCA_BTL_OFI_DEFAULT_MAX_CQE];
    struct fi_cq_err_entry cqerr = {0};

    mca_btl_ofi_completion_context_t *c_ctx;
    mca_btl_ofi_base_completion_t *comp;
    mca_btl_ofi_rdma_completion_t *rdma_comp;
    mca_btl_ofi_frag_completion_t *frag_comp;

    ret = fi_cq_read(context->cq, &cq_entry, mca_btl_ofi_component.num_cqe_read);

    if (0 < ret) {
        events_read = ret;
        for (int i = 0; i < events_read; i++) {
            if (NULL != cq_entry[i].op_context) {
                ++events;

                c_ctx = (mca_btl_ofi_completion_context_t*) cq_entry[i].op_context;

                /* We are casting to every type  here just for simplicity. */
                comp = (mca_btl_ofi_base_completion_t*) c_ctx->comp;
                frag_comp = (mca_btl_ofi_frag_completion_t*) c_ctx->comp;
                rdma_comp = (mca_btl_ofi_rdma_completion_t*) c_ctx->comp;

                switch (comp->type) {
                case MCA_BTL_OFI_TYPE_GET:
                case MCA_BTL_OFI_TYPE_PUT:
                case MCA_BTL_OFI_TYPE_AOP:
                case MCA_BTL_OFI_TYPE_AFOP:
                case MCA_BTL_OFI_TYPE_CSWAP:
                    /* call the callback */
                    if (rdma_comp->cbfunc) {
                        rdma_comp->cbfunc (comp->btl, comp->endpoint,
                                           rdma_comp->local_address, rdma_comp->local_handle,
                                           rdma_comp->cbcontext, rdma_comp->cbdata, OPAL_SUCCESS);
                    }

                    MCA_BTL_OFI_NUM_RDMA_DEC((mca_btl_ofi_module_t*) comp->btl);
                    break;

                case MCA_BTL_OFI_TYPE_RECV:
                    mca_btl_ofi_recv_frag((mca_btl_ofi_module_t*)  comp->btl,
                                          (mca_btl_ofi_endpoint_t*) comp->endpoint,
                                          context, frag_comp->frag);
                    break;

                case MCA_BTL_OFI_TYPE_SEND:
                    MCA_BTL_OFI_NUM_SEND_DEC((mca_btl_ofi_module_t*) comp->btl);
                    mca_btl_ofi_frag_complete(frag_comp->frag, OPAL_SUCCESS);
                    break;

                default:
                    /* catasthrophic */
                    BTL_ERROR(("unknown completion type"));
                    MCA_BTL_OFI_ABORT();
                }

                /* return the completion handler */
                opal_free_list_return(comp->my_list, (opal_free_list_item_t*) comp);
            }
        }
    } else if (OPAL_UNLIKELY(ret == -FI_EAVAIL)) {
        ret = fi_cq_readerr(context->cq, &cqerr, 0);

        /* cq readerr failed!? */
        if (0 > ret) {
            BTL_ERROR(("%s:%d: Error returned from fi_cq_readerr: %s(%d)",
                       __FILE__, __LINE__, fi_strerror(-ret), ret));
        } else {
            BTL_ERROR(("fi_cq_readerr: (provider err_code = %d)\n",
                       cqerr.prov_errno));
        }
        MCA_BTL_OFI_ABORT();
    }
#ifdef FI_EINTR
    /* sometimes, sockets provider complain about interupt. We do nothing. */
    else if (OPAL_UNLIKELY(ret == -FI_EINTR)) {

    }
#endif
    /* If the error is not FI_EAGAIN, report the error and abort. */
    else if (OPAL_UNLIKELY(ret != -FI_EAGAIN)) {
        BTL_ERROR(("fi_cq_read returned error %d:%s", ret, fi_strerror(-ret)));
        MCA_BTL_OFI_ABORT();
    }

    return events;
}



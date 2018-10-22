/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(BTL_UCT_DEVICE_CONTEXT_H)
#define BTL_UCT_DEVICE_CONTEXT_H

#include "btl_uct.h"
#include "btl_uct_rdma.h"
#include "btl_uct_frag.h"

/**
 * @brief Create a new device context for the given transport
 *
 * @param[in] module     btl uct module
 * @param[in] tl         btl uct tl pointer
 * @param[in] context_id identifier for this context (0..MCA_BTL_UCT_MAX_WORKERS-1)
 */
mca_btl_uct_device_context_t *mca_btl_uct_context_create (mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl, int context_id, bool enable_progress);

/**
 * @brief Destroy a device context and release all resources
 *
 * @param[in] context   btl uct device context
 *
 * This call frees a device context and all assoicated resources. It is not
 * valid to use the device context after this returns.
 */
void mca_btl_uct_context_destroy (mca_btl_uct_device_context_t *context);

static inline bool mca_btl_uct_context_trylock (mca_btl_uct_device_context_t *context)
{
    return OPAL_THREAD_TRYLOCK(&context->mutex);
}

static inline void mca_btl_uct_context_lock (mca_btl_uct_device_context_t *context)
{
    OPAL_THREAD_LOCK (&context->mutex);
}

static inline void mca_btl_uct_context_unlock (mca_btl_uct_device_context_t *context)
{
    OPAL_THREAD_UNLOCK (&context->mutex);
}

#define MCA_BTL_UCT_CONTEXT_SERIALIZE(context,code)     \
    do {                                                \
        mca_btl_uct_context_lock (context);             \
        code;                                           \
        mca_btl_uct_context_unlock(context);            \
    } while (0);

static inline int mca_btl_uct_get_context_index (void)
{
    static volatile uint32_t next_uct_index = 0;
    int context_id;

#if OPAL_C_HAVE__THREAD_LOCAL
    if (mca_btl_uct_component.bind_threads_to_contexts) {
        static _Thread_local int uct_index = -1;

        context_id = uct_index;
        if (OPAL_UNLIKELY(-1 == context_id)) {
            context_id = uct_index = opal_atomic_fetch_add_32 ((volatile int32_t *) &next_uct_index, 1) %
                mca_btl_uct_component.num_contexts_per_module;
        }
    } else {
#endif
        /* avoid using atomics in this. i doubt it improves performance to ensure atomicity on the next
         * index in this case. */
        context_id = next_uct_index++ % mca_btl_uct_component.num_contexts_per_module;
#if OPAL_C_HAVE__THREAD_LOCAL
    }
#endif

    return context_id;
}

static inline mca_btl_uct_device_context_t *
mca_btl_uct_module_get_tl_context_specific (mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl, int context_id)
{
    mca_btl_uct_device_context_t *context = tl->uct_dev_contexts[context_id];

    if (OPAL_UNLIKELY(NULL == context)) {
        OPAL_THREAD_LOCK(&module->lock);
        context = tl->uct_dev_contexts[context_id];
        if (OPAL_UNLIKELY(NULL == context)) {
            context = tl->uct_dev_contexts[context_id] = mca_btl_uct_context_create (module, tl, context_id, true);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    return context;
}

static inline mca_btl_uct_device_context_t *mca_btl_uct_module_get_rdma_context (mca_btl_uct_module_t *module)
{
    return mca_btl_uct_module_get_tl_context_specific (module, module->rdma_tl, mca_btl_uct_get_context_index ());
}

static inline mca_btl_uct_device_context_t *mca_btl_uct_module_get_rdma_context_specific (mca_btl_uct_module_t *module, int context_id)
{
    return mca_btl_uct_module_get_tl_context_specific (module, module->rdma_tl, context_id);
}

static inline mca_btl_uct_device_context_t *mca_btl_uct_module_get_am_context (mca_btl_uct_module_t *module)
{
    return mca_btl_uct_module_get_tl_context_specific (module, module->am_tl, mca_btl_uct_get_context_index ());
}

static inline void mca_btl_uct_device_handle_completions (mca_btl_uct_device_context_t *dev_context)
{
    mca_btl_uct_uct_completion_t *comp;

    while (NULL != (comp = (mca_btl_uct_uct_completion_t *) opal_fifo_pop (&dev_context->completion_fifo))) {
        int rc = UCS_OK == comp->status ? OPAL_SUCCESS : OPAL_ERROR;

        if (comp->frag) {
            /* reset the count */
            comp->uct_comp.count = 1;
            mca_btl_uct_frag_complete (comp->frag, rc);

            continue;
        }

        /* we may be calling the callback before remote completion. this is in violation of the
         * btl interface specification but should not hurt in non-ob1 use cases. if this ever
         * becomes a problem we can look at possible solutions. */
        comp->cbfunc (comp->btl, comp->endpoint, comp->local_address, comp->local_handle,
                      comp->cbcontext, comp->cbdata, rc);
        mca_btl_uct_uct_completion_release (comp);
    }
}

static inline int mca_btl_uct_context_progress (mca_btl_uct_device_context_t *context)
{
    int ret = 0;

    if (!context->uct_worker) {
        return 0;
    }

    if (!mca_btl_uct_context_trylock (context)) {
        ret = uct_worker_progress (context->uct_worker);
        mca_btl_uct_context_unlock (context);

        mca_btl_uct_device_handle_completions (context);
    }

    return ret;
}

#endif /* BTL_UCT_DEVICE_CONTEXT_H */

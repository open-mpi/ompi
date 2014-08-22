/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_scif.h"
#include "btl_scif_frag.h"

#include "opal/include/opal/align.h"
#include "opal/memoryhooks/memory.h"
#include "ompi/runtime/params.h"

#include "opal/mca/base/mca_base_pvar.h"

#include <scif.h>

static int btl_scif_component_register(void);
static int btl_scif_component_open(void);
static int btl_scif_component_close(void);
static mca_btl_base_module_t **mca_btl_scif_component_init(int *, bool, bool);
static int mca_btl_scif_component_progress(void);

mca_btl_scif_component_t mca_btl_scif_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        .btl_version = {
            MCA_BTL_BASE_VERSION_2_0_0,

            .mca_component_name = "scif",
            .mca_component_major_version = OMPI_MAJOR_VERSION,
            .mca_component_minor_version = OMPI_MINOR_VERSION,
            .mca_component_release_version = OMPI_RELEASE_VERSION,
            .mca_open_component = btl_scif_component_open,
            .mca_close_component = btl_scif_component_close,
            .mca_query_component = NULL,
            .mca_register_component_params = btl_scif_component_register,
        },
        .btl_data = {
            .param_field = MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        .btl_init = mca_btl_scif_component_init,
        .btl_progress = mca_btl_scif_component_progress,
    }
};

static int btl_scif_component_register(void)
{
    (void) mca_base_var_group_component_register(&mca_btl_scif_component.super.btl_version,
                                                 "SCIF byte transport layer");

    mca_btl_scif_component.scif_free_list_num = 8;
    (void) mca_base_component_var_register(&mca_btl_scif_component.super.btl_version,
                                           "free_list_num", "Initial fragment free list size",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_scif_component.scif_free_list_num);
    mca_btl_scif_component.scif_free_list_max = 16384;
    (void) mca_base_component_var_register(&mca_btl_scif_component.super.btl_version,
                                           "free_list_max", "Maximum fragment free list size",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_scif_component.scif_free_list_max);
    mca_btl_scif_component.scif_free_list_inc = 64;
    (void) mca_base_component_var_register(&mca_btl_scif_component.super.btl_version,
                                           "free_list_inc", "Fragment free list size increment",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_scif_component.scif_free_list_inc);

    mca_btl_scif_component.segment_size = 8 * 1024;
    (void) mca_base_component_var_register(&mca_btl_scif_component.super.btl_version,
                                           "segment_size", "Size of memory segment to "
                                           "allocate for each remote process (default: "
                                           "8k)", MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_scif_component.segment_size);

    mca_btl_scif_component.rma_use_cpu = false;
    (void) mca_base_component_var_register(&mca_btl_scif_component.super.btl_version,
                                           "rma_use_cpu", "Use CPU instead of DMA "
                                           "for RMA copies (default: false)", MCA_BASE_VAR_TYPE_BOOL,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_scif_component.rma_use_cpu);


    mca_btl_scif_component.rma_sync = true;
    (void) mca_base_component_var_register(&mca_btl_scif_component.super.btl_version,
                                           "rma_sync", "Use synchronous RMA instead of "
                                           "an RMA fence (default: true)", MCA_BASE_VAR_TYPE_BOOL,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_scif_component.rma_sync);

#if defined(SCIF_TIMING)
    mca_btl_scif_component.aquire_buffer_time = 0.0;
    (void) mca_base_component_pvar_register(&mca_btl_scif_component.super.btl_version,
                                            "aquire_buffer_time", "Aggregate time spent "
                                            "aquiring send buffers", OPAL_INFO_LVL_9,
                                            MCA_BASE_PVAR_CLASS_AGGREGATE, MCA_BASE_VAR_TYPE_DOUBLE,
                                            NULL, MCA_BASE_VAR_BIND_NO_OBJECT, MCA_BASE_PVAR_FLAG_READONLY |
                                            MCA_BASE_PVAR_FLAG_CONTINUOUS, NULL, NULL, NULL,
                                            &mca_btl_scif_component.aquire_buffer_time);

    mca_btl_scif_component.send_time = 0.0;
    (void) mca_base_component_pvar_register(&mca_btl_scif_component.super.btl_version,
                                            "send_time", "Aggregate time spent writing to "
                                            "send buffers", OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_AGGREGATE,
                                            MCA_BASE_VAR_TYPE_DOUBLE, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                            NULL, NULL, NULL, &mca_btl_scif_component.send_time);

    mca_btl_scif_component.sendi_time = 0.0;
    (void) mca_base_component_pvar_register(&mca_btl_scif_component.super.btl_version,
                                            "sendi_time", "Aggregate time spent writing to "
                                            "send buffers in sendi", OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_AGGREGATE,
                                            MCA_BASE_VAR_TYPE_DOUBLE, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                            NULL, NULL, NULL, &mca_btl_scif_component.sendi_time);

    mca_btl_scif_component.get_time = 0.0;
    (void) mca_base_component_pvar_register(&mca_btl_scif_component.super.btl_version,
                                            "get_time", "Aggregate time spent in DMA read (scif_readfrom)",
                                            OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_AGGREGATE,
                                            MCA_BASE_VAR_TYPE_DOUBLE, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                            NULL, NULL, NULL, &mca_btl_scif_component.get_time);

    mca_btl_scif_component.get_count = 0;
    (void) mca_base_component_pvar_register(&mca_btl_scif_component.super.btl_version,
                                            "get_count", "Number of times btl_scif_get was called",
                                            OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_COUNTER,
                                            MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                            NULL, NULL, NULL, &mca_btl_scif_component.get_count);

    mca_btl_scif_component.put_time = 0.0;
    (void) mca_base_component_pvar_register(&mca_btl_scif_component.super.btl_version,
                                            "put_time", "Aggregate time spent in DMA write (scif_writeto)",
                                            OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_AGGREGATE,
                                            MCA_BASE_VAR_TYPE_DOUBLE, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                            NULL, NULL, NULL, &mca_btl_scif_component.put_time);

    mca_btl_scif_component.put_count = 0;
    (void) mca_base_component_pvar_register(&mca_btl_scif_component.super.btl_version,
                                            "put_count", "Number of times btl_scif_put was called",
                                            OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_COUNTER,
                                            MCA_BASE_VAR_TYPE_UNSIGNED_LONG, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                            NULL, NULL, NULL, &mca_btl_scif_component.put_count);
#endif

    mca_btl_scif_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;
    mca_btl_scif_module.super.btl_eager_limit               = 1 * 1024;
    mca_btl_scif_module.super.btl_rndv_eager_limit          = 1 * 1024;
    mca_btl_scif_module.super.btl_rdma_pipeline_frag_size   = 4 * 1024 * 1024;
    mca_btl_scif_module.super.btl_max_send_size             = 1 * 1024;
    mca_btl_scif_module.super.btl_rdma_pipeline_send_length = 1 * 1024;

    /* threshold for put */
    mca_btl_scif_module.super.btl_min_rdma_pipeline_size    = 1 * 1024;

    mca_btl_scif_module.super.btl_flags = MCA_BTL_FLAGS_SEND |
        MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_SEND_INPLACE;

    mca_btl_scif_module.super.btl_seg_size = sizeof (mca_btl_scif_segment_t);

    mca_btl_scif_module.super.btl_bandwidth = 50000; /* Mbs */
    mca_btl_scif_module.super.btl_latency   = 2;     /* Microsecs */

    /* Call the BTL based to register its MCA params */
    mca_btl_base_param_register(&mca_btl_scif_component.super.btl_version,
                                &mca_btl_scif_module.super);

    return OMPI_SUCCESS;
}

static int btl_scif_component_open(void)
{
    return OMPI_SUCCESS;
}

static int btl_scif_component_close(void)
{
    return OMPI_SUCCESS;
}

static void mca_btl_scif_autoset_leave_pinned (void) {
    int value = opal_mem_hooks_support_level();

    if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) & value)) {
        /* Set leave pinned to 1 if leave pinned pipeline is not set */
        if (-1 == ompi_mpi_leave_pinned) {
            ompi_mpi_leave_pinned = !ompi_mpi_leave_pinned_pipeline;
        }
    } else {
        ompi_mpi_leave_pinned = 0;
        ompi_mpi_leave_pinned_pipeline = 0;
    }
}

static int mca_btl_scif_modex_send (void)
{
    mca_btl_scif_modex_t modex;
    memset(&modex, 0, sizeof(mca_btl_scif_modex_t));
    modex.port_id = mca_btl_scif_module.port_id;

    return ompi_modex_send (&mca_btl_scif_component.super.btl_version, &modex, sizeof (modex));
}


static mca_btl_base_module_t **mca_btl_scif_component_init (int *num_btl_modules,
                                                            bool enable_progress_threads,
                                                            bool enable_mpi_threads)
{
    struct mca_btl_base_module_t **base_modules;
    int rc;

    BTL_VERBOSE(("btl/scif initializing"));

    signal (SIGSEGV, SIG_DFL);

    /* we currently need the memory hooks to determine when
     * registrations are no longer valid. */
    mca_btl_scif_autoset_leave_pinned ();

    if (32768 < mca_btl_scif_module.super.btl_eager_limit) {
        mca_btl_scif_module.super.btl_eager_limit = 32768;
    }

    /* the segment should be large enough to hold at least one eager packet */
    if (4 * mca_btl_scif_module.super.btl_eager_limit > mca_btl_scif_component.segment_size) {
        mca_btl_scif_component.segment_size = 4 * mca_btl_scif_module.super.btl_eager_limit;
    }

    /* round up to a multiple of 4096 */
    mca_btl_scif_component.segment_size = (mca_btl_scif_component.segment_size + 0xfff) & ~0xfff;

    base_modules = (struct mca_btl_base_module_t **)
        calloc (1, sizeof (struct mca_btl_base_module_t *));
    if (OPAL_UNLIKELY(NULL == base_modules)) {
        BTL_ERROR(("Malloc failed : %s:%d", __FILE__, __LINE__));
        return NULL;
    }

    /* initialize the module */
    rc = mca_btl_scif_module_init ();
    if (OMPI_SUCCESS != rc) {
        BTL_VERBOSE(("btl/scif error initializing module"));
        free (base_modules);
        return NULL;
    }

    base_modules[0] = &mca_btl_scif_module.super;
    mca_btl_scif_module.exiting = false;

    rc = mca_btl_scif_modex_send ();
    if (OMPI_SUCCESS != rc) {
        BTL_VERBOSE(("btl/scif error sending modex"));
        free (base_modules);
        return NULL;
    }

    *num_btl_modules = 1;

    BTL_VERBOSE(("btl/scif done initializing modules"));

    return base_modules;
}

static int mca_btl_scif_progress_recvs (mca_btl_base_endpoint_t *ep)
{
    const mca_btl_active_message_callback_t *reg;
    unsigned int start = ep->recv_buffer.start;
    unsigned int end   = ep->recv_buffer.endp[0];
    mca_btl_scif_base_frag_t frag;
    mca_btl_scif_frag_hdr_t *hdr;
    /* changing this value does not appear to have a signifigant impact
     * on performance */
    int frags_per_loop = 5;

    if (end == start) {
        return 0;
    }

    end &= ~ (1 << 31);
    start &= ~ (1 << 31);

    /* force all prior reads to complete before continuing */
    opal_atomic_rmb ();

    do {
        hdr = (mca_btl_scif_frag_hdr_t *) (ep->recv_buffer.buffer + start);

        /* force all prior reads to complete before continuing */
        MB();

        BTL_VERBOSE(("got frag with header {.tag = %d, .size = %d} from offset %u",
                     hdr->tag, hdr->size, start));
#if defined(SCIF_USE_SEQ)
        if (hdr->seq != ep->seq_expected) {
            break;
        }

        ep->seq_expected++;
#endif

        /* message to skip the rest of the buffer */
        if (0xff != hdr->tag) {
            reg = mca_btl_base_active_message_trigger + hdr->tag;

            /* fragment fits entirely in the remaining buffer space. some
             * btl users do not handle fragmented data so we can't split
             * the fragment without introducing another copy here. this
             * limitation has not appeared to cause any performance
             * problems. */
            frag.base.des_dst_cnt = 1;
            frag.segments[0].base.seg_len = hdr->size;
            frag.segments[0].base.seg_addr.pval = (void *) (hdr + 1);

            frag.base.des_dst = &frag.segments[0].base;

            /* call the registered callback function */
            reg->cbfunc(&mca_btl_scif_module.super, hdr->tag, &frag.base, reg->cbdata);
        }

        start = (start + hdr->size + sizeof (*hdr) + 63) & ~63;

        /* skip unusable space at the end of the buffer */
        if (mca_btl_scif_component.segment_size == start) {
            start = 64;
            ep->recv_buffer.start = ((ep->recv_buffer.start & (1 << 31)) ^ (1 << 31)) | 64;
        } else {
            ep->recv_buffer.start = (ep->recv_buffer.start & (1 << 31)) | start;
        }
    } while (start != end && --frags_per_loop);

    /* let the sender know where we stopped */
    ep->recv_buffer.startp[0] = ep->recv_buffer.start;

    /* return the number of fragments processed */
    return 5 - frags_per_loop;
}

static int mca_btl_scif_progress_sends (mca_btl_base_endpoint_t *ep)
{
    /* try sending any wait listed fragments */
    if (OPAL_UNLIKELY(0 != opal_list_get_size (&ep->frag_wait_list))) {
        return mca_btl_scif_progress_send_wait_list (ep);
    }

    return 0;
}

static int mca_btl_scif_component_progress (void)
{
    unsigned int i;
    int count = 0;

    /* progress all connected endpoints */
    for (i = 0, count = 0 ; i < mca_btl_scif_module.endpoint_count ; ++i) {
        if (MCA_BTL_SCIF_EP_STATE_CONNECTED == mca_btl_scif_module.endpoints[i].state) {
            /* poll all connected endpoints */
            count += mca_btl_scif_progress_recvs (mca_btl_scif_module.endpoints + i);
            /* if any fragments are waiting try to send them now */
            count += mca_btl_scif_progress_sends (mca_btl_scif_module.endpoints + i);
        }
    }

    return count;
}

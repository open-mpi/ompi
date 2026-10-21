/*
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2026 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"
#include "opal/prefetch.h"
#include "opal/runtime/opal_params.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/mpool/mpool.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "pml_ob1.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_rdmafrag.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_sendreq.h"
#include "ompi/mca/bml/base/base.h"

#include "opal/types.h"
#include "pml_ob1_accelerator.h"
#include "pml_ob1.h"
#include "opal/mca/accelerator/base/base.h"

static opal_accelerator_stream_t *dtoh_stream = NULL;
static opal_accelerator_stream_t *htod_stream = NULL;

static opal_mutex_t pml_ob1_accelerator_event_lock;

/* Set to true once the accelerator streams have been created.  Read on the hot
 * path by the inline mca_pml_ob1_accelerator_ensure_init() in the header. */
bool mca_pml_ob1_accelerator_streams_initialized = false;

/* Pool of accelerator completion events, shared across directions.  An event is
 * recorded against whichever stream an asynchronous copy was queued on (see
 * mca_pml_ob1_record_event()) and later reaped in
 * mca_pml_ob1_progress_one_event(). */
static opal_accelerator_event_t **accelerator_event_array = NULL;

/* Array of fragments currently being moved by accelerator async non-blocking
 * operations */
static struct mca_btl_base_descriptor_t **accelerator_event_frag_array = NULL;

/* First free/available location in accelerator_event_array */
static int accelerator_event_first_avail;

/* First currently-being used location in accelerator_event_array */
static int accelerator_event_first_used;

/* Number of status items currently in use */
static volatile int accelerator_event_num_used;

/* Current number of events allocated in the pool.  The pool starts empty and
 * grows on demand, in batches of mca_pml_ob1_accelerator_events_batch, up to the
 * mca_pml_ob1_accelerator_events_max cap. */
static int accelerator_event_size = 0;

/* Grow the event pool by one batch of mca_pml_ob1_accelerator_events_batch
 * events, never exceeding the mca_pml_ob1_accelerator_events_max cap.  Must be
 * called with the ring full (num_used == size) and with
 * pml_ob1_accelerator_event_lock held.  On success the ring has at least one free
 * slot; returns OPAL_ERR_OUT_OF_RESOURCE when already at the cap. */
static int mca_pml_ob1_accelerator_grow_events(void)
{
    int old_size = accelerator_event_size;
    int new_size, i, result;
    opal_accelerator_event_t **new_events;
    struct mca_btl_base_descriptor_t **new_frags;

    if (old_size >= mca_pml_ob1_accelerator_events_max) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* mca_pml_ob1_accelerator_events_batch is validated at registration to be
     * in [1, mca_pml_ob1_accelerator_events_max]. */
    new_size = old_size + mca_pml_ob1_accelerator_events_batch;
    if (new_size > mca_pml_ob1_accelerator_events_max) {
        new_size = mca_pml_ob1_accelerator_events_max;
    }

    new_events = (opal_accelerator_event_t **) calloc(new_size, sizeof(opal_accelerator_event_t *));
    new_frags = (struct mca_btl_base_descriptor_t **) malloc(new_size * sizeof(struct mca_btl_base_descriptor_t *));
    if (NULL == new_events || NULL == new_frags) {
        opal_output_verbose(1, mca_pml_ob1_output, "No memory to grow accelerator event pool.");
        free(new_events);
        free(new_frags);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Re-linearize the ring: the in-flight entries, in order, start at
     * first_used and wrap exactly once.  Copy them to the front of the new
     * arrays as two contiguous runs -- [first_used, old_size) then
     * [0, first_used) -- so the occupied region becomes [0, old_size) again.
     * (With old_size == 0 the arrays are still NULL, so there is nothing to
     * copy.) */
    if (old_size > 0) {
        int base = accelerator_event_first_used;
        int wrap = old_size - base; /* entries from first_used to end of array */
        memcpy(new_events, &accelerator_event_array[base], wrap * sizeof(*new_events));
        memcpy(&new_events[wrap], accelerator_event_array, base * sizeof(*new_events));
        memcpy(new_frags, &accelerator_event_frag_array[base], wrap * sizeof(*new_frags));
        memcpy(&new_frags[wrap], accelerator_event_frag_array, base * sizeof(*new_frags));
    }

    /* Create the fresh events that make up the new batch.  If the accelerator
     * runs out partway, keep whatever we did create and shrink the new size to
     * match -- a smaller grow is still progress, and the surplus slots are
     * simply left unused.  Only fail if we could not create a single new event,
     * since the ring would then still be full. */
    for (i = old_size; i < new_size; i++) {
        result = opal_accelerator.create_event(MCA_ACCELERATOR_NO_DEVICE_ID, &new_events[i], false);
        if (OPAL_SUCCESS != result) {
            opal_output_verbose(1, mca_pml_ob1_output,
                                "Accelerator create event failed after %d of %d new events.",
                                i - old_size, new_size - old_size);
            break;
        }
    }

    if (i == old_size) {
        /* Nothing gained; the old ring arrays are still intact, so drop the
         * freshly allocated (empty) ones and report the failure. */
        free(new_events);
        free(new_frags);
        return result;
    }
    new_size = i; /* however many events we actually created */

    free(accelerator_event_array);
    free(accelerator_event_frag_array);
    accelerator_event_array = new_events;
    accelerator_event_frag_array = new_frags;
    accelerator_event_first_used = 0;
    accelerator_event_first_avail = old_size; /* first fresh slot */
    accelerator_event_size = new_size;
    /* num_used is unchanged by a grow. */

    opal_output_verbose(10, mca_pml_ob1_output,
                        "Grew accelerator event pool to %d (cap %d)",
                        accelerator_event_size, mca_pml_ob1_accelerator_events_max);
    return OPAL_SUCCESS;
}

int mca_pml_ob1_record_event(char *msg, struct mca_btl_base_descriptor_t *frag,
                             opal_accelerator_stream_t *stream)
{
    int result;

    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        return 0;
    }

    /* The event pool is direction-agnostic; the caller selects which stream the
     * asynchronous operation was queued on. */
    if (NULL == stream) {
        return OPAL_ERR_BAD_PARAM;
    }

    OPAL_THREAD_LOCK(&pml_ob1_accelerator_event_lock);
    /* No free slot: grow the pool by a batch.  Only if that fails (we are
     * already at the cap) do we return an error telling the user to raise it. */
    if (accelerator_event_num_used == accelerator_event_size) {
        result = mca_pml_ob1_accelerator_grow_events();
        if (OPAL_SUCCESS != result) {
            opal_output_verbose(1, mca_pml_ob1_output, "Out of event handles. Max: %d. Suggested to rerun with new max with --mca pml_ob1_accelerator_events_max %d.",
                                mca_pml_ob1_accelerator_events_max, mca_pml_ob1_accelerator_events_max + 100);
            OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
            return result;
        }
    }

    result = opal_accelerator.record_event(MCA_ACCELERATOR_NO_DEVICE_ID, accelerator_event_array[accelerator_event_first_avail], stream);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
        opal_output_verbose(1, mca_pml_ob1_output, "Event Record failed.");
        OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
        return OPAL_ERROR;
    }
    accelerator_event_frag_array[accelerator_event_first_avail] = frag;

    /* Bump up the first available slot and number used by 1 */
    accelerator_event_first_avail++;
    if (accelerator_event_first_avail >= accelerator_event_size) {
        accelerator_event_first_avail = 0;
    }
    accelerator_event_num_used++;

    OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
    return OPAL_SUCCESS;
}

opal_accelerator_stream_t *mca_pml_ob1_get_dtoh_stream(void)
{
    (void) mca_pml_ob1_accelerator_ensure_init();
    return dtoh_stream;
}

opal_accelerator_stream_t *mca_pml_ob1_get_htod_stream(void)
{
    (void) mca_pml_ob1_accelerator_ensure_init();
    return htod_stream;
}

/**
 * Progress any event completions.
 */
int mca_pml_ob1_progress_one_event(struct mca_btl_base_descriptor_t **frag)
{
    int result;

    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        return 0;
    }

    OPAL_THREAD_LOCK(&pml_ob1_accelerator_event_lock);
    if (accelerator_event_num_used > 0) {
        opal_output_verbose(30, mca_pml_ob1_output,
                            "mca_pml_ob1_progress_one_event, outstanding_events=%d",
                            accelerator_event_num_used);

        result = opal_accelerator.query_event(MCA_ACCELERATOR_NO_DEVICE_ID, accelerator_event_array[accelerator_event_first_used]);

        /* We found an event that is not ready, so return. */
        if (OPAL_ERR_RESOURCE_BUSY == result) {
            opal_output_verbose(30, mca_pml_ob1_output,
                                "Accelerator event query returned OPAL_ERR_RESOURCE_BUSY");
            *frag = NULL;
            OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
            return 0;
        } else if (OPAL_SUCCESS != result) {
            opal_output_verbose(1, mca_pml_ob1_output, "Accelerator event query failed: %d,", result);
            *frag = NULL;
            OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
            return OPAL_ERROR;
        }

        *frag = accelerator_event_frag_array[accelerator_event_first_used];

        /* Bump counters, loop around the circular buffer if necessary */
        --accelerator_event_num_used;
        ++accelerator_event_first_used;
        if (accelerator_event_first_used >= accelerator_event_size) {
            accelerator_event_first_used = 0;
        }
        /* A return value of 1 indicates an event completed and a frag was returned */
        OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
        return 1;
    }
    OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
    return 0;
}

int mca_pml_ob1_accelerator_init(void)
{
    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        return 0;
    }

    OBJ_CONSTRUCT(&pml_ob1_accelerator_event_lock, opal_mutex_t);

    /* The event pool starts empty.  Streams are created on first use of a
     * device buffer by mca_pml_ob1_accelerator_ensure_init(), and the event
     * array grows on demand (in batches of
     * mca_pml_ob1_accelerator_events_batch, up to
     * mca_pml_ob1_accelerator_events_max) in
     * mca_pml_ob1_accelerator_grow_events(). */
    accelerator_event_size = 0;
    accelerator_event_num_used = 0;
    accelerator_event_first_avail = 0;
    accelerator_event_first_used = 0;
    accelerator_event_array = NULL;
    accelerator_event_frag_array = NULL;

    return OPAL_SUCCESS;
}

void mca_pml_ob1_accelerator_fini(void)
{
    int i;

    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        return;
    }

    if (NULL != accelerator_event_array) {
        for (i = 0; i < accelerator_event_size; i++) {
            if (NULL != accelerator_event_array[i]) {
                OBJ_RELEASE(accelerator_event_array[i]);
            }
        }
        free(accelerator_event_array);
    }
    if (NULL != accelerator_event_frag_array) {
        free(accelerator_event_frag_array);
    }

    if (NULL != htod_stream) {
        OBJ_RELEASE(htod_stream);
    }
    if (NULL != dtoh_stream) {
        OBJ_RELEASE(dtoh_stream);
    }

    OBJ_DESTRUCT(&pml_ob1_accelerator_event_lock);
}

size_t mca_pml_ob1_rdma_cuda_btls(
    mca_bml_base_endpoint_t* bml_endpoint,
    unsigned char* base,
    size_t size,
    mca_pml_ob1_com_btl_t* rdma_btls);

int mca_pml_ob1_accelerator_need_buffers(void * rreq,
                                         mca_btl_base_module_t* btl);

void mca_pml_ob1_accelerator_add_ipc_support(struct mca_btl_base_module_t* btl, int32_t flags,
                                             ompi_proc_t* errproc, char* btlinfo);

/* Slow path for the inline mca_pml_ob1_accelerator_ensure_init(): create the
 * accelerator streams on first use of a device buffer.  The event pool is grown
 * separately and on demand by mca_pml_ob1_accelerator_grow_events(). */
int mca_pml_ob1_accelerator_create_streams(void)
{
    int result;

    /* Create the streams under the lock, re-checking the flag in case another
     * thread won the race while we waited. */
    OPAL_THREAD_LOCK(&pml_ob1_accelerator_event_lock);
    if (mca_pml_ob1_accelerator_streams_initialized) {
        OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
        return OPAL_SUCCESS;
    }

    result = opal_accelerator.create_stream(MCA_ACCELERATOR_NO_DEVICE_ID, &dtoh_stream);
    if (OPAL_SUCCESS != result) {
        opal_output_verbose(1, mca_pml_ob1_output, "Failed to create accelerator dtoh_stream stream.");
        OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
        return result;
    }

    result = opal_accelerator.create_stream(MCA_ACCELERATOR_NO_DEVICE_ID, &htod_stream);
    if (OPAL_SUCCESS != result) {
        opal_output_verbose(1, mca_pml_ob1_output, "Failed to create accelerator htod_stream stream.");
        OBJ_RELEASE(dtoh_stream);
        dtoh_stream = NULL;
        OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
        return result;
    }

    /* Publish the streams before the flag that advertises them, so a thread that
     * observes the flag on the lock-free fast path (the inline
     * mca_pml_ob1_accelerator_ensure_init()) also sees the fully-created streams
     * (required on weakly-ordered architectures such as aarch64 and ppc64). */
    opal_atomic_wmb();
    mca_pml_ob1_accelerator_streams_initialized = true;
    OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_event_lock);
    return OPAL_SUCCESS;
}

/**
 * Handle the accelerator buffer.
 */
int mca_pml_ob1_send_request_start_accelerator(mca_pml_ob1_send_request_t* sendreq,
                                               mca_bml_base_btl_t* bml_btl,
                                               size_t size) {
    int rc;
#if OPAL_CUDA_GDR_SUPPORT
    /* With some BTLs, switch to RNDV from RGET at large messages */
    if ((sendreq->req_send.req_base.req_convertor.flags & CONVERTOR_ACCELERATOR) &&
        (sendreq->req_send.req_bytes_packed > (bml_btl->btl->btl_accelerator_rdma_limit - sizeof(mca_pml_ob1_hdr_t)))) {
        return mca_pml_ob1_send_request_start_rndv(sendreq, bml_btl, 0, 0);
    }
#endif /* OPAL_CUDA_GDR_SUPPORT */

    if (opal_convertor_need_buffers(&sendreq->req_send.req_base.req_convertor) == false) {
        unsigned char *base;
        opal_convertor_get_current_pointer( &sendreq->req_send.req_base.req_convertor, (void**)&base );
        if( 0 != (sendreq->req_rdma_cnt = (uint32_t)mca_pml_ob1_rdma_cuda_btls(
                                                                           sendreq->req_endpoint,
                                                                           base,
                                                                           sendreq->req_send.req_bytes_packed,
                                                                           sendreq->req_rdma))) {
            rc = mca_pml_ob1_send_request_start_rdma(sendreq, bml_btl,
                                                     sendreq->req_send.req_bytes_packed);
            if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
                mca_pml_ob1_free_rdma_resources(sendreq);
            }
        } else {
            if (bml_btl->btl_flags & MCA_BTL_FLAGS_ACCELERATOR_PUT) {
                rc = mca_pml_ob1_send_request_start_rndv(sendreq, bml_btl, size,
                                                         MCA_PML_OB1_HDR_FLAGS_CONTIG);
            } else {
                rc = mca_pml_ob1_send_request_start_rndv(sendreq, bml_btl, 0, 0);
            }
        }
    } else {
        /* Do not send anything with first rendezvous message as copying GPU
         * memory into RNDV message is expensive. */
        rc = mca_pml_ob1_send_request_start_rndv(sendreq, bml_btl, 0, 0);
    }
    return rc;
}



size_t mca_pml_ob1_rdma_cuda_btls(
    mca_bml_base_endpoint_t* bml_endpoint,
    unsigned char* base,
    size_t size,
    mca_pml_ob1_com_btl_t* rdma_btls)
{
    int num_btls = mca_bml_base_btl_array_get_size(&bml_endpoint->btl_send);
    double weight_total = 0;
    int num_btls_used = 0, n;

    /* shortcut when there are no rdma capable btls */
    if(num_btls == 0) {
        return 0;
    }

    /* check to see if memory is registered */
    for(n = 0; n < num_btls && num_btls_used < mca_pml_ob1.max_rdma_per_request;
            n++) {
        mca_bml_base_btl_t* bml_btl =
            mca_bml_base_btl_array_get_index(&bml_endpoint->btl_send, n);

        if (bml_btl->btl_flags & MCA_BTL_FLAGS_ACCELERATOR_GET) {
            mca_btl_base_registration_handle_t *handle = NULL;

            if( NULL != bml_btl->btl->btl_register_mem ) {
                /* register the memory */
                handle = bml_btl->btl->btl_register_mem (bml_btl->btl, bml_btl->btl_endpoint,
                                                         base, size,
#if OPAL_CUDA_GDR_SUPPORT
                                                         MCA_BTL_REG_FLAG_CUDA_GPU_MEM |
#endif
                                                         MCA_BTL_REG_FLAG_REMOTE_READ);
            }

            if(NULL == handle)
                continue;

            rdma_btls[num_btls_used].bml_btl = bml_btl;
            rdma_btls[num_btls_used].btl_reg = handle;
            weight_total += bml_btl->btl_weight;
            num_btls_used++;
        }
    }

    /* if we don't use leave_pinned and all BTLs that already have this memory
     * registered amount to less then half of available bandwidth - fall back to
     * pipeline protocol */
    if(0 == num_btls_used || (!opal_leave_pinned && weight_total < 0.5))
        return 0;

    mca_pml_ob1_calc_weighted_length(rdma_btls, num_btls_used, size,
                                     weight_total);

    return num_btls_used;
}

int mca_pml_ob1_accelerator_need_buffers(void * rreq,
                                         mca_btl_base_module_t* btl)
{
    mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)rreq;
    int eprc;
    mca_bml_base_endpoint_t* bml_endpoint = mca_bml_base_get_endpoint (recvreq->req_recv.req_base.req_proc,
                                                                      &eprc);

    /* No endpoint yet, so nothing tells us this btl can get from
     * accelerator memory; answer conservatively (copy in/out). */
    if (OPAL_UNLIKELY(NULL == bml_endpoint)) {
        return true;
    }

    mca_bml_base_btl_t *bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_send, btl);

    /* A btl could be in the rdma list but not in the send list so check there also */
    if (NULL == bml_btl) {
        bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma, btl);
    }
    /* We should always be able to find back the bml_btl based on the btl */
    assert(NULL != bml_btl);

    if ((recvreq->req_recv.req_base.req_convertor.flags & CONVERTOR_ACCELERATOR) &&
        (bml_btl->btl_flags & MCA_BTL_FLAGS_ACCELERATOR_GET)) {
        return opal_convertor_need_buffers(&recvreq->req_recv.req_base.req_convertor);
    }
    return true;
}

/*
 * This function enables us to start using RDMA get protocol with GPU buffers.
 * We do this by adjusting the flags in the BML structure.  This is not the
 * best thing, but this may go away if CUDA IPC is supported everywhere in the
 * future. */
void mca_pml_ob1_accelerator_add_ipc_support(struct mca_btl_base_module_t* btl, int32_t flags,
                                      ompi_proc_t* errproc, char* btlinfo)
{
    mca_bml_base_endpoint_t* ep;
    int btl_verbose_stream = 0;
    int i;

    assert(NULL != errproc);
    assert(NULL != errproc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML]);
    if (NULL != btlinfo) {
        btl_verbose_stream = *(int *)btlinfo;
    }
    ep = (mca_bml_base_endpoint_t*)errproc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];

    /* Find the corresponding bml and adjust the flag to support CUDA get */
    for( i = 0; i < (int)ep->btl_send.arr_size; i++ ) {
        if( ep->btl_send.bml_btls[i].btl == btl ) {
            if (4 < opal_output_get_verbosity(btl_verbose_stream)) {
                char *errhost = opal_get_proc_hostname(&errproc->super);
                opal_output(0, "BTL %s: rank=%d enabling accelerator IPC "
                            "to rank=%d on node=%s \n",
                            btl->btl_component->btl_version.mca_component_name,
                            OMPI_PROC_MY_NAME->vpid,
                            ((ompi_process_name_t*)&errproc->super.proc_name)->vpid,
                            errhost);
                free(errhost);
            }
            ep->btl_send.bml_btls[i].btl_flags |= MCA_BTL_FLAGS_ACCELERATOR_GET;
        }
    }
}

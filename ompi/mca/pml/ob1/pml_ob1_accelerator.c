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

static opal_mutex_t pml_ob1_accelerator_htod_lock;
static opal_mutex_t pml_ob1_accelerator_dtoh_lock;

/* Set to true once streams and events have been created. */
static bool pml_ob1_accelerator_streams_initialized = false;

/* Array of accelerator events to be queried for sending side and
 * receiving side. */
static opal_accelerator_event_t **accelerator_event_dtoh_array = NULL;
static opal_accelerator_event_t **accelerator_event_htod_array = NULL;

/* Array of fragments currently being moved by accelerator async non-blocking
 * operations */
static struct mca_btl_base_descriptor_t **accelerator_event_dtoh_frag_array = NULL;
static struct mca_btl_base_descriptor_t **accelerator_event_htod_frag_array = NULL;

/* First free/available location in accelerator_event_status_array */
static int accelerator_event_dtoh_first_avail, accelerator_event_htod_first_avail;

/* First currently-being used location in the accelerator_event_status_array */
static int accelerator_event_dtoh_first_used, accelerator_event_htod_first_used;

/* Number of status items currently in use */
static volatile int accelerator_event_dtoh_num_used, accelerator_event_htod_num_used;

/* Size of array holding events */
static int accelerator_event_htod_most = 0;

/* Create streams and events on first use of a device buffer.
 * At runtime, callers must hold pml_ob1_accelerator_htod_lock.
 * The exception is the eager path in mca_pml_ob1_accelerator_init(),
 * which runs single-threaded before MPI_Init completes. */
static int mca_pml_ob1_accelerator_lazy_init(void)
{
    int result;
    int i;

    if (pml_ob1_accelerator_streams_initialized) {
        return OPAL_SUCCESS;
    }

    result = opal_accelerator.create_stream(MCA_ACCELERATOR_NO_DEVICE_ID, &dtoh_stream);
    if (OPAL_SUCCESS != result) {
        opal_output_verbose(1, mca_pml_ob1_output, "Failed to create accelerator dtoh_stream stream.");
        return result;
    }

    result = opal_accelerator.create_stream(MCA_ACCELERATOR_NO_DEVICE_ID, &htod_stream);
    if (OPAL_SUCCESS != result) {
        opal_output_verbose(1, mca_pml_ob1_output, "Failed to create accelerator htod_stream stream.");
        OBJ_RELEASE(dtoh_stream);
        dtoh_stream = NULL;
        return result;
    }

    for (i = 0; i < mca_pml_ob1_accelerator_events_max; i++) {
        result = opal_accelerator.create_event(MCA_ACCELERATOR_NO_DEVICE_ID, &accelerator_event_dtoh_array[i],
                                               false);
        if (OPAL_SUCCESS != result) {
            opal_output_verbose(1, mca_pml_ob1_output, "Accelerator create dtoh event failed.");
            goto cleanup;
        }
    }

    for (i = 0; i < mca_pml_ob1_accelerator_events_max; i++) {
        result = opal_accelerator.create_event(MCA_ACCELERATOR_NO_DEVICE_ID, &accelerator_event_htod_array[i],
                                               false);
        if (OPAL_SUCCESS != result) {
            opal_output_verbose(1, mca_pml_ob1_output, "Accelerator create htod event failed.");
            goto cleanup;
        }
    }

    /* Publish the streams and events before the flag that advertises them.
     * The matching acquire barrier is in mca_pml_ob1_accelerator_ensure_init(),
     * so a thread that observes the flag on the lock-free fast path is
     * guaranteed to also see the fully-created streams and event arrays
     * (required on weakly-ordered architectures such as aarch64 and ppc64). */
    opal_atomic_wmb();
    pml_ob1_accelerator_streams_initialized = true;
    return OPAL_SUCCESS;

cleanup:
    /* Release any events already created, then release the streams. */
    for (i = 0; i < mca_pml_ob1_accelerator_events_max; i++) {
        if (NULL != accelerator_event_htod_array[i]) {
            OBJ_RELEASE(accelerator_event_htod_array[i]);
            accelerator_event_htod_array[i] = NULL;
        }
        if (NULL != accelerator_event_dtoh_array[i]) {
            OBJ_RELEASE(accelerator_event_dtoh_array[i]);
            accelerator_event_dtoh_array[i] = NULL;
        }
    }
    OBJ_RELEASE(htod_stream);
    htod_stream = NULL;
    OBJ_RELEASE(dtoh_stream);
    dtoh_stream = NULL;
    return result;
}

int mca_pml_ob1_record_htod_event(char *msg, struct mca_btl_base_descriptor_t *frag)
{
    int result;

    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        return 0;
    }

    /* First make sure there is room to store the event.  If not, then
     * return an error.  The error message will tell the user to try and
     * run again, but with a larger array for storing events. */
    result = mca_pml_ob1_accelerator_ensure_init();
    if (OPAL_SUCCESS != result) {
        return result;
    }

    OPAL_THREAD_LOCK(&pml_ob1_accelerator_htod_lock);
    if (accelerator_event_htod_num_used == mca_pml_ob1_accelerator_events_max) {
        opal_output_verbose(1, mca_pml_ob1_output, "Out of event handles. Max: %d. Suggested to rerun with new max with --mca pml_ob1_accelerator_events_max %d.",
                            mca_pml_ob1_accelerator_events_max, mca_pml_ob1_accelerator_events_max + 100);
        OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_htod_lock);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (accelerator_event_htod_num_used > accelerator_event_htod_most) {
        accelerator_event_htod_most = accelerator_event_htod_num_used;
        /* Just print multiples of 10 */
        if (0 == (accelerator_event_htod_most % 10)) {
            opal_output_verbose(20, mca_pml_ob1_output, "Maximum HtoD events used is now %d",
                                accelerator_event_htod_most);
        }
    }

    result = opal_accelerator.record_event(MCA_ACCELERATOR_NO_DEVICE_ID, accelerator_event_htod_array[accelerator_event_htod_first_avail], htod_stream);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
        opal_output_verbose(1, mca_pml_ob1_output, "Event Record failed.");
        OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_htod_lock);
        return OPAL_ERROR;
    }
    accelerator_event_htod_frag_array[accelerator_event_htod_first_avail] = frag;

    /* Bump up the first available slot and number used by 1 */
    accelerator_event_htod_first_avail++;
    if (accelerator_event_htod_first_avail >= mca_pml_ob1_accelerator_events_max) {
        accelerator_event_htod_first_avail = 0;
    }
    accelerator_event_htod_num_used++;

    OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_htod_lock);
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
 * Progress any htod event completions.
 */
int mca_pml_ob1_progress_one_htod_event(struct mca_btl_base_descriptor_t **frag)
{
    int result;

    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        return 0;
    }

    OPAL_THREAD_LOCK(&pml_ob1_accelerator_htod_lock);
    if (accelerator_event_htod_num_used > 0) {
        opal_output_verbose(30, mca_pml_ob1_output,
                            "mca_pml_ob1_progress_one_htod_event, outstanding_events=%d",
                            accelerator_event_htod_num_used);

        result = opal_accelerator.query_event(MCA_ACCELERATOR_NO_DEVICE_ID, accelerator_event_htod_array[accelerator_event_htod_first_used]);

        /* We found an event that is not ready, so return. */
        if (OPAL_ERR_RESOURCE_BUSY == result) {
            opal_output_verbose(30, mca_pml_ob1_output,
                                "Accelerator event query returned OPAL_ERR_RESOURCE_BUSY");
            *frag = NULL;
            OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_htod_lock);
            return 0;
        } else if (OPAL_SUCCESS != result) {
            opal_output_verbose(1, mca_pml_ob1_output, "Accelerator event query failed: %d,", result);
            *frag = NULL;
            OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_htod_lock);
            return OPAL_ERROR;
        }

        *frag = accelerator_event_htod_frag_array[accelerator_event_htod_first_used];

        /* Bump counters, loop around the circular buffer if necessary */
        --accelerator_event_htod_num_used;
        ++accelerator_event_htod_first_used;
        if (accelerator_event_htod_first_used >= mca_pml_ob1_accelerator_events_max) {
            accelerator_event_htod_first_used = 0;
        }
        /* A return value of 1 indicates an event completed and a frag was returned */
        OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_htod_lock);
        return 1;
    }
    OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_htod_lock);
    return 0;
}

int mca_pml_ob1_accelerator_init(void)
{
    int rc = OPAL_SUCCESS;

    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        return 0;
    }

    OBJ_CONSTRUCT(&pml_ob1_accelerator_htod_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&pml_ob1_accelerator_dtoh_lock, opal_mutex_t);

    /* Set up an array of pointers to store outstanding async dtoh events.
     * Used on the sending side for asynchronous copies. */
    accelerator_event_dtoh_num_used = 0;
    accelerator_event_dtoh_first_avail = 0;
    accelerator_event_dtoh_first_used = 0;

    accelerator_event_dtoh_array = calloc(mca_pml_ob1_accelerator_events_max, sizeof(opal_accelerator_event_t *));
    if (NULL == accelerator_event_dtoh_array) {
        opal_output_verbose(1, mca_pml_ob1_output, "No memory.");
        rc = OPAL_ERROR;
        goto cleanup_and_error;
    }

    /* The first available status index is 0.  Make an empty frag array. */
    accelerator_event_dtoh_frag_array = (struct mca_btl_base_descriptor_t **) malloc(
        sizeof(struct mca_btl_base_descriptor_t *) * mca_pml_ob1_accelerator_events_max);
    if (NULL == accelerator_event_dtoh_frag_array) {
        opal_output_verbose(1, mca_pml_ob1_output, "No memory.");
        rc = OPAL_ERROR;
        goto cleanup_and_error;
    }

    /* Set up an array of pointers to store outstanding async htod events.
     * Used on the receiving side for asynchronous copies. */
    accelerator_event_htod_num_used = 0;
    accelerator_event_htod_first_avail = 0;
    accelerator_event_htod_first_used = 0;

    accelerator_event_htod_array = calloc(mca_pml_ob1_accelerator_events_max, sizeof(opal_accelerator_event_t *));
    if (NULL == accelerator_event_htod_array) {
        opal_output_verbose(1, mca_pml_ob1_output, "No memory.");
        rc = OPAL_ERROR;
        goto cleanup_and_error;
    }

    /* The first available status index is 0.  Make an empty frag array. */
    accelerator_event_htod_frag_array = (struct mca_btl_base_descriptor_t **) malloc(
        sizeof(struct mca_btl_base_descriptor_t *) * mca_pml_ob1_accelerator_events_max);
    if (NULL == accelerator_event_htod_frag_array) {
        opal_output_verbose(1, mca_pml_ob1_output, "No memory.");
        rc = OPAL_ERROR;
        goto cleanup_and_error;
    }

    /* When lazy init is disabled, create streams and events immediately. */
    if (!mca_pml_ob1.accelerator_lazy_init) {
        rc = mca_pml_ob1_accelerator_lazy_init();
    }

cleanup_and_error:
    if (OPAL_SUCCESS != rc) {
        if (NULL != accelerator_event_dtoh_array) {
            free(accelerator_event_dtoh_array);
            accelerator_event_dtoh_array = NULL;
        }
        if (NULL != accelerator_event_dtoh_frag_array) {
            free(accelerator_event_dtoh_frag_array);
            accelerator_event_dtoh_frag_array = NULL;
        }
        if (NULL != accelerator_event_htod_array) {
            free(accelerator_event_htod_array);
            accelerator_event_htod_array = NULL;
        }
        if (NULL != accelerator_event_htod_frag_array) {
            free(accelerator_event_htod_frag_array);
            accelerator_event_htod_frag_array = NULL;
        }
        OBJ_DESTRUCT(&pml_ob1_accelerator_htod_lock);
        OBJ_DESTRUCT(&pml_ob1_accelerator_dtoh_lock);
    }

    return rc;
}

void mca_pml_ob1_accelerator_fini(void)
{
    int i;

    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        return;
    }

    if (NULL != accelerator_event_htod_array) {
        for (i = 0; i < mca_pml_ob1_accelerator_events_max; i++) {
            if (NULL != accelerator_event_htod_array[i]) {
                OBJ_RELEASE(accelerator_event_htod_array[i]);
            }
        }
        free(accelerator_event_htod_array);
    }

    if (NULL != accelerator_event_dtoh_array) {
        for (i = 0; i < mca_pml_ob1_accelerator_events_max; i++) {
            if (NULL != accelerator_event_dtoh_array[i]) {
                OBJ_RELEASE(accelerator_event_dtoh_array[i]);
            }
        }
        free(accelerator_event_dtoh_array);
    }
    if (NULL != accelerator_event_dtoh_frag_array) {
        free(accelerator_event_dtoh_frag_array);
    }
    if (NULL != accelerator_event_htod_frag_array) {
        free(accelerator_event_htod_frag_array);
    }

    if (NULL != htod_stream) {
        OBJ_RELEASE(htod_stream);
    }
    if (NULL != dtoh_stream) {
        OBJ_RELEASE(dtoh_stream);
    }

    OBJ_DESTRUCT(&pml_ob1_accelerator_htod_lock);
    OBJ_DESTRUCT(&pml_ob1_accelerator_dtoh_lock);
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

int mca_pml_ob1_accelerator_ensure_init(void)
{
    int rc = OPAL_SUCCESS;
    if (OPAL_LIKELY(pml_ob1_accelerator_streams_initialized)) {
        /* Acquire: pair with the release (wmb) in lazy_init so the stream and
         * event pointers read after this point are visible and non-stale. */
        opal_atomic_rmb();
        return OPAL_SUCCESS;
    }
    OPAL_THREAD_LOCK(&pml_ob1_accelerator_htod_lock);
    rc = mca_pml_ob1_accelerator_lazy_init();
    OPAL_THREAD_UNLOCK(&pml_ob1_accelerator_htod_lock);
    return rc;
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

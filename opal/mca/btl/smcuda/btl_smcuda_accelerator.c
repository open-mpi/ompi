/* 
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal/mca/btl/btl.h"
#include "ompi/constants.h"
#include "btl_smcuda.h"

#include "btl_smcuda_accelerator.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/accelerator/accelerator.h"

static opal_mutex_t btl_smcuda_accelerator_ipc_lock;
static opal_accelerator_stream_t *ipc_stream = NULL;

opal_accelerator_event_t **accelerator_event_ipc_array = NULL;
static struct mca_btl_base_descriptor_t **accelerator_event_ipc_frag_array = NULL;

/* First free/available location in accelerator_event_status_array */
static int accelerator_event_ipc_first_avail;

/* First currently-being used location in the accelerator_event_status_array */
static int accelerator_event_ipc_first_used;

/* Number of status items currently in use */
static volatile int accelerator_event_ipc_num_used;

/* Size of array holding events */
static int accelerator_event_max = 400;
static int accelerator_event_ipc_most = 0;
static bool smcuda_accelerator_initialized = false;

void mca_btl_smcuda_accelerator_fini(void);

int mca_btl_smcuda_accelerator_init(void)
{
    int rc = OPAL_SUCCESS;
    int i;
    OBJ_CONSTRUCT(&btl_smcuda_accelerator_ipc_lock, opal_mutex_t);
    /* The first available status index is 0.  Make an empty frag
       array. */

    rc = opal_accelerator.create_stream(MCA_ACCELERATOR_NO_DEVICE_ID, &ipc_stream);
    if (OPAL_SUCCESS != rc) {
        opal_output_verbose(1, mca_btl_smcuda_component.cuda_ipc_output, "Failed to create accelerator ipc_stream stream.");
        goto cleanup_and_error;
    }

    accelerator_event_ipc_num_used = 0;
    accelerator_event_ipc_first_avail = 0;
    accelerator_event_ipc_first_used = 0;

    accelerator_event_ipc_array = calloc(accelerator_event_max, sizeof(opal_accelerator_event_t *));
    if (NULL == accelerator_event_ipc_array) {
        opal_output_verbose(1, mca_btl_smcuda_component.cuda_ipc_output, "No memory.");
        rc = OPAL_ERROR;
        goto cleanup_and_error;
    }
    /* Create the events since they can be reused. */
    for (i = 0; i < accelerator_event_max; i++) {
        rc = opal_accelerator.create_event(MCA_ACCELERATOR_NO_DEVICE_ID, &accelerator_event_ipc_array[i]);
        if (OPAL_SUCCESS != rc) {
            opal_output_verbose(1, mca_btl_smcuda_component.cuda_ipc_output, "Accelerator create event failed.");
            rc = OPAL_ERROR;
            goto cleanup_and_error;
        }
    }

    /* The first available status index is 0.  Make an empty frag
       array. */

    accelerator_event_ipc_frag_array = (struct mca_btl_base_descriptor_t **) malloc(sizeof(struct mca_btl_base_descriptor_t *) * accelerator_event_max);
    if (NULL == accelerator_event_ipc_frag_array) {
        opal_output_verbose(1, mca_btl_smcuda_component.cuda_ipc_output, "No memory.");
        rc = OPAL_ERROR;
        goto cleanup_and_error;
    }

    smcuda_accelerator_initialized = true;

cleanup_and_error:
    if (OPAL_SUCCESS != rc) {
        if (NULL != accelerator_event_ipc_array) {
            for (i = 0; i < accelerator_event_max; i++) {
                if (NULL != accelerator_event_ipc_array[i]) {
                    OBJ_RELEASE(accelerator_event_ipc_array[i]);
                }
            }
            free(accelerator_event_ipc_array);
        }
        if (NULL != accelerator_event_ipc_frag_array) {
            free(accelerator_event_ipc_frag_array);
        }
        if (NULL != ipc_stream) {
            OBJ_RELEASE(ipc_stream);
        }
        OBJ_DESTRUCT(&btl_smcuda_accelerator_ipc_lock);
    }

    return rc;
}

void mca_btl_smcuda_accelerator_fini(void)
{
    int i;

    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null") ||
        false == smcuda_accelerator_initialized) {
        return;
    }

    if (NULL != accelerator_event_ipc_array) {
        for (i = 0; i < accelerator_event_max; i++) {
            if (NULL != accelerator_event_ipc_array[i]) {
                OBJ_RELEASE(accelerator_event_ipc_array[i]);
            }
        }
        free(accelerator_event_ipc_array);
    }

    if (NULL != accelerator_event_ipc_frag_array) {
        free(accelerator_event_ipc_frag_array);
    }

    OBJ_RELEASE(ipc_stream);

    OBJ_DESTRUCT(&btl_smcuda_accelerator_ipc_lock);
    smcuda_accelerator_initialized = false;
}

/*
 * Function is called every time progress is called with the sm BTL.  If there
 * are outstanding events, check to see if one has completed.  If so, hand
 * back the fragment for further processing.
 */
int mca_btl_smcuda_progress_one_ipc_event(struct mca_btl_base_descriptor_t **frag)
{   
    int result;
    
    if (OPAL_LIKELY(0 == accelerator_event_ipc_num_used))
        return 0;
    
    OPAL_THREAD_LOCK(&btl_smcuda_accelerator_ipc_lock);
    if (accelerator_event_ipc_num_used > 0) {
        opal_output_verbose(20, mca_btl_smcuda_component.cuda_ipc_output,
                            "smcuda: progress_one_accelerator_ipc_event, outstanding_events=%d",
                            accelerator_event_ipc_num_used);
        result = opal_accelerator.query_event(MCA_ACCELERATOR_NO_DEVICE_ID, accelerator_event_ipc_array[accelerator_event_ipc_first_used]);
        
        /* We found an event that is not ready, so return. */
        if (OPAL_ERR_RESOURCE_BUSY == result) {
            opal_output_verbose(20, mca_btl_smcuda_component.cuda_ipc_output,
                                "smcuda: event query returned not ready");
            *frag = NULL;
            OPAL_THREAD_UNLOCK(&btl_smcuda_accelerator_ipc_lock);
            return 0;
        } else if (OPAL_SUCCESS != result) {
            opal_output_verbose(1, mca_btl_smcuda_component.cuda_ipc_output,
                            "smcuda: event query failed: %d", result);
            *frag = NULL;
            OPAL_THREAD_UNLOCK(&btl_smcuda_accelerator_ipc_lock);
            return OPAL_ERROR;
        }
        
        *frag = accelerator_event_ipc_frag_array[accelerator_event_ipc_first_used];
        opal_output_verbose(10, mca_btl_smcuda_component.cuda_ipc_output, "smcuda: event query returned %d", result);
        
        /* Bump counters, loop around the circular buffer if necessary */
        --accelerator_event_ipc_num_used;
        ++accelerator_event_ipc_first_used;
        if (accelerator_event_ipc_first_used >= accelerator_event_max) {
            accelerator_event_ipc_first_used = 0;
        }
        /* A return value of 1 indicates an event completed and a frag was returned */
        OPAL_THREAD_UNLOCK(&btl_smcuda_accelerator_ipc_lock);
        return 1;
    }
    OPAL_THREAD_UNLOCK(&btl_smcuda_accelerator_ipc_lock);
    return 0;
}

/*
 * Start the asynchronous copy.  Then record and save away an event that will
 * be queried to indicate the copy has completed.
 */
int mca_btl_smcuda_memcpy(void *dst, void *src, size_t amount, char *msg,
                           struct mca_btl_base_descriptor_t *frag)
{
    int result;
    OPAL_THREAD_LOCK(&btl_smcuda_accelerator_ipc_lock);

    /* First make sure there is room to store the event.  If not, then
     * return an error.  The error message will tell the user to try and
     * run again, but with a larger array for storing events. */
    if (accelerator_event_ipc_num_used == accelerator_event_max) {
        opal_output_verbose(1, mca_btl_smcuda_component.cuda_ipc_output, "smcuda: Out of event handles");
        OPAL_THREAD_UNLOCK(&btl_smcuda_accelerator_ipc_lock);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (accelerator_event_ipc_num_used > accelerator_event_ipc_most) {
        accelerator_event_ipc_most = accelerator_event_ipc_num_used;
        /* Just print multiples of 10 */
        if (0 == (accelerator_event_ipc_most % 10)) {
            opal_output_verbose(20, mca_btl_smcuda_component.cuda_ipc_output, "smcuda: Maximum ipc events used is now %d",
                                accelerator_event_ipc_most);
        }
    }

    result = opal_accelerator.mem_copy_async(MCA_ACCELERATOR_NO_DEVICE_ID, MCA_ACCELERATOR_NO_DEVICE_ID,
                                             dst, src, amount, ipc_stream, MCA_ACCELERATOR_TRANSFER_UNSPEC);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
        opal_output_verbose(1, mca_btl_smcuda_component.cuda_ipc_output, "smcuda: memcpy async failed: %d",
                            result);
        OPAL_THREAD_UNLOCK(&btl_smcuda_accelerator_ipc_lock);
        return OPAL_ERROR;
    } else {
        opal_output_verbose(20, mca_btl_smcuda_component.cuda_ipc_output,
                            "smcuda: cuMemcpyAsync passed: dst=%p, src=%p, size=%d", dst, src,
                            (int) amount);
    }

    result = opal_accelerator.record_event(MCA_ACCELERATOR_NO_DEVICE_ID, accelerator_event_ipc_array[accelerator_event_ipc_first_avail], ipc_stream);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != result)) {
        opal_output_verbose(1, mca_btl_smcuda_component.cuda_ipc_output, "Event Record failed.");
        OPAL_THREAD_UNLOCK(&btl_smcuda_accelerator_ipc_lock);
        return OPAL_ERROR;
    }
    accelerator_event_ipc_frag_array[accelerator_event_ipc_first_avail] = frag;

    /* Bump up the first available slot and number used by 1 */
    accelerator_event_ipc_first_avail++;
    if (accelerator_event_ipc_first_avail >= accelerator_event_max) {
        accelerator_event_ipc_first_avail = 0;
    }
    accelerator_event_ipc_num_used++;

    OPAL_THREAD_UNLOCK(&btl_smcuda_accelerator_ipc_lock);
    return OPAL_SUCCESS;
}

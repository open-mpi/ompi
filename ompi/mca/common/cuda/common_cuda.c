/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * This file contains various support functions for doing CUDA
 * operations.  Some of the features are only available in CUDA 4.1
 * and later, so some code is conditionalized around the
 * OMPI_CUDA_SUPPORT_41 macro.
 */
#include "ompi_config.h"

#include <errno.h>
#include <unistd.h>
#include <cuda.h>

#include "opal/align.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_cuda.h"
#include "opal/util/output.h"
#include "ompi/mca/mpool/base/base.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "common_cuda.h"

static bool common_cuda_initialized = false;
static bool common_cuda_init_function_added = false;
static int mca_common_cuda_verbose;
static int mca_common_cuda_output = 0;
static bool mca_common_cuda_enabled = false;
static bool mca_common_cuda_register_memory = true;
static bool mca_common_cuda_warning = true;
static opal_list_t common_cuda_memory_registrations;

/* Structure to hold memory registrations that are delayed until first
 * call to send or receive a GPU pointer */
struct common_cuda_mem_regs_t {
    opal_list_item_t super;
    void *ptr;
    size_t amount;
    char *msg;
};
typedef struct common_cuda_mem_regs_t common_cuda_mem_regs_t;
OBJ_CLASS_DECLARATION(common_cuda_mem_regs_t);
OBJ_CLASS_INSTANCE( common_cuda_mem_regs_t,
                    opal_list_item_t,
                    NULL,
                    NULL );

#if OMPI_CUDA_SUPPORT_41
static int mca_common_cuda_async = 1;

/* Array of CUDA events to be queried */
CUevent *cuda_event_status_array;

/* Array of fragments currently being moved by cuda async non-blocking
 * operations */
struct mca_btl_base_descriptor_t **cuda_event_frag_array;

/* First free/available location in cuda_event_status_array */
int cuda_event_status_first_avail;

/* First currently-being used location in the cuda_event_status_array */
int cuda_event_status_first_used;

/* Number of status items currently in use */
int cuda_event_status_num_used;

/* Size of array holding events */
int cuda_event_max = 200;

#define CUDA_COMMON_TIMING 0
#if CUDA_COMMON_TIMING
/* Some timing support structures.  Enable this to help analyze
 * internal performance issues. */
static struct timespec ts_start;
static struct timespec ts_end;
static double accum;
#define THOUSAND  1000L
#define MILLION   1000000L
static float mydifftime(struct timespec ts_start, struct timespec ts_end);
#endif /* CUDA_COMMON_TIMING */

/* These functions are typically unused in the optimized builds. */
static void cuda_dump_evthandle(int, void *, char *) __opal_attribute_unused__ ;
static void cuda_dump_memhandle(int, void *, char *) __opal_attribute_unused__ ;
#if OPAL_ENABLE_DEBUG
#define CUDA_DUMP_MEMHANDLE(a) cuda_dump_memhandle a
#define CUDA_DUMP_EVTHANDLE(a) cuda_dump_evthandle a
#else
#define CUDA_DUMP_MEMHANDLE(a)
#define CUDA_DUMP_EVTHANDLE(a)
#endif /* OPAL_ENABLE_DEBUG */

#endif /* OMPI_CUDA_SUPPORT_41 */

static int mca_common_cuda_init(void)
{
    int id, value, i, s;
    CUresult res;
    CUcontext cuContext;
    common_cuda_mem_regs_t *mem_reg;

    if (common_cuda_initialized) {
        return OMPI_SUCCESS;
    }

    /* Set different levels of verbosity in the cuda related code. */
    id = mca_base_param_reg_int_name("mpi", "common_cuda_verbose", 
                                     "Set level of common cuda verbosity",
                                     false, false, 0, &mca_common_cuda_verbose);
    mca_common_cuda_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_common_cuda_output, mca_common_cuda_verbose);

    /* Control whether system buffers get CUDA pinned or not.  Allows for 
     * performance analysis. */
    id = mca_base_param_reg_int_name("mpi", "common_cuda_register_memory",
                                     "Whether to cuMemHostRegister preallocated BTL buffers",
                                     false, false, 
                                     (int) mca_common_cuda_register_memory, &value);
    mca_common_cuda_register_memory = OPAL_INT_TO_BOOL(value);

    /* Control whether we see warnings when CUDA memory registration fails.  This is
     * useful when CUDA support is configured in, but we are running a regular MPI
     * application without CUDA. */
    id = mca_base_param_reg_int_name("mpi", "common_cuda_warning",
                                     "Whether to print warnings when CUDA registration fails",
                                     false, false, 
                                     (int) mca_common_cuda_warning, &value);
    mca_common_cuda_warning = OPAL_INT_TO_BOOL(value);

#if OMPI_CUDA_SUPPORT_41
    /* Use this flag to test async vs sync copies */
    id = mca_base_param_reg_int_name("mpi", "common_cuda_memcpy_async",
                                     "Set to 0 to force CUDA sync copy instead of async",
                                     false, false, mca_common_cuda_async, &i);
    mca_common_cuda_async = i;

    /* Use this parameter to increase the number of outstanding events allows */
    id = mca_base_param_reg_int_name("mpi", "common_cuda_event_max",
                                     "Set number of oustanding CUDA events",
                                     false, false, cuda_event_max, &i);
    cuda_event_max = i;
#endif /* OMPI_CUDA_SUPPORT_41 */

    /* Check to see if this process is running in a CUDA context.  If
     * so, all is good.  If not, then disable registration of memory. */
    res = cuCtxGetCurrent(&cuContext);
    if (CUDA_SUCCESS != res) {
        if (mca_common_cuda_warning) {
            /* Check for the not initialized error since we can make suggestions to
             * user for this error. */
            if (CUDA_ERROR_NOT_INITIALIZED == res) {
                orte_show_help("help-mpi-common-cuda.txt", "cuCtxGetCurrent failed not initialized",
                               true);
            } else {
                orte_show_help("help-mpi-common-cuda.txt", "cuCtxGetCurrent failed",
                               true, res);
            }
        }
        mca_common_cuda_enabled = false;
        mca_common_cuda_register_memory = false;
    } else if ((CUDA_SUCCESS == res) && (NULL == cuContext)) {
        if (mca_common_cuda_warning) {
            orte_show_help("help-mpi-common-cuda.txt", "cuCtxGetCurrent returned NULL",
                           true);
        }
        mca_common_cuda_enabled = false;
        mca_common_cuda_register_memory = false;
    } else {
        /* All is good.  mca_common_cuda_register_memory will retain its original
         * value.  Normally, that is 1, but the user can override it to disable
         * registration of the internal buffers. */
        mca_common_cuda_enabled = true;
        opal_output_verbose(20, mca_common_cuda_output,
                            "CUDA: cuCtxGetCurrent succeeded");
    }

#if OMPI_CUDA_SUPPORT_41
    if (true == mca_common_cuda_enabled) {
        /* Set up an array to store outstanding async copy events */
        cuda_event_status_array = NULL;
        cuda_event_frag_array = NULL;
        cuda_event_status_num_used = 0;
        cuda_event_status_first_avail = 0;
        cuda_event_status_first_used = 0;

        cuda_event_status_array = (CUevent *) malloc(sizeof(CUevent) * cuda_event_max);
        if (NULL == cuda_event_status_array) {
            orte_show_help("help-mpi-common-cuda.txt", "No memory",
                           true, errno, strerror(errno));
            return OMPI_ERROR;
        }

        /* Create the events since they can be reused. */
        for (i = 0; i < cuda_event_max; i++) {
            res = cuEventCreate(&cuda_event_status_array[i], CU_EVENT_DISABLE_TIMING);
            if (CUDA_SUCCESS != res) {
                orte_show_help("help-mpi-common-cuda.txt", "cuEventCreate failed",
                               true, res);
                return OMPI_ERROR;
            }
        }

        /* The first available status index is 0.  Make an empty frag
           array. */
        cuda_event_frag_array = (struct mca_btl_base_descriptor_t **)
            malloc(sizeof(struct mca_btl_base_descriptor_t *) * cuda_event_max);
        if (NULL == cuda_event_frag_array) {
            orte_show_help("help-mpi-common-cuda.txt", "No memory",
                           true, errno, strerror(errno));
            return OMPI_ERROR;
        }
    }

#endif /* OMPI_CUDA_SUPPORT_41 */

    s = opal_list_get_size(&common_cuda_memory_registrations);
    for(i = 0; i < s; i++) {
        mem_reg = (common_cuda_mem_regs_t *)
            opal_list_remove_first(&common_cuda_memory_registrations);
        if (mca_common_cuda_enabled && mca_common_cuda_register_memory) {
            res = cuMemHostRegister(mem_reg->ptr, mem_reg->amount, 0);
            if (res != CUDA_SUCCESS) {
                /* If registering the memory fails, print a message and continue.
                 * This is not a fatal error. */
                orte_show_help("help-mpi-common-cuda.txt", "cuMemHostRegister failed",
                               true, mem_reg->ptr, mem_reg->amount,
                               orte_process_info.nodename, res, mem_reg->msg);
            } else {
                opal_output_verbose(20, mca_common_cuda_output,
                                    "CUDA: cuMemHostRegister OK on mpool %s: "
                                    "address=%p, bufsize=%d",
                                    mem_reg->msg, mem_reg->ptr, (int)mem_reg->amount);
            }
        }
        free(mem_reg->msg);
        OBJ_RELEASE(mem_reg);
    }

    opal_output_verbose(30, mca_common_cuda_output,
                        "CUDA: initialized");
    common_cuda_initialized = true;
    return OMPI_SUCCESS;
}

/**
 * Call the CUDA register function so we pin the memory in the CUDA
 * space.
 */
void mca_common_cuda_register(void *ptr, size_t amount, char *msg) {
    int res;

    if (!common_cuda_initialized) {
        common_cuda_mem_regs_t *regptr;
        if (!common_cuda_init_function_added) {
            opal_cuda_add_initialization_function(&mca_common_cuda_init);
            OBJ_CONSTRUCT(&common_cuda_memory_registrations, opal_list_t);
            common_cuda_init_function_added = true;
        }
        regptr = OBJ_NEW(common_cuda_mem_regs_t);
        regptr->ptr = ptr;
        regptr->amount = amount;
        regptr->msg = strdup(msg);
        opal_list_append(&common_cuda_memory_registrations,
                         (opal_list_item_t*)regptr);
        return;
    }

    if (mca_common_cuda_enabled && mca_common_cuda_register_memory) {
        res = cuMemHostRegister(ptr, amount, 0);
        if (res != CUDA_SUCCESS) {
            /* If registering the memory fails, print a message and continue.
             * This is not a fatal error. */
            orte_show_help("help-mpi-common-cuda.txt", "cuMemHostRegister failed",
                           true, ptr, amount, 
                           orte_process_info.nodename, res, msg);
        } else {
            opal_output_verbose(20, mca_common_cuda_output,
                                "CUDA: cuMemHostRegister OK on mpool %s: "
                                "address=%p, bufsize=%d",
                                msg, ptr, (int)amount);
        }
    }
}

/**
 * Call the CUDA unregister function so we unpin the memory in the CUDA
 * space.
 */
void mca_common_cuda_unregister(void *ptr, char *msg) {
    int res, i, s;
    common_cuda_mem_regs_t *mem_reg;

    /* This can happen if memory was queued up to be registered, but
     * no CUDA operations happened, so it never was registered.
     * Therefore, just release any of the resources. */
    if (false == common_cuda_initialized) {
        s = opal_list_get_size(&common_cuda_memory_registrations);
        for(i = 0; i < s; i++) {
            mem_reg = (common_cuda_mem_regs_t *)
                opal_list_remove_first(&common_cuda_memory_registrations);
            free(mem_reg->msg);
            OBJ_RELEASE(mem_reg);
        }
        return;
    }

    if (mca_common_cuda_enabled && mca_common_cuda_register_memory) {
        res = cuMemHostUnregister(ptr);
        if (res != CUDA_SUCCESS) {
            /* If unregistering the memory fails, print a message and continue.
             * This is not a fatal error. */
            orte_show_help("help-mpi-common-cuda.txt", "cuMemHostUnregister failed",
                           true, ptr,
                           orte_process_info.nodename, res, msg);
        } else {
            opal_output_verbose(20, mca_common_cuda_output,
                                "CUDA: cuMemHostUnregister OK on mpool %s: "
                                "address=%p",
                                msg, ptr);
        }
    }
}

#if OMPI_CUDA_SUPPORT_41
/*
 * Get the memory handle of a local section of memory that can be sent
 * to the remote size so it can access the memory.  This is the
 * registration function for the sending side of a message transfer.
 */
int cuda_getmemhandle(void *base, size_t size, mca_mpool_base_registration_t *newreg,
                      mca_mpool_base_registration_t *hdrreg)

{
    CUmemorytype memType;
    CUresult result;
    CUipcMemHandle memHandle;
    CUdeviceptr pbase;
    size_t psize;

    mca_mpool_rcuda_reg_t *cuda_reg = (mca_mpool_rcuda_reg_t*)newreg;

    /* We should only be there if this is a CUDA device pointer */
    result = cuPointerGetAttribute(&memType,
                                   CU_POINTER_ATTRIBUTE_MEMORY_TYPE, (CUdeviceptr)base);
    assert(CUDA_SUCCESS == result);
    assert(CU_MEMORYTYPE_DEVICE == memType);

    /* Get the memory handle so we can send it to the remote process. */
    result = cuIpcGetMemHandle(&memHandle, (CUdeviceptr)base);
    CUDA_DUMP_MEMHANDLE((100, &memHandle, "GetMemHandle-After"));

    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuIpcGetMemHandle failed",
                       true, result, base);
        return OMPI_ERROR;
    } else {
        opal_output_verbose(20, mca_common_cuda_output,
                            "CUDA: cuIpcGetMemHandle passed: base=%p",
                            base);
    }

    /* Need to get the real base and size of the memory handle.  This is
     * how the remote side saves the handles in a cache. */
    result = cuMemGetAddressRange(&pbase, &psize, (CUdeviceptr)base);
    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuMemGetAddressRange failed",
                       true, result, base);
        return OMPI_ERROR;
    } else {
        opal_output_verbose(10, mca_common_cuda_output,
                            "CUDA: cuMemGetAddressRange passed: addr=%p, size=%d, pbase=%p, psize=%d ",
                            base, (int)size, (void *)pbase, (int)psize);
    }

    /* Store all the information in the registration */
    cuda_reg->base.base = (void *)pbase;
    cuda_reg->base.bound = (unsigned char *)pbase + psize - 1;
    memcpy(&cuda_reg->memHandle, &memHandle, sizeof(memHandle));

    /* Need to record the event to ensure that any memcopies
     * into the device memory have completed.  The event handle
     * associated with this event is sent to the remote process
     * so that it will wait on this event prior to copying data
     * out of the device memory. */
    result = cuEventRecord((CUevent)cuda_reg->event, 0);
    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuEventRecord failed",
                       true, result, base);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/*
 * This function is called by the local side that called the cuda_getmemhandle.
 * There is nothing to be done so just return.
 */
int cuda_ungetmemhandle(void *reg_data, mca_mpool_base_registration_t *reg) 
{
    CUDA_DUMP_EVTHANDLE((10, ((mca_mpool_rcuda_reg_t *)reg)->evtHandle, "cuda_ungetmemhandle"));
    opal_output_verbose(5, mca_common_cuda_output,
                        "CUDA: cuda_ungetmemhandle: base=%p",
                        reg_data);
    return OMPI_SUCCESS;
}

/* 
 * Open a memory handle that refers to remote memory so we can get an address
 * that works on the local side.  This is the registration function for the
 * remote side of a transfer.  newreg contains the new handle.  hddrreg contains
 * the memory handle that was received from the remote side.
 */
int cuda_openmemhandle(void *base, size_t size, mca_mpool_base_registration_t *newreg,
                       mca_mpool_base_registration_t *hdrreg)
{
    CUresult result;
    CUipcMemHandle memHandle;
    mca_mpool_rcuda_reg_t *cuda_newreg = (mca_mpool_rcuda_reg_t*)newreg;

    /* Need to copy into memory handle for call into CUDA library. */
    memcpy(&memHandle, cuda_newreg->memHandle, sizeof(memHandle));
    CUDA_DUMP_MEMHANDLE((100, &memHandle, "Before call to cuIpcOpenMemHandle"));

    /* Open the memory handle and store it into the registration structure. */
    result = cuIpcOpenMemHandle((CUdeviceptr *)&newreg->alloc_base, memHandle,
                                CU_IPC_MEM_LAZY_ENABLE_PEER_ACCESS);

    /* If there are some stale entries in the cache, they can cause other
     * registrations to fail.  Let the caller know that so that can attempt
     * to clear them out. */
    if (CUDA_ERROR_ALREADY_MAPPED == result) {
        opal_output_verbose(10, mca_common_cuda_output,
                            "Failed to get handle for p=%p, signal upper layer\n", base);
        return OMPI_ERR_WOULD_BLOCK;
    }
    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuIpcOpenMemHandle failed",
                       true, result, base);
        /* Currently, this is a non-recoverable error */
        return OMPI_ERROR;
    } else {
        opal_output_verbose(10, mca_common_cuda_output,
                            "CUDA: cuIpcOpenMemHandle passed: base=%p",
                            newreg->alloc_base);
        CUDA_DUMP_MEMHANDLE((200, &memHandle, "cuIpcOpenMemHandle"));
    }

    return OMPI_SUCCESS;
}

/* 
 * Close a memory handle that refers to remote memory. 
 */
int cuda_closememhandle(void *reg_data, mca_mpool_base_registration_t *reg)
{
    CUresult result;
    mca_mpool_rcuda_reg_t *cuda_reg = (mca_mpool_rcuda_reg_t*)reg;

    result = cuIpcCloseMemHandle((CUdeviceptr)cuda_reg->base.alloc_base);
    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuIpcCloseMemHandle failed",
                       true, result, cuda_reg->base.alloc_base);
        /* We will just continue on and hope things continue to work. */
    } else {
        opal_output_verbose(10, mca_common_cuda_output,
                            "CUDA: cuIpcCloseMemHandle passed: base=%p",
                            cuda_reg->base.alloc_base);
        CUDA_DUMP_MEMHANDLE((10, cuda_reg->memHandle, "cuIpcCloseMemHandle"));
    }

    return OMPI_SUCCESS;
}

void mca_common_cuda_construct_event_and_handle(uint64_t **event, void **handle)
{
    CUresult result;

    result = cuEventCreate((CUevent *)event, CU_EVENT_INTERPROCESS | CU_EVENT_DISABLE_TIMING);
    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuEventCreate failed",
                       true, result);
    }

    result = cuIpcGetEventHandle((CUipcEventHandle *)handle, (CUevent)*event);
    if (CUDA_SUCCESS != result){
        orte_show_help("help-mpi-common-cuda.txt", "cuIpcGetEventHandle failed",
                       true, result);
    }

    CUDA_DUMP_EVTHANDLE((10, handle, "construct_event_and_handle"));

}

void mca_common_cuda_destruct_event(uint64_t *event)
{
    CUresult result;

    result = cuEventDestroy((CUevent)event);
    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuEventDestroy failed",
                       true, result);
    }
}


/*
 * Put remote event on stream to ensure that the the start of the
 * copy does not start until the completion of the event.
 */
void mca_common_wait_stream_synchronize(mca_mpool_rcuda_reg_t *rget_reg)
{
    CUipcEventHandle evtHandle;
    CUevent event;
    CUresult result;

    memcpy(&evtHandle, rget_reg->evtHandle, sizeof(evtHandle));
    CUDA_DUMP_EVTHANDLE((2, &evtHandle, "stream_synchronize"));

    result = cuIpcOpenEventHandle(&event, evtHandle);
    if (CUDA_SUCCESS != result){
        orte_show_help("help-mpi-common-cuda.txt", "cuIpcOpenEventHandle failed",
                       true, result);
    }

    /* BEGIN of Workaround - There is a bug in CUDA 4.1 RC2 and earlier
     * versions.  Need to record an event on the stream, even though
     * it is not used, to make sure we do not short circuit our way
     * out of the cuStreamWaitEvent test.
     */
    result = cuEventRecord(event, 0);
    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuEventRecord failed",
                       true, result);
    }
    /* END of Workaround */

    result = cuStreamWaitEvent(0, event, 0);
    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuStreamWaitEvent failed",
                       true, result);
    }

    /* All done with this event. */
    result = cuEventDestroy(event);
    if (CUDA_SUCCESS != result) {
        orte_show_help("help-mpi-common-cuda.txt", "cuEventDestroy failed",
                       true, result);
    }
}

/*
 * Start the asynchronous copy.  Then record and save away an event that will
 * be queried to indicate the copy has completed.
 */
int mca_common_cuda_memcpy(void *dst, void *src, size_t amount, char *msg, 
                           struct mca_btl_base_descriptor_t *frag, int *done)
{
    CUresult result;
    int iter;

    /* First make sure there is room to store the event.  If not, then
     * return an error.  The error message will tell the user to try and
     * run again, but with a larger array for storing events. */
    if (cuda_event_status_num_used == cuda_event_max) {
        orte_show_help("help-mpi-common-cuda.txt", "Out of cuEvent handles",
                       true, cuda_event_max, cuda_event_max+100, cuda_event_max+100);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* This is the standard way to run.  Running with synchronous copies is available
     * to measure the advantages of asynchronous copies. */
    if (OPAL_LIKELY(mca_common_cuda_async)) {
        result = cuMemcpyAsync((CUdeviceptr)dst, (CUdeviceptr)src, amount, 0);
        if (CUDA_SUCCESS != result) {
            orte_show_help("help-mpi-common-cuda.txt", "cuMemcpyAsync failed",
                           true, dst, src, amount, result);
            return OMPI_ERROR;
        } else {
            opal_output_verbose(20, mca_common_cuda_output,
                                "CUDA: cuMemcpyAsync passed: dst=%p, src=%p, size=%d",
                                dst, src, (int)amount);
        }
        result = cuEventRecord(cuda_event_status_array[cuda_event_status_first_avail], 0);
        if (CUDA_SUCCESS != result) {
            orte_show_help("help-mpi-common-cuda.txt", "cuEventRecord failed",
                           true, result);
            return OMPI_ERROR;
        }
        cuda_event_frag_array[cuda_event_status_first_avail] = frag;

        /* Bump up the first available slot and number used by 1 */
        cuda_event_status_first_avail++;
        if (cuda_event_status_first_avail >= cuda_event_max) {
            cuda_event_status_first_avail = 0;
        }
        cuda_event_status_num_used++;

        *done = 0;
    } else {
        /* Mimic the async function so they use the same memcpy call. */
        result = cuMemcpyAsync((CUdeviceptr)dst, (CUdeviceptr)src, amount, 0);
        if (CUDA_SUCCESS != result) {
            orte_show_help("help-mpi-common-cuda.txt", "cuMemcpyAsync failed",
                           true, dst, src, amount, result);
            return OMPI_ERROR;
        } else {
            opal_output_verbose(20, mca_common_cuda_output,
                                "CUDA: cuMemcpyAsync passed: dst=%p, src=%p, size=%d",
                                dst, src, (int)amount);
        }

        /* Record an event, then wait for it to complete with calls to cuEventQuery */
        result = cuEventRecord(cuda_event_status_array[cuda_event_status_first_avail], 0);
        if (CUDA_SUCCESS != result) {
            orte_show_help("help-mpi-common-cuda.txt", "cuEventRecord failed",
                           true, result);
            return OMPI_ERROR;
        }

        cuda_event_frag_array[cuda_event_status_first_avail] = frag;

        /* Bump up the first available slot and number used by 1 */
        cuda_event_status_first_avail++;
        if (cuda_event_status_first_avail >= cuda_event_max) {
            cuda_event_status_first_avail = 0;
        }
        cuda_event_status_num_used++;

        result = cuEventQuery(cuda_event_status_array[cuda_event_status_first_used]);
        if ((CUDA_SUCCESS != result) && (CUDA_ERROR_NOT_READY != result)) {
            orte_show_help("help-mpi-common-cuda.txt", "cuEventQuery failed",
                           true, result);
            return OMPI_ERROR;
        }

        iter = 0;
        while (CUDA_ERROR_NOT_READY == result) {
            if (0 == (iter % 10)) {
                opal_output(-1, "EVENT NOT DONE (iter=%d)", iter);
            }
            result = cuEventQuery(cuda_event_status_array[cuda_event_status_first_used]);
            if ((CUDA_SUCCESS != result) && (CUDA_ERROR_NOT_READY != result)) {
                orte_show_help("help-mpi-common-cuda.txt", "cuEventQuery failed",
                               true, result);
                return OMPI_ERROR;
            }
            iter++;
        }

        --cuda_event_status_num_used;
        ++cuda_event_status_first_used;
        if (cuda_event_status_first_used >= cuda_event_max) {
            cuda_event_status_first_used = 0;
        }
        *done = 1;
    }  
    return OMPI_SUCCESS;
}

/*
 * Function is called every time progress is called with the sm BTL.  If there
 * are outstanding events, check to see if one has completed.  If so, hand
 * back the fragment for further processing.
 */
int progress_one_cuda_event(struct mca_btl_base_descriptor_t **frag) {
    CUresult result;

    if (cuda_event_status_num_used > 0) {
        opal_output_verbose(20, mca_common_cuda_output,
                           "CUDA: progress_one_cuda_event, outstanding_events=%d",
                            cuda_event_status_num_used);

        result = cuEventQuery(cuda_event_status_array[cuda_event_status_first_used]);

        /* We found an event that is not ready, so return. */
        if (CUDA_ERROR_NOT_READY == result) {
            opal_output_verbose(20, mca_common_cuda_output,
                                "CUDA: cuEventQuery returned CUDA_ERROR_NOT_READY");
            *frag = NULL;
            return 0;
        } else if (CUDA_SUCCESS != result) {
            orte_show_help("help-mpi-common-cuda.txt", "cuEventQuery failed",
                           true, result);
            *frag = NULL;
            return OMPI_ERROR;
        }

        *frag = cuda_event_frag_array[cuda_event_status_first_used];
        opal_output_verbose(5, mca_common_cuda_output,
                            "CUDA: cuEventQuery returned %d", result);

        /* Bump counters, loop around the circular buffer if necessary */
        --cuda_event_status_num_used;
        ++cuda_event_status_first_used;
        if (cuda_event_status_first_used >= cuda_event_max) {
            cuda_event_status_first_used = 0;
        }
        /* A return value of 1 indicates an event completed and a frag was returned */
        return 1;
    }
    return 0;
}

/**
 * Need to make sure the handle we are retrieving from the cache is still
 * valid.  Compare the cached handle to the one received. 
 */
int mca_common_cuda_memhandle_matches(mca_mpool_rcuda_reg_t *new_reg, 
                                      mca_mpool_rcuda_reg_t *old_reg)
{

    if (0 == memcmp(new_reg->memHandle, old_reg->memHandle, sizeof(new_reg->memHandle))) {
        return 1;
    } else {
        return 0;
    }
            
}

/*
 * Function to dump memory handle information.  This is based on 
 * definitions from cuiinterprocess_private.h.
 */
static void cuda_dump_memhandle(int verbose, void *memHandle, char *str) {

    struct InterprocessMemHandleInternal 
    {
        /* The first two entries are the CUinterprocessCtxHandle */
        int64_t ctxId; /* unique (within a process) id of the sharing context */
        int     pid;   /* pid of sharing context */

        int64_t size;
        int64_t blocksize;
        int64_t offset;
        int     gpuId;
        int     subDeviceIndex;
        int64_t serial;
    } memH;

    if (NULL == str) {
        str = "CUDA";
    }
    memcpy(&memH, memHandle, sizeof(memH));
    opal_output_verbose(verbose, mca_common_cuda_output,
                        "%s:ctxId=%d, pid=%d, size=%d, blocksize=%d, offset=%d, gpuId=%d, "
                        "subDeviceIndex=%d, serial=%d",
                        str, (int)memH.ctxId, memH.pid, (int)memH.size, (int)memH.blocksize, (int)memH.offset,
                        memH.gpuId, memH.subDeviceIndex, (int)memH.serial);
}

/*
 * Function to dump memory handle information.  This is based on 
 * definitions from cuiinterprocess_private.h.
 */
static void cuda_dump_evthandle(int verbose, void *evtHandle, char *str) {

    struct InterprocessEventHandleInternal 
    {
        /* The first two entries are the CUinterprocessCtxHandle */
        int64_t ctxId; /* unique (within a process) id of the sharing context */
        int     pid;   /* pid of sharing context */

        int     pad;   /* pad to match the structure */
        int     index;
    } evtH;

    if (NULL == str) {
        str = "CUDA";
    }
    memcpy(&evtH, evtHandle, sizeof(evtH));
    opal_output_verbose(verbose, mca_common_cuda_output,
                        "%s:ctxId=%d, pid=%d, index=%d",
                        str, (int)evtH.ctxId, evtH.pid, (int)evtH.index);
}


/* Return microseconds of elapsed time. Microseconds are relevant when
 * trying to understand the fixed overhead of the communication. Used 
 * when trying to time various functions.
 *
 * Cut and past the following to get timings where wanted.
 * 
 *   clock_gettime(CLOCK_MONOTONIC, &ts_start);
 *   FUNCTION OF INTEREST
 *   clock_gettime(CLOCK_MONOTONIC, &ts_end);
 *   accum = mydifftime(ts_start, ts_end);
 *   opal_output(0, "Function took   %7.2f usecs\n", accum);
 *
 */
#if CUDA_COMMON_TIMING
static float mydifftime(struct timespec ts_start, struct timespec ts_end) {
    float seconds;
    float microseconds;
    float nanoseconds;

    /* If we did not rollover the seconds clock, then we just take
     * the difference between the nanoseconds clock for actual time */
    if (0 == (ts_end.tv_sec - ts_start.tv_sec)) {
        nanoseconds = (float)(ts_end.tv_nsec - ts_start.tv_nsec);
        return nanoseconds / THOUSAND;
    } else {
        seconds = (float)(ts_end.tv_sec - ts_start.tv_sec); 

        /* Note that this value can be negative or positive
         * which is fine.  In the case that it is negative, it
         * just gets subtracted from the difference which is what
         * we want. */
        nanoseconds = (float)(ts_end.tv_nsec - ts_start.tv_nsec);
        microseconds = (seconds * MILLION) + (nanoseconds/THOUSAND);
        return microseconds;
    }
}
#endif /* CUDA_COMMON_TIMING */

#endif /* OMPI_CUDA_SUPPORT_41 */

/*
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <errno.h>
#include <unistd.h>
#include <cuda.h>

#include "opal/align.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_cuda.h"

static bool initialized = false;
static int opal_cuda_verbose;
static int opal_cuda_output = 0;
static void opal_cuda_support_init(void);
static int (*common_cuda_initialization_function)(void) = NULL;

/* This function allows the common cuda code to register an
 * initialization function that gets called the first time an attempt
 * is made to send or receive a GPU pointer.  This allows us to delay
 * some CUDA initialization until after MPI_Init().
 */
void opal_cuda_add_initialization_function(int (*fptr)(void)) {
    common_cuda_initialization_function = fptr;
}

void mca_cuda_convertor_init(opal_convertor_t* convertor, const void *pUserBuf)
{   
    int res;
    CUmemorytype memType;
    CUdeviceptr dbuf = (CUdeviceptr)pUserBuf;

    res = cuPointerGetAttribute(&memType,
                                CU_POINTER_ATTRIBUTE_MEMORY_TYPE, dbuf);
    if (res != CUDA_SUCCESS) {
        /* If we cannot determine it is device pointer,
         * just assume it is not. */
        return;
    } else if (memType == CU_MEMORYTYPE_HOST) {
        /* Host memory, nothing to do here */
        return;
    }
    /* Must be a device pointer */
    assert(memType == CU_MEMORYTYPE_DEVICE);

    /* Only do the initialization on the first GPU access */
    if (!initialized) {
        opal_cuda_support_init();
    }

    convertor->cbmemcpy = (memcpy_fct_t)&opal_cuda_memcpy;
    convertor->flags |= CONVERTOR_CUDA;
}

/* Checks the type of pointer
 *
 * @param dest   One pointer to check
 * @param source Another pointer to check
 */
bool opal_cuda_check_bufs(char *dest, char *src)
{
    int res;
    CUmemorytype memType = CU_MEMORYTYPE_HOST;

    res = cuPointerGetAttribute(&memType, CU_POINTER_ATTRIBUTE_MEMORY_TYPE, (CUdeviceptr)dest);
    if( memType == CU_MEMORYTYPE_DEVICE){
        return true;
    }
    res = cuPointerGetAttribute(&memType, CU_POINTER_ATTRIBUTE_MEMORY_TYPE, (CUdeviceptr)src);
    if( memType == CU_MEMORYTYPE_DEVICE){
        return true;
    }
    /* Assuming it is a host pointer for all other situations */
    return false;
}

/*
 * With CUDA enabled, all contiguous copies will pass through this function.
 * Therefore, the first check is to see if the convertor is a GPU buffer.
 * Note that if there is an error with any of the CUDA calls, the program
 * aborts as there is no recovering.
 */
void *opal_cuda_memcpy(void *dest, const void *src, size_t size, opal_convertor_t* convertor)
{
    int res;

    if (!(convertor->flags & CONVERTOR_CUDA)) {
        return memcpy(dest, src, size);
    }
            
    if (convertor->flags & CONVERTOR_CUDA_ASYNC) {
        res = cuMemcpyAsync((CUdeviceptr)dest, (CUdeviceptr)src, size,
                            (CUstream)convertor->stream);
    } else {
        res = cuMemcpy((CUdeviceptr)dest, (CUdeviceptr)src, size);
    }

    if (res != CUDA_SUCCESS) {
        opal_output(0, "CUDA: Error in cuMemcpy: res=%d, dest=%p, src=%p, size=%d",
                    res, dest, src, (int)size);
        abort();
    } else {
        return dest;
    }
}

/*
 * This function is needed in cases where we do not have contiguous
 * datatypes.  The current code has macros that cannot handle a convertor
 * argument to the memcpy call.
 */
void *opal_cuda_memcpy_sync(void *dest, void *src, size_t size)
{
    int res;
    res = cuMemcpy((CUdeviceptr)dest, (CUdeviceptr)src, size);
    if (res != CUDA_SUCCESS) {
        opal_output(0, "CUDA: Error in cuMemcpy: res=%d, dest=%p, src=%p, size=%d",
                    res, dest, src, (int)size);
        abort();
    } else {
        return dest;
    }
}

/*
 * In some cases, need an implementation of memmove.  This is not fast, but
 * it is not often needed.
 */
void *opal_cuda_memmove(void *dest, void *src, size_t size)
{
    CUdeviceptr tmp;
    int res;

    res = cuMemAlloc(&tmp,size);
    res = cuMemcpy(tmp, (CUdeviceptr) src, size);
    if(res != CUDA_SUCCESS){
        opal_output(0, "CUDA: memmove-Error in cuMemcpy: res=%d, dest=%p, src=%p, size=%d",
                    res, (void *)tmp, src, (int)size);
        abort();
    }
    res = cuMemcpy((CUdeviceptr) dest, tmp, size);
    if(res != CUDA_SUCCESS){
        opal_output(0, "CUDA: memmove-Error in cuMemcpy: res=%d, dest=%p, src=%p, size=%d",
                    res, dest, (void *)tmp, (int)size);
        abort();
    }
    cuMemFree(tmp);
    return dest;
}

/**
 * This function gets called once to check if the program is running in a cuda
 * environment. 
 */
static void opal_cuda_support_init(void)
{
    int id;
    CUresult res;
    CUcontext cuContext;

    if (initialized) {
        return;
    }

    /* Callback into the common cuda initialization routine. This is only
     * set if some work had been done already in the common cuda code.*/
    if (NULL != common_cuda_initialization_function) {
        common_cuda_initialization_function();
    }

    /* Set different levels of verbosity in the cuda related code. */
    id = mca_base_param_reg_int_name("opal", "cuda_verbose", 
                                     "Set level of opal cuda verbosity",
                                     false, false, 0, &opal_cuda_verbose);
    opal_cuda_output = opal_output_open(NULL);
    opal_output_set_verbosity(opal_cuda_output, opal_cuda_verbose);

    /* Check to see if this process is running in a CUDA context.  If so,
     * all is good.  Currently, just print out a message in verbose mode
     * to help with debugging. */
    res = cuCtxGetCurrent(&cuContext);
    if (CUDA_SUCCESS != res) {
        opal_output_verbose(10, opal_cuda_output,
                            "CUDA: cuCtxGetCurrent failed, CUDA device pointers will not work");
    } else {
        opal_output_verbose(10, opal_cuda_output,
                            "CUDA: cuCtxGetCurrent succeeded, CUDA device pointers will work");
    }

    initialized = true;
}

/**
 * Tell the convertor that copies will be asynchronous CUDA copies.  The
 * flags are cleared when the convertor is reinitialized.
 */
void opal_cuda_set_copy_function_async(opal_convertor_t* convertor, void *stream)
{
    convertor->flags |= CONVERTOR_CUDA_ASYNC;
    convertor->stream = stream;
}

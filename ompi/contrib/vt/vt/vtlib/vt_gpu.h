/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_GPU_H_
#define _VT_GPU_H_

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_inttypes.h"    /* VampirTrace integer types */
#include "vt_thrd.h"        /* thread creation for GPU kernels */
#include "vt_trc.h"         /* VampirTrace events */
#include "vt_error.h"       /* VampirTrace warning and error messages */

/* defines the maximum string length of a function/kernel executed on GPU */
#define VTGPU_KERNEL_STRING_SIZE 256

/* default and maximum buffer size for asynchronous on-device tasks (in bytes) */
#define VTGPU_DEFAULT_BSIZE 8192
#define VTGPU_MAX_BSIZE     2097152 /* 8192^8 bytes */

/* defines for GPU GROUP and GPU COMM (8 bit only!!!) */
#define VTGPU_NO_GPU   0x00 /* thread is no gpu and does no gpu communication */
#define VTGPU_GPU      0x01 /* thread is a GPU thread */
#define VTGPU_GPU_COMM 0x02 /* thread does gpu communication (CPU or GPU) */

/* performance counter available? */
#define VTGPU_NO_PC    0x04 /* no performance counter for this thread available */

/* device/host communication directions (8 bit only!!!) */
#define VTGPU_DEV2HOST  0x00 /* device to host copy */
#define VTGPU_HOST2DEV  0x01 /* host to device copy */
#define VTGPU_DEV2DEV   0x02 /* device to device copy */
#define VTGPU_HOST2HOST 0x04 /* host to host copy */

/****************** common for CUDA driver API and CUPTI **********************/
#if (defined(VT_CUDAWRAP) || defined(VT_CUPTI))

#include "vt_cuda_driver_api.h"

# define CHECK_CU_ERROR(_err, _msg) \
  if(_err != CUDA_SUCCESS){ \
    vt_gpu_handleCuError(_err, _msg, __FILE__,__LINE__); \
  }

/*
 * Handles errors returned from CUDA driver API calls.
 * 
 * @param ecode the CUDA driver API error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
EXTERN void vt_gpu_handleCuError(CUresult ecode, const char* msg,
                                 const char *file, const int line);

#else

# define CHECK_CU_ERROR(_err, _msg)

#endif
/******************************************************************************/

/****************************** CUDA driver API *******************************/
#if (defined(VT_CUDAWRAP))

/* is CUDA driver API tracing suspended? */
# define VTGPU_CUDA_SUSPENDED 0x08

# define VT_SUSPEND_CUDA_TRACING(_tid) vt_gpu_prop[_tid] |= VTGPU_CUDA_SUSPENDED
# define VT_RESUME_CUDA_TRACING(_tid)  vt_gpu_prop[_tid] &= ~VTGPU_CUDA_SUSPENDED
# define VT_CUDA_IS_SUSPENDED(_tid) \
    ((vt_gpu_prop[_tid] & VTGPU_CUDA_SUSPENDED) == VTGPU_CUDA_SUSPENDED)
#else

# define VT_SUSPEND_CUDA_TRACING(tid)
# define VT_RESUME_CUDA_TRACING(tid)
# define VT_CUDA_IS_SUSPENDED(tid)

#endif
/******************************************************************************/

/* 
 * gobal communicator id for all GPU threads
 */
EXTERN uint32_t vt_gpu_groupCID;

/* 
 * communicator for all node local threads communicating with GPU
 */
EXTERN uint32_t vt_gpu_commCID;

/*
 * Process/Thread IDs, which participate in gpu communication.
 * Index of the list is the thread ID (VTThrd...)
 */
EXTERN uint8_t *vt_gpu_prop;

/*
 * flag: Is debugging on? (yes: do not call CUDA functions in finalize)
 */
EXTERN uint8_t vt_gpu_debug;

/* 
 * flag: abort program on GPU error, if enabled 
 */
EXTERN uint8_t vt_gpu_error;

/*
 * Initialization for all GPU API wrappers.
 * VampirTrace IDS have to be locked, before calling this function.
 */
EXTERN void vt_gpu_init(void);

/*
 * Finalization for all GPU API wrappers.
 * VampirTrace IDS have to be locked, before calling this function.
 */
EXTERN void vt_gpu_finalize(void);

/* 
 * Uses VampirTrace Thread API to create a GPU thread
 *
 * @param tname the name of the thread to be registered
 * @param ptid the parent thread id
 * @param vt_tid pointer to the thread id of the thread to be registered
 */
EXTERN void vt_gpu_registerThread(const char* tname, uint32_t ptid,
                                  uint32_t *vt_tid);

#endif /* _VT_GPU_H_ */

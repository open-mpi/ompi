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

/*#if (defined(VT_CUDARTWRAP) || defined(VT_CUDAWRAP) || defined(VT_OPENCLWRAP))*/

#include "vt_inttypes.h"    /* VampirTrace integer types */
#include "vt_thrd.h"        /* thread creation for GPU kernels */
#include "vt_trc.h"         /* VampirTrace events */
#include "vt_error.h"       /* VampirTrace warning and error messages */

/* defines the maximum string length of a function/kernel executed on GPU */
#define VTGPU_KERNEL_STRING_SIZE 256

/* default and maximum buffer size for asynchronous on-device tasks (in bytes) */
#define VTGPU_DEFAULT_BSIZE 8192
#define VTGPU_MAX_BSIZE     2097152 /* 8192^8 bytes */

/* defines for GPU GROUP and GPU COMM */
#define VTGPU_NO_GPU   0x00 /* thread is no gpu and does no gpu communication */
#define VTGPU_GPU      0x01 /* thread is a GPU thread */
#define VTGPU_GPU_COMM 0x02 /* thread does gpu communication (CPU or GPU) */

/* performance counter available? */
#define VTGPU_NO_PC    0x04 /* no performance counter for this thread available */

/*** some specials for the CUDA driver API ***/
#if (defined(VT_CUDAWRAP))
#include "vt_cuda_driver_api.h"

#define CHECK_CU_ERROR(err, cufunc) \
  if(err != CUDA_SUCCESS){ \
    vt_error_msg("Error %d for CUDA Driver API function '%s'.", err, cufunc); \
  }

/* is CUDA driver API tracing suspended? */
# define VTGPU_CUDA_SUSPENDED 0x08

# define VT_SUSPEND_CUDA_TRACING(_tid) vt_gpu_prop[_tid] |= VTGPU_CUDA_SUSPENDED
# define VT_RESUME_CUDA_TRACING(_tid)  vt_gpu_prop[_tid] &= ~VTGPU_CUDA_SUSPENDED
# define VT_CUDA_IS_SUSPENDED(_tid) \
    ((vt_gpu_prop[_tid] & VTGPU_CUDA_SUSPENDED) == VTGPU_CUDA_SUSPENDED)
#else

#define CHECK_CU_ERROR(err, cufunc)

# define VT_SUSPEND_CUDA_TRACING(tid)
# define VT_RESUME_CUDA_TRACING(tid)
# define VT_CUDA_IS_SUSPENDED(tid)

#endif

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
 * Initializion for all GPU API wrappers.
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
 * @param the parent thread id
 * @param vt_tid pointer to the thread id of the thread to be registered
 */
EXTERN void vt_gpu_registerThread(const char* tname, uint32_t ptid,
                                uint32_t *vt_tid);

/*#endif  VT_CUDARTWRAP || VT_CUDAWRAP || VT_OPENCLWRAP */

#endif /* _VT_GPU_H_ */

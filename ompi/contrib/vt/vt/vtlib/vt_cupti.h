/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef VT_CUPTI_H
#define	VT_CUPTI_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

/* Disable all compiler warnings before including the actual
   CUPTI header file. */
#ifdef __GNUC__
# pragma GCC system_header
#endif /* __GNUC__ */
#include "cupti.h"

#define VT_CUPTI_CALL(_err, _msg)                        \
  if(_err != CUPTI_SUCCESS){                             \
    vt_cupti_handleError(_err, _msg,__FILE__, __LINE__); \
  }

/* flag: tracing of cudaMalloc*() and cudaFree*() enabled? */
EXTERN uint8_t vt_cupti_trace_gpu_mem;

/* flag: tracing of kernels enabled? */
EXTERN uint8_t vt_cupti_trace_kernels;

/* flag: tracing of (asynchronous) memory copies enabled? */
EXTERN uint8_t vt_cupti_trace_mcpy;

/* CUPTI global CUDA kernel counter group ID */
EXTERN uint32_t vt_cupti_cgid_cuda_kernel;

/* GPU memory allocation counter */
EXTERN uint32_t vt_cupti_cid_cudaMalloc;

/* global counter IDs for CUPTI callback and activity API */
EXTERN uint32_t vt_cupti_cid_blocksPerGrid;
EXTERN uint32_t vt_cupti_cid_threadsPerBlock;
EXTERN uint32_t vt_cupti_cid_threadsPerKernel;

/*
 * Handles errors returned from CUPTI function calls.
 * 
 * @param ecode the CUDA driver API error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
EXTERN void vt_cupti_handleError(CUptiResult err, const char* msg,
                                 const char *file, const int line);

#endif	/* VT_CUPTI_H */


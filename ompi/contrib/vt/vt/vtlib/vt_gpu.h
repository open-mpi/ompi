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

#ifndef _VT_GPU_H_
#define _VT_GPU_H_

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_defs.h"
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

/* 
 * Get the rank ID for a given VampirTrace thread ID.
 * The MPI RMA functions take the rank ID instead of the VampirTrace process ID!
 */
#define VT_GPU_RANK_ID(thread_id) \
  (VT_PROCESS_ID(vt_my_trace, thread_id)-1)

#if (defined(VT_CUDARTWRAP) || defined(VT_CUPTI))
/*
 * Parse the device function name:
 * "_Z<kernel_length><kernel_name><templates>..." (no name space)
 * "_ZN<ns_length><ns_name>...<ns_length><ns_name><kernel_length>..." (with name space)
 *
 * @param kname the extracted kernel name
 * @param devFunc the CUDA internal kernel function name
 */
EXTERN void vt_cuda_symbolToKernel(char *kname, const char* devFunc);
#endif /* defined(VT_CUDARTWRAP) || defined(VT_CUPTI) */


#if (defined(VT_CUDA) && defined(VT_CUPTI))

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

#else /* defined(VT_CUDA) && defined(VT_CUPTI) */

# define CHECK_CU_ERROR(_err, _msg)

#endif /* defined(VT_CUDA) && defined(VT_CUPTI) */


/* device/host communication directions */
typedef enum {
  VT_GPU_DEV2HOST  = 0x00, /* device to host copy */
  VT_GPU_HOST2DEV  = 0x01, /* host to device copy */
  VT_GPU_DEV2DEV   = 0x02, /* device to device copy */
  VT_GPU_HOST2HOST = 0x04,  /* host to host copy */
  VT_GPU_COPYDIRECTION_UNKNOWN = 0x08  /* unknown */
} vt_gpu_copy_kind_t;

/* 
 * global communicator id for all GPU threads
 */
EXTERN uint32_t vt_gpu_groupCID;

/* 
 * communicator for all node local threads communicating with GPU
 */
EXTERN uint32_t vt_gpu_commCID;

/*
 * Process/Thread IDs, which participate in GPU communication.
 * Index of the list is the thread ID (VTThrd...)
 */
EXTERN uint8_t *vt_gpu_prop;

/*
 * flag: write GPU idle time as region into first GPU stream/queue?
 */
EXTERN uint8_t vt_gpu_trace_idle;

/*
 * flag: Is debugging on? (yes: do not call CUDA functions in finalize)
 */
EXTERN uint8_t vt_gpu_debug;

/* 
 * flag: abort program on GPU error, if enabled 
 */
EXTERN uint8_t vt_gpu_error;

/* 
 * VampirTrace region ID for GPU idle time 
 */
EXTERN uint32_t vt_gpu_rid_idle;

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

/***************************** hashing of strings *****************************/

/* The key of the hash node is a string and the value an unsigned 32bit integer. 
   It is used to store region names with its corresponding region IDs. */
typedef struct vt_gpu_hnString_st {
  char                      *sname; /**< name of the symbol */
  uint32_t                  rid;    /**< associated region group identifier */
  struct vt_gpu_hnString_st *next;  /**< bucket for collision */
} vt_gpu_hn_string_t;

/*
 * Stores a hash value in the hash table.
 * 
 * @param n pointer to a char (string) - the hash nodes key
 * @param rid integer - the hash nodes value
 * 
 * @return pointer to the hash node
 */
EXTERN void* vt_gpu_stringHashPut(const char* n, uint32_t rid);

/*
 * Retrieves the hash node for a given key.
 * 
 * @param n pointer to a char (string) - the hash nodes key
 * 
 * @return pointer to the hash node
 */
EXTERN void* vt_gpu_stringHashGet(const char* n);

/*
 * Clears the hash table. Frees all allocated hash nodes.
 */
EXTERN void vt_gpu_stringhashClear(void);

#endif /* _VT_GPU_H_ */

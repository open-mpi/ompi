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

#ifndef VT_CUPTI_COMMON_H
#define	VT_CUPTI_COMMON_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#include "vt_inttypes.h"
#include "vt_thrd.h"
#include "vt_cuda_driver_api.h"
#include "vt_cupti.h" /* the CUPTI header */

#define VT_CUPTI_NO_CUDA_DEVICE (CUdevice)VT_NO_ID
#define VT_CUPTI_NO_DEVICE_ID VT_NO_ID
#define VT_CUPTI_NO_CONTEXT_ID VT_NO_ID

#define VT_CUPTI_CALL(_err, _msg)                        \
  if(_err != CUPTI_SUCCESS){                             \
    vt_cupti_handleError(_err, _msg,__FILE__, __LINE__); \
  }

/* mutex for locking the CUPTI environment */
#if (defined(VT_MT) || defined(VT_HYB))
EXTERN VTThrdMutex* VTThrdMutexCupti;
# define VT_CUPTI_LOCK() VTThrd_lock(&VTThrdMutexCupti)
# define VT_CUPTI_UNLOCK() VTThrd_unlock(&VTThrdMutexCupti)
#else /* VT_MT || VT_HYB */
# define VT_CUPTI_LOCK()
# define VT_CUPTI_UNLOCK()
#endif /* VT_MT || VT_HYB */

#if defined(VT_CUPTI_EVENTS)
/* 
 * VampirTrace CUPTI event (single linked list element) 
 */
typedef struct vtcuptievtctr_st
{
  CUpti_EventID cuptiEvtID;             /**< CUPTI event ID */
  uint32_t vtCID;                       /**< VampirTrace counter ID */
  /*CUpti_EventDomainID cuptiDomainID;    *< CUPTI domain ID */
  struct vtcuptievtctr_st *next;
}vt_cupti_evtctr_t;

/* 
 * Structure that stores events to be trace for specific device capability 
 * (single linked list element)
 */
typedef struct vtcuptievtdev_st
{
  int dev_major;    /**< Major CUDA device capability */
  int dev_minor;    /**< Minor CUDA device capability */
  CUdevice cuDev;   /**< CUDA device */
  vt_cupti_evtctr_t *vtcuptiEvtList; /**< list of events to be traced for this device*/
  size_t evtNum;    /**< Number of tracable CUPTI events */
  struct vtcuptievtdev_st *next;
}vt_cupti_device_t;

/* 
 * VampirTrace CUPTI event group and its counters and properties.
 */
typedef struct vtcuptievtgrp_st
{
  CUpti_EventGroup evtGrp;   /**< CUPTI event group, created for this context */
  CUpti_EventID *cuptiEvtIDs; /**< CUPTI event IDs to be traced */
  uint32_t *vtCIDs;          /**< VampirTrace counter ids */
  size_t evtNum;             /**< number of CUPTI events in this group */
  uint8_t enabled;           /**< is the threads CUPTI capturing enabled */
  struct vtcuptievtgrp_st *next;
}vt_cupti_evtgrp_t;

/* 
 * VampirTrace CUPTI events specific context data.
 */
typedef struct vtcuptievents_st
{
  vt_cupti_device_t *vtDevCap;   /**< pointer to device capability (events, ...) */
  vt_cupti_evtgrp_t *vtGrpList;  /**< list of VT CUPTI event groups */
  uint64_t *counterData;      /**< preallocated buffer for counter data */
  CUpti_EventID *cuptiEvtIDs; /**< preallocated buffer for CUPTI event IDs*/
}vt_cupti_events_t;
#endif

#if (defined(VT_CUPTI_CALLBACKS) || defined(VT_CUPTI_ACTIVITY))
/* 
 * data structure contains information about allocated CUDA memory
 */
typedef struct vt_cupti_gpumem_st
{
  void *memPtr;                 /**< pointer value to allocated memory */
  size_t size;                  /**< number of bytes allocated */
  uint32_t tid;                 /**< thread id used with this malloc */
  struct vt_cupti_gpumem_st *next;
}vt_cupti_gpumem_t;
#endif /* VT_CUPTI_CALLBACKS || VT_CUPTI_ACTIVITY */

#if defined(VT_CUPTI_CALLBACKS)
/* 
 * structure of a VampirTrace CUPTI CUDA stream
 */
typedef struct vt_cupticb_strm_st
{
  CUstream stream;             /**< the CUDA stream */
  uint32_t tid;                /**< VT thread id for this stream (unique) */
  struct vt_cupticb_strm_st *next;
}vt_cupticb_strm_t;

/* 
 * structure of a VampirTrace CUPTI CUDA runtime kernel
 */
typedef struct vt_cupticb_kernel_st
{
  cudaStream_t stream;        /**< the CUDA stream */
  uint32_t blocksPerGrid;     /**< number of blocks per grid */
  uint32_t threadsPerBlock;   /**< number of threads per block */
  struct vt_cupticb_kernel_st *prev;
}vt_cupti_kernel_t;


/* 
 * VampirTrace CUPTI callbacks specific context data.
 */
typedef struct vtcupticallbacks_st
{
# if defined(VT_CUPTI_ACTIVITY)
  uint8_t concurrentKernels;
  uint8_t streamsCreated;
# else
  vt_cupti_gpumem_t *gpuMemList;    /**< list of allocated GPU memory fields */
  size_t gpuMemAllocated;           /**< memory allocated on CUDA device */
  vt_cupticb_strm_t *strmList;      /**< CUDA stream list */
  uint32_t strmNum;           /**< Number of streams created for this device */
  vt_cupti_kernel_t *kernelData;    /**< pointer to top of CUDA runtime kernel 
                                         configuration stack */
  uint8_t stack_size;               /**< number of params on the stack */
  uint8_t callbacks_enabled;        /**< execute callback function? */
# endif
}vt_cupti_callbacks_t;
#endif

#if defined(VT_CUPTI_ACTIVITY)
/* 
 * VampirTrace CUPTI activity synchronization structure
 */
typedef struct vt_cuptiact_sync_st
{
  uint64_t hostStart;   /**< host measurement interval start timestamp */
  uint64_t hostStop;    /**< host measurement interval stop timestamp */
  uint64_t gpuStart;    /**< gpu measurement interval start timestamp */
  double factor;        /**< synchronization factor for time interval */
}vt_cupti_sync_t;

/* 
 * VampirTrace CUPTI activity stream
 */
typedef struct vt_cuptiact_strm_st
{
  uint32_t strmID;             /**< the CUDA stream */
  uint32_t vtThrdID;           /**< VT thread id for this stream (unique) */
  uint64_t vtLastTime;         /**< last written VampirTrace timestamp */
  uint8_t destroyed;           /**< Is stream destroyed? Ready for reuse? */
  struct vt_cuptiact_strm_st *next;
}vt_cuptiact_strm_t;

/* 
 * VampirTrace CUPTI activity specific context data.
 */
typedef struct vtcuptiactivity_st
{
  vt_cuptiact_strm_t *strmList;     /**< list of streams */
  uint32_t defaultStrmID;           /**< CUPTI stream ID of default stream */
  vt_cupti_gpumem_t *gpuMemList; /**< list of allocated GPU memory fields */
  size_t gpuMemAllocated;           /**< memory allocated on CUDA device */
  vt_cupti_sync_t sync;          /**< store synchronization information */
  uint8_t *buffer;                  /**< CUPTI activity buffer pointer */
  uint64_t vtLastGPUTime;           /**< last written VampirTrace timestamp */
  uint8_t gpuIdleOn;                /**< has idle region enter been written last */
}vt_cupti_activity_t;
#endif

/* 
 * VampirTrace CUPTI context.
 */
typedef struct vtcuptictx_st
{
  CUcontext cuCtx;                  /**< CUDA context handle */
  uint32_t ctxID;                   /**< context ID */
  uint32_t devID;                   /**< device ID */
  CUdevice cuDev;                   /**< CUDA device handle */
  uint32_t ptid;                    /**< VampirTrace process/thread */
#if defined(VT_CUPTI_EVENTS)
  vt_cupti_events_t* events;
#endif
#if defined(VT_CUPTI_ACTIVITY)
  vt_cupti_activity_t* activity;
#endif
#if defined(VT_CUPTI_CALLBACKS)
  vt_cupti_callbacks_t* callbacks;
#endif
  struct vtcuptictx_st *next;
}vt_cupti_ctx_t;

EXTERN vt_cupti_ctx_t *vt_cupti_ctxList;

/* CUPTI global CUDA kernel counter group ID */
EXTERN uint32_t vt_cupti_cgid_cuda_kernel;

/* global counter IDs for CUPTI callback and activity API */
EXTERN uint32_t vt_cupti_cid_blocksPerGrid;
EXTERN uint32_t vt_cupti_cid_threadsPerBlock;
EXTERN uint32_t vt_cupti_cid_threadsPerKernel;

EXTERN void vt_cupti_init(void);

EXTERN void vt_cupti_finalize(void);

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

/*
 * Create a VampirTrace CUPTI context.
 * 
 * @param cuCtx CUDA context
 * @param cuDev CUDA device
 * @param ctxID ID of the CUDA context
 * @param devID ID of the CUDA device
 * 
 * @return pointer to created VampirTrace CUPTI context
 */
EXTERN vt_cupti_ctx_t* vt_cupti_createCtx(CUcontext cuCtx,
                                          CUdevice cuDev,
                                          uint32_t cuCtxID,
                                          uint32_t devID);

/*
 * Prepend the given VampirTrace CUPTI context to the global context list.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context to be prepended
 */
EXTERN void vt_cupti_prependCtx(vt_cupti_ctx_t *vtCtx);

/*
 * Get a VampirTrace CUPTI context by CUDA context.
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI context
 */
EXTERN vt_cupti_ctx_t* vt_cupti_getCtx(CUcontext cuCtx);

/*
 * Get a VampirTrace CUPTI context by CUDA context without locking.
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI context
 */
EXTERN vt_cupti_ctx_t* vt_cupti_getCtxNoLock(CUcontext cuCtx);

/*
 * Retrieves the VampirTrace CUPTI context for the CUDA context associated with
 * the calling host thread.
 * 
 * @param ptid the VampirTrace process/thread ID
 * 
 * @return VampirTrace CUPTI context
 */
EXTERN vt_cupti_ctx_t* vt_cupti_getCurrentCtx(uint32_t ptid);

/*
 * Get or if not available create a VampirTrace CUPTI context by CUDA context.
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI context
 */
EXTERN vt_cupti_ctx_t* vt_cupti_getCreateCtx(CUcontext cuCtx);

/*
 * Remove a context from the global context list and return it.
 * 
 * @param cuCtx pointer to the CUDA context
 * 
 * @return the VampirTrace CUPTI context, which has been removed 
 */
EXTERN vt_cupti_ctx_t* vt_cupti_removeCtx(CUcontext *cuCtx);

/*
 * Free the allocated memory for this VampirTrace CUPTI context.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context
 */
EXTERN void vt_cupti_freeCtx(vt_cupti_ctx_t *vtCtx);

#endif	/* VT_CUPTI_COMMON_H */


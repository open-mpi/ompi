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

#include "vt_env.h"             /* get environment variables */
#include "vt_pform.h"           /* VampirTrace time measurement */
#include "vt_gpu.h"             /* common for GPU */
#include "vt_cupti.h"           /* Support for CUPTI */
#include "vt_cupti_activity.h"

#include "stdio.h"

/* reduce buffer size for alignment, if necessary */
#define ALIGN_BUFFER(buffer, align) \
  (((uintptr_t) (buffer) & ((align)-1)) ? \
        ((buffer) - ((uintptr_t) (buffer) & ((align)-1))) : (buffer)) 

#define VT_CUPTI_ACT_DEFAULT_BSIZE 65536

/* mutex for locking global CUPTI activity attributes */
#if (defined(VT_MT) || defined(VT_HYB))
static VTThrdMutex* VTThrdMutexCuptiAct = NULL;
# define VT_CUPTI_ACT_LOCK() VTThrd_lock(&VTThrdMutexCuptiAct)
# define VT_CUPTI_ACT_UNLOCK() VTThrd_unlock(&VTThrdMutexCuptiAct)
#else /* VT_MT || VT_HYB */
# define VT_CUPTI_ACT_LOCK()
# define VT_CUPTI_ACT_UNLOCK()
#endif /* VT_MT || VT_HYB */

/*
 * Register the finalize function before the CUDA and CUPTI library clean up 
 * their data.
 
#define VT_CUPTI_ACT_REGISTER_FINALIZE                         \
  if(!vt_cuptiact_finalize_registered){                        \
    VT_CUPTI_ACT_LOCK(); \
    if(!vt_cuptiact_finalize_registered){                      \
      atexit(vt_cupti_activity_finalize);                      \
      vt_cntl_msg(2, "[CUPTI Activity] Finalize registered!"); \
      vt_cuptiact_finalize_registered = 1;                     \
    }                                                          \
    VT_CUPTI_ACT_UNLOCK(); \
  }
*/

/* 
 * VampirTrace CUPTI activity synchronization structure
 */
typedef struct vt_cuptiact_sync_st
{
  uint64_t hostStart;   /**< host measurement interval start timestamp */
  uint64_t hostStop;    /**< host measurement interval stop timestamp */
  uint64_t gpuStart;    /**< gpu measurement interval start timestamp */
  double factor;        /**< synchronization factor for time interval */
}vt_cuptiact_sync_t;

/* 
 * structure of a VampirTrace CUDA malloc (initiated with cudaMalloc*() 
 */
typedef struct vt_cuptiact_gpumem_st
{
  void *memPtr;                 /**< pointer value to allocated memory */
  size_t size;                  /**< number of bytes allocated */
  uint32_t tid;                 /**< thread id used with this malloc */
  struct vt_cuptiact_gpumem_st *next;
}vt_cuptiact_gpumem_t;

/* 
 * VampirTrace CUPTI activity stream
 */
typedef struct vt_cuptiact_strm_st
{
  uint32_t strmID;             /**< the CUDA stream */
  uint32_t vtThrdID;           /**< VT thread id for this stream (unique) */
  uint64_t vtLastTime;         /**< last written VampirTrace timestamp */
  struct vt_cuptiact_strm_st *next;
}vt_cuptiact_strm_t;

/* 
 * VampirTrace CUPTI activity context.
 */
typedef struct vtcuptiactctx_st
{
  uint32_t ctxID;                   /**< context ID */
  CUcontext cuCtx;                  /**< CUDA context handle */
  uint32_t devID;                   /**< device ID */
  CUdevice cuDev;                   /**< CUDA device handle */
  uint32_t ptid;                    /**< VampirTrace process/thread */
  vt_cuptiact_strm_t *strmList;     /**< list of streams */
  vt_cuptiact_gpumem_t *gpuMemList; /**< list of allocated GPU memory fields */
  size_t gpuMemAllocated;           /**< memory allocated on CUDA device */
  vt_cuptiact_sync_t sync;          /**< store synchronization information */
  uint8_t *buffer;                  /**< CUPTI activity buffer pointer */
  uint64_t vtLastGPUTime;           /**< last written VampirTrace timestamp in this context */
  uint8_t gpuIdleOn;                /**< has idle region enter been written last */
  struct vtcuptiactctx_st *next;
}vt_cuptiact_ctx_t;

/* initialization and finalization flags */
static uint8_t vt_cuptiact_initialized = 0;
static uint8_t vt_cuptiact_finalized = 0;
/*static uint8_t vt_cuptiact_finalize_registered = 0;*/

/* VampirTrace global CUPTI activity buffer 
static uint8_t *vt_cuptiact_global_buffer = NULL;*/

/* size of the activity buffer */
static size_t vt_cuptiact_bufSize = VT_CUPTI_ACT_DEFAULT_BSIZE;

/* cupti activity specific kernel counter IDs */
static uint32_t vt_cuptiact_cid_knStaticSharedMem = VT_NO_ID;
static uint32_t vt_cuptiact_cid_knDynamicSharedMem = VT_NO_ID;
static uint32_t vt_cuptiact_cid_knLocalMemTotal = VT_NO_ID;
static uint32_t vt_cuptiact_cid_knRegistersPerThread = VT_NO_ID;

/* list of VampirTrace CUPTI activity context list */
static vt_cuptiact_ctx_t* vt_cuptiact_ctxList = NULL;

/*********************** function declarations ***************************/

static vt_cuptiact_ctx_t* vt_cuptiact_createContext(uint32_t ctxID, 
                                                    CUcontext cuCtx, 
                                                    uint32_t devID);

static void vt_cuptiact_destroyContext(vt_cuptiact_ctx_t* vtCtx);

static vt_cuptiact_ctx_t* vt_cuptiact_getCtx(CUcontext cuCtx);

static void vt_cuptiact_writeRecord(CUpti_Activity *record, 
                                    vt_cuptiact_ctx_t *vtCtx);

static void vt_cuptiact_writeMemcpyRecord(CUpti_ActivityMemcpy *mcpy, 
                                          vt_cuptiact_ctx_t *vtCtx);

static void vt_cuptiact_writeKernelRecord(CUpti_ActivityKernel *kernel, 
                                          vt_cuptiact_ctx_t *vtCtx);

/******************************************************************************/

void vt_cupti_activity_init()
{
  if(!vt_cuptiact_initialized){
#if (defined(VT_MT) || defined(VT_HYB))
    VTThrd_createMutex(&VTThrdMutexCuptiAct);
#endif
    VT_CUPTI_ACT_LOCK();
    if(!vt_cuptiact_initialized){
      vt_cntl_msg(2, "[CUPTI Activity] Initializing ... ");
      
      {        
        vt_cuptiact_bufSize = vt_env_cudatrace_bsize();
        
        /* no buffer size < 1024 bytes allowed (see CUPTI documentation) */
        if(vt_cuptiact_bufSize < 1024){
          if(vt_cuptiact_bufSize > 0){
            vt_warning("[CUPTI Activity] Buffer size has to be at least 1024 "
                       "bytes! It has been set to %d.", vt_cuptiact_bufSize);
          }
          vt_cuptiact_bufSize = VT_CUPTI_ACT_DEFAULT_BSIZE;
        }
        
        /* queue a global buffer to initialize CUPTI before CUDA init 
        vt_cuptiact_buffer = (uint8_t *)malloc(vt_cuptiact_bufSize);
        VT_CUPTI_CALL(cuptiActivityEnqueueBuffer(NULL, 0, 
                                      vt_cuptiact_buffer, vt_cuptiact_bufSize), 
                      "cuptiActivityEnqueueBuffer");*/
      }
      
      if(vt_cupti_trace_kernels > 1){
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
        vt_cuptiact_cid_knStaticSharedMem = vt_def_counter(VT_MASTER_THREAD, 
                      "staticSharedMemory", "Bytes",
                      VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                      vt_cupti_cgid_cuda_kernel, 0);
        vt_cuptiact_cid_knDynamicSharedMem = vt_def_counter(VT_MASTER_THREAD, 
                      "dynamicSharedMemory", "Bytes",
                      VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                      vt_cupti_cgid_cuda_kernel, 0);
        vt_cuptiact_cid_knLocalMemTotal = vt_def_counter(VT_MASTER_THREAD, 
                      "localMemoryPerKernel", "Bytes",
                      VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                      vt_cupti_cgid_cuda_kernel, 0);
        vt_cuptiact_cid_knRegistersPerThread = vt_def_counter(VT_MASTER_THREAD, 
                      "registersPerThread", "#",
                      VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                      vt_cupti_cgid_cuda_kernel, 0);
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif
      }
      
      /*** enable the activities ***/
      /* enable kernel tracing */
      if(vt_cupti_trace_kernels > 0){
        VT_CUPTI_CALL(cuptiActivityEnable(CUPTI_ACTIVITY_KIND_KERNEL), 
                      "cuptiActivityEnable");
      }
      
      /* enable memory copy tracing */
      if(vt_cupti_trace_mcpy){
        VT_CUPTI_CALL(cuptiActivityEnable(CUPTI_ACTIVITY_KIND_MEMCPY), 
                      "cuptiActivityEnable");
      }
      
      /* register the finalize function of VampirTrace CUPTI to be called before
       * the program exits */
      atexit(vt_cupti_activity_finalize);

      vt_cuptiact_initialized = 1;
      VT_CUPTI_ACT_UNLOCK();
    }
  }
}

void vt_cupti_activity_finalize()
{
  if(!vt_cuptiact_finalized && vt_cuptiact_initialized){
    VT_CUPTI_ACT_LOCK();
    if(!vt_cuptiact_finalized && vt_cuptiact_initialized){      
      vt_cntl_msg(2, "[CUPTI Activity] Finalizing ... ");
      
      vt_cuptiact_finalized = 1;
      VT_CUPTI_ACT_UNLOCK();
 
      while(vt_cuptiact_ctxList != NULL){
        vt_cuptiact_ctx_t *vtCtx = vt_cuptiact_ctxList;
        
        /* write buffered activities, which have not been dumped yet */
        vt_cuptiact_flushCtxActivities(vtCtx->cuCtx);
        
        /* set pointer to next context before freeing current one */
        vt_cuptiact_ctxList = vt_cuptiact_ctxList->next;

        /* free the context */
        vt_cuptiact_destroyContext(vtCtx);
      }

#if (defined(VT_MT) || defined (VT_HYB))
      VTTHRD_LOCK_ENV();
      VTThrd_deleteMutex(&VTThrdMutexCuptiAct);
      VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB */
    }
  }
}

/*
 * Allocate a new buffer and add it to the queue specified by a CUDA context.
 * 
 * @param cuCtx the CUDA context, specifying the queue
 */
static uint8_t* vt_cuptiact_queueNewBuffer(CUcontext cuCtx)
{
  uint8_t *buffer = (uint8_t *)malloc(vt_cuptiact_bufSize);
	
  VT_CUPTI_CALL(cuptiActivityEnqueueBuffer(cuCtx, 0, ALIGN_BUFFER(buffer, 8), 
                                           vt_cuptiact_bufSize), 
                "cuptiActivityEnqueueBuffer");
  
  return buffer;
}

void vt_cuptiact_addContext(CUcontext cuCtx, CUdevice cuDev)
{
  vt_cuptiact_ctx_t *vtCtx = NULL;
  
  if(vt_cuptiact_getCtx(cuCtx) != NULL) return;
  
  /*vt_cntl_msg(1, "ctx: %d dev: %d", cuCtx, cuDev);*/
  
  vtCtx = vt_cuptiact_createContext((uint32_t)-1, cuCtx, (uint32_t)cuDev);
  
  /* prepend context */
  VT_CUPTI_ACT_LOCK();
    vtCtx->next = vt_cuptiact_ctxList;
    vt_cuptiact_ctxList = vtCtx;
  VT_CUPTI_ACT_UNLOCK();
  
  /* queue new buffer to context to record activities*/
  vtCtx->buffer = vt_cuptiact_queueNewBuffer(cuCtx);
}
  
/*
 * Create a VampirTrace CUPTI Activity stream.
 * 
 * @param devID ID of the CUDA device
 * @param strmID ID of the CUDA stream
 * 
 * @return pointer to created VampirTrace CUPTI Activity stream
 */
static vt_cuptiact_strm_t* vt_cuptiact_createStream(vt_cuptiact_ctx_t *vtCtx, 
                                                    uint32_t strmID)
{
  vt_cuptiact_strm_t *vtStrm = NULL;
          
  vtStrm = (vt_cuptiact_strm_t *)malloc(sizeof(vt_cuptiact_strm_t));
  if(vtStrm == NULL)
    vt_error_msg("[CUPTI Activity] Could not allocate memory for stream!");
  vtStrm->strmID = strmID;
  vtStrm->vtLastTime = vt_start_time;
  vtStrm->next = NULL;
  
  /* if no valid stream ID is given, create default stream 0 */
  if((uint32_t)-1 == strmID) strmID = 0;
  
  {
    char thread_name[16];
    
    /* create VT-User-Thread with name and parent id and get its id */
    if(vtCtx->devID == VT_NO_ID){
      if(-1 == snprintf(thread_name, 15, "CUDA[?:%d]", strmID))
        vt_cntl_msg(1, "Could not create thread name for CUDA thread!");
    }else{
      if(-1 == snprintf(thread_name, 15, "CUDA[%d:%d]", vtCtx->devID, strmID))
        vt_cntl_msg(1, "Could not create thread name for CUDA thread!");
    }
    
    VT_CHECK_THREAD;
    vt_gpu_registerThread(thread_name, VT_MY_THREAD, &(vtStrm->vtThrdID));
  }
  
    /* if first stream created for this device, make it the default stream */
  if(vtCtx->strmList == NULL){
    /* write enter event for GPU_IDLE on first stream */
    if(vt_gpu_trace_idle == 1){
      vt_enter(vtStrm->vtThrdID, &vt_start_time, vt_gpu_rid_idle);
      /*vt_warning("IDLEente: %llu (%d)", vt_start_time, vtStrm->vtThrdID);*/
    }
    vtCtx->gpuIdleOn = 1;
  }
  
  return vtStrm;
}

/*
 * Create a VampirTrace CUPTI Activity context.
 * 
 * @param ctxID ID of the CUDA context
 * @param devID ID of the CUDA device
 * 
 * @return pointer to created VampirTrace CUPTI Activity context
 */
static vt_cuptiact_ctx_t* vt_cuptiact_createContext(uint32_t ctxID, 
                                                    CUcontext cuCtx, 
                                                    uint32_t devID)
{
  vt_cuptiact_ctx_t* vtCtx = NULL;
  
  /* create new context, as it is not listed */
  vtCtx = (vt_cuptiact_ctx_t *)malloc(sizeof(vt_cuptiact_ctx_t));
  if(vtCtx == NULL) 
    vt_error_msg("[CUPTI Activity] Could not allocate memory for context!");
  vtCtx->ctxID = ctxID;
  vtCtx->next = NULL;
  vtCtx->strmList = NULL;
  vtCtx->gpuMemAllocated = 0;
  vtCtx->gpuMemList = NULL;
  vtCtx->buffer = NULL;
  vtCtx->vtLastGPUTime = vt_start_time;
  vtCtx->gpuIdleOn = 1;
  
  /* 
   * Get time synchronization factor between host and GPU time for measurement 
   * interval 
   */
  {
    VT_CUPTI_CALL(cuptiGetTimestamp(&(vtCtx->sync.gpuStart)), "cuptiGetTimestamp");
    vtCtx->sync.hostStart = vt_pform_wtime();
  }
  
  VT_CHECK_THREAD;
  vtCtx->ptid = VT_MY_THREAD;
  
  if(cuCtx == NULL) CHECK_CU_ERROR(cuCtxGetCurrent(&cuCtx), NULL);
  vtCtx->cuCtx = cuCtx;
  
  if(devID == (uint32_t)-1){
    CUdevice cuDev;
    
    /* driver API prog: correct cuDev, but result is 201 (invalid context) */
    if(CUDA_SUCCESS != cuCtxGetDevice(&cuDev)){
      devID = VT_NO_ID;
    }else{
      devID = (uint32_t)cuDev;
    }
  }
  
  vtCtx->devID = devID;
  vtCtx->cuDev = devID;
  
  /*vt_cntl_msg(1,"device id: %d", devID);*/
  
  return vtCtx;
}

/*
 * Destroy a VampirTrace CUPTI Activity context.
 * 
 * @param vtCtx VampirTrace CUPTI Activity context
 */
static void vt_cuptiact_destroyContext(vt_cuptiact_ctx_t* vtCtx)
{
  /* write exit event for GPU idle time */
  if(vt_gpu_trace_idle == 1){
    uint64_t idle_end = vt_pform_wtime();
    vt_exit(vtCtx->strmList->vtThrdID, &idle_end);
    /*vt_warning("IDLEexit: %llu (%d)", idle_end, vtCtx->strmList->vtThrdID);*/
    vtCtx->gpuIdleOn = 0;
  }
  
  /* cleanup stream list */
  while(vtCtx->strmList != NULL){
    vt_cuptiact_strm_t *vtStrm = vtCtx->strmList;
    
    vtCtx->strmList = vtCtx->strmList->next;
    
    free(vtStrm);
    vtStrm = NULL;
  }
  
  /* free CUDA malloc entries, if user application has memory leaks */
  while(vtCtx->gpuMemList != NULL){
    vt_cuptiact_gpumem_t *vtMem =  vtCtx->gpuMemList;
    
    if(vt_cupti_trace_gpu_mem > 1)
      vt_cntl_msg(1, "[CUPTI Activity] Free of %d bytes GPU memory missing!", 
                     vtMem->size);
    
    vtCtx->gpuMemList = vtMem->next;
    free(vtMem);
    vtMem = NULL;
  }
  
  /* free activity buffer */
  if(vtCtx->buffer != NULL){
    free(vtCtx->buffer);
  }
  
  free(vtCtx);
}

/*
 * Get a VampirTrace CUPTI Activity context by CUDA context
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI Activity context
 */
static vt_cuptiact_ctx_t* vt_cuptiact_getCtx(CUcontext cuCtx)
{
  vt_cuptiact_ctx_t* vtCtx = NULL;
  
  /* lookup context */
  VT_CUPTI_ACT_LOCK();
  vtCtx = vt_cuptiact_ctxList;
  while(vtCtx != NULL){
    if(vtCtx->cuCtx == cuCtx){
      VT_CUPTI_ACT_UNLOCK();
      return vtCtx;
    }
    vtCtx = vtCtx->next;
  }
  VT_CUPTI_ACT_UNLOCK();
  
  return NULL;
}

/*
 * Check for a VampirTrace activity stream by stream ID. If it does not exist,
 * create it.
 * 
 * @param vtCtx VampirTrace CUPTI Activity context
 * @param strmID the CUDA stream ID provided by CUPTI callback API
 * 
 * @return the VampirTrace CUDA stream
 */
static vt_cuptiact_strm_t* vt_cuptiact_checkStream(vt_cuptiact_ctx_t* vtCtx, 
                                                   uint32_t strmID)
{
  vt_cuptiact_strm_t *currStrm = NULL;
  vt_cuptiact_strm_t *lastStrm = NULL;
  
  if(vtCtx == NULL){
    vt_warning("[CUPTI Activity] No context given!");
    return NULL;
  }
  
  /* lookup stream */
  /*VT_CUPTI_ACT_LOCK();*/
  currStrm = vtCtx->strmList;
  lastStrm = vtCtx->strmList;
  while(currStrm != NULL){
    if(currStrm->strmID == strmID){
      /*VT_CUPTI_ACT_UNLOCK();*/
      return currStrm;
    }
    lastStrm = currStrm;
    currStrm = currStrm->next;
  }
  
  /* check if stream 0 has already been created */
  if(vtCtx->strmList != NULL && vtCtx->strmList->strmID == (uint32_t)-1){
    vtCtx->strmList->strmID = strmID;
    return vtCtx->strmList;
  }
  
  currStrm = vt_cuptiact_createStream(vtCtx, strmID);
  
  /* append */
  if(NULL != lastStrm) lastStrm->next = currStrm;
  else vtCtx->strmList = currStrm;
  
  /*VT_CUPTI_ACT_UNLOCK();*/
  return currStrm;
}

void vt_cuptiact_flushCtxActivities(CUcontext cuCtx)
{ 
  CUptiResult status;
  uint8_t *buffer = NULL;
  size_t bufSize;
  CUpti_Activity *record = NULL;
  vt_cuptiact_ctx_t *vtCtx = NULL;
  uint64_t hostStop, gpuStop;
  
  /* check if the buffer contains records */
  status = cuptiActivityQueryBuffer(cuCtx, 0, &bufSize);
  if(status != CUPTI_SUCCESS){
    if(CUPTI_ERROR_QUEUE_EMPTY == status || 
       CUPTI_ERROR_MAX_LIMIT_REACHED != status){
      return;
    }
  }
  
  /* get the corresponding VampirTrace CUPTI context */
  vtCtx = vt_cuptiact_getCtx(cuCtx);
  if(vtCtx == NULL){
    vt_warning("[CUPTI Activity] Context not found!");
    return;
  }
  
  vt_cntl_msg(2,"[CUPTI Activity] Handle context %d activities", cuCtx);
  
  /* lock the whole buffer flush */
  VT_CUPTI_ACT_LOCK();
  
  /* dump the contents of the global queue */
  VT_CUPTI_CALL(cuptiActivityDequeueBuffer(cuCtx, 0, &buffer, 
                &bufSize), "cuptiActivityDequeueBuffer");

  /* 
   * Get time synchronization factor between host and GPU time for measured 
   * period 
   */
  {
    VT_CUPTI_CALL(cuptiGetTimestamp(&gpuStop), "cuptiGetTimestamp");
    hostStop = vt_pform_wtime();
    vtCtx->sync.hostStop = hostStop;
    
    vtCtx->sync.factor = (double)(hostStop - vtCtx->sync.hostStart)
                     /(double)(gpuStop - vtCtx->sync.gpuStart);
    
  }

  /*vt_cntl_msg(1, "hostStop: %llu , gpuStop: %llu", hostStopTS, gpuStopTS);
  vt_cntl_msg(1, "factor: %lf", syncFactor);*/
  
  do{
    status = cuptiActivityGetNextRecord(buffer, bufSize, &record);
    if(status == CUPTI_SUCCESS) {
      vt_cuptiact_writeRecord(record, vtCtx);
    }else if(status == CUPTI_ERROR_MAX_LIMIT_REACHED){
      break;
    }else{
      VT_CUPTI_CALL(status, "cuptiActivityGetNextRecord");
    }
  }while(1);

  /* report any records dropped from the global queue */
  {
    size_t dropped;
    
    VT_CUPTI_CALL(cuptiActivityGetNumDroppedRecords(cuCtx, 0, &dropped), 
                  "cuptiActivityGetNumDroppedRecords");
    if(dropped != 0)
      vt_warning("[CUPTI Activity] Dropped %u records. Current buffer size: %llu \n"
                 "To avoid dropping of records increase the buffer size!\n"
                 "Proposed minimum VT_CUDATRACE_BUFFER_SIZE=%llu", 
                 (unsigned int)dropped, vt_cuptiact_bufSize, 
                 vt_cuptiact_bufSize + dropped/2 * 
                 (sizeof(CUpti_ActivityKernel) + sizeof(CUpti_ActivityMemcpy)));
  }
  
  /* enter GPU idle region after last kernel, if exited before */
  if(vtCtx->gpuIdleOn == 0){
    vt_enter(vtCtx->strmList->vtThrdID, &(vtCtx->vtLastGPUTime), vt_gpu_rid_idle);
    vtCtx->gpuIdleOn = 1;
    /*vt_warning("IDLfente: %llu (%d)", vtCtx->vtLastGPUTime, vtCtx->strmList->vtThrdID);*/
  }
  
  /* enqueue buffer again */
  VT_CUPTI_CALL(cuptiActivityEnqueueBuffer(cuCtx, 0, buffer, 
                vt_cuptiact_bufSize), "cuptiActivityEnqueueBuffer");
  
    
  /* set new synchronization point */
  vtCtx->sync.hostStart = hostStop;
  vtCtx->sync.gpuStart = gpuStop;
  
  VT_CUPTI_ACT_UNLOCK();
}

/*
 * Select record type and call respective function.
 * 
 * @param record the basic CUPTI activity record
 * @param vtCtx the VampirTrace CUPTI activity context
 */
static void vt_cuptiact_writeRecord(CUpti_Activity *record, 
                                    vt_cuptiact_ctx_t *vtCtx)
{
  switch (record->kind) {
    case CUPTI_ACTIVITY_KIND_KERNEL: {
      vt_cuptiact_writeKernelRecord((CUpti_ActivityKernel *)record, vtCtx);
      break;
    }
    
    case CUPTI_ACTIVITY_KIND_MEMCPY: {
      vt_cuptiact_writeMemcpyRecord((CUpti_ActivityMemcpy *)record, vtCtx);
      break;
    }
    default: {
      break;
    }
  }
}

/*
 * Use the CUPTI activity kernel record to write the corresponding VampirTrace
 * events.
 * 
 * @param kernel the CUPTI activity kernel record
 * @param vtCtx the VampirTrace CUPTI activity context
 */
static void vt_cuptiact_writeKernelRecord(CUpti_ActivityKernel *kernel, 
                                          vt_cuptiact_ctx_t *vtCtx)
{
  /* get VampirTrace thread ID for the kernel's stream */  
  vt_cuptiact_strm_t *vtStrm = vt_cuptiact_checkStream(vtCtx, kernel->streamId);
  uint32_t vtThrdID = vtStrm->vtThrdID;
  uint32_t knRID = VT_NO_ID;

  /* get the VampirTrace region ID for the kernel */
  vt_gpu_hn_string_t *hn = vt_gpu_stringHashGet(kernel->name);
  
  if(hn){
    knRID = hn->rid;
  }else{
    char knName[VTGPU_KERNEL_STRING_SIZE];

    vt_cuda_symbolToKernel(knName, kernel->name);
    knRID = vt_def_region(VT_MASTER_THREAD, knName, VT_NO_ID,
                          VT_NO_LNO, VT_NO_LNO, "CUDA_KERNEL", VT_FUNCTION);

    hn = vt_gpu_stringHashPut(kernel->name, knRID);
  }

  /* write events */
  {
    uint64_t start = vtCtx->sync.hostStart 
                   + (kernel->start - vtCtx->sync.gpuStart) * vtCtx->sync.factor;
    uint64_t stop = start + (kernel->end - kernel->start) * vtCtx->sync.factor;
    
    /* if current activity's start time is before last written timestamp */
    if(start < vtStrm->vtLastTime){
      vt_warning("[CUPTI Activity] Kernel: start time < last written timestamp!");
      return;
    }
    
    /* check if time between start and stop is increasing */
    if(stop < start){
      vt_warning("[CUPTI Activity] Kernel: start time > stop time!");
      return;
    }
    
    /* check if synchronization stop time is before kernel stop time */
    if(vtCtx->sync.hostStop < stop){
      vt_warning("[CUPTI Activity] Kernel: sync stop time < stop time!");
      return;
    }
    
    /* set the last VampirTrace timestamp, written in this stream */
    vtStrm->vtLastTime = stop;

    /*vt_cntl_msg(1, "'%s'(%d) start: %llu; stop: %llu (tid: %d)", 
                   knName, knRID, start, stop, vtThrdID);*/
            /* GPU idle time will be written to first CUDA stream in list */
    if(vt_gpu_trace_idle){
      if(vtCtx->gpuIdleOn){
        vt_exit(vtCtx->strmList->vtThrdID, &start);
        /*vt_warning("IDLEexit: %llu (%d)", start, vtCtx->strmList->vtThrdID);*/
        vtCtx->gpuIdleOn = 0;
      }else if(start > vtCtx->vtLastGPUTime){
        /* idle is off and kernels are consecutive */
        vt_enter(vtCtx->strmList->vtThrdID, &(vtCtx->vtLastGPUTime), vt_gpu_rid_idle);
        vt_exit(vtCtx->strmList->vtThrdID, &start);
        /*vt_warning("IDLEente: %llu (%d)", vtCtx->vtLastGPUTime, vtCtx->strmList->vtThrdID);
        vt_warning("IDLEexit: %llu (%d)", start, vtCtx->strmList->vtThrdID);*/
      }
    }

    vt_enter(vtThrdID, &start, knRID);
    /*vt_warning("KERNente: %llu (%d)", start, vtThrdID);*/
    
    /* use counter to provide additional information for kernels */
    if(vt_cupti_trace_kernels > 1){
      /* grid and block size counter (start) */
      {
        uint32_t threadsPerBlock = kernel->blockX * kernel->blockY * kernel->blockZ;
        uint32_t blocksPerGrid = kernel->gridX * kernel->gridY * kernel->gridZ;

        vt_count(vtThrdID, &start, vt_cupti_cid_blocksPerGrid, 
                 blocksPerGrid);
        vt_count(vtThrdID, &start, vt_cupti_cid_threadsPerBlock, 
                 threadsPerBlock);
        vt_count(vtThrdID, &start, vt_cupti_cid_threadsPerKernel,
                 threadsPerBlock * blocksPerGrid);
      }

      /* memory counter (start) */
      vt_count(vtThrdID, &start, vt_cuptiact_cid_knStaticSharedMem,
               kernel->staticSharedMemory);
      vt_count(vtThrdID, &start, vt_cuptiact_cid_knDynamicSharedMem,
               kernel->dynamicSharedMemory);
      vt_count(vtThrdID, &start, vt_cuptiact_cid_knLocalMemTotal,
               kernel->localMemoryTotal);
      vt_count(vtThrdID, &start, vt_cuptiact_cid_knRegistersPerThread,
               kernel->registersPerThread);

      /* memory counter (stop) */
      vt_count(vtThrdID, &stop, vt_cuptiact_cid_knStaticSharedMem, 0);
      vt_count(vtThrdID, &stop, vt_cuptiact_cid_knDynamicSharedMem, 0);
      vt_count(vtThrdID, &stop, vt_cuptiact_cid_knLocalMemTotal, 0);
      vt_count(vtThrdID, &stop, vt_cuptiact_cid_knRegistersPerThread, 0);

      /* grid and block size counter (stop) */
      vt_count(vtThrdID, &stop, vt_cupti_cid_blocksPerGrid, 0);
      vt_count(vtThrdID, &stop, vt_cupti_cid_threadsPerBlock, 0);
      vt_count(vtThrdID, &stop, vt_cupti_cid_threadsPerKernel, 0);
    }
    
    vt_exit(vtThrdID, &stop);
    /*vt_warning("KERNexit: %llu (%d)", stop, vtThrdID);*/
    
    if(vtCtx->vtLastGPUTime < stop) vtCtx->vtLastGPUTime = stop;
  }

  /*vt_cntl_msg(1, "KERNEL '%s' [%llu ns] device %u, context %u, stream %u, "
                 "correlation %u/r%u\n"
                 "\t grid [%u,%u,%u], block [%u,%u,%u], "
                 "shared memory (static %u, dynamic %u)",
             kernel->name, (unsigned long long)(kernel->end - kernel->start),
             kernel->deviceId, kernel->contextId, kernel->streamId, 
             kernel->correlationId, kernel->runtimeCorrelationId,
             kernel->gridX, kernel->gridY, kernel->gridZ,
             kernel->blockX, kernel->blockY, kernel->blockZ,
             kernel->staticSharedMemory, kernel->dynamicSharedMemory);*/
}

/*
 * Use the CUPTI activity memory copy record to write the corresponding 
 * VampirTrace events.
 * 
 * @param mcpy the CUPTI activity memory copy record
 * @param vtCtx the VampirTrace CUPTI activity context
 */
static void vt_cuptiact_writeMemcpyRecord(CUpti_ActivityMemcpy *mcpy, 
                                          vt_cuptiact_ctx_t *vtCtx)
{
  vt_gpu_copy_kind_t kind = VT_GPU_COPYDIRECTION_UNKNOWN;

  uint32_t vtThrdID;
  uint64_t start, stop;
  vt_cuptiact_strm_t *vtStrm = NULL;
  
  /*vt_cntl_msg(1,"mcpycopykind: %d (strm %d)", mcpy->copyKind, mcpy->streamId);*/
  if(mcpy->copyKind == CUPTI_ACTIVITY_MEMCPY_KIND_DTOD) return;
  
  start = vtCtx->sync.hostStart 
                 + (mcpy->start - vtCtx->sync.gpuStart) * vtCtx->sync.factor;
  stop = start + (mcpy->end - mcpy->start) * vtCtx->sync.factor;
  
  /* get VampirTrace thread ID for the kernel's stream */
  vtStrm = vt_cuptiact_checkStream(vtCtx, mcpy->streamId);
  vtThrdID = vtStrm->vtThrdID;
  
    /* if current activity's start time is before last written timestamp */
  if(start < vtStrm->vtLastTime){
    vt_cntl_msg(1, "[CUPTI Activity] Memcpy: start time < last written timestamp!");
    return;
  }
  
  /* check if time between start and stop is increasing */
  if(stop < start){
    vt_warning("[CUPTI Activity] Memcpy: start time > stop time!");
    return;
  }

  /* check if synchronization stop time is before kernel stop time */
  if(vtCtx->sync.hostStop < stop){
    vt_warning("[CUPTI Activity] Memcpy: sync stop time < stop time!");
    return;
  }
  
  /* set the last VampirTrace timestamp, written in this stream */
  vtStrm->vtLastTime = stop;
  
  /* check copy direction */
  if(mcpy->srcKind == CUPTI_ACTIVITY_MEMORY_KIND_DEVICE){
    if(mcpy->dstKind == CUPTI_ACTIVITY_MEMORY_KIND_DEVICE){
      kind = VT_GPU_DEV2DEV;
    }else{
      kind = VT_GPU_DEV2HOST;
    }
  }else{
    if(mcpy->dstKind == CUPTI_ACTIVITY_MEMORY_KIND_DEVICE){
      kind = VT_GPU_HOST2DEV;
    }else{
      kind = VT_GPU_HOST2HOST;
    }
  }
  
  /*VT_CUPTI_ACT_LOCK();*/
  if(kind != VT_GPU_DEV2DEV) vt_gpu_prop[vtCtx->ptid] |= VTGPU_GPU_COMM;
  vt_gpu_prop[vtThrdID] |= VTGPU_GPU_COMM;
  /*VT_CUPTI_ACT_UNLOCK();*/
  /*
  vt_warning("MCPYente: %llu (%d)", start, vtThrdID);
  vt_warning("MCPYexit: %llu (%d)", stop, vtThrdID);
  */
  if(kind == VT_GPU_HOST2DEV){
    vt_mpi_rma_get(vtThrdID, &start, VT_GPU_RANK_ID(vtCtx->ptid),
                   vt_gpu_commCID, 0, mcpy->bytes);
  }else if(kind == VT_GPU_DEV2HOST){
    vt_mpi_rma_put(vtThrdID, &start, VT_GPU_RANK_ID(vtCtx->ptid),
                   vt_gpu_commCID, 0, mcpy->bytes);
  }else if(kind == VT_GPU_DEV2DEV){
    vt_mpi_rma_get(vtThrdID, &start, VT_GPU_RANK_ID(vtThrdID),
                   vt_gpu_commCID, 0, mcpy->bytes);
  }
  
  if(kind != VT_GPU_HOST2HOST){
    vt_mpi_rma_end(vtThrdID, &stop, vt_gpu_commCID, 0);
  }
  
  if(vtCtx->vtLastGPUTime < stop) vtCtx->vtLastGPUTime = stop;
  
  /*vt_cntl_msg(1, "MEMCPY %llu -> %llu[%llu ns] device %u, context %u, stream %u, "
                     "correlation %u/r%u",
               mcpy->start, mcpy->end, 
               (unsigned long long)(mcpy->end - mcpy->start),
               mcpy->deviceId, mcpy->contextId, mcpy->streamId, 
               mcpy->correlationId, mcpy->runtimeCorrelationId);*/
}

/*
 * Increases the "Allocated CUDA memory" counter.
 *
 * @param ctxUID CUDA context identifier (@see CUPTI callback info)
 * @param devPtr pointer to the allocated memory (needed for vtcudaFree())
 * @param size the number of bytes allocated
 */
void vt_cuptiact_writeMalloc(uint32_t ctxID, CUcontext cuCtx, 
                             void *devPtr, size_t size)
{
  uint64_t vtTime;
  vt_cuptiact_ctx_t* vtCtx = NULL;
  vt_cuptiact_gpumem_t *vtMalloc = 
                    (vt_cuptiact_gpumem_t*)malloc(sizeof(vt_cuptiact_gpumem_t));
  
  if(devPtr == NULL) return;
  
  /* flush activity buffer */
  vt_cuptiact_flushCtxActivities(cuCtx);
  
  vtMalloc->memPtr = devPtr;
  vtMalloc->size = size;
  
  vtCtx = vt_cuptiact_getCtx(cuCtx);
  if(vtCtx == NULL){
    vtCtx = vt_cuptiact_createContext(ctxID, cuCtx, (uint32_t)-1);
  }
  
  /* lock the work on the context */
  VT_CUPTI_ACT_LOCK();
  
  /* add malloc entry to list */
  vtMalloc->next = vtCtx->gpuMemList;
  vtCtx->gpuMemList = vtMalloc;
  
  /* increase allocated memory counter */
  vtCtx->gpuMemAllocated += size;

  /* check if first CUDA stream is available */
  if(vtCtx->strmList == NULL){
    vtCtx->strmList = vt_cuptiact_createStream(vtCtx, (uint32_t)-1);
    vt_count(vtCtx->strmList->vtThrdID, &vt_start_time, vt_cupti_cid_cudaMalloc, 0);
  }
  
  VT_CUPTI_ACT_UNLOCK();
  
  /* write counter value */
  vtTime = vt_pform_wtime();
  vt_count(vtCtx->strmList->vtThrdID, &vtTime, vt_cupti_cid_cudaMalloc, 
           (uint64_t)(vtCtx->gpuMemAllocated));
}

/*
 * Decreases the "Allocated CUDA memory" counter.
 *
 * @param ctxUID CUDA context identifier (@see CUPTI callback info)
 * @param devPtr pointer to the allocated memory
 */
void vt_cuptiact_writeFree(uint32_t ctxID, CUcontext cuCtx, void *devPtr)
{
  uint64_t vtTime;
  vt_cuptiact_ctx_t* vtCtx = NULL;
  vt_cuptiact_gpumem_t *curMalloc = NULL;
  vt_cuptiact_gpumem_t *lastMalloc = NULL;

  if(devPtr == NULL) return;
  
  /* flush activity buffer */
  vt_cuptiact_flushCtxActivities(cuCtx);
  
  vtCtx = vt_cuptiact_getCtx(cuCtx);
  if(vtCtx == NULL){
    vtCtx = vt_cuptiact_createContext(ctxID, cuCtx, (uint32_t)-1);
  }
  
  VT_CUPTI_ACT_LOCK();

  curMalloc = vtCtx->gpuMemList;
  lastMalloc = vtCtx->gpuMemList;

  /* lookup the CUDA malloc entry by its memory pointer */
  while(curMalloc != NULL){
    if(devPtr == curMalloc->memPtr){

      /* decrease allocated counter value and write it */
      vtTime = vt_pform_wtime();
      vtCtx->gpuMemAllocated -= curMalloc->size;
      vt_count(vtCtx->strmList->vtThrdID, &vtTime, vt_cupti_cid_cudaMalloc,
               (uint64_t)(vtCtx->gpuMemAllocated));


      /* set pointer over current element to next one */
      lastMalloc->next = curMalloc->next;

      /* if current element is the first list entry, set the list entry */
      if(curMalloc == vtCtx->gpuMemList){
        vtCtx->gpuMemList = curMalloc->next;
      }

      /* free VT memory of CUDA malloc */
      curMalloc->next = NULL;
      free(curMalloc);
      curMalloc = NULL;

      /* set mallocList to NULL, if last element freed */
      if(vtCtx->gpuMemAllocated == 0) {
        vtCtx->gpuMemList = NULL;
      }
      
      VT_CUPTI_ACT_UNLOCK();
      return;
    }

    lastMalloc = curMalloc;
    curMalloc = curMalloc->next;
  }

  VT_CUPTI_ACT_UNLOCK();

  vt_warning("[CUPTI Activity] free CUDA memory, which has not been allocated!");
}

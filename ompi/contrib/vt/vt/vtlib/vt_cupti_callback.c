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

#include "config.h"         /* snprintf */

#include "vt_env.h"         /* get environment variables */
#include "vt_pform.h"       /* VampirTrace time measurement */
#include "vt_defs.h"        /* VampirTrace constants */
#include "vt_error.h"       /* VampirTrace warning and error messages */
#include "vt_mallocwrap.h"  /* wrapping of malloc and free */
#include "vt_gpu.h"         /* common for GPU */
#include "util/hash.h"

#include "vt_cupti.h"           /* CUPTI header */
#include "vt_cupti_common.h"    /* CUPTI common structures, functions, etc. */

#include "vt_cupti_callback.h"
#if defined(VT_CUPTI_EVENTS)
#include "vt_cupti_events.h"    /* Support for CUPTI events */
#endif

#if defined(VT_CUPTI_ACTIVITY)
#include "vt_cupti_activity.h"  /* Support for CUPTI activity */
#endif

#include <stdio.h>
#include <string.h>

#define DISABLE_CUDART_CALLBACK(_cbid)                                         \
  {                                                                            \
    CUptiResult cuptiErr = cuptiEnableCallback(                                \
                         0, vt_cupticb_cudart_subscriber,                      \
                         CUPTI_CB_DOMAIN_RUNTIME_API,                          \
                         _cbid);                                               \
    VT_CUPTI_CALL(cuptiErr, "[CUPTI Callbacks] Disable CUDA runtime callback");\
  }

#define ENABLE_CUDART_CALLBACK(_cbid)                                          \
  {                                                                            \
    CUptiResult cuptiErr = cuptiEnableCallback(                                \
                         1, vt_cupticb_cudart_subscriber,                      \
                         CUPTI_CB_DOMAIN_RUNTIME_API,                          \
                         _cbid);                                               \
    VT_CUPTI_CALL(cuptiErr, "[CUPTI Callbacks] Enable CUDA runtime callback"); \
  }

#define DISABLE_CUDART_CALLBACKS()                                             \
  {                                                                            \
    CUptiResult cuptiErr = cuptiEnableDomain(0, vt_cupticb_cudart_subscriber,  \
                                             CUPTI_CB_DOMAIN_RUNTIME_API);     \
    VT_CUPTI_CALL(cuptiErr, "[CUPTI Callbacks] Disable CUDA runtime domain");  \
  }

#define ENABLE_CUDART_CALLBACKS()                                              \
  {                                                                            \
    CUptiResult cuptiErr = cuptiEnableDomain(1, vt_cupticb_cudart_subscriber,  \
                                             CUPTI_CB_DOMAIN_RUNTIME_API);     \
    VT_CUPTI_CALL(cuptiErr, "[CUPTI Callbacks] Enable CUDA runtime domain");   \
  }

#define SUSPEND_CALLBACKS(_vtCtx) _vtCtx->callbacks_enabled = 0;
#define RESUME_CALLBACKS(_vtCtx) _vtCtx->callbacks_enabled = 1;

/* global subscriber handles */
static CUpti_SubscriberHandle vt_cupticb_subscriber;

/* flag: tracing of CUDA runtime API enabled? */
static uint8_t vt_cupticb_trace_runtimeAPI = 0;

/* flag: tracing of CUDA driver API enabled? */
static uint8_t vt_cupticb_trace_driverAPI = 0;

/* flag: use CUPTI events for counter capturing? */
static uint8_t vt_cupticb_trace_events = 1;

#if (defined(VT_CUPTI_EVENTS) && !defined(VT_CUPTI_ACTIVITY))
/* flag: sampling for CUPTI counter values enabled? */
static uint8_t vt_cupticb_event_sampling = 0;
#endif

/* initialization and finalization flags */
static uint8_t vt_cupticb_initialized = 0;
static uint8_t vt_cupticb_finalized = 0;

/* VampirTrace region ID for synchronization of host and CUDA device*/
static uint32_t vt_cupticb_rid_sync = VT_NO_ID;

#if !defined(VT_CUPTI_ACTIVITY)

/* 
 * Synchronization Level:
 * 0 no extra synchronization
 * 1 synchronize before synchronous memory copy or synchronization - correct
 *   data transfer rates for communication
 * 2 show synchronization in extra region group to get host wait time
 */
static uint8_t vt_cupticb_syncLevel = 3;
#endif /* !defined(VT_CUPTI_ACTIVITY) */

/**************** The callback functions to be registered *********************/

/* CUDA runtime API callback function */
/* some of CUPTI API functions have changed */
#if defined(VT_CUPTI_ACTIVITY)
void CUPTIAPI vt_cupticb_all(void *, CUpti_CallbackDomain,
                                CUpti_CallbackId, const void *);
void (*vt_cupticb_all_ptr)(void *, CUpti_CallbackDomain, 
                              CUpti_CallbackId, const void *)
      = vt_cupticb_all;

void CUPTIAPI vt_cupticb_cudart(void *, CUpti_CallbackDomain,
                                CUpti_CallbackId, const CUpti_CallbackData *);

void vt_cupticb_driverAPI(CUpti_CallbackId, const CUpti_CallbackData *);

void vt_cupticb_resource(CUpti_CallbackId, const CUpti_ResourceData *);

void vt_cupticb_sync(CUpti_CallbackId, const CUpti_SynchronizeData *);

#else
void CUPTIAPI vt_cupticb_cudart(void *, CUpti_CallbackDomain,
                                CUpti_CallbackId, const CUpti_CallbackData *);
void (*vt_cupticb_cudart_ptr)(void *, CUpti_CallbackDomain, 
                              CUpti_CallbackId, const CUpti_CallbackData *)
      = vt_cupticb_cudart;

/******************************************************************************/

/*********************** Internal function declarations ***********************/
static void vt_cupticb_handle_cudart_knconf(const CUpti_CallbackData *);
static void vt_cupticb_handle_cudart_kernel(const CUpti_CallbackData *);

static void vt_cupticb_handle_cudart_memcpy(const CUpti_CallbackData *, 
                                       enum cudaMemcpyKind, uint64_t, uint64_t);
static void vt_cupticb_handle_cudart_mcpyAsync(const CUpti_CallbackData *cbInfo,
                 enum cudaMemcpyKind kind, uint64_t bytes, cudaStream_t cuStrm);

static void vt_cupticb_handle_malloc(uint64_t, CUcontext, void *, size_t);
static void vt_cupticb_handle_free(uint64_t ctxUID, CUcontext, void *devPtr);
/******************************************************************************/
#endif /* VT_CUPTI_ACTIVITY */

/* hash table to map CUpti_CallbackIds to VampirTrace rids */
#define VT_CUPTICB_CUDA_API_FUNC_MAX 1024
static uint32_t vt_cupticb_cudaApiFuncTab[VT_CUPTICB_CUDA_API_FUNC_MAX];

static uint32_t vt_cupticb_cudaApiHashFunc(CUpti_CallbackDomain domain,
                                           CUpti_CallbackId cid)
{
  uint32_t idx = 0;

  /* Use an offset for the driver API functions, if CUDA runtime and driver
      API recording is enabled (uncommon case) */
  if( vt_cupticb_trace_driverAPI && vt_cupticb_trace_runtimeAPI ){
    uint16_t offset = 0;

    if( domain == CUPTI_CB_DOMAIN_DRIVER_API ){
      offset = VT_CUPTICB_CUDA_API_FUNC_MAX/2;
    }

    idx = offset + ( uint32_t )cid;

    if( ( domain == CUPTI_CB_DOMAIN_RUNTIME_API ) &&
          ( idx >= (uint32_t)(VT_CUPTICB_CUDA_API_FUNC_MAX - offset) )){
      idx = 0;

      vt_error_msg("[CUPTI Callbacks] Hash table for CUDA runtime API "
                    "function %d is to small!", cid );
    }
  }else{
    idx = ( uint32_t )cid;
  }

  if( idx >= VT_CUPTICB_CUDA_API_FUNC_MAX ){
    idx = 0;

    vt_error_msg("[CUPTI Callbacks] Hash table for CUDA API "
                 "function %d is to small!", cid );
  }
  
  return (uint32_t)idx;
}

static void vt_cupticb_cudaApiFuncPut(CUpti_CallbackDomain domain, 
                                    CUpti_CallbackId cid, uint32_t rid)
{  
  vt_cupticb_cudaApiFuncTab[vt_cupticb_cudaApiHashFunc(domain, cid)] = rid;
}

static uint32_t vt_cupticb_cudaApiFuncGet(CUpti_CallbackDomain domain, 
                                          CUpti_CallbackId cid)
{
  return vt_cupticb_cudaApiFuncTab[vt_cupticb_cudaApiHashFunc(domain, cid)];
}


/*
 * Set a CUPTI callback function for a specific CUDA runtime or driver function 
 * or for a whole domain (runtime or driver API)
 * 
 * @param subscriber handle to the initialize subscriber
 * @param callback the callback function
 * @param domain The domain of the callback
 * @param cbid The ID of the API function associated with this callback, if it
 *             is not valid, the whole domain will be enabled
 */
static void vt_cupti_set_callback(CUpti_CallbackFunc callback,
                                  CUpti_CallbackDomain domain,
                                  CUpti_CallbackId cbid)
{
  CUptiResult cuptiErr;
  static uint8_t initflag = 1;

  if(initflag){
    initflag = 0;
    
    CHECK_CU_ERROR(cuInit(0), "cuInit");
    
    /* only one subscriber allowed at a time */
    cuptiErr = cuptiSubscribe(&vt_cupticb_subscriber, callback, NULL);
    VT_CUPTI_CALL(cuptiErr, "cuptiSubscribe");
  }
  
  if(CUPTI_CB_DOMAIN_INVALID == domain){
    cuptiEnableAllDomains(1, vt_cupticb_subscriber);
  }else{
    if((cbid == CUPTI_RUNTIME_TRACE_CBID_INVALID) || 
       (cbid == CUPTI_DRIVER_TRACE_CBID_INVALID)){
      cuptiErr = cuptiEnableDomain(1, vt_cupticb_subscriber, domain);
      VT_CUPTI_CALL(cuptiErr, "cuptiEnableDomain");
    }else{
      cuptiErr = cuptiEnableCallback(1, vt_cupticb_subscriber, domain, cbid);
      VT_CUPTI_CALL(cuptiErr, "cuptiEnableCallback");
    }
  }
}



# if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 3))
/*
 * Creates a VampirTrace CUPTI callbacks context.
 * 
 * @param cuDev the CUDA device
 * 
 * @return the VampirTrace CUPTI callbacks context
 */
static vt_cupti_callbacks_t* vt_cupticb_createCbCtx(CUdevice cuDev)
{
  vt_cupti_callbacks_t *vtCbCtx = NULL;
  int concurrentKernels = 0;
  
  /* create new context, as it is not listed */
  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
  vtCbCtx = (vt_cupti_callbacks_t *)malloc(sizeof(vt_cupti_callbacks_t));
  if(vtCbCtx == NULL) 
    vt_error_msg("[CUPTI Callbacks] Could not allocate memory for callbacks context!");
  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
  
  vtCbCtx->streamsCreated = 0;
  
  /* check for concurrent kernel support */
  CHECK_CU_ERROR(cuDeviceGetAttribute(&concurrentKernels, 
                 CU_DEVICE_ATTRIBUTE_CONCURRENT_KERNELS, cuDev), 
                 "cuDeviceGetAttribute");
  
  vtCbCtx->concurrentKernels = concurrentKernels;
  
  return vtCbCtx;
}
# endif

#if !defined(VT_CUPTI_ACTIVITY)
/*
 * Creates a VampirTrace CUPTI stream object and returns it.
 *
 *  @param stream the CUDA stream id
 *  @param ptid the VampirTrace thread ID of the calling thread
 *
 *  @return the created stream object
 */
static vt_cupticb_strm_t* vt_cupticb_createStream(uint32_t ptid, 
                                                  CUstream cuStrm,
                                                  vt_cupti_ctx_t *vtCtx)
{
  vt_cupti_callbacks_t *vtcuptiCallbacks = vtCtx->callbacks;
  uint32_t gpu_tid = 0;
  char thread_name[16];
  vt_cupticb_strm_t *vtStrm;
  uint32_t strmNum = 0;

  if(vtCtx == NULL){
    vt_warning("[CUPTI Callbacks] Create stream without VampirTrace context");
    return NULL;
  }
  
  strmNum = vtcuptiCallbacks->strmNum;

  /* allocate memory for stream */
  vtStrm = (vt_cupticb_strm_t *)malloc(sizeof(vt_cupticb_strm_t));
  if(vtStrm == NULL) 
    vt_error_msg("[CUPTI Callbacks] malloc(sizeof(vt_cupti_strm_t)) failed!");
  
  vtStrm->next = NULL;
  vtStrm->stream = cuStrm;
  
    /* set the stream number */
  if(cuStrm == NULL){
    strmNum = 1;
  }else{
    vtcuptiCallbacks->strmNum++;
  }

  /* create VT-User-Thread with name and parent id and get its id */
  if(-1 == snprintf(thread_name, 15, "CUDA[%d:%d]", (uint32_t)vtCtx->devID, strmNum))
    vt_cntl_msg(1, "Could not create thread name for CUDA thread!");
  
  vt_gpu_registerThread(thread_name, ptid, &gpu_tid);
  vtStrm->tid = gpu_tid;
  
  if(vt_gpu_init_time < vt_start_time)
    vt_gpu_init_time = vt_start_time;
  
  if(vt_gpu_trace_kernels > 1){    
    /* set count values to zero */
    vt_count(gpu_tid, &vt_gpu_init_time, vt_cupti_cid_blocksPerGrid, 0);
    vt_count(gpu_tid, &vt_gpu_init_time, vt_cupti_cid_threadsPerBlock, 0);
    vt_count(gpu_tid, &vt_gpu_init_time, vt_cupti_cid_threadsPerKernel, 0);
  }
  
  if(vtcuptiCallbacks->strmList == NULL){
    /* write enter event for GPU_IDLE on first stream */
    if(vt_gpu_trace_idle)
      vt_enter(gpu_tid, &vt_gpu_init_time, vt_gpu_rid_idle);

    /* set the counter value for cudaMalloc to 0 on first stream */
    if(vt_gpu_trace_memusage > 0)
      vt_count(gpu_tid, &vt_gpu_init_time, vt_gpu_cid_memusage, 0);
  }

  return vtStrm;
}

/*
 * Creates new VampirTrace CUPTI callback context.
 * 
 * @param cuCtxID ID of the CUDA context
 * 
 * @return the VampirTrace CUPTI context object
 */
static vt_cupti_callbacks_t* vt_cupticb_createCbCtx(vt_cupti_ctx_t *vtCtx, 
                                                    CUstream cuStrm)
{
  vt_cupti_callbacks_t *vtCbCtx = 
          (vt_cupti_callbacks_t*)malloc(sizeof(vt_cupti_callbacks_t));
  if(vtCbCtx == NULL) 
    vt_error_msg("[CUPTI Callbacks] Could not allocate memory for vt_cupti_callbacks_t!");
  
  /* enable handling of callbacks */
  vtCbCtx->callbacks_enabled = 1;
  
  /* initialize GPU memory allocation parameter */
  vtCbCtx->gpuMemList = NULL;
  vtCbCtx->gpuMemAllocated = 0;
  
  
  /* create first empty CUDA stream */
  VT_CHECK_THREAD;
  vtCbCtx->strmNum = 2;
  vtCbCtx->strmList = NULL;
  vtCbCtx->strmList = vt_cupticb_createStream(vtCtx->ptid, cuStrm, vtCtx);
  
  /* initialize CUDA kernel configure stack */
  {
    vt_cupti_kernel_t *vtKn = NULL;
    
    vtKn = (vt_cupti_kernel_t*)malloc(sizeof(vt_cupti_kernel_t));
    if(vtKn == NULL) 
      vt_error_msg("Could not allocate memory for vt_cupti_kernel_t!");
    
    vtKn->prev = NULL;
    vtCbCtx->kernelData = vtKn;
    vtCbCtx->stack_size = 0;
  }
  
  return vtCbCtx;
}

static vt_cupti_ctx_t* vt_cupticb_checkCtx(CUcontext cuCtx, cudaStream_t cuStrm)
{
  vt_cupti_ctx_t* vtCtx = vt_cupti_getCreateCtx(cuCtx);
  
  if(vtCtx->callbacks == NULL) 
    vtCtx->callbacks = vt_cupticb_createCbCtx(vtCtx, cuStrm);
  
  return vtCtx;
}

/*
 * Lookup a VampirTrace CUPTI stream and if not available,
 * create a new one.
 *
 * @param ptid the VampirTrace thread ID of the calling thread
 * @param vtCtx VampirTrace CUPTI context object
 * @param cuStrm pointer to CUDA stream
 *
 * @return a VampirTrace stream object
 */
static vt_cupticb_strm_t* vt_cupticb_checkStream(uint32_t ptid, 
                                                 vt_cupti_ctx_t *vtCtx, 
                                                 CUstream cuStrm)
{
  vt_cupticb_strm_t *currStrm = NULL;
  vt_cupticb_strm_t *lastStrm = NULL;
  
  if(vtCtx == NULL){
    vt_warning("[CUPTI CALLBACKS] Check stream without VampirTrace context.");
    return NULL;
  }
  
  currStrm = vtCtx->callbacks->strmList;
 
  /* this should never happen */
  if(currStrm == NULL) return NULL;
  
  /* lookup the current stream */
  do{
    if(cuStrm == currStrm->stream) return currStrm;
    lastStrm = currStrm;
    currStrm = currStrm->next;
  }while(currStrm != NULL);

  /* create new VampirTrace Stream structure/object and append it to list */
  lastStrm->next = vt_cupticb_createStream(ptid, cuStrm, vtCtx);
  
  return lastStrm->next;
}
#endif

#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 2))
/*
 * This CUPTI callback function chooses the CUPTI domain.
 *
 * @param userdata pointer to the user data
 * @param domain the callback domain (runtime or driver API)
 * @param cbid the ID of the callback function in the given domain
 * @param cbInfo information about the callback
 */
void CUPTIAPI vt_cupticb_all(void *userdata, 
                             CUpti_CallbackDomain domain,
                             CUpti_CallbackId cbid, 
                             const void *cbInfo)
{
  if(CUPTI_CB_DOMAIN_RUNTIME_API == domain)
    vt_cupticb_cudart(userdata, domain, cbid, (CUpti_CallbackData *)cbInfo);
  
  if(CUPTI_CB_DOMAIN_DRIVER_API == domain)
    vt_cupticb_driverAPI(cbid, (CUpti_CallbackData *)cbInfo);
  
  if(CUPTI_CB_DOMAIN_RESOURCE == domain)
    vt_cupticb_resource(cbid, (CUpti_ResourceData *)cbInfo);
  
  if(CUPTI_CB_DOMAIN_SYNCHRONIZE == domain)
    vt_cupticb_sync(cbid, (CUpti_SynchronizeData *)cbInfo);
}
#endif

/*
 * This callback function is used to trace the CUDA runtime API.
 *
 * @param userdata pointer to the user data
 * @param domain the callback domain (runtime API)
 * @param cbid the ID of the callback function in the given domain
 * @param cbInfo information about the callback
 */
void CUPTIAPI vt_cupticb_cudart(void *userdata, 
                                CUpti_CallbackDomain domain,
                                CUpti_CallbackId cbid, 
                                const CUpti_CallbackData *cbInfo)
{
  uint32_t ptid;
  uint64_t time;
  uint32_t rid_func = VT_NO_ID;
  uint32_t hash_api_rid = VT_NO_ID;
  
  if(cbid == CUPTI_RUNTIME_TRACE_CBID_INVALID) return;
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;
  
  /* get the VampirTrace region ID for the API function */
  hash_api_rid = vt_cupticb_cudaApiFuncGet(CUPTI_CB_DOMAIN_RUNTIME_API, cbid);
  if(hash_api_rid != VT_NO_ID){
    rid_func = hash_api_rid;
  }else{
    rid_func = vt_def_region(VT_MASTER_THREAD, cbInfo->functionName, VT_NO_ID,
                             VT_NO_LNO, VT_NO_LNO, "CUDART_API", VT_FUNCTION);
    
    vt_cupticb_cudaApiFuncPut(CUPTI_CB_DOMAIN_RUNTIME_API, cbid, rid_func);
  }
  
  /*********** write enter and exit records for CUDA runtime API **************/
  time = vt_pform_wtime();
  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    (void)vt_enter(ptid, &time, rid_func);
  }
  
  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    vt_exit(ptid, &time);
  }

  /*
   * Semantic Function Instrumentation
   */
  switch(cbid){
  /********************** CUDA memory allocation ******************************/
    case CUPTI_RUNTIME_TRACE_CBID_cudaMalloc_v3020: {
      if(vt_gpu_trace_memusage > 0 && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMalloc_v3020_params *params = 
                            (cudaMalloc_v3020_params *)cbInfo->functionParams;
#if defined(VT_CUPTI_ACTIVITY)
        vt_cuptiact_writeMalloc((uint32_t)cbInfo->contextUid, cbInfo->context, 
                                *(params->devPtr), params->size);
#else
        vt_cupticb_handle_malloc(cbInfo->contextUid, cbInfo->context,
                                 *(params->devPtr), 
                                 params->size);
#endif
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMallocPitch_v3020: {
      if(vt_gpu_trace_memusage > 0 && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMallocPitch_v3020_params *params = 
                         (cudaMallocPitch_v3020_params *)cbInfo->functionParams;
#if defined(VT_CUPTI_ACTIVITY)
        vt_cuptiact_writeMalloc((uint32_t)cbInfo->contextUid, cbInfo->context,
                                *(params->devPtr), 
                                params->height * (*(params->pitch)));
#else
        vt_cupticb_handle_malloc(cbInfo->contextUid, cbInfo->context, 
                                 *(params->devPtr), 
                                 params->height * (*(params->pitch)));
#endif
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMallocArray_v3020: {
      if(vt_gpu_trace_memusage > 0 && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMallocArray_v3020_params *params = 
                         (cudaMallocArray_v3020_params *)cbInfo->functionParams;
#if defined(VT_CUPTI_ACTIVITY)
        vt_cuptiact_writeMalloc((uint32_t)cbInfo->contextUid, cbInfo->context,
                                *(params->array), 
                                params->height * params->width);
#else
        vt_cupticb_handle_malloc(cbInfo->contextUid, cbInfo->context,
                                 *(params->array), 
                                 params->height * params->width);
#endif
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMalloc3D_v3020: {
      if(vt_gpu_trace_memusage > 0 && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMalloc3D_v3020_params *params = 
                         (cudaMalloc3D_v3020_params *)cbInfo->functionParams;
#if defined(VT_CUPTI_ACTIVITY)
        vt_cuptiact_writeMalloc((uint32_t)cbInfo->contextUid, cbInfo->context,
                                params->pitchedDevPtr->ptr, 
   params->pitchedDevPtr->pitch * params->extent.height * params->extent.depth);
#else
        vt_cupticb_handle_malloc(cbInfo->contextUid, cbInfo->context,
                                 params->pitchedDevPtr->ptr, 
   params->pitchedDevPtr->pitch * params->extent.height * params->extent.depth);
#endif
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMalloc3DArray_v3020: {
      if(vt_gpu_trace_memusage > 0 && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMalloc3DArray_v3020_params *params = 
                       (cudaMalloc3DArray_v3020_params *)cbInfo->functionParams;
#if defined(VT_CUPTI_ACTIVITY)
        vt_cuptiact_writeMalloc((uint32_t)cbInfo->contextUid, cbInfo->context,
                                *(params->array), 
          params->extent.width * params->extent.height * params->extent.depth);
#else
        vt_cupticb_handle_malloc(cbInfo->contextUid, cbInfo->context,
                                 *(params->array), 
          params->extent.width * params->extent.height * params->extent.depth);
#endif
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaFree_v3020: {
      if(vt_gpu_trace_memusage > 0 && cbInfo->callbackSite == CUPTI_API_ENTER){
#if defined(VT_CUPTI_ACTIVITY)
        vt_cuptiact_writeFree((uint32_t)cbInfo->contextUid, cbInfo->context,
                     ((cudaFree_v3020_params *)cbInfo->functionParams)->devPtr);
#else
        vt_cupticb_handle_free(cbInfo->contextUid, cbInfo->context,
                    ((cudaFree_v3020_params *)cbInfo->functionParams)->devPtr);
#endif
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaFreeArray_v3020: {
      if(vt_gpu_trace_memusage > 0 && cbInfo->callbackSite == CUPTI_API_ENTER){
#if defined(VT_CUPTI_ACTIVITY)
        vt_cuptiact_writeFree((uint32_t)cbInfo->contextUid, cbInfo->context,
                 ((cudaFreeArray_v3020_params *)cbInfo->functionParams)->array);
#else
        vt_cupticb_handle_free(cbInfo->contextUid, cbInfo->context,
                 ((cudaFreeArray_v3020_params *)cbInfo->functionParams)->array);
#endif
      }
      
      break;
    }
#if !defined(VT_CUPTI_ACTIVITY)
    /****************** the CUDA runtime kernel configure call ******************/
    case CUPTI_RUNTIME_TRACE_CBID_cudaConfigureCall_v3020: {
      if(vt_gpu_trace_kernels) 
        vt_cupticb_handle_cudart_knconf(cbInfo);

      break;
    }

    /***** the CUDA runtime kernel launch ******/
    case CUPTI_RUNTIME_TRACE_CBID_cudaLaunch_v3020: {
      if(vt_gpu_trace_kernels)
        vt_cupticb_handle_cudart_kernel(cbInfo);
      
      break;
    }
  /****************************************************************************/
    
  /****************** synchronous CUDA memory copies **************************/
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy_v3020: {
      cudaMemcpy_v3020_params *params = 
            (cudaMemcpy_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)params->count, 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2D_v3020: {
      cudaMemcpy2D_v3020_params *params = 
              (cudaMemcpy2D_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)(params->height * params->width), 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyToArray_v3020: {
      cudaMemcpyToArray_v3020_params *params = 
              (cudaMemcpyToArray_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)params->count, 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DToArray_v3020: {
      cudaMemcpy2DToArray_v3020_params *params = 
              (cudaMemcpy2DToArray_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)(params->height * params->width), 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyFromArray_v3020: {
      cudaMemcpyFromArray_v3020_params *params = 
              (cudaMemcpyFromArray_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)params->count, 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DFromArray_v3020: {
      cudaMemcpy2DFromArray_v3020_params *params = 
              (cudaMemcpy2DFromArray_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)(params->height * params->width), 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyArrayToArray_v3020: {
      cudaMemcpyArrayToArray_v3020_params *params = 
              (cudaMemcpyArrayToArray_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)params->count, 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DArrayToArray_v3020: {
      cudaMemcpy2DArrayToArray_v3020_params *params = 
              (cudaMemcpy2DArrayToArray_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)(params->height * params->width), 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyToSymbol_v3020: {
      cudaMemcpyToSymbol_v3020_params *params = 
              (cudaMemcpyToSymbol_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)params->count, 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyFromSymbol_v3020: {
      cudaMemcpyFromSymbol_v3020_params *params = 
              (cudaMemcpyFromSymbol_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->kind, 
                                      (uint64_t)params->count, 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy3D_v3020: {
      cudaMemcpy3D_v3020_params *params = 
              (cudaMemcpy3D_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, params->p->kind, 
                (uint64_t)(params->p->extent.height * params->p->extent.width * 
                           params->p->extent.depth), 
                                      time);
      break;
    }
    
    /*case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyPeer_v4000: {
      cudaMemcpyPeer_v4000_params *params = 
            (cudaMemcpyPeer_v4000_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, cudaMemcpyDeviceToDevice, 
                                      (uint64_t)params->count, 
                                      time);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy3DPeer_v4000: {
      cudaMemcpy3DPeer_v4000_params *params = 
              (cudaMemcpy3DPeer_v4000_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_memcpy(cbInfo, cudaMemcpyDeviceToDevice, 
                (uint64_t)(params->p->extent.height * params->p->extent.width * 
                           params->p->extent.depth), 
                                      time);
      break;
    }*/
    /**************************************************************************/
    
    /******************** asynchronous memory copies **************************/
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyAsync_v3020: {
      cudaMemcpyAsync_v3020_params *params = 
            (cudaMemcpyAsync_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                         (uint64_t)params->count, 
                                         params->stream);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyToArrayAsync_v3020: {
      cudaMemcpyToArrayAsync_v3020_params *params = 
              (cudaMemcpyToArrayAsync_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                         (uint64_t)params->count, 
                                         params->stream);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyFromArrayAsync_v3020: {
      cudaMemcpyFromArrayAsync_v3020_params *params = 
              (cudaMemcpyFromArrayAsync_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                      (uint64_t)params->count, 
                                      params->stream);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DAsync_v3020: {
      cudaMemcpy2DAsync_v3020_params *params = 
              (cudaMemcpy2DAsync_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                      (uint64_t)(params->height * params->width), 
                                      params->stream);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DToArrayAsync_v3020: {
      cudaMemcpy2DToArrayAsync_v3020_params *params = 
              (cudaMemcpy2DToArrayAsync_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                      (uint64_t)(params->height * params->width), 
                                      params->stream);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy2DFromArrayAsync_v3020: {
      cudaMemcpy2DFromArrayAsync_v3020_params *params = 
              (cudaMemcpy2DFromArrayAsync_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                      (uint64_t)(params->height * params->width), 
                                      params->stream);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyToSymbolAsync_v3020: {
      cudaMemcpyToSymbolAsync_v3020_params *params = 
              (cudaMemcpyToSymbolAsync_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                      (uint64_t)params->count, 
                                      params->stream);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyFromSymbolAsync_v3020: {
      cudaMemcpyFromSymbolAsync_v3020_params *params = 
              (cudaMemcpyFromSymbolAsync_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->kind, 
                                      (uint64_t)params->count, 
                                      params->stream);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy3DAsync_v3020: {
      cudaMemcpy3DAsync_v3020_params *params = 
              (cudaMemcpy3DAsync_v3020_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, params->p->kind, 
                (uint64_t)(params->p->extent.height * params->p->extent.width * 
                           params->p->extent.depth), 
                                      params->stream);
      break;
    }
    /*
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyPeerAsync_v4000: {
      cudaMemcpyPeerAsync_v4000_params *params = 
            (cudaMemcpyPeerAsync_v4000_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, cudaMemcpyDeviceToDevice, 
                                      (uint64_t)params->count, 
                                      params->stream);
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy3DPeerAsync_v4000: {
      cudaMemcpy3DPeerAsync_v4000_params *params = 
              (cudaMemcpy3DPeerAsync_v4000_params *)cbInfo->functionParams;
    
      vt_cupticb_handle_cudart_mcpyAsync(cbInfo, cudaMemcpyDeviceToDevice, 
                (uint64_t)(params->p->extent.height * params->p->extent.width * 
                           params->p->extent.depth), 
                                      params->stream);
      break;
    }*/
#endif /* !VT_CUPTI_ACTIVITY */
    /**************************************************************************/

    default: break;
  }
  /****************************************************************************/
}


#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 2))
/*
 * This callback function is used to trace the CUDA runtime API.
 *
 * @param cbid the ID of the callback function in the given domain
 * @param cbInfo information about the callback
 */
void CUPTIAPI vt_cupticb_driverAPI(CUpti_CallbackId cbid, 
                                   const CUpti_CallbackData *cbInfo)
{
  uint32_t ptid;
  uint64_t time;
  uint32_t rid_func = VT_NO_ID;
  uint32_t hash_api_rid = VT_NO_ID;
  
  if(cbid == CUPTI_DRIVER_TRACE_CBID_INVALID) return;
  
  /* vt_cntl_msg(1,"cid %d", cbid); 
  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    if(cbid == CUPTI_DRIVER_TRACE_CBID_cuCtxCreate_v2){
      cuCtxCreate_v2_params *params = 
              (cuCtxCreate_v2_params *)cbInfo->functionParams;
      vt_cuptiact_addContext(*(params->pctx), params->dev);
      
    }else if(cbid == CUPTI_DRIVER_TRACE_CBID_cuCtxCreate){
      cuCtxCreate_params *params = 
              (cuCtxCreate_params *)cbInfo->functionParams;
      vt_cuptiact_addContext(*(params->pctx), params->dev);
    }
  }*/
  
  if(!vt_cupticb_trace_driverAPI) return;
  
  /* internal callback switch 
  {
    vt_cupti_ctx_t *vtCtx = vt_cupti_getContext(cbInfo->contextUid);
    
    if(NULL != vtCtx && vtCtx->callbacks_enabled == 0) return;
  }*/
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;
  
  /* get the VampirTrace region ID for the API function */
  hash_api_rid = vt_cupticb_cudaApiFuncGet(CUPTI_CB_DOMAIN_DRIVER_API, cbid);
  if(hash_api_rid != VT_NO_ID){
    rid_func = hash_api_rid;
  }else{
    rid_func = vt_def_region(VT_MASTER_THREAD, cbInfo->functionName, VT_NO_ID,
                             VT_NO_LNO, VT_NO_LNO, "CUDRV_API", VT_FUNCTION);
    
    vt_cupticb_cudaApiFuncPut(CUPTI_CB_DOMAIN_DRIVER_API, cbid, rid_func);
  }
  
  /*********** write enter and exit records for CUDA runtime API **************/
  time = vt_pform_wtime();
  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    (void)vt_enter(ptid, &time, rid_func);
  }
  
  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    vt_exit(ptid, &time);
  }
}

/*
 * This callback function is used to handle synchronization calls.
 *
 * @param cbid the ID of the callback function in the given domain
 * @param syncData synchronization data (CUDA context, CUDA stream)
 */
void vt_cupticb_sync(CUpti_CallbackId cbid, 
                     const CUpti_SynchronizeData *syncData)
{
  if(CUPTI_CBID_SYNCHRONIZE_CONTEXT_SYNCHRONIZED == cbid){
    vt_cntl_msg(2, "[CUPTI Callbacks] Synchronize called");
    
    VT_CUPTI_LOCK();
    vt_cuptiact_flushCtxActivities(vt_cupti_getCtxNoLock(syncData->context));
    VT_CUPTI_UNLOCK();
  }
  
  /*if(CUPTI_CBID_SYNCHRONIZE_STREAM_SYNCHRONIZED == cbid){    
    vt_cntl_msg(2, "[CUPTI Callbacks] Stream synchronize called");
  }*/
}

/*
 * This callback function is used to handle resource usage.
 *
 * @param cbid the ID of the callback function in the given domain
 * @param resData resource information (CUDA context, CUDA stream)
 */
void vt_cupticb_resource(CUpti_CallbackId cbid, 
                         const CUpti_ResourceData *resData)
{  
  switch(cbid){
  /********************** CUDA memory allocation ******************************/
    case CUPTI_CBID_RESOURCE_CONTEXT_CREATED: {
      vt_cupti_ctx_t *vtCtx = NULL;
      CUcontext cuCtx = resData->context;
      
      /*vt_cntl_msg(2, "[CUPTI Callbacks] Creating context %d", cuCtx);*/
      
      
      vtCtx = vt_cupti_getCreateCtx(cuCtx);
      
      /* 
       * Create the VampirTrace CUPTI callbacks context for CUPTI >3
       * (needed for concurrent kernel tracing)
       */
#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 3))
      if(vtCtx->callbacks == NULL)
        vtCtx->callbacks = vt_cupticb_createCbCtx(vtCtx->cuDev);
#endif
      
      /* CUdevice is always '0' (try simpleP2P example)
      {
        CUdevice cuDev;
        if(CUDA_SUCCESS != cuCtxGetDevice(&cuDev)){
          vt_warning("could not get CUdevice from context");
        }
        vt_cntl_msg(1, "CUcontext %d, CUdevice %d", cuCtx, cuDev);
      }*/

      /* add the context without tracing CUDA driver API calls, if enabled */
      if(vt_cupticb_trace_driverAPI){
        cuptiEnableDomain(0, vt_cupticb_subscriber, CUPTI_CB_DOMAIN_DRIVER_API);
        vt_cuptiact_setupActivityContext(vtCtx);
        cuptiEnableDomain(1, vt_cupticb_subscriber, CUPTI_CB_DOMAIN_DRIVER_API);
      }else{
        vt_cuptiact_setupActivityContext(vtCtx);
      }
      
      break;
    }
    
    case CUPTI_CBID_RESOURCE_CONTEXT_DESTROY_STARTING: {
      vt_cntl_msg(2, "[CUPTI Callbacks] Destroying context ...");

      /* Only flush the activities of the context. The user code has to ensure, 
         that the context is synchronized */
      VT_CUPTI_LOCK();
      vt_cuptiact_flushCtxActivities(vt_cupti_getCtxNoLock(resData->context));
      VT_CUPTI_UNLOCK();
      
      break;
    }
    
    case CUPTI_CBID_RESOURCE_STREAM_CREATED: {
      /*if(vt_gpu_stream_reuse){
        uint32_t strmID;
        
        VT_CUPTI_CALL(cuptiGetStreamId(resData->context, 
                                       resData->resourceHandle.stream, 
                                       &strmID), 
                      "cuptiGetStreamId");
        
        vt_cntl_msg(2, "[CUPTI Callbacks] Creating stream %d (context %d)", 
                       strmID, resData->context);
      }*/
      
#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 3))
        if(vt_gpu_trace_kernels > 0){
          vt_cupti_ctx_t *vtCtx = vt_cupti_getCreateCtx(resData->context);
          
          /* if the device is capable of concurrent kernels */
          if(vtCtx->callbacks->concurrentKernels){
            
            vtCtx->callbacks->streamsCreated++;

            vt_cntl_msg(2, "[CUPTI Callbacks] Creating stream %d (context %d)", 
                          vtCtx->callbacks->streamsCreated, resData->context);

            if(vtCtx->callbacks->streamsCreated < 1)
              vtCtx->callbacks->streamsCreated++;
            else
              vt_cuptiact_enableConcurrentKernel(vtCtx);
          }
        }
#endif
      
      break;
    }
    
    case CUPTI_CBID_RESOURCE_STREAM_DESTROY_STARTING: {
      if(vt_gpu_stream_reuse){
        uint32_t ptid, strmID;
        uint64_t time;
  
        VT_CHECK_THREAD;
        ptid = VT_MY_THREAD;
        
        time = vt_pform_wtime();
        vt_enter(ptid, &time, vt_cupticb_rid_sync);

/*#if (defined(CUDA_VERSION) && (CUDA_VERSION < 5000))*/
        /* implicitly flush context activities via cuCtxSynchronize() */
        if(vt_cupticb_trace_driverAPI){
          cuptiEnableDomain(0, vt_cupticb_subscriber, CUPTI_CB_DOMAIN_DRIVER_API);
          CHECK_CU_ERROR(cuCtxSynchronize(), NULL);
          cuptiEnableDomain(1, vt_cupticb_subscriber, CUPTI_CB_DOMAIN_DRIVER_API);
        }else{
          CHECK_CU_ERROR(cuCtxSynchronize(), NULL);
        }
/*#else*/
        /* TODO: NVIDIA bug??? *
        * cuCtxSynchronize() runs into a lock here, therefore just flush *
        VT_CUPTI_LOCK();
        vt_cuptiact_flushCtxActivities(vt_cupti_getCtx(resData->context));
        VT_CUPTI_UNLOCK();*
*#endif*/
        
        time = vt_pform_wtime();
        vt_exit(ptid, &time);
        
        /* get the stream id from stream type */
        VT_CUPTI_CALL(cuptiGetStreamId(resData->context, 
                                       resData->resourceHandle.stream, 
                                       &strmID), 
                      "cuptiGetStreamId");
        
        /* mark the stream as destroyed to be available for reuse */
        vt_cuptiact_markStreamAsDestroyed(resData->context, 
                                          strmID);
        
        vt_cntl_msg(2, "[CUPTI Callbacks] Destroying stream %d (context %d)", 
                       strmID, resData->context);
      }
      
      break;
    }
    
    default: break;
  }
}
#endif

#if !defined(VT_CUPTI_ACTIVITY)
/*
 * Increases the "Allocated CUDA memory" counter.
 *
 * @param ctxUID CUDA context identifier (@see CUPTI callback info)
 * @param devPtr pointer to the allocated memory (needed for vtcudaFree())
 * @param size the number of bytes allocated
 */
static void vt_cupticb_handle_malloc(uint64_t ctxID, CUcontext cuCtx, 
                                     void *devPtr, size_t size)
{
  uint64_t vtTime;
  vt_cupti_callbacks_t *vtCtx = vt_cupticb_checkCtx(cuCtx, NULL);
  vt_cupti_gpumem_t *vtMalloc = (vt_cupti_gpumem_t*)malloc(sizeof(vt_cupti_gpumem_t));
  
  vtMalloc->memPtr = devPtr;
  vtMalloc->size = size;
  
  /* add malloc entry to list */
  vtMalloc->next = vtCtx->gpuMemList;
  vtCtx->gpuMemList = vtMalloc;
  
  /* increase allocated memory counter */
  vtCtx->gpuMemAllocated += size;

  /* check if first CUDA stream is available */
  if(vtCtx->strmList == NULL){
    VT_CHECK_THREAD;
    vt_cupticb_checkStream(VT_MY_THREAD, vtCtx, NULL);
  }
  
  /* write counter value */
  vtTime = vt_pform_wtime();
  vt_count(vtCtx->strmList->tid, &vtTime, vt_gpu_cid_memusage, 
           (uint64_t)(vtCtx->gpuMemAllocated));
}

/*
 * Decreases the "Allocated CUDA memory" counter.
 *
 * @param ctxUID CUDA context identifier (@see CUPTI callback info)
 * @param devPtr pointer to the allocated memory
 */
static void vt_cupticb_handle_free(uint64_t ctxUID, CUcontext cuCtx, 
                                   void *devPtr)
{
  uint64_t vtTime;
  vt_cupti_callbacks_t *vtCtx = vt_cupticb_checkCtx(cuCtx, NULL)->callbacks;
  vt_cupti_gpumem_t *curMalloc = NULL;
  vt_cupti_gpumem_t *lastMalloc = NULL;

  if(devPtr == NULL) return;

  curMalloc = vtCtx->gpuMemList;
  lastMalloc = vtCtx->gpuMemList;

  while(curMalloc != NULL){
    if(devPtr == curMalloc->memPtr){

      /* decrease allocated counter value and write it */
      vtTime = vt_pform_wtime();
      vtCtx->gpuMemAllocated -= curMalloc->size;
      vt_count(vtCtx->strmList->tid, &vtTime, vt_gpu_cid_memusage,
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
      return;
    }

    lastMalloc = curMalloc;
    curMalloc = curMalloc->next;
  }

  vt_warning("[CUPTICB] free CUDA memory, which has not been allocated!");
}

/*
 * This function handles the cudaConfigureCall callback.
 * Kernel configuration data are written on the kernel configure stack.
 * 
 * @param cbInfo information about the callback
 */
static void vt_cupticb_handle_cudart_knconf(const CUpti_CallbackData *cbInfo)
{
  /* configure call parameter have to be saved for kernel launch on a per 
   * thread basis. */
  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    vt_cupti_kernel_t *vtParams = NULL;
    cudaConfigureCall_v3020_params *params = 
           (cudaConfigureCall_v3020_params *) cbInfo->functionParams;
    vt_cupti_callbacks_t *vtCbCtx = 
            vt_cupticb_checkCtx(cbInfo->contextUid, params->stream)->callbacks;

    /* Is another kernel already configured? */
    if(vtCbCtx->stack_size > 0){
      /* memory already allocated */
      if(vtCbCtx->kernelData->prev == NULL){
        vtParams = (vt_cupti_kernel_t*)malloc(sizeof(vt_cupti_kernel_t));
        if(vtParams == NULL)
          vt_error_msg("Could not allocate memory for vt_cupti_kernel_t!");

        vtParams->prev = NULL;
      }else{
        vtParams = vtCbCtx->kernelData->prev;
      }

      /* add to kernel configure call parameter stack */
      vtParams->prev = vtCbCtx->kernelData;
      vtCbCtx->kernelData = vtParams;
    }else{
      vtParams = vtCbCtx->kernelData;
    }

    vtParams->blocksPerGrid = params->gridDim.x * params->gridDim.y
                            * params->gridDim.z;
    vtParams->threadsPerBlock = params->blockDim.x * params->blockDim.y 
                             * params->blockDim.z;

    vtParams->stream = params->stream;

    (vtCbCtx->stack_size)++;
  }
}

/*
 * This function can be called at the beginning and end of a CUDA kernel launch.
 * Time stamps will be written to the corresponding CUDA stream.
 * !!! The kernel has to be configured (cudaConfigureCall) !!!

 * @param cbInfo information about the callback
 */
static void vt_cupticb_handle_cudart_kernel(const CUpti_CallbackData *cbInfo)
{
  uint64_t time;
  
  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    uint32_t knRID = VT_NO_ID;
    const char *symName = cbInfo->symbolName;
    vt_cupticb_strm_t *vtStrm = NULL;
    vt_gpu_hn_string_t *hn = NULL;
    vt_cupti_ctx_t *vtCtx = NULL;
    vt_cupti_callbacks_t *vtCbCtx = NULL;
    uint32_t ptid;
      
    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;
    
    if(cbInfo->symbolName == NULL) symName = "_Z7noSymbolName";

    /* get the VampirTrace region ID for the kernel */
    hn = vt_gpu_stringHashGet(symName);

    if(hn){
      knRID = hn->rid;
    }else{
      char *knName = NULL;
      
      VT_CUPTI_LOCK();
      knName = vt_cuda_demangleKernel(symName);
      knRID = vt_def_region(VT_MASTER_THREAD, knName, VT_NO_ID,
                            VT_NO_LNO, VT_NO_LNO, "CUDA_KERNEL", VT_FUNCTION);
      VT_CUPTI_UNLOCK();

      hn = vt_gpu_stringHashPut(symName, knRID);
      /*hn->fname = knName;*/
    }

    /* get the VampirTrace thread ID the kernel is running on */      
    vtCtx = vt_cupticb_checkCtx(cbInfo->contextUid, NULL);
    vtCbCtx = vtCtx->callbacks;

    vtStrm = vt_cupticb_checkStream(ptid, vtCtx, vtCtx->callbacks->kernelData->stream);

    /* save address into 64 Bit correlation value for exit callback */
    *cbInfo->correlationData = (uint64_t)vtStrm;

    /* write the event records */
    CHECK_CU_ERROR(cuCtxSynchronize(), NULL);

    /* write VT kernel start events */
    time = vt_pform_wtime();
    
    if(vt_gpu_trace_idle) vt_exit(vtCbCtx->strmList->tid, &time);
    vt_enter(vtStrm->tid, &time, knRID);

    if(vt_gpu_trace_kernels > 1){
      vt_count(vtStrm->tid, &time, vt_cupti_cid_blocksPerGrid, 
               vtCbCtx->kernelData->blocksPerGrid);
      vt_count(vtStrm->tid, &time, vt_cupti_cid_threadsPerBlock, 
               vtCbCtx->kernelData->threadsPerBlock);
      vt_count(vtStrm->tid, &time, vt_cupti_cid_threadsPerKernel,
               vtCbCtx->kernelData->threadsPerBlock * 
               vtCbCtx->kernelData->blocksPerGrid);
    }
    
#if defined(VT_CUPTI_EVENTS)
    if(vt_cupticb_trace_events){
      vtCtx = vt_cuptievt_getOrCreateCurrentCtx(ptid);
      vt_cuptievt_resetCounter(vtCtx->events, vtStrm->tid, &time);
    }
#endif
    
    /* take the configure parameters from stack */
    (vtCbCtx->stack_size)--;
    if(vtCbCtx->stack_size > 0){
      vtCbCtx->kernelData = vtCbCtx->kernelData->prev;
    }
  }

  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    vt_cupticb_strm_t *vtStrm = (vt_cupticb_strm_t *)(*cbInfo->correlationData);
    uint32_t tid = vtStrm->tid;
    vt_cupti_ctx_t *vtCtx = vt_cupticb_checkCtx(cbInfo->contextUid, NULL);
    uint32_t ptid;

    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;
    
#if defined(VT_CUPTI_EVENTS)
    if(vt_cupticb_trace_events){
      time = vt_pform_wtime();
      vt_enter(ptid, &time, vt_cupticb_rid_sync);

      if(vt_cupticb_event_sampling){
        CUresult ret = CUDA_SUCCESS;
        /* sampling of CUPTI counter values */
        do{
          time = vt_pform_wtime();
          vt_cuptievt_writeCounter(vtCtx->events, tid, &time);
          ret = cuStreamQuery(vtStrm->stream);
        }while(ret != CUDA_SUCCESS);
      }else{
        CHECK_CU_ERROR(cuCtxSynchronize(), NULL);
      }

      time = vt_pform_wtime();
      vt_cuptievt_writeCounter(vtCtx->events, tid, &time);
      vt_exit(ptid, &time);
    }else
#endif /* VT_CUPTI_EVENTS */
    {   
      /*SUSPEND_CALLBACKS(vtCtx);*/
      if(vt_cupticb_syncLevel > 0){
        time = vt_pform_wtime();
        vt_enter(ptid, &time, vt_cupticb_rid_sync);
        CHECK_CU_ERROR(cuCtxSynchronize(), NULL);
        time = vt_pform_wtime();
        vt_exit(ptid, &time);
      }
      /*RESUME_CALLBACKS(vtCtx);*/
    }

    if(vt_gpu_trace_kernels > 1){
      /* write VT kernel stop events */
      vt_count(tid, &time, vt_cupti_cid_blocksPerGrid, 0);
      vt_count(tid, &time, vt_cupti_cid_threadsPerBlock, 0);
      vt_count(tid, &time, vt_cupti_cid_threadsPerKernel, 0);
    }

    vt_exit(tid, &time);

    if(vt_gpu_trace_idle){
      vt_enter(vtCtx->callbacks->strmList->tid, &time, vt_gpu_rid_idle);
    }
  }
}

/*
 * Handle synchronous CUDA runtime memory copy calls.
 *
 * @param cbInfo information about the callback
 * @param kind
 * @param bytes
 * @param time
 */
static void vt_cupticb_handle_cudart_memcpy(
                                     const CUpti_CallbackData *cbInfo,
                                     enum cudaMemcpyKind kind,
                                     uint64_t bytes, uint64_t time)
{
  uint32_t strmID;
  uint32_t ptid;
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;

  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    /* get the VampirTrace thread ID the kernel is running on */
    {
      vt_cupti_ctx_t *vtCtx = 
              vt_cupticb_checkCtx(cbInfo->contextUid, NULL);
      vt_cupticb_strm_t *vtStrm = 
              vtStrm = vt_cupticb_checkStream(ptid, vtCtx, NULL);

      strmID = vtStrm->tid;

      /* save address into 64 Bit correlation value for exit callback */
      *cbInfo->correlationData = (uint64_t)vtStrm;
      
      /* synchronize to get host waiting time */
      /*DISABLE_CUDART_DOMAIN();
      DISABLE_CUDART_CALLBACK(CUPTI_RUNTIME_TRACE_CBID_cudaDeviceSynchronize_v3020);
      SUSPEND_CALLBACKS(vtCtx);*/
      if(vt_cupticb_syncLevel > 0){
        if(vt_cupticb_syncLevel > 1) vt_enter(ptid, &time, vt_cupticb_rid_sync);
        CHECK_CU_ERROR(cuCtxSynchronize(), NULL);
        time = vt_pform_wtime();
        if(vt_cupticb_syncLevel > 1) vt_exit(ptid, &time);
      }
      /*RESUME_CALLBACKS(vtCtx);
      ENABLE_CUDART_CALLBACKS();
      ENABLE_CUDART_CALLBACK(CUPTI_RUNTIME_TRACE_CBID_cudaDeviceSynchronize_v3020);
      */
    }

    VT_CUPTI_LOCK();
    if(kind != cudaMemcpyDeviceToDevice) vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;
    vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;
    VT_CUPTI_UNLOCK();

    /*time = vt_pform_wtime();*/
    if(kind == cudaMemcpyHostToDevice){
      vt_mpi_rma_put(ptid, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToHost){
      vt_mpi_rma_get(ptid, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_get(strmID, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }
  }

  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    strmID = ((vt_cupticb_strm_t *)(*cbInfo->correlationData))->tid;

    /*time = vt_pform_wtime();*/
    if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_end(strmID, &time, vt_gpu_commCID, 0);
    }else if(kind != cudaMemcpyHostToHost){
      vt_mpi_rma_end(ptid, &time, vt_gpu_commCID, 0);
    } 
  }
}

/*
 * Handle asynchronous CUDA runtime memory copy calls.
 *
 * @param cbInfo information about the callback
 * @param kind the direction of the transfer
 * @param bytes the number of transfered bytes
 * @param cuStrm the CUDA stream
 */
static void vt_cupticb_handle_cudart_mcpyAsync(const CUpti_CallbackData *cbInfo,
                                               enum cudaMemcpyKind kind,
                                               uint64_t bytes,
                                               cudaStream_t cuStrm)
{
  uint32_t strmID;
  uint32_t ptid;
  uint64_t time;
  
  if(!vt_gpu_trace_mcpy) return;
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;

  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    /* get the VampirTrace thread ID the kernel is running on */
    {
      vt_cupti_ctx_t *vtCtx = vt_cupticb_checkCtx(cbInfo->contextUid, cuStrm);
      vt_cupticb_strm_t *vtStrm = vt_cupticb_checkStream(ptid, vtCtx, cuStrm);

      strmID = vtStrm->tid;

      /* save address into 64 Bit correlation value for exit callback */
      *cbInfo->correlationData = (uint64_t)vtStrm;
    }

    VT_CUPTI_LOCK();
    if(kind != cudaMemcpyDeviceToDevice) vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;
    vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;
    VT_CUPTI_UNLOCK();

    time = vt_pform_wtime();
    if(kind == cudaMemcpyHostToDevice){
      vt_mpi_rma_put(ptid, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToHost){
      vt_mpi_rma_get(ptid, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_get(strmID, &time, VT_GPU_RANK_ID(strmID),
                     vt_gpu_commCID, 0, bytes);
    }
  }

  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    strmID = ((vt_cupticb_strm_t *)(*cbInfo->correlationData))->tid;
    
    time = vt_pform_wtime();
    
    /* synchronize to get host waiting time */
    if(vt_cupticb_syncLevel > 0){
      if(vt_cupticb_syncLevel > 1) vt_enter(ptid, &time, vt_cupticb_rid_sync);
      CHECK_CU_ERROR(cuCtxSynchronize(), NULL);
      time = vt_pform_wtime();
      if(vt_cupticb_syncLevel > 1) vt_exit(ptid, &time);
    }

    if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_end(strmID, &time, vt_gpu_commCID, 0);
    }else if(kind != cudaMemcpyHostToHost){
      vt_mpi_rma_end(ptid, &time, vt_gpu_commCID, 0);
    } 
  }
}

static void vt_cupti_callback_finalizeContext(vt_cupti_ctx_t *vtCtx)
{
  uint32_t ptid;
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;
  
#if defined(VT_CUPTI_EVENTS)
  if(vt_cupticb_trace_events && vt_gpu_debug == 0){
    uint64_t time = vt_pform_wtime();
    vt_cupticb_strm_t *curStrm = vtCtx->callbacks->strmList;
    vt_cupti_ctx_t* vtcuptiCtx = vt_cuptievt_getOrCreateCurrentCtx(ptid);

    while(curStrm != NULL){
      vt_cuptievt_resetCounter(vtcuptiCtx, curStrm->tid, &time);
      curStrm = curStrm->next;
    }

    vt_cuptievt_finalize_device(ptid, 0);
  }
#endif /* VT_CUPTI_EVENTS */
  
#if !defined(VT_CUPTI_ACTIVITY)
  /* write idle end time to CUDA stream 0 */
  if(vt_gpu_trace_idle){
    uint64_t idle_end = vt_pform_wtime();
    vt_exit(vtCtx->callbacks->strmList->tid, &idle_end);
  }
#endif /* !VT_CUPTI_ACTIVITY */
  
  /* cleanup stream list */
  if(vtCtx->callbacks->strmList != NULL){
    free(vtCtx->callbacks->strmList);
    vtCtx->callbacks->strmList = NULL;
  }
}
#endif /* !VT_CUPTI_ACTIVITY */

/* -------------START: Implementation of public functions ------------------ */
/* ------------------------------------------------------------------------- */

/**
 * Initialize the VampirTrace CUPTI callback implementation.
 */
void vt_cupti_callback_init()
{
  if(!vt_cupticb_initialized){
    vt_cupti_init();
    VT_CUPTI_LOCK();
    if(!vt_cupticb_initialized){
      
      vt_cntl_msg(2, "[CUPTI Callbacks] Initializing ... ");
      
      /* check the CUDA APIs to be traced */
      vt_cupticb_trace_driverAPI = 0;
      vt_cupticb_trace_runtimeAPI = 0;
      
      /* check whether VT_GPUTRACE is set */
      if(vt_gpu_get_config() != 0){
        
        /* CUDA runtime API */
        if((vt_gpu_config & VT_GPU_TRACE_RUNTIME_API) == VT_GPU_TRACE_RUNTIME_API){
          vt_cupticb_trace_runtimeAPI = 1;
        }
        
        /* CUDA driver API */
        if((vt_gpu_config & VT_GPU_TRACE_DRIVER_API) == VT_GPU_TRACE_DRIVER_API){
          vt_cupticb_trace_driverAPI = 1;
        }
      }

      /* set callback for CUDA API functions */
#if defined(VT_CUPTI_ACTIVITY)  
      if(vt_cupticb_trace_runtimeAPI){
        vt_cupti_set_callback(vt_cupticb_all_ptr, 
                              CUPTI_CB_DOMAIN_RUNTIME_API,
                              CUPTI_RUNTIME_TRACE_CBID_INVALID);
      }

      if(vt_cupticb_trace_driverAPI){
        vt_cupti_set_callback(vt_cupticb_all_ptr, 
                              CUPTI_CB_DOMAIN_DRIVER_API,
                              CUPTI_DRIVER_TRACE_CBID_INVALID);
      }
#else
      if(vt_cupticb_trace_runtimeAPI){
        vt_cupti_set_callback(vt_cupticb_cudart_ptr, 
                              CUPTI_CB_DOMAIN_RUNTIME_API,
                              CUPTI_RUNTIME_TRACE_CBID_INVALID);
      }
#endif

      /* reset the hash table for CUDA API functions */
      memset(vt_cupticb_cudaApiFuncTab, VT_NO_ID, 
              VT_CUPTICB_CUDA_API_FUNC_MAX * sizeof(uint32_t));
        
      /* if GPU streams are necessary */
      if(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy || vt_gpu_trace_memusage > 0){
#if (defined(VT_MT) || defined(VT_HYB))
        VTTHRD_LOCK_IDS();
#endif
        /* initialize GPU common stuff */
        vt_gpu_init();

        /* get global counter group IDs */
        if(vt_gpu_trace_kernels > 1){
          vt_cupti_cgid_cuda_kernel = 
                            vt_def_counter_group(VT_MASTER_THREAD, "CUDA_KERNEL");

          vt_cupti_cid_blocksPerGrid = vt_def_counter(VT_MASTER_THREAD, 
                        "blocks_per_grid", "#",
                        VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                        vt_cupti_cgid_cuda_kernel, 0);
          vt_cupti_cid_threadsPerBlock = vt_def_counter(VT_MASTER_THREAD, 
                        "threads_per_block", "#",
                        VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                        vt_cupti_cgid_cuda_kernel, 0);
          vt_cupti_cid_threadsPerKernel = vt_def_counter(VT_MASTER_THREAD, 
                        "threads_per_kernel", "#",
                        VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, 
                        vt_cupti_cgid_cuda_kernel, 0);
        }

#if defined(VT_CUPTI_ACTIVITY)
        if(vt_gpu_stream_reuse){
          vt_cupticb_rid_sync = vt_def_region(VT_MASTER_THREAD, "cudaSynchronize", 
                      VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "CUDA_SYNC", VT_FUNCTION);
        }
#else
        if(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy){
          vt_cupticb_syncLevel = (uint8_t)vt_env_cudatrace_sync();
          vt_cupticb_rid_sync = vt_def_region(VT_MASTER_THREAD, "cudaSynchronize", 
                      VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "CUDA_SYNC", VT_FUNCTION);
        }
#endif
        
#if (defined(VT_MT) || defined(VT_HYB))
        VTTHRD_UNLOCK_IDS();
#endif
        
#if defined(VT_CUPTI_ACTIVITY)  
        /*if(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy || 
           vt_gpu_trace_memusage > 0)*/{
          vt_cupti_set_callback(vt_cupticb_all_ptr, 
                                CUPTI_CB_DOMAIN_RESOURCE,
                                CUPTI_RUNTIME_TRACE_CBID_INVALID);
       
          vt_cupti_activity_init();
        }
        
        if(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy){
          vt_cupti_set_callback(vt_cupticb_all_ptr, 
                                CUPTI_CB_DOMAIN_SYNCHRONIZE,
                                CUPTI_RUNTIME_TRACE_CBID_INVALID);
        }
#endif
        
#if (defined(VT_CUPTI_EVENTS) && !defined(VT_CUPTI_ACTIVITY))
      if(vt_gpu_trace_kernels > 0 && vt_env_cupti_events() != NULL){
          vt_cupticb_trace_events = 1;
          vt_cupticb_event_sampling = (uint8_t)vt_env_cupti_sampling();
          
          vt_cupti_events_init();
          
          /* TODO: check exit handler problems with CUPTI events */
          vt_gpu_debug = 1;
        }else{
          vt_cupticb_trace_events = 0;
        }
#endif /* T_CUPTI_EVENTS && ! VT_CUPTI_ACTIVITY */
        
        /* reset the GPU idle start time */
        if(vt_gpu_trace_idle) vt_gpu_init_time = vt_pform_wtime();
        
      }
      /* register the finalize function of VampirTrace CUPTI to be called before
        * the program exits */
      atexit(vt_cupti_callback_finalize);

      vt_cupticb_initialized = 1;        
    } /* !vt_cupticb_initialized */
    VT_CUPTI_UNLOCK();
  } /* !vt_cupticb_initialized */
}

/**
 * Finalize the VampirTrace CUPTI callback implementation.
 */
void vt_cupti_callback_finalize()
{
  if(!vt_cupticb_finalized && vt_cupticb_initialized){
    VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
    
    /* finalize CUPTI activities outside the lock, as it uses the same lock */
#if defined(VT_CUPTI_ACTIVITY)
    if(vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy  || 
         vt_gpu_trace_memusage > 0){
      vt_cupti_activity_finalize();
    }
#endif

    VT_CUPTI_LOCK();
    if(!vt_cupticb_finalized && vt_cupticb_initialized){

      vt_cntl_msg(2, "[CUPTI Callbacks] Finalizing ... ");
      
      if(vt_cupticb_trace_runtimeAPI || vt_cupticb_trace_driverAPI ||
         vt_gpu_trace_kernels > 0 || vt_gpu_trace_mcpy || 
         vt_gpu_trace_memusage > 0){
        VT_CUPTI_CALL(cuptiUnsubscribe(vt_cupticb_subscriber), 
                          "cuptiUnsubscribe");
      }
      
#if !defined(VT_CUPTI_ACTIVITY)
      /* clean up the VampirTrace CUPTI context list */
      while(vt_cupti_ctxList != NULL){
        vt_cupti_ctx_t *vtCtx = vt_cupti_ctxList;

        vt_cupti_ctxList = vt_cupti_ctxList->next;

        vt_cupti_callback_finalizeContext(vtCtx);

        free(vtCtx);
        vtCtx = NULL;
      }
#endif

      if(vt_cupticb_trace_events) vt_cupti_events_finalize();

      vt_gpu_finalize();
      
      if(vt_gpu_trace_kernels > 0)
        vt_gpu_stringhashClear();
      
      vt_cupticb_finalized = 1;
      VT_CUPTI_UNLOCK();
      
      vt_cupti_finalize();
    }
    
    VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
  }
}

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

#include "config.h"         /* snprintf */

#include "vt_defs.h"        /* global definitions */
#include "vt_env.h"         /* get environment variables */
#include "vt_pform.h"       /* VampirTrace time measurement */
#include "vt_defs.h"        /* VampirTrace constants */
#include "vt_error.h"       /* VampirTrace warning and error messages */
#include "vt_gpu.h"         /* common for GPU */
#include "util/hash.h"

#include "vt_cupti.h"       /* Support for CUPTI */
#include "vt_cupti_callback.h"
#include "vt_cupti_events.h"       /* Support for CUPTI events */

#include <stdio.h>
#include <string.h>


/* mutex for locking global CUPTI callback lists */
#if (defined(VT_MT) || defined(VT_HYB))
static VTThrdMutex* VTThrdMutexCuptiCB = NULL;
# define CUPTI_CB_LOCK() VTThrd_lock(&VTThrdMutexCuptiCB)
# define CUPTI_CB_UNLOCK() VTThrd_unlock(&VTThrdMutexCuptiCB)
#else /* VT_MT || VT_HYB */
# define CUPTI_CB_LOCK()
# define CUPTI_CB_UNLOCK()
#endif /* VT_MT || VT_HYB */

#define CHECK_CUPTI_ERROR(_err, _msg)                    \
  if(_err != CUPTI_SUCCESS){                             \
    vt_cupti_handleError(_err, _msg,__FILE__, __LINE__); \
  }

#define DISABLE_CUDART_CALLBACK(_cbid)                                         \
  {                                                                            \
    CUptiResult cuptiErr = cuptiEnableCallback(                                \
                         0, vt_cupticb_cudart_subscriber,                      \
                         CUPTI_CB_DOMAIN_RUNTIME_API,                          \
                         _cbid);                                               \
    CHECK_CUPTI_ERROR(cuptiErr, "cuptiEnableCallback");                        \
  }

#define ENABLE_CUDART_CALLBACK(_cbid)                                          \
  {                                                                            \
    CUptiResult cuptiErr = cuptiEnableCallback(                                \
                         1, vt_cupticb_cudart_subscriber,                      \
                         CUPTI_CB_DOMAIN_RUNTIME_API,                          \
                         _cbid);                                               \
    CHECK_CUPTI_ERROR(cuptiErr, "cuptiEnableCallback");                        \
  }

#define DISABLE_CUDART_CALLBACKS()                                             \
  {                                                                            \
    CUptiResult cuptiErr = cuptiEnableDomain(0, vt_cupticb_cudart_subscriber,  \
                                             CUPTI_CB_DOMAIN_RUNTIME_API);     \
    CHECK_CUPTI_ERROR(cuptiErr, "cuptiEnableDomain");                          \
  }

#define ENABLE_CUDART_CALLBACKS()                                              \
  {                                                                            \
    CUptiResult cuptiErr = cuptiEnableDomain(1, vt_cupticb_cudart_subscriber,  \
                                             CUPTI_CB_DOMAIN_RUNTIME_API);     \
    CHECK_CUPTI_ERROR(cuptiErr, "cuptiEnableDomain");                          \
  }

#define SUSPEND_CALLBACKS(_vtCtx) _vtCtx->callbacks_enabled = 0;
#define RESUME_CALLBACKS(_vtCtx) _vtCtx->callbacks_enabled = 1;

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
}vt_cupticb_kernel_t;

/* 
 * structure of a VampirTrace CUDA malloc (initiated with cudaMalloc*() 
 */
typedef struct vt_cupticb_gpumem_st
{
  void *memPtr;                 /**< pointer value to allocated memory */
  size_t size;                  /**< number of bytes allocated */
  uint32_t tid;                 /**< thread id used with this malloc */
  struct vt_cupticb_gpumem_st *next;
}vt_cupticb_gpumem_t;

/*
 * VampirTrace CUDA context element
 *  - used in single linked list
 */
typedef struct vt_cupticb_ctx_st
{
  uint64_t ctxUID;       /**< unique CUDA context ID (available in callback) */
  /*CUcontext cuCtx;        *< the CUDA context */
  CUdevice dev;           /**< the CUDA device */
  vt_cupticb_gpumem_t *gpuMemList;    /**< list of allocated GPU memory fields */
  size_t gpuMemAllocated;          /**< memory allocated on CUDA device */
  vt_cupticb_strm_t *strmList; /**< CUDA stream list */
  vt_cupticb_kernel_t *kernelData;  /**< pointer to top of CUDA runtime kernel 
                                         configuration stack */
  uint8_t stack_size;               /**< number of params on the stack */
  uint8_t callbacks_enabled; /**< execute callback function? */
  struct vt_cupticb_ctx_st *next;
}vt_cupticb_ctx_t;

/* list of VampirTrace CUDA contexts */
static vt_cupticb_ctx_t* vt_cupticb_ctxList = NULL;

static CUpti_SubscriberHandle vt_cupticb_cudart_subscriber;

/* flag: tracing of CUDA runtime API enabled? */
static uint8_t vt_cupticb_trace_cudart = 0;

/* flag: tracing of kernels enabled? */
static uint8_t vt_cupticb_trace_kernels = 1;

/* flag: tracing of asynchronous memory copies enabled? */
static uint8_t vt_cupticb_trace_mcpyAsync = 0;

/* flag: use CUPTI events for counter capturing? */
static uint8_t vt_cupticb_trace_events = 1;

/* flag: sampling for CUPTI counter values enabled? */
static uint8_t vt_cupticb_event_sampling = 0;

/* flag: write GPU idle time as region in CUDA stream 0? */
static uint8_t vt_cupticb_trace_gpu_idle = 0;

/* flag: tracing of cudaMalloc*() and cudaFree*() enabled? */
static uint8_t vt_cupticb_trace_gpu_mem = 0;

/* initialization and finalization flags */
static uint8_t vt_cupticb_initialized = 0;
static uint8_t vt_cupticb_finalized = 0;

/* 
 * Synchronization Level:
 * 0 no extra synchronization
 * 1 synchronize before synchronous memory copy or synchronization - correct
 *   data transfer rates for communication
 * 2 show synchronization in extra region group to get host wait time
 */
static uint8_t vt_cupticb_syncLevel = 3;

/* VampirTrace region ID for GPU idle */
static uint32_t vt_cupticb_rid_idle = VT_NO_ID;

/* VampirTrace region ID for synchronization of host and CUDA device*/
static uint32_t vt_cupticb_rid_sync = VT_NO_ID;

/* global counter IDs */
static uint32_t vt_cupticb_cid_blocksPerGrid;
static uint32_t vt_cupticb_cid_threadsPerBlock;
static uint32_t vt_cupticb_cid_threadsPerKernel;
static uint32_t vt_cupticb_cid_cudaMalloc;   /* GPU memory allocation counter */

/**************** The callback functions to be registered *********************/

/* CUDA runtime API callback function */
/* some of CUPTI API functions have changed */
#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 2))
void CUPTIAPI vt_cupticb_cudart(void *, CUpti_CallbackDomain,
                                CUpti_CallbackId, const void *);
void (*vt_cupticb_cudart_ptr)(void *, CUpti_CallbackDomain, 
                              CUpti_CallbackId, const void *)
      = vt_cupticb_cudart;
#else
void CUPTIAPI vt_cupticb_cudart(void *, CUpti_CallbackDomain,
                                CUpti_CallbackId, const CUpti_CallbackData *);
void (*vt_cupticb_cudart_ptr)(void *, CUpti_CallbackDomain, 
                              CUpti_CallbackId, const CUpti_CallbackData *)
      = vt_cupticb_cudart;
#endif

/******************************************************************************/

/*********************** Internal function declarations ***********************/
static void vt_cupticb_handle_cudart_knconf(const CUpti_CallbackData *);
static void vt_cupticb_handle_cudart_kernel(const CUpti_CallbackData *);

static void vt_cupticb_handle_cudart_memcpy(const CUpti_CallbackData *, 
                                       enum cudaMemcpyKind, uint64_t, uint64_t);
static void vt_cupticb_handle_cudart_mcpyAsync(const CUpti_CallbackData *cbInfo,
                 enum cudaMemcpyKind kind, uint64_t bytes, cudaStream_t cuStrm);

static void vt_cupticb_handle_malloc(uint64_t, void *, size_t);
static void vt_cupticb_handle_free(uint64_t ctxUID, void *devPtr);
/******************************************************************************/


/* hashing of CUDA API function and kernel names */
#define VT_CUPTICB_CUDA_API_HASH_MAX 512

static uint32_t vt_cupticb_cudaApiHtab[VT_CUPTICB_CUDA_API_HASH_MAX];

static uint32_t vt_cupticb_cudaApiHashFunc(CUpti_CallbackDomain domain,
                                    CUpti_CallbackId cid)
{
  uint32_t idx = 0;
  uint8_t offset = 0;
  
  if(domain == CUPTI_CB_DOMAIN_DRIVER_API) offset = 255;
  
  idx = offset + (uint32_t)cid;
  
  if(idx >= VT_CUPTICB_CUDA_API_HASH_MAX) 
    vt_error_msg("[CUPTI] Hash table for CUDA API callbacks to small!");
  
  return (uint32_t)idx;
}

static void vt_cupticb_cudaApiHashPut(CUpti_CallbackDomain domain, 
                                    CUpti_CallbackId cid, uint32_t rid)
{  
  vt_cupticb_cudaApiHtab[vt_cupticb_cudaApiHashFunc(domain, cid)] = rid;
}

static uint32_t vt_cupticb_cudaApiHashGet(CUpti_CallbackDomain domain, 
                                        CUpti_CallbackId cid)
{
  return vt_cupticb_cudaApiHtab[vt_cupticb_cudaApiHashFunc(domain, cid)];
}


/************************ hashing of CUDA kernel names ************************/
#define VT_CUPTICB_KERNEL_HASH_MAX 1021

typedef struct vt_cupticb_knHn_st {
  char*                      sname;  /**< name of the symbol */
  /*char*                      fname;  *< name of the function */
  uint32_t                   rid;    /**< associated region group identifier */
  struct vt_cupticb_knHn_st* next;   /**< bucket for collision */
} vt_cupticb_kernel_hn_t;

static vt_cupticb_kernel_hn_t* vt_cupticb_kernel_htab[VT_CUPTICB_KERNEL_HASH_MAX];

static void* vt_cupticb_kernelHashPut(const char* n, uint32_t rid)
{
  uint32_t id = (uint32_t)vt_hash((uint8_t*)n, strlen(n), 0) 
              % VT_CUPTICB_KERNEL_HASH_MAX;
  vt_cupticb_kernel_hn_t *add = 
                (vt_cupticb_kernel_hn_t*)malloc(sizeof(vt_cupticb_kernel_hn_t));
  
  add->sname = strdup(n);
  /*add->fname = NULL;*/
  add->rid = rid;
  add->next = vt_cupticb_kernel_htab[id];
  vt_cupticb_kernel_htab[id] = add;
  
  return add;
}

static void* vt_cupticb_kernelHashGet(const char* n)
{
  uint32_t id = (uint32_t)vt_hash((uint8_t*)n, strlen(n), 0) % VT_CUPTICB_KERNEL_HASH_MAX;
  vt_cupticb_kernel_hn_t *curr = vt_cupticb_kernel_htab[id];
  
  while ( curr ) {
    if ( strcmp( curr->sname, n ) == 0 )
      return curr;

    curr = curr->next;
  }
  
  return NULL;
}

static void vt_cupticb_hashClear(void)
{
  int i;
  vt_cupticb_kernel_hn_t* tmp_node;

  for ( i = 0; i < VT_CUPTICB_KERNEL_HASH_MAX; i++ )
  {
    while( vt_cupticb_kernel_htab[i] )
    {
      tmp_node = vt_cupticb_kernel_htab[i]->next;
      free( vt_cupticb_kernel_htab[i]->sname );
      free( vt_cupticb_kernel_htab[i] );
      vt_cupticb_kernel_htab[i] = tmp_node;
    }
  }
}

/*
 * Handles errors returned from CUPTI function calls.
 * 
 * @param ecode the CUDA driver API error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
static void vt_cupti_handleError(CUptiResult err, const char* msg,
                                 const char *file, const int line)
{
  const char *errstr;
  
  if(msg != NULL) vt_cntl_msg(1, msg);
  
  cuptiGetResultString(err, &errstr);
  
  if(vt_gpu_error){
    vt_error_msg("[CUPTI] %s:%d:'%s'", file, line, errstr);
  }else{
    vt_warning("[CUPTI] %s:%d:'%s'", file, line, errstr);
  }
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
static void vt_cupti_set_callback(CUpti_SubscriberHandle *subscriber, 
                                  CUpti_CallbackFunc callback,
                                  CUpti_CallbackDomain domain,
                                  CUpti_CallbackId cbid)
{
  CUptiResult cuptiErr;
  static uint8_t initflag = 1;

  if(initflag){
    initflag = 0;
    
    CHECK_CU_ERROR(cuInit(0), "cuInit");
  }

  cuptiErr = cuptiSubscribe(subscriber, callback, NULL);
  CHECK_CUPTI_ERROR(cuptiErr, "cuptiSubscribe");
  
  if((cbid == CUPTI_RUNTIME_TRACE_CBID_INVALID) || 
     (cbid == CUPTI_DRIVER_TRACE_CBID_INVALID)){
    cuptiErr = cuptiEnableDomain(1, *subscriber, domain);
    CHECK_CUPTI_ERROR(cuptiErr, "cuptiEnableDomain");
  }else{
    cuptiErr = cuptiEnableCallback(1, *subscriber, domain, cbid);
    CHECK_CUPTI_ERROR(cuptiErr, "cuptiEnableCallback");
  }
}

/*
 * Creates a VampirTrace CUPTI stream object and returns it.
 *
 *  @param stream the CUDA stream id
 *  @param ptid the VampirTrace thread ID of the calling thread
 *
 *  @return the created stream object
 */
static vt_cupticb_strm_t* vt_cupticb_createStream(uint32_t ptid, 
                                                  CUstream stream,
                                                  CUdevice device)
{
  char thread_name[16];
  vt_cupticb_strm_t *vtStrm;

  /* allocate memory for stream */
  vtStrm = (vt_cupticb_strm_t*)malloc(sizeof(vt_cupticb_strm_t));
  if(vtStrm == NULL) vt_error_msg("malloc(sizeof(vt_cupti_strm_t)) failed!");
  vtStrm->next = NULL;
  vtStrm->stream = stream;

  /* create VT-User-Thread with name and parent id and get its id */
  if(-1 == snprintf(thread_name, 15, "CUDA[%d]", (uint32_t)device))
    vt_cntl_msg(1, "Could not create thread name for CUDA thread!");
  vt_gpu_registerThread(thread_name, ptid, &(vtStrm->tid));

  /* set the threads property to GPU */
  CUPTI_CB_LOCK();
    vt_gpu_prop[vtStrm->tid] = VTGPU_GPU;
  CUPTI_CB_UNLOCK();
  
  /* set count values to zero */
  vt_count(vtStrm->tid, &vt_start_time, vt_cupticb_cid_blocksPerGrid, 0);
  vt_count(vtStrm->tid, &vt_start_time, vt_cupticb_cid_threadsPerBlock, 0);
  vt_count(vtStrm->tid, &vt_start_time, vt_cupticb_cid_threadsPerKernel, 0);
  
  if(vt_cupticb_trace_gpu_mem)
    vt_count(vtStrm->tid, &vt_start_time, vt_cupticb_cid_cudaMalloc, 0);

  return vtStrm;
}

/*
 * Retrieve the VampirTrace CUPTI context by its ID or NULL if not available.
 * 
 * @param cuCtxID ID of the CUDA context
 * 
 * @return the VampirTrace CUPTI context object
 */
static vt_cupticb_ctx_t* vt_cupticb_getCtx(uint64_t cuCtxUID)
{
  vt_cupticb_ctx_t *vtCtx = NULL;
  
  /*** search CUDA context in list ***/
  CUPTI_CB_LOCK();
  vtCtx = vt_cupticb_ctxList;
  while(vtCtx != NULL){
    if(vtCtx->ctxUID == cuCtxUID){
      CUPTI_CB_UNLOCK();
      return vtCtx;
    }
    vtCtx = vtCtx->next;
  }     
  CUPTI_CB_UNLOCK();
  
  return NULL;
}

/*
 * Creates new VampirTrace CUPTI callback context.
 * 
 * @param cuCtxID ID of the CUDA context
 * 
 * @return the VampirTrace CUPTI context object
 */
static vt_cupticb_ctx_t* vt_cupticb_createCtx(uint64_t cuCtxUID)
{
  vt_cupticb_ctx_t *vtCtx = (vt_cupticb_ctx_t*)malloc(sizeof(vt_cupticb_ctx_t));
  if(vtCtx == NULL) 
    vt_error_msg("Could not allocate memory for vt_cupticb_ctx_t!");
  
  vtCtx->ctxUID = cuCtxUID;
  
  /* enable handling of callbacks */
  vtCtx->callbacks_enabled = 1;
  
  CHECK_CU_ERROR(cuCtxGetDevice(&vtCtx->dev), NULL);
  
  /* initialize GPU memory allocation parameter */
  vtCtx->gpuMemList = NULL;
  vtCtx->gpuMemAllocated = 0;
  
  
  /* create first empty CUDA stream */
  VT_CHECK_THREAD;
  vtCtx->strmList = vt_cupticb_createStream(VT_MY_THREAD, NULL, vtCtx->dev);
  
  if(vt_cupticb_trace_gpu_idle)
    vt_enter(vtCtx->strmList->tid, &vt_start_time, vt_cupticb_rid_idle);
  
  /* initialize CUDA kernel configure stack */
  {
    vt_cupticb_kernel_t *vtKn = NULL;
    
    vtKn = (vt_cupticb_kernel_t*)malloc(sizeof(vt_cupticb_kernel_t));
    if(vtKn == NULL) 
      vt_error_msg("Could not allocate memory for vt_cupti_kernel_t!");
    
    vtKn->prev = NULL;
    vtCtx->kernelData = vtKn;
    vtCtx->stack_size = 0;
  }
  
  return vtCtx;
}

/*
 * Lookup a VampirTrace CUPTI callback context by its ID and if not available,
 * create a new one.
 * 
 * @param cuCtxID ID of the CUDA context
 * 
 * @return the VampirTrace CUPTI context object
 */
static vt_cupticb_ctx_t* vt_cupticb_checkCtx(uint64_t cuCtxUID)
{
  vt_cupticb_ctx_t *vtCtx = NULL;
  
  /* search for existing CUPTI context */
  vtCtx = vt_cupticb_getCtx(cuCtxUID);
  
  if(vtCtx == NULL){
    /* create new CUPTI callback context */
    vtCtx = vt_cupticb_createCtx(cuCtxUID);

    /* prepend CUDA context to global list */
    CUPTI_CB_LOCK();
      vtCtx->next = vt_cupticb_ctxList;
      vt_cupticb_ctxList = vtCtx;
    CUPTI_CB_UNLOCK();
  }
  
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
                                                 vt_cupticb_ctx_t *vtCtx, 
                                                 CUstream *cuStrm)
{
  vt_cupticb_strm_t *currStrm, *lastStrm;
  
  currStrm = vtCtx->strmList;
  
  /* no VampirTrace stream object available yet, create first stream */
  if(currStrm == NULL){
    if(cuStrm != NULL) 
      currStrm = vt_cupticb_createStream(ptid, *cuStrm, vtCtx->dev);
    else 
      currStrm = vt_cupticb_createStream(ptid, NULL, vtCtx->dev);
    
    vtCtx->strmList = currStrm;
    
    return currStrm;
  }
  
  /* if no CUDA stream is given, return first stream in list */
  if(cuStrm == NULL) return currStrm;
  if(*cuStrm == NULL) return currStrm;
  
  /* lookup the current stream */
  do{
    if(*cuStrm == currStrm->stream) return currStrm;
    lastStrm = currStrm;
    currStrm = currStrm->next;
  }while(currStrm != NULL);

  /* create new VampirTrace Stream structure/object and append it to list */
  lastStrm->next = vt_cupticb_createStream(ptid, *cuStrm, vtCtx->dev);
  
  return lastStrm->next;
}

/*
 * Parse the device function name:
 * "_Z<kernel_length><kernel_name><templates>..." (no name space)
 * "_ZN<ns_length><ns_name>...<ns_length><ns_name><kernel_length>..." (with name space)
 *
 * @param elem pointer to the kernel element
 * @param devFunc the CUDA internal kernel function name
 */
static void vt_cupticb_extractKernelName(char *kname, const char* devFunc)
{
  int i = 0;       /* position in device function (source string) */
  int nlength = 0; /* length of name space or kernel */
  int ePos = 0;    /* position in final kernel string */
  char *curr_elem, kn_templates[VTGPU_KERNEL_STRING_SIZE];
  char *tmpEnd, *tmpElemEnd;

  /*vt_cntl_msg(1,"[CUDART] device function name: %s'", devFunc);*/

  /* init for both cases: name space available or not */
  if(devFunc[2] == 'N'){
    nlength = atoi(&devFunc[3]); /* get length of first name space */
    i = 4;
  }else{
    nlength = atoi(&devFunc[2]); /* get length of kernel */
    i = 3;
  }

  /* unless string null termination */
  while(devFunc[i] != '\0'){
    /* found either name space or kernel name (no digits) */
    if(devFunc[i] < '0' || devFunc[i] > '9'){
      /* copy name to kernel function */
      if((ePos + nlength) < VTGPU_KERNEL_STRING_SIZE){
        (void)strncpy(&kname[ePos], &devFunc[i], nlength);
        ePos += nlength; /* set next position to write */
      }else{
        nlength = VTGPU_KERNEL_STRING_SIZE - ePos;
        (void)strncpy(&kname[ePos], &devFunc[i], nlength);
        vt_cntl_msg(1,"[CUDART]: kernel name '%s' contains more than %d chars!",
                      devFunc, VTGPU_KERNEL_STRING_SIZE);
        return;
      }

      i += nlength; /* jump over name */
      nlength = atoi(&devFunc[i]); /* get length of next name space or kernel */

      /* finish if no digit after name space or kernel */
      if(nlength == 0){
        kname[ePos] = '\0'; /* set string termination */
        break;
      }else{
        if((ePos + 3) < VTGPU_KERNEL_STRING_SIZE){
          (void)strncpy(&kname[ePos], "::\0", 3);
          ePos += 2;
        }else{
          vt_cntl_msg(1,"[CUDART]: kernel name '%s' contains more than %d chars!",
                        devFunc, VTGPU_KERNEL_STRING_SIZE);
          return;
        }
      }
    }else i++;
  }

  /* copy the end of the kernel name string to extract templates */
  if(-1 == snprintf(kn_templates, VTGPU_KERNEL_STRING_SIZE, "%s", &devFunc[i+1]))
    vt_cntl_msg(1, "[CUDART]: Error parsing kernel '%s'", devFunc);
  curr_elem = kn_templates; /* should be 'L' */

  /* search templates (e.g. "_Z10cptCurrentILb1ELi10EEv6SField8SParListifff") */
  tmpEnd=strstr(curr_elem,"EE");
  /* check for templates: curr_elem[0] points to 'L' AND string contains "EE" */
  if(tmpEnd != NULL && curr_elem[0]=='L'){ /* templates exist */
    tmpEnd[1] = '\0'; /* set 2nd 'E' to \0 as string end marker */

    /* write at position 'I' with '<' */
    /* elem->name[ePos]='<'; */
    if(-1 == snprintf(&(kname[ePos]),VTGPU_KERNEL_STRING_SIZE-ePos,"<"))
      vt_cntl_msg(1,"[CUDART] Parsing templates of kernel '%s' failed!", devFunc);
    ePos++; /* continue with next character */

    do{
      int res;
      curr_elem++; /* set pointer to template type length or template type */
      /* find end of template element */
      tmpElemEnd = strchr(curr_elem + atoi(curr_elem), 'E');
      tmpElemEnd[0] = '\0'; /* set termination char after template element */
      /* find next non-digit char */
      while(*curr_elem >= '0' && *curr_elem <= '9') curr_elem++;
      /* append template value to kernel name */
      if(-1 == (res = snprintf(&(kname[ePos]),
                               VTGPU_KERNEL_STRING_SIZE-ePos,"%s,",curr_elem)))
        vt_cntl_msg(1,"[CUDART]: Parsing templates of kernel '%s' crashed!", devFunc);
      ePos += res; /* continue after template value */
      curr_elem =tmpElemEnd + 1; /* set current element to begin of next template */
    }while(tmpElemEnd < tmpEnd);
    if((ePos-1) < VTGPU_KERNEL_STRING_SIZE) (void)strncpy(&kname[ePos-1], ">\0", 2);
    else vt_cntl_msg(1,"[CUDART]: Templates of '%s' too long for internal buffer!", devFunc);
  } /* else: kernel has no templates */
  /*vt_cntl_msg(1,"[CUDART] function name: %s'",e->name);*/
}

/*
 * This callback function is used to trace the CUDA runtime API.
 *
 * @param userdata pointer to the user data
 * @param domain the callback domain (runtime or driver API)
 * @param cbid the ID of the callback function in the given domain
 * @param cbInfo information about the callback
 */
void CUPTIAPI vt_cupticb_cudart(void *userdata, 
                                CUpti_CallbackDomain domain,
                                CUpti_CallbackId cbid, 
#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 2))
                                const void *cbInf)
{
  const CUpti_CallbackData *cbInfo = (CUpti_CallbackData*) cbInf;
#else
                                const CUpti_CallbackData *cbInfo)
{
#endif
  uint32_t ptid;
  uint64_t time;
  uint32_t rid_func = VT_NO_ID;
  uint32_t hash_api_rid = VT_NO_ID;
  uint8_t do_trace = 0;
  
  if(cbid == CUPTI_RUNTIME_TRACE_CBID_INVALID) return;
  
  /* internal callback switch 
  {
    vt_cupti_ctx_t *vtCtx = vt_cupti_getContext(cbInfo->contextUid);
    
    if(NULL != vtCtx && vtCtx->callbacks_enabled == 0) return;
  }*/
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;
  
  /* get the VampirTrace region ID for the API function */
  hash_api_rid = vt_cupticb_cudaApiHashGet(domain, cbid);
  if(hash_api_rid != VT_NO_ID){
    rid_func = hash_api_rid;
  }else{
    rid_func = vt_def_region(VT_MASTER_THREAD, cbInfo->functionName, VT_NO_ID,
                             VT_NO_LNO, VT_NO_LNO, "CUDART_API", VT_FUNCTION);
    
    vt_cupticb_cudaApiHashPut(domain, cbid, rid_func);
  }
  
  /*********** write enter and exit records for CUDA runtime API **************/
  time = vt_pform_wtime();
  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    do_trace = vt_enter(ptid, &time, rid_func);
  }
  
  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    vt_exit(ptid, &time);
  }

  /*
   * Semantic Function Instrumentation
   */
  switch(cbid){
  /****************** the CUDA runtime kernel configure call ******************/
    case CUPTI_RUNTIME_TRACE_CBID_cudaConfigureCall_v3020: {
      if(vt_cupticb_trace_kernels) 
        vt_cupticb_handle_cudart_knconf(cbInfo);

      break;
    }

    /***** the CUDA runtime kernel launch ******/
    case CUPTI_RUNTIME_TRACE_CBID_cudaLaunch_v3020: {
      if(vt_cupticb_trace_kernels) 
        vt_cupticb_handle_cudart_kernel(cbInfo);
      
      break;
    }
  /****************************************************************************/
  
  /********************** CUDA memory allocation ******************************/
    case CUPTI_RUNTIME_TRACE_CBID_cudaMalloc_v3020: {
      if(vt_cupticb_trace_gpu_mem && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMalloc_v3020_params *params = 
                            (cudaMalloc_v3020_params *)cbInfo->functionParams;
      
        vt_cupticb_handle_malloc(cbInfo->contextUid, *(params->devPtr), 
                                params->size);
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMallocPitch_v3020: {
      if(vt_cupticb_trace_gpu_mem && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMallocPitch_v3020_params *params = 
                         (cudaMallocPitch_v3020_params *)cbInfo->functionParams;
      
        vt_cupticb_handle_malloc(cbInfo->contextUid, *(params->devPtr), 
                                params->height * (*(params->pitch)));
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMallocArray_v3020: {
      if(vt_cupticb_trace_gpu_mem && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMallocArray_v3020_params *params = 
                         (cudaMallocArray_v3020_params *)cbInfo->functionParams;
      
        vt_cupticb_handle_malloc(cbInfo->contextUid, *(params->array), 
                                params->height * params->width);
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMalloc3D_v3020: {
      if(vt_cupticb_trace_gpu_mem && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMalloc3D_v3020_params *params = 
                         (cudaMalloc3D_v3020_params *)cbInfo->functionParams;
      
        vt_cupticb_handle_malloc(cbInfo->contextUid, params->pitchedDevPtr->ptr, 
   params->pitchedDevPtr->pitch * params->extent.height * params->extent.depth);
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMalloc3DArray_v3020: {
      if(vt_cupticb_trace_gpu_mem && cbInfo->callbackSite == CUPTI_API_EXIT){
        cudaMalloc3DArray_v3020_params *params = 
                       (cudaMalloc3DArray_v3020_params *)cbInfo->functionParams;
      
        vt_cupticb_handle_malloc(cbInfo->contextUid, *(params->array), 
          params->extent.width * params->extent.height * params->extent.depth);
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaFree_v3020: {
      if(vt_cupticb_trace_gpu_mem && cbInfo->callbackSite == CUPTI_API_ENTER){
        vt_cupticb_handle_free(cbInfo->contextUid, 
                    ((cudaFree_v3020_params *)cbInfo->functionParams)->devPtr);
      }
      
      break;
    }
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaFreeArray_v3020: {
      if(vt_cupticb_trace_gpu_mem && cbInfo->callbackSite == CUPTI_API_ENTER){
        vt_cupticb_handle_free(cbInfo->contextUid, 
                 ((cudaFreeArray_v3020_params *)cbInfo->functionParams)->array);
      }
      
      break;
    }
    
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
    
    case CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyPeer_v4000: {
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
    }
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
    }
    /**************************************************************************/

    default: break;
  }
  /****************************************************************************/
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
    vt_cupticb_kernel_t *vtParams = NULL;
    vt_cupticb_ctx_t *vtCtx = vt_cupticb_checkCtx(cbInfo->contextUid);
    cudaConfigureCall_v3020_params * params = 
           (cudaConfigureCall_v3020_params *) cbInfo->functionParams;

    /* Is another kernel already configured? */
    if(vtCtx->stack_size > 0){
      /* memory already allocated */
      if(vtCtx->kernelData->prev == NULL){
        vtParams = (vt_cupticb_kernel_t*)malloc(sizeof(vt_cupticb_kernel_t));
        if(vtParams == NULL)
          vt_error_msg("Could not allocate memory for vt_cupti_kernel_t!");

        vtParams->prev = NULL;
      }else{
        vtParams = vtCtx->kernelData->prev;
      }

      /* add to kernel configure call parameter stack */
      vtParams->prev = vtCtx->kernelData;
      vtCtx->kernelData = vtParams;
    }else{
      vtParams = vtCtx->kernelData;
    }

    vtParams->blocksPerGrid = params->gridDim.x * params->gridDim.y
                            * params->gridDim.z;
    vtParams->threadsPerBlock = params->blockDim.x * params->blockDim.y 
                             * params->blockDim.z;

    vtParams->stream = params->stream;

    (vtCtx->stack_size)++;
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
    vt_cupticb_kernel_hn_t *hn = NULL;
    vt_cupticb_ctx_t *vtCtx = NULL;
    uint32_t ptid;
      
    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;

    /* get the VampirTrace region ID for the kernel */
    hn = vt_cupticb_kernelHashGet(symName);

    if(hn){
      knRID = hn->rid;
    }else{
      char knName[VTGPU_KERNEL_STRING_SIZE];

      vt_cupticb_extractKernelName(knName, symName);
      knRID = vt_def_region(VT_MASTER_THREAD, knName, VT_NO_ID,
                            VT_NO_LNO, VT_NO_LNO, "CUDA_KERNEL", VT_FUNCTION);

      hn = vt_cupticb_kernelHashPut(symName, knRID);
      /*hn->fname = knName;*/
    }

    /* get the VampirTrace thread ID the kernel is running on */      
    {
      vtCtx = vt_cupticb_checkCtx(cbInfo->contextUid);

      vtStrm = vt_cupticb_checkStream(ptid, vtCtx, &vtCtx->kernelData->stream);

      /* save address into 64 Bit correlation value for exit callback */
      *cbInfo->correlationData = (uint64_t)vtStrm;
    }

    /* write the event records */
    CHECK_CU_ERROR(cuCtxSynchronize(), NULL);

    /* write VT kernel start events */
    time = vt_pform_wtime();
    
    if(vt_cupticb_trace_gpu_idle) vt_exit(vtCtx->strmList->tid, &time);
    vt_enter(vtStrm->tid, &time, knRID);

    vt_count(vtStrm->tid, &time, vt_cupticb_cid_blocksPerGrid, 
             vtCtx->kernelData->blocksPerGrid);
    vt_count(vtStrm->tid, &time, vt_cupticb_cid_threadsPerBlock, 
             vtCtx->kernelData->threadsPerBlock);
    vt_count(vtStrm->tid, &time, vt_cupticb_cid_threadsPerKernel,
             vtCtx->kernelData->threadsPerBlock * 
             vtCtx->kernelData->blocksPerGrid);
    
    if(vt_cupticb_trace_events){
      vt_cuptievt_ctx_t *vtcuptiCtx = vt_cuptievt_getCurrentContext(ptid);
      vt_cuptievt_resetCounter(vtcuptiCtx, vtStrm->tid, &time);
    }
    
    /* take the configure parameters from stack */
    (vtCtx->stack_size)--;
    if(vtCtx->stack_size > 0){
      vtCtx->kernelData = vtCtx->kernelData->prev;
    }
  }

  if(cbInfo->callbackSite == CUPTI_API_EXIT){
    vt_cupticb_strm_t *vtStrm = (vt_cupticb_strm_t *)(*cbInfo->correlationData);
    uint32_t tid = vtStrm->tid;
    vt_cupticb_ctx_t *vtCtx = vt_cupticb_checkCtx(cbInfo->contextUid);
    uint32_t ptid;
      
    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;
    
    if(vt_cupticb_trace_events){
      vt_cuptievt_ctx_t *vtcuptiCtx = vt_cuptievt_getCurrentContext(ptid);

      time = vt_pform_wtime();
      vt_enter(ptid, &time, vt_cupticb_rid_sync);

      if(vt_cupticb_event_sampling){
        CUresult ret = CUDA_SUCCESS;
        /* sampling of CUPTI counter values */
        do{
          time = vt_pform_wtime();
          vt_cuptievt_writeCounter(vtcuptiCtx, tid, &time);
          ret = cuStreamQuery(vtStrm->stream);
        }while(ret != CUDA_SUCCESS);
      }else{
        CHECK_CU_ERROR(cuCtxSynchronize(), NULL);
      }

      time = vt_pform_wtime();
      vt_cuptievt_writeCounter(vtcuptiCtx, tid, &time);
      vt_exit(ptid, &time);
    }else{   
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

    /* write VT kernel stop events */
    vt_count(tid, &time, vt_cupticb_cid_blocksPerGrid, 0);
    vt_count(tid, &time, vt_cupticb_cid_threadsPerBlock, 0);
    vt_count(tid, &time, vt_cupticb_cid_threadsPerKernel, 0);

    vt_exit(tid, &time);

    if(vt_cupticb_trace_gpu_idle){
      vt_enter(vtCtx->strmList->tid, &time, vt_cupticb_rid_idle);
    }
  }
}

/*
 * Increases the "Allocated CUDA memory" counter.
 *
 * @param ctxUID CUDA context identifier (@see CUPTI callback info)
 * @param devPtr pointer to the allocated memory (needed for vtcudaFree())
 * @param size the number of bytes allocated
 */
static void vt_cupticb_handle_malloc(uint64_t ctxUID, void *devPtr, 
                                            size_t size)
{
  uint64_t vtTime;
  vt_cupticb_ctx_t *vtCtx = vt_cupticb_checkCtx(ctxUID);
  vt_cupticb_gpumem_t *vtMalloc = (vt_cupticb_gpumem_t*)malloc(sizeof(vt_cupticb_gpumem_t));

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
  vt_count(vtCtx->strmList->tid, &vtTime, vt_cupticb_cid_cudaMalloc, 
           (uint64_t)(vtCtx->gpuMemAllocated));
}

/*
 * Decreases the "Allocated CUDA memory" counter.
 *
 * @param ctxUID CUDA context identifier (@see CUPTI callback info)
 * @param devPtr pointer to the allocated memory
 */
static void vt_cupticb_handle_free(uint64_t ctxUID, void *devPtr)
{
  uint64_t vtTime;
  vt_cupticb_ctx_t *vtCtx = vt_cupticb_checkCtx(ctxUID);
  vt_cupticb_gpumem_t *curMalloc = NULL;
  vt_cupticb_gpumem_t *lastMalloc = NULL;

  if(devPtr == NULL) return;

  curMalloc = vtCtx->gpuMemList;
  lastMalloc = vtCtx->gpuMemList;

  while(curMalloc != NULL){
    if(devPtr == curMalloc->memPtr){

      /* decrease allocated counter value and write it */
      vtTime = vt_pform_wtime();
      vtCtx->gpuMemAllocated -= curMalloc->size;
      vt_count(vtCtx->strmList->tid, &vtTime, vt_cupticb_cid_cudaMalloc,
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
      vt_cupticb_ctx_t *vtCtx = vt_cupticb_checkCtx(cbInfo->contextUid);
      vt_cupticb_strm_t *vtStrm = vt_cupticb_checkStream(ptid, vtCtx, NULL);

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

    CUPTI_CB_LOCK();
    if(kind != cudaMemcpyDeviceToDevice) vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;
    vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;
    CUPTI_CB_UNLOCK();

    /*time = vt_pform_wtime();*/
    if(kind == cudaMemcpyHostToDevice){
      vt_mpi_rma_put(ptid, &time, strmID * 65536 + vt_my_trace,
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToHost){
      vt_mpi_rma_get(ptid, &time, strmID * 65536 + vt_my_trace,
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_get(strmID, &time, strmID * 65536 + vt_my_trace,
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
  
  if(!vt_cupticb_trace_mcpyAsync) return;
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;

  if(cbInfo->callbackSite == CUPTI_API_ENTER){
    /* get the VampirTrace thread ID the kernel is running on */
    {
      vt_cupticb_ctx_t *vtCtx = vt_cupticb_checkCtx(cbInfo->contextUid);
      vt_cupticb_strm_t *vtStrm = vt_cupticb_checkStream(ptid, vtCtx, &cuStrm);

      strmID = vtStrm->tid;

      /* save address into 64 Bit correlation value for exit callback */
      *cbInfo->correlationData = (uint64_t)vtStrm;
    }

    CUPTI_CB_LOCK();
    if(kind != cudaMemcpyDeviceToDevice) vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;
    vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;
    CUPTI_CB_UNLOCK();

    time = vt_pform_wtime();
    if(kind == cudaMemcpyHostToDevice){
      vt_mpi_rma_put(ptid, &time, strmID * 65536 + vt_my_trace,
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToHost){
      vt_mpi_rma_get(ptid, &time, strmID * 65536 + vt_my_trace,
                     vt_gpu_commCID, 0, bytes);
    }else if(kind == cudaMemcpyDeviceToDevice){
      vt_mpi_rma_get(strmID, &time, strmID * 65536 + vt_my_trace,
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

static void vt_cupti_callback_finalizeContext(vt_cupticb_ctx_t *vtCtx)
{
  uint32_t ptid;
  
  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;
  
  if(vt_cupticb_trace_events && vt_gpu_debug == 0){
    uint64_t time = vt_pform_wtime();
    vt_cupticb_strm_t *curStrm = vtCtx->strmList;
    vt_cuptievt_ctx_t* vtcuptiCtx = vt_cuptievt_getCurrentContext(ptid);

    while(curStrm != NULL){
      vt_cuptievt_resetCounter(vtcuptiCtx, curStrm->tid, &time);
      curStrm = curStrm->next;
    }

    vt_cuptievt_finalize_device(ptid, 0);
  }
  
  /* write idle end time to CUDA stream 0 */
  if(vt_cupticb_trace_gpu_idle){
    uint64_t idle_end = vt_pform_wtime();
    vt_exit(vtCtx->strmList->tid, &idle_end);
  }
  
  /* cleanup stream list */
  if(vtCtx->strmList != NULL){
    free(vtCtx->strmList);
    vtCtx->strmList = NULL;
  }
}

/* -------------START: Implementation of public functions ------------------ */
/* ------------------------------------------------------------------------- */

/**
 * Initialize the VampirTrace CUPTI callback implementation.
 */
void vt_cupti_callback_init()
{
  if(!vt_cupticb_initialized){
#if (defined(VT_MT) || defined(VT_HYB))
    VTThrd_createMutex(&VTThrdMutexCuptiCB);
#endif
    CUPTI_CB_LOCK();
    if(!vt_cupticb_initialized){
      vt_cntl_msg(2, "[CUPTI Callback] Initializing ... ");
      
      /* get some environment variables */
      vt_cupticb_trace_cudart = (uint8_t)vt_env_cudarttrace();
      vt_cupticb_trace_kernels = (uint8_t)vt_env_cudatrace_kernel();
      vt_cupticb_trace_mcpyAsync = (uint8_t)vt_env_cudatrace_memcpyasync();
      vt_cupticb_trace_gpu_mem = (uint8_t)vt_env_cudatrace_gpumem();
      vt_cupticb_trace_gpu_idle = (uint8_t)vt_env_cudatrace_idle();
      vt_cupticb_syncLevel = (uint8_t)vt_env_cudatrace_sync();

      /* check for CUPTI event capturing */
      if(vt_env_cupti_metrics() == NULL){
        vt_cupticb_trace_events = 0;
      }else{
        vt_cupticb_trace_events = 1;
        vt_cupticb_event_sampling = (uint8_t)vt_env_cupti_sampling();
      }

    #if (defined(VT_MT) || defined(VT_HYB))
        VTTHRD_LOCK_IDS();
    #endif

      /* initialize GPU common stuff */
      vt_gpu_init();

      /* get global counter group IDs */
      {
        uint32_t cgid_kn = vt_def_counter_group(VT_MASTER_THREAD, "CUDA_KERNEL");

        vt_cupticb_cid_blocksPerGrid = vt_def_counter(VT_MASTER_THREAD, 
                      "blocks_per_grid", "#",
                      VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, cgid_kn, 0);
        vt_cupticb_cid_threadsPerBlock = vt_def_counter(VT_MASTER_THREAD, 
                      "threads_per_block", "#",
                      VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, cgid_kn, 0);
        vt_cupticb_cid_threadsPerKernel = vt_def_counter(VT_MASTER_THREAD, 
                      "threads_per_kernel", "#",
                      VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, cgid_kn, 0);
      }
      
      if(vt_cupticb_trace_gpu_mem){
        vt_cupticb_cid_cudaMalloc = vt_def_counter(VT_MASTER_THREAD, 
                 "gpu_mem_usage", "Bytes",
                 VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED,
                 vt_def_counter_group(VT_MASTER_THREAD, "CUDA_MEMORY_USAGE"),
                 0);
      }

      /* get global region IDs */
      vt_cupticb_rid_sync = vt_def_region(VT_MASTER_THREAD, "cudaSynchronize", 
                      VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "CUDA_SYNC", VT_FUNCTION);

      

      if(vt_cupticb_trace_gpu_idle){
        vt_cupticb_rid_idle = vt_def_region(VT_MASTER_THREAD, "gpu_idle", 
                      VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "CUDA_IDLE", VT_FUNCTION);
      }

    #if (defined(VT_MT) || defined(VT_HYB))
        VTTHRD_UNLOCK_IDS();
    #endif

      /* set callback for CUDA runtime API functions */
      if(vt_cupticb_trace_cudart){
        vt_cupti_set_callback(&vt_cupticb_cudart_subscriber, 
                              vt_cupticb_cudart_ptr, 
                              CUPTI_CB_DOMAIN_RUNTIME_API,
                              CUPTI_RUNTIME_TRACE_CBID_INVALID);
      }

      /* reset the hash table for CUDA API functions */
      memset(vt_cupticb_cudaApiHtab, VT_NO_ID, 
             VT_CUPTICB_CUDA_API_HASH_MAX * sizeof(uint32_t));

      /* TODO: check exit handler problems with CUPTI events */
      if(vt_cupticb_trace_events) vt_gpu_debug = 1;
      
      /* register the finalize function of VampirTrace CUPTI to be called before
       * the program exits */
      atexit(vt_cupti_callback_finalize);

      vt_cupticb_initialized = 1;
      CUPTI_CB_UNLOCK();
    }
  }
}

/**
 * Finalize the VampirTrace CUPTI callback implementation.
 */
void vt_cupti_callback_finalize()
{
  if(!vt_cupticb_finalized){

    CUPTI_CB_LOCK();
    if(!vt_cupticb_finalized){

      vt_cntl_msg(2, "[CUPTI CALLBACK] Finalizing ... ");
  
      if(vt_cupticb_trace_cudart){
        CHECK_CUPTI_ERROR(cuptiUnsubscribe(vt_cupticb_cudart_subscriber), 
                          "cuptiUnsubscribe");
      }

      /* clean up the VampirTrace CUDA context list */
      while(vt_cupticb_ctxList != NULL){
        vt_cupticb_ctx_t *vtCtx = vt_cupticb_ctxList;

        vt_cupticb_ctxList = vt_cupticb_ctxList->next;

        vt_cupti_callback_finalizeContext(vtCtx);

        free(vtCtx);
      }

      if(vt_cupticb_trace_events) vt_cupti_events_finalize();

      vt_cupticb_hashClear();
      vt_gpu_finalize();
      
      vt_cupticb_finalized = 1;
      CUPTI_CB_UNLOCK();

#if (defined(VT_MT) || defined (VT_HYB))
      VTTHRD_LOCK_ENV();
      VTThrd_deleteMutex(&VTThrdMutexCuptiCB);
      VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB */
    }
  }
}

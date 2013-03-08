/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_thrd.h"            /* thread creation for GPU kernels */
#include "vt_gpu.h"             /* common for GPU */
#include "vt_mallocwrap.h"      /* wrapping of malloc and free */
#include "vt_cupti.h"           /* Support for CUPTI */
#include "vt_cupti_common.h"    /* CUPTI common structures, functions, etc. */

/* mutex for locking the CUPTI environment */
#if (defined(VT_MT) || defined(VT_HYB))
VTThrdMutex* VTThrdMutexCupti = NULL;
#endif /* VT_MT || VT_HYB */

/* set the list of CUPTI contexts to 'empty' */
vt_cupti_ctx_t *vt_cupti_ctxList = NULL;

/* CUPTI global CUDA kernel counter group ID */
uint32_t vt_cupti_cgid_cuda_kernel = VT_NO_ID;

/* global kernel counter IDs */
uint32_t vt_cupti_cid_blocksPerGrid = VT_NO_ID;
uint32_t vt_cupti_cid_threadsPerBlock = VT_NO_ID;
uint32_t vt_cupti_cid_threadsPerKernel = VT_NO_ID;

static uint8_t vt_cupti_initialized = 0;
static uint8_t vt_cupti_finalized   = 0;

void vt_cupti_init()
{
  if(!vt_cupti_initialized){ 
#if (defined(VT_MT) || defined(VT_HYB))
    VTThrd_createMutex(&VTThrdMutexCupti);
#endif
    VT_CUPTI_LOCK();
    if(!vt_cupti_initialized){
      vt_cntl_msg(2, "[CUPTI] Initializing ... ");
      
      /* register the finalize function of VampirTrace CUPTI to be called before
       * the program exits */
      atexit(vt_cupti_finalize);
      
      vt_cupti_initialized = 1;
      VT_CUPTI_UNLOCK();
    }
  }
}

/*
 * Finalize the CUPTI common interface.
 * - free the VampirTrace CUPTI context list
 */
void vt_cupti_finalize()
{
  if(!vt_cupti_finalized && vt_cupti_initialized){
    VT_CUPTI_LOCK();
    if(!vt_cupti_finalized && vt_cupti_initialized){
      vt_cntl_msg(2, "[CUPTI] Finalizing ... ");
      
      /* free VampirTrace CUPTI context structures */
      while(vt_cupti_ctxList != NULL){
        vt_cupti_ctx_t *tmp =  vt_cupti_ctxList;

        vt_cupti_ctxList = vt_cupti_ctxList->next;

        vt_cupti_freeCtx(tmp);
        tmp = NULL;
      }
      
      vt_cupti_finalized = 1;
      VT_CUPTI_UNLOCK();

#if (defined(VT_MT) || defined (VT_HYB))
      VTTHRD_LOCK_ENV();
      VTThrd_deleteMutex(&VTThrdMutexCupti);
      VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB */
    }
  }
}

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
vt_cupti_ctx_t* vt_cupti_createCtx(CUcontext cuCtx, CUdevice cuDev,
                                   uint32_t cuCtxID, uint32_t cuDevID)
{
  vt_cupti_ctx_t* vtCtx = NULL;
  
  /* create new context */
  vtCtx = (vt_cupti_ctx_t *)malloc(sizeof(vt_cupti_ctx_t));
  if(vtCtx == NULL) 
    vt_error_msg("[CUPTI] Could not allocate memory for VT CUPTI context!");
  vtCtx->ctxID = cuCtxID;
  vtCtx->next = NULL;
  
  VT_CHECK_THREAD;
  vtCtx->ptid = VT_MY_THREAD;
  
  /* get the current CUDA context, if it is not given */
  if(cuCtx == NULL) CHECK_CU_ERROR(cuCtxGetCurrent(&cuCtx), NULL);
  vtCtx->cuCtx = cuCtx;
  
  /* try to get CUDA device (ID), if they are not given */
  if(cuDevID == VT_CUPTI_NO_DEVICE_ID){
    if(cuDev == VT_CUPTI_NO_CUDA_DEVICE){
      /* neither device ID nor CUDA device is given */
      if(CUDA_SUCCESS == cuCtxGetDevice(&cuDev)){
        cuDevID = (uint32_t)cuDev;
      }
    }else{
      /* no device ID, but CUDA device is given */
      cuDevID = (uint32_t)cuDev;
    }
  }
  
  vtCtx->devID = cuDevID;
  vtCtx->cuDev = cuDev;
  
#if defined(VT_CUPTI_ACTIVITY)
  vtCtx->activity = NULL;
#endif

#if defined(VT_CUPTI_CALLBACKS)
  vtCtx->callbacks = NULL;
#endif
  
#if defined(VT_CUPTI_EVENTS)
  vtCtx->events = NULL;
#endif

  vt_cntl_msg(2, "[CUPTI] Created context for CUcontext %d, CUdevice %d", 
               cuCtx, cuDev);
  
  return vtCtx;
}

/*
 * Prepend the given VampirTrace CUPTI context to the global context list.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context to be prepended
 */
void vt_cupti_prependCtx(vt_cupti_ctx_t *vtCtx)
{
  VT_CUPTI_LOCK();
  vtCtx->next = vt_cupti_ctxList;
  vt_cupti_ctxList = vtCtx;
  VT_CUPTI_UNLOCK();
}

/*
 * Get a VampirTrace CUPTI context by CUDA context
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI context
 */
vt_cupti_ctx_t* vt_cupti_getCtx(CUcontext cuCtx)
{
  vt_cupti_ctx_t* vtCtx = NULL;
  
  /* lookup context */
  VT_CUPTI_LOCK();
  vtCtx = vt_cupti_ctxList;
  while(vtCtx != NULL){
    if(vtCtx->cuCtx == cuCtx){
      VT_CUPTI_UNLOCK();
      return vtCtx;
    }
    vtCtx = vtCtx->next;
  }
  VT_CUPTI_UNLOCK();
  
  return NULL;
}

/*
 * Get a VampirTrace CUPTI context by CUDA context without locking.
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI context
 */
vt_cupti_ctx_t* vt_cupti_getCtxNoLock(CUcontext cuCtx)
{
  vt_cupti_ctx_t* vtCtx = NULL;

  vtCtx = vt_cupti_ctxList;
  while(vtCtx != NULL){
    if(vtCtx->cuCtx == cuCtx){
      return vtCtx;
    }
    vtCtx = vtCtx->next;
  }
  
  return NULL;
}

/*
 * Get or if not available create a VampirTrace CUPTI context by CUDA context.
 * 
 * @param cuCtx the CUDA context
 * 
 * @return VampirTrace CUPTI context
 */
vt_cupti_ctx_t* vt_cupti_getCreateCtx(CUcontext cuCtx)
{
  vt_cupti_ctx_t* vtCtx = NULL;
  
  /* lookup context */
  VT_CUPTI_LOCK();
  vtCtx = vt_cupti_ctxList;
  while(vtCtx != NULL){
    if(vtCtx->cuCtx == cuCtx){
      VT_CUPTI_UNLOCK();
      return vtCtx;
    }
    vtCtx = vtCtx->next;
  }
  VT_CUPTI_UNLOCK();
  
  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
  vtCtx = vt_cupti_createCtx(cuCtx, VT_CUPTI_NO_CUDA_DEVICE,
                             VT_CUPTI_NO_CONTEXT_ID, VT_CUPTI_NO_DEVICE_ID);
  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
  
  vt_cupti_prependCtx(vtCtx);
  
  return vtCtx;
}

/*
 * Remove a context from the global context list and return it.
 * 
 * @param cuCtx pointer to the CUDA context
 * @return the VampirTrace CUPTI context, which has been removed 
 */
vt_cupti_ctx_t* vt_cupti_removeCtx(CUcontext *cuCtx)
{
  vt_cupti_ctx_t *currCtx = NULL;
  vt_cupti_ctx_t *lastCtx = NULL;

  VT_CUPTI_LOCK();
  currCtx = vt_cupti_ctxList;
  lastCtx = vt_cupti_ctxList;
  while(currCtx != NULL){
    if(currCtx->cuCtx == *cuCtx){
      /* if first element in list */
      if(currCtx == vt_cupti_ctxList){
        vt_cupti_ctxList = vt_cupti_ctxList->next;
      }else{
        lastCtx->next = currCtx->next;
      }
      VT_CUPTI_UNLOCK();
      return currCtx;
    }
    lastCtx = currCtx;
    currCtx = currCtx->next;
  }
  VT_CUPTI_UNLOCK();

  vt_cntl_msg(2, "[CUPTI] Could not remove context (CUDA Context not found)!");
  return NULL;
}

/*
 * Free the allocated memory for this VampirTrace CUPTI context.
 * 
 * @param vtCtx pointer to the VampirTrace CUPTI context
 */
void vt_cupti_freeCtx(vt_cupti_ctx_t *vtCtx)
{
  if(vtCtx == NULL)
    return;
  
#if defined(VT_CUPTI_ACTIVITY)
  if(vtCtx->activity != NULL)
    free(vtCtx->activity);
#endif

#if (defined(VT_CUPTI_CALLBACKS) && !defined(VT_CUPTI_ACTIVITY))
  if(vtCtx->callbacks != NULL)
    free(vtCtx->callbacks);
#endif
  
#if defined(VT_CUPTI_EVENTS)
  if(vtCtx->events != NULL)
    free(vtCtx->events);
#endif
  
  free(vtCtx);
}

/*
 * Handles errors returned from CUPTI function calls.
 * 
 * @param ecode the CUDA driver API error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
void vt_cupti_handleError(CUptiResult err, const char* msg,
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

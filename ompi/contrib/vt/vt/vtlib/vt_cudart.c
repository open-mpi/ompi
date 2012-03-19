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

#include "vt_defs.h"        /* global definitions */
#include "vt_env.h"         /* get environment variables */
#include "vt_pform.h"       /* VampirTrace time measurement */
#include "vt_defs.h"        /* VampirTrace constants */
#include "vt_error.h"       /* VampirTrace warning and error messages */
#include "vt_libwrap.h"     /* wrapping of CUDA Runtime API functions */
#include "vt_cudartwrap.h"  /* CUDA wrapper functions for external use */
#include "vt_gpu.h"         /* common for GPU */
#include "vt_memhook.h"     /* Switch memory tracing on/off */

#if (defined(VT_CUPTI_EVENTS))
#include "vt_cupti_events.h"       /* Support for CUPTI events */
#endif

#include <stdio.h>
#include <string.h>

/* mutex for locking the CUDA runtime wrap environment */
#if (defined(VT_MT) || defined(VT_HYB))
VTThrdMutex* VTThrdMutexCudart = NULL;
#endif /* VT_MT || VT_HYB */

/*
 * Register the finalize function of the CUDA wrapper to be called before
 * the program exits and CUDA has done its implicit clean-up.
 * A CUDA function (context creating???) has to be called before, as
 * VampirTrace CUDA wrapper has to finalize before CUDA does its clean-up!!!
 */
#define REGISTER_FINALIZE                              \
  if(vt_cudart_trace_enabled && !finalize_registered){ \
    CUDARTWRAP_LOCK();                                 \
    if(!finalize_registered){                          \
      atexit(vt_cudartwrap_finalize);                  \
      vt_cntl_msg(2, "[CUDART] Finalize registered!"); \
      finalize_registered = 1;                         \
    }                                                  \
    CUDARTWRAP_UNLOCK();                               \
  }

/*
 * Macro for synchronous (blocking) CUDA Memory Copies.
 * VampirTrace communication events are written on host stream!!!
 * !!! DeviceToDevice copies are written on CUDA stream in between the
 *     asynchronous tasks. Therefore do only write them after a flush!
 *
 * @param _kind the cudaMemcpyKind
 * @param _bytes the number of bytes to be transfered
 * @param _call the function call of the CUDA Runtime API function
 */
#define CUDA_SEND_RECV(_ptid, _kind, _bytes, _call){                           \
  uint64_t time = 0;                                                           \
  uint8_t do_traceE = 0; /* is call limit reached */                           \
  VTCUDADevice* vtDev = NULL;                                                  \
  uint32_t strmID = 0;                                                         \
  VTCUDAStrm *strm;                                                            \
  vtDev = VTCUDAcheckThread(NULL, _ptid, &strm);                               \
  strmID = strm->tid;                                                          \
  if(_kind != cudaMemcpyHostToHost){                                           \
    if(syncLevel > 2) VTCUDAflush(vtDev, _ptid);                               \
    else if(syncLevel > 0){                                                    \
      time = vt_pform_wtime();                                                 \
      if(syncLevel > 1) vt_enter(_ptid, &time, rid_sync);                      \
      VT_CUDART_CALL(cudaThreadSynchronize_ptr(),"vtcudaSync() failed!");      \
      if(syncLevel > 1){time = vt_pform_wtime(); vt_exit(_ptid, &time);}       \
    }                                                                          \
    CUDARTWRAP_LOCK();                                                         \
      if(_kind != cudaMemcpyDeviceToDevice)                                    \
        vt_gpu_prop[_ptid] |= VTGPU_GPU_COMM;                                  \
      vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;                                   \
    CUDARTWRAP_UNLOCK();                                                       \
  }                                                                            \
  if(syncLevel == 1 && time != 0){ /* no hostTohost and sync==1 */             \
    do_traceE = vt_enter(_ptid, &time, VT_LIBWRAP_FUNC_ID);                    \
    time = vt_pform_wtime();                                                   \
  }else{                                                                       \
    time = vt_pform_wtime();                                                   \
    do_traceE = vt_enter(_ptid, &time, VT_LIBWRAP_FUNC_ID);                    \
  }                                                                            \
  if(do_traceE){                                                               \
    if(_kind == cudaMemcpyHostToDevice){                                       \
      vt_mpi_rma_put(_ptid, &time, VT_GPU_RANK_ID(strmID),                     \
                     vt_gpu_commCID, 0, (uint64_t)_bytes);                     \
    }else if(_kind == cudaMemcpyDeviceToHost){                                 \
      vt_mpi_rma_get(_ptid, &time, VT_GPU_RANK_ID(strmID),                     \
                     vt_gpu_commCID, 0, (uint64_t)_bytes);                     \
    }else if(_kind == cudaMemcpyDeviceToDevice && syncLevel > 2){              \
      vt_mpi_rma_get(strmID, &time, VT_GPU_RANK_ID(strmID),                    \
                     vt_gpu_commCID, 0, (uint64_t)_bytes);                     \
    }                                                                          \
  }                                                                            \
  _call  /* the CUDA memcpy call itself */                                     \
  time = vt_pform_wtime();                                                     \
  if(do_traceE){                                                               \
    if(_kind == cudaMemcpyDeviceToDevice && syncLevel > 2){                    \
      vt_mpi_rma_end(strmID, &time, vt_gpu_commCID, 0);                        \
    }else if(_kind != cudaMemcpyHostToHost){                                   \
      vt_mpi_rma_end(_ptid, &time, vt_gpu_commCID, 0);                         \
    }                                                                          \
  }                                                                            \
  if(syncLevel > 2) vtDev->sync.lastTime = time;                               \
  REGISTER_FINALIZE;                                                           \
  vt_exit(_ptid, &time);                                                       \
}

#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 4000))
/*
 * CUDA peer-to-peer memory copy macro.
 * 
 * @param _ptid name of the VampirTrace process/thread id variable
 * @param _cuSrcDev the CUDA source device pointer
 * @param _cuDstDev the CUDA destination device pointer
 * @param _bytes number of bytes to be transfered
 * @param _call the CUDA function call itself
 */
#define VT_CUDART_MEMCPY_PEER2PEER(_ptid, _cuSrcDev, _cuDstDev, _bytes, _call){\
  VTCUDADevice *vtSrcDev = VTCUDAcheckDeviceByID(_cuSrcDev);\
  VTCUDADevice *vtDstDev = VTCUDAcheckDeviceByID(_cuDstDev);\
  uint8_t do_traceE = 0;\
  uint64_t time = vt_cudart_setupMemcpyPeer2Peer(_ptid, vtSrcDev, vtDstDev);\
\
  if(syncLevel == 1){\
    do_traceE = vt_enter(_ptid, &time, VT_LIBWRAP_FUNC_ID);\
    time = vt_pform_wtime();\
  }else{\
    time = vt_pform_wtime();\
    do_traceE = vt_enter(_ptid, &time, VT_LIBWRAP_FUNC_ID);\
  }\
\
  if(do_traceE){\
    vt_mpi_rma_get(vtSrcDev->strmList->tid, &time, \
                   VT_GPU_RANK_ID(vtDstDev->strmList->tid),\
                   vt_gpu_commCID, 0, _bytes);\
  }\
\
  _call  /* the CUDA memcpy call itself */\
\
  time = vt_pform_wtime();\
  if(do_traceE){\
    vt_mpi_rma_end(vtSrcDev->strmList->tid, &time, vt_gpu_commCID, 0); \
  }\
  if(syncLevel > 2){\
    vtSrcDev->sync.lastTime = time;\
    vtDstDev->sync.lastTime = time;\
  }\
  REGISTER_FINALIZE;\
  vt_exit(_ptid, &time);\
}

# define VT_CUDART_MEMCPY(_dst, _src, _kind, _bytes, _call){ \
  if(vt_cudart_trace_enabled){\
    uint32_t ptid = 0;\
    VT_CHECK_THREAD;\
    ptid = VT_MY_THREAD;\
    if(vt_is_trace_on(ptid)){\
      int cuSrcDev;\
      int cuDstDev;\
      vt_cudart_getMemcpyKind((void *)_src, (void *)_dst, &(_kind), \
                              &cuSrcDev, &cuDstDev);\
      if(_kind != cudaMemcpyDefault){ \
        CUDA_SEND_RECV(ptid, _kind, _bytes, _call)\
      }else{ /* peer-to-peer */ \
        VT_CUDART_MEMCPY_PEER2PEER(ptid, cuSrcDev, cuDstDev, _bytes, _call)\
      }\
    }else{\
      _call  /* the CUDA memcpy call itself */\
    }\
  }else{\
    _call  /* the CUDA memcpy call itself */\
  }\
}
#else
# define VT_CUDART_MEMCPY(_dst, _src, _kind, _bytes, _call){                   \
  if(vt_cudart_trace_enabled){                                                 \
    uint32_t ptid = 0;                                                         \
    VT_CHECK_THREAD;                                                           \
    ptid = VT_MY_THREAD;                                                       \
    if(vt_is_trace_on(ptid)){                                                  \
      CUDA_SEND_RECV(ptid, _kind, _bytes, _call)                               \
    }else{                                                                     \
      _call  /* the CUDA memcpy call itself */                                 \
    }                                                                          \
  }else{                                                                       \
    _call  /* the CUDA memcpy call itself */                                   \
  }                                                                            \
}
#endif

/* 
 * Asynchronous memory copies are not allowed to be buffered and then recorded
 * in stream zero! Otherwise the enabled GPU idle time measurement will cause
 * errors. 
 */
#if (defined(VT_CUPTI_EVENTS))
# define CUDA_MEMCPY_ASYNC(_kind, _bytes, _stream, _call)    \
  if(vt_cudart_trace_enabled){                               \
    if(trace_cupti_events || _stream == NULL)                \
      CUDA_MEMCPY_ASYNC_CUPTI(_kind, _bytes, _stream, _call) \
    else                                                     \
      CUDA_MEMCPY_ASYNC_EVT(_kind, _bytes, _stream, _call)   \
  }else{                                                     \
    _call                                                    \
  }
#else
#define CUDA_MEMCPY_ASYNC(kind, bytes, stream, _call) \
        CUDA_MEMCPY_ASYNC_EVT(kind, bytes, stream, _call)
#endif

/*
 * Records a memory copy and stores it in the entry buffer.
 * VampirTrace communication events are written on CUDA stream!!!
 *
 * @param kind kind/direction of memory copy
 * @param bytes number of bytes to be transfered
 * @param stream the CUDA stream
 * @param _call the CUDA function call itself
 */
#define CUDA_MEMCPY_ASYNC_EVT(kind, bytes, stream, _call){                   \
  VTCUDAMemcpy *mcpy = NULL;                                                 \
\
  if((kind != cudaMemcpyHostToHost) && trace_memcpyAsync){                   \
    mcpy = addMemcpy2Buf(kind, bytes, stream);                               \
/*vt_cntl_msg(1,"[CUDART] cudaMemcpyAsync on stream %llu ", (uin64_t)stream);*/\
    if(mcpy != NULL)                                                         \
      VT_CUDART_CALL(cudaEventRecord_ptr(mcpy->evt->strt, stream), NULL);    \
  }                                                                          \
  VT_LIBWRAP_FUNC_START(vt_cudart_lw);                                       \
\
  _call  /* the CUDA memCpy call itself */                                   \
\
  VT_LIBWRAP_FUNC_END(vt_cudart_lw);                                         \
  if((kind != cudaMemcpyHostToHost) && trace_memcpyAsync && mcpy != NULL){   \
    VT_CUDART_CALL(cudaEventRecord_ptr(mcpy->evt->stop, stream), NULL);      \
  }                                                                          \
  REGISTER_FINALIZE;                                                         \
}

/*
 * Asynchronous communication is synchronized, when used with CUPTI events 
 * capturing enabled.
 * @param _kind kind/direction of memory copy
 * @param _bytes number of bytes to be transfered
 * @param _stream the CUDA stream
 * @param _call the CUDA function call itself
 */
#define CUDA_MEMCPY_ASYNC_CUPTI(_kind, _bytes, _stream, _call){              \
  uint8_t do_trace = 0; /* Is trace on? */                                   \
  uint64_t time = 0;                                                         \
  uint32_t ptid = 0;                                                         \
  uint32_t strmID = 0;                                                       \
  VTCUDAStrm *vtStrm;                                                        \
\
  VT_CHECK_THREAD;                                                           \
  ptid = VT_MY_THREAD;                                                       \
  (void)VTCUDAcheckThread(_stream, ptid, &vtStrm);                           \
  time = vt_pform_wtime();                                                   \
  do_trace = vt_enter(ptid, &time, VT_LIBWRAP_FUNC_ID);                      \
  if(do_trace && trace_memcpyAsync){                                         \
    strmID = vtStrm->tid;                                                    \
    if(syncLevel > 1) vt_enter(ptid, &time, rid_sync);                       \
    VT_CUDART_CALL(cudaThreadSynchronize_ptr(),"vtcudaSync() failed!");      \
    if(syncLevel > 1){time = vt_pform_wtime(); vt_exit(ptid, &time);}        \
    if(_kind == cudaMemcpyHostToDevice){                                     \
      vt_mpi_rma_put(ptid, &time, VT_GPU_RANK_ID(strmID),                    \
                     vt_gpu_commCID, 0, _bytes);                             \
    }else if(_kind == cudaMemcpyDeviceToHost){                               \
      vt_mpi_rma_get(ptid, &time, VT_GPU_RANK_ID(strmID),                    \
                     vt_gpu_commCID, 0, _bytes);                             \
    }else if(_kind == cudaMemcpyDeviceToDevice && syncLevel > 2){            \
      vt_mpi_rma_get(strmID, &time, VT_GPU_RANK_ID(strmID),                  \
                     vt_gpu_commCID, 0, _bytes);                             \
      CUDARTWRAP_LOCK();                                                     \
      vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;                                 \
      CUDARTWRAP_UNLOCK();                                                   \
    }                                                                        \
  }                                                                          \
\
  _call  /* the CUDA memcpy call itself */                                   \
\
  if(do_trace && trace_memcpyAsync){                                         \
    VT_CUDART_CALL(cudaThreadSynchronize_ptr(),"vtcudaSync() failed!");      \
    time = vt_pform_wtime();                                                 \
    if(_kind == cudaMemcpyDeviceToDevice && syncLevel > 2){                  \
      vt_mpi_rma_end(strmID, &time, vt_gpu_commCID, 0);                      \
    }else if(_kind == cudaMemcpyHostToDevice || _kind == cudaMemcpyDeviceToHost){\
      vt_mpi_rma_end(ptid, &time, vt_gpu_commCID, 0);                        \
      CUDARTWRAP_LOCK();                                                     \
      vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;                                   \
      vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;                                 \
      CUDARTWRAP_UNLOCK();                                                   \
    }                                                                        \
  }else time = vt_pform_wtime();                                             \
  REGISTER_FINALIZE;                                                         \
  vt_exit(ptid, &time);                                                      \
}

#define VT_CUDART_CALL(_err, _msg) \
  if(cudaSuccess != _err)         \
    __checkCUDACall(_err, _msg, __FILE__,__LINE__)

/* library wrapper object */
VTLibwrap* vt_cudart_lw = VT_LIBWRAP_NULL;

/* library wrapper attributes */
VTLibwrapAttr vt_cudart_lw_attr = VT_LIBWRAP_ATTR_INITIALIZER(vt_cudartwrap_lw_attr_init);

/* flag: cuda specific stuff initialized? */
uint8_t vt_cudart_initialized = 0;

/* flag: is the finalize function registered with atexit() */
static uint8_t finalize_registered = 0;

/* flag: tracing of CUDA API enabled? */
uint8_t vt_cudart_trace_enabled = 0;

/* flag: synchronization and flush points during runtime enabled? */
static uint8_t syncLevel = 3;

/* flag: tracing of kernels enabled? */
static uint8_t trace_kernels = 1;

/* region filter for kernel filtering */
static RFG_Filter* vt_cudart_filter = NULL;

/* flag: tracing of asynchronous memory copies enabled? */
static uint8_t trace_memcpyAsync = 1;

/* flag: tracing of cudaMalloc*() and cudaFree*() enabled? */
static uint8_t trace_gpumem = 0;

/* flag: trace NVIDIA CUPTI events/counters */
static uint8_t trace_cupti_events = 0;

#if defined(VT_CUPTI_EVENTS)
/* flag: sampling for CUPTI counter values enabled? */
static uint8_t cupti_event_sampling = 0;
#endif

/* flag: event based tracing (kernels, memcpyAsync) enabled? */
static uint8_t trace_events = 1;

/* number of bytes used to buffer asynchronous tasks */
static size_t vt_cudart_bufSize = VTGPU_MAX_BSIZE;

/* flag: CUDA wrapper already finalized? */
static uint8_t finalized = 0;

/* global region IDs for wrapper internal tracing */
static uint32_t rid_check, rid_create, rid_sync, rid_flush;

/* global counter IDs */
static uint32_t cid_blocksPerGrid;    /* number of blocks per grid */
static uint32_t cid_threadsPerBlock;  /* number of threads per block */
static uint32_t cid_threadsPerKernel; /* number of threads per kernel */
static uint32_t cid_cudaMalloc;       /* CUDA GPU memory allocation counter */

/* structure for VampirTrace - CUDA time synchronization */
typedef struct
{
  cudaEvent_t strtEvt;   /**< the start event */
  cudaEvent_t stopEvt;   /**< the stop event */
  uint64_t strtTime;     /**< VampirTrace start time */
  uint64_t lastTime;     /**< VampirTrace time after cuda memory copy */
}VTCUDAsync;

/* structure of a VampirTrace CUDA stream */
typedef struct vtcudaStream
{
  cudaStream_t stream;         /**< the CUDA stream */
  uint32_t tid;                /**< VT thread id for this stream (unique) */
  cudaEvent_t lastEvt;         /**< last written CUDA event (needed in flush) */
  uint64_t lastVTTime;         /**< last written VampirTrace time */
  struct vtcudaStream *next;   /**< points to next cuda stream in list */
}VTCUDAStrm;

typedef enum{
  VTCUDABUF_ENTRY_TYPE__Kernel,
	VTCUDABUF_ENTRY_TYPE__Memcpy
} VTCUDABuf_EntryTypes;

typedef struct
{
  cudaEvent_t strt;    /**< the start event */
  cudaEvent_t stop;    /**< the stop event */
} VTCUDABufEvt;

/* basic CUDA task buffer entry */
typedef struct
{
  VTCUDABuf_EntryTypes type;  /**< type of buffer entry */
  VTCUDAStrm *strm;           /**< corresponding stream/thread */
  VTCUDABufEvt *evt;          /**< points to start/stop cuda event */
} VTCUDAbufEntry;

/* structure for a CUDA kernel call */
typedef struct
{
  VTCUDABuf_EntryTypes type;  /**< type of buffer entry */
  VTCUDAStrm *strm;           /**< corresponding stream/thread */
  VTCUDABufEvt *evt;          /**< points to start/stop cuda event */
  uint32_t blocksPerGrid;     /**< number of blocks per grid */
  uint32_t threadsPerBlock;   /**< number of threads per block */
  uint32_t rid;               /**< VampirTrace region id */
}VTCUDAKernel;

/* kernel configure stack element */
typedef struct
{
  VTCUDAStrm *strm;           /**< corresponding stream/thread */
  uint32_t blocksPerGrid;     /**< number of blocks per grid */
  uint32_t threadsPerBlock;   /**< number of threads per block */
}VTCUDAknconf;

/* structure for an asynchronous CUDA memory copy call */
typedef struct
{
  VTCUDABuf_EntryTypes type; /**< type of buffer entry */
  VTCUDAStrm *strm;          /**< the corresponding stream/thread */
  VTCUDABufEvt *evt;         /**< points to start/stop cuda event */
  uint32_t pid;              /**< the callers process/thread id */
  enum cudaMemcpyKind kind;  /**< CUDA memory copy kind (e.g. host->device) */
  size_t byteCount;          /**< number of bytes */
}VTCUDAMemcpy;

/* structure of a VampirTrace CUDA malloc (initiated with cudaMalloc*() */
typedef struct vtcMallocStruct
{
  void *memPtr;                 /**< pointer value to allocated memory */
  size_t size;                  /**< number of bytes allocated */
  uint32_t tid;                 /**< thread id used with this malloc */
  struct vtcMallocStruct *next; /**< points to next cuda stream in list */
}VTCUDAmalloc;

/*
 * structure for a CUDA device (to be used as a list element)
 */
typedef struct vtcudaDev_st
{
  int device;                /**< CUDA device id (first key) */
  uint32_t ptid;             /**< the host thread id (second key) */
  uint8_t concurrentKernels; /**< is concurrent kernel execution supported? */
  VTCUDAStrm *strmList;      /**< CUDA stream list */
  uint32_t strmNum;          /**< Number of streams created for this device */
  VTCUDAmalloc *mallocList;  /**< list of not yet freed cudaMalloc* calls */
  size_t mallocated;         /**< memory allocated on CUDA device */
  VTCUDAsync sync;           /**< synchronization time and events */
  buffer_t asyncbuf;         /**< points to the first byte in buffer */
  buffer_t buf_pos;          /**< current buffer position */
  buffer_t buf_size;         /**< buffer size (in bytes) */
  buffer_t conf_stack;       /**< top of the kernel configure stack */
  VTCUDABufEvt *evtbuf;      /**< the preallocated cuda event list */
  VTCUDABufEvt *evtbuf_pos;  /**< current unused event space */
  struct vtcudaDev_st *next; /**< pointer to next element in list */
}VTCUDADevice;

/* list of CUDA devices */
static VTCUDADevice* cudaDevices = NULL;

/* 
 * The structure of a CUDA kernel element. The list will be filled in
 * __cudaRegisterFunction() and used in cudaLaunch() to get function name from
 * function pointer.
 */
typedef struct knSymbol_st {
  const char* pointer;                  /**< the host function */
  const char* knSymbolName;             /**< name of the CUDA kernel symbol */
  char name[VTGPU_KERNEL_STRING_SIZE];  /**< clean name of the cuda kernel */
  uint32_t rid;                         /**< region id for this kernel */
  struct knSymbol_st *next;             /**< pointer to next kernel element */
}VTCUDAkernelSymbol;

/* 
 * List of all CUDA kernel symbols (their full qualified names and the mapped 
 * VampirTrace region IDs.
 */ 
static VTCUDAkernelSymbol *kernelListHead = NULL;

/* maximum events needed for task buffer size */
static size_t maxEvtNum = VTGPU_MAX_BSIZE / sizeof(VTCUDAKernel);

/* pointer to CUDA functions which should not be traced */
static cudaError_t (*cudaGetDeviceCount_ptr)(int*) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaGetDevice_ptr)(int*) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaGetDeviceProperties_ptr)(struct cudaDeviceProp *, int) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaEventCreate_ptr)(cudaEvent_t *) = VT_LIBWRAP_NULL;
/*static cudaError_t (*cudaEventCreateWithFlags_ptr)(cudaEvent_t *, int) = VT_LIBWRAP_NULL;*/
static cudaError_t (*cudaEventRecord_ptr)(cudaEvent_t, cudaStream_t) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaEventSynchronize_ptr)(cudaEvent_t) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaEventElapsedTime_ptr)(float *, cudaEvent_t, cudaEvent_t) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaEventDestroy_ptr)(cudaEvent_t) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaEventQuery_ptr)(cudaEvent_t) = VT_LIBWRAP_NULL;
/*static cudaError_t (*cudaStreamSynchronize_ptr)(cudaStream_t) = VT_LIBWRAP_NULL;*/
static cudaError_t (*cudaStreamQuery_ptr)(cudaStream_t) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaThreadSynchronize_ptr)(void) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaDeviceSynchronize_ptr)(void) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaGetLastError_ptr)(void) = VT_LIBWRAP_NULL;
static const char *(*cudaGetErrorString_ptr)(cudaError_t) = VT_LIBWRAP_NULL;

/* some CUDA runtime API used in wrapper since CUDA 4.0 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 4000))
static cudaError_t (*cudaSetDevice_ptr)(int) = VT_LIBWRAP_NULL;
static cudaError_t (*cudaPointerGetAttributes_ptr)(struct cudaPointerAttributes *, void *) = VT_LIBWRAP_NULL;
#endif /* defined(CUDART_VERSION) && (CUDART_VERSION >= 4000) */

/*
 * CUDA wrapper function declarations
 */
static void VTCUDAflush(VTCUDADevice*, uint32_t);
static VTCUDADevice* VTCUDAgetDevice(uint32_t ptid);
static VTCUDAkernelSymbol* getKernelByHostFunction(const char* hostFun);
static void extractKernelName(VTCUDAkernelSymbol *e, const char* devFunc);

/* 
 * Checks if a CUDA runtime API call returns successful and respectively prints
 * the error.
 * 
 * @param ecode the CUDA error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
static void __checkCUDACall(cudaError_t ecode, const char* msg,
                            const char *file, const int line)
{
  if(msg != NULL) vt_cntl_msg(1, "[CUDART] %s",  msg);
  
  if(vt_gpu_error){
    vt_error_msg("[CUDA Error <%s>:%i] %s", file, line,
                 cudaGetErrorString_ptr(ecode));
  }else{
    vt_warning("[CUDA <%s>:%i] %s", file, line,
               cudaGetErrorString_ptr(ecode));
  }
}

/*
 * initializer function for library wrapper attributes
   (called from first triggered wrapper event)
 */
void vt_cudartwrap_lw_attr_init(VTLibwrapAttr* attr)
{
  /* initialize library wrapper attributes */
  attr->shlibs_num = 0;
#ifdef CUDARTSHLIB_PATHNAME
  attr->shlibs_num = 1;
  attr->shlibs[0] = CUDARTSHLIB_PATHNAME;
#endif /* CUDARTSHLIB_PATHNAME */
  attr->func_group = "CUDART_API";
  attr->wait_for_init = 0;

  /* *** do some additional initialization *** */
  /* create mutex for locking */
#if (defined(VT_MT) || defined (VT_HYB))
  VTThrd_createMutex(&VTThrdMutexCudart);
#endif /* VT_MT || VT_HYB */
}

/*
 * Initialization in first invoked CUDA runtime wrapper function
 */
void vt_cudartwrap_init(void)
{
  /* as we are now accessing VampirTrace internals, it needs to be "alive" */
  if(vt_is_alive == 0) return;

  /* Is CUDA tracing enabled? */
  vt_cudart_trace_enabled = (uint8_t)vt_env_cudarttrace();
  if(vt_env_cudatrace() == 1) vt_cudart_trace_enabled = 1;
  else vt_cudart_trace_enabled = 0;

  if(vt_cudart_trace_enabled){
    size_t minTaskSize = sizeof(VTCUDAKernel) + sizeof(VTCUDAMemcpy);
    size_t minBufSize = sizeof(VTCUDAKernel) + sizeof(VTCUDAknconf);
    
#if defined(VT_CUPTI_CALLBACKS)
    /* do not use wrapper environment for CUDA runtime API tracing */
    if(vt_env_cudatrace_cupti()){
      vt_cudart_trace_enabled = 0;
      vt_cudart_initialized = 1;
      return;
    }
#endif

    trace_kernels = (uint8_t)vt_env_gputrace_kernel();
    trace_memcpyAsync = (uint8_t)vt_env_cudatrace_memcpy();
    syncLevel = (uint8_t)vt_env_cudatrace_sync();

    trace_events = 0;
    
#if defined(VT_CUPTI_EVENTS)
    if(vt_env_cupti_events() != NULL && trace_kernels){
      trace_cupti_events = 1;
      cupti_event_sampling = (uint8_t)vt_env_cupti_sampling();
    }else{
      trace_cupti_events = 0;
    }
    
    /* check whether CUPTI event gathering is enabled */
    if(!trace_cupti_events)
#endif
    {
      if(trace_memcpyAsync){
        minTaskSize = sizeof(VTCUDAMemcpy);
        minBufSize = sizeof(VTCUDAMemcpy);
        trace_events = 1;
      }

      if(trace_kernels){
        if(sizeof(VTCUDAKernel) < minTaskSize) minTaskSize = sizeof(VTCUDAKernel);
        if(sizeof(VTCUDAKernel) + sizeof(VTCUDAknconf) > minBufSize) 
          minBufSize = sizeof(VTCUDAKernel) + sizeof(VTCUDAknconf);
        trace_events = 1;
      }
    }

    /* if events are used */
    if(trace_events){
      /* get user-defined task buffer size and check it */
      vt_cudart_bufSize = vt_env_cudatrace_bsize();
      
      if(vt_cudart_bufSize < minBufSize){
        if(vt_cudart_bufSize > 0){
          vt_warning("[CUDART] Minimal buffer size is %d bytes", minBufSize);
        }
        vt_cudart_bufSize = VTGPU_DEFAULT_BSIZE;
      }else if(VTGPU_MAX_BSIZE < vt_cudart_bufSize){
        vt_warning("[CUDART] Current CUDA buffer size requires %d CUDA events.\n"
                   "The recommended max. CUDA buffer size is %d. "
                   "(export VT_CUDATRACE_BUFFER_SIZE=2097152)",
                   2*vt_cudart_bufSize/minTaskSize, VTGPU_MAX_BSIZE);
        /* TODO: dynamic event creation for more than 2097152 bytes CUDA buffer size */
      }

      /* determine maximum necessary VT-events (=2 CUDA events) */
      maxEvtNum = vt_cudart_bufSize / minTaskSize;

      vt_cntl_msg(2,"[CUDART] Current CUDA buffer size: %d bytes \n"
                    "(Kernel: %d bytes, MemcpyAsync: %d bytes, "
                    "Pre-created events: %d)", vt_cudart_bufSize,
                    sizeof(VTCUDAKernel), sizeof(VTCUDAMemcpy), maxEvtNum);

    }

    trace_gpumem = (uint8_t)vt_env_gputrace_memusage();
    
    /* read filter file for CUDA kernel filtering */
    {
      const char *filter_file = vt_env_filter_spec();

      if(filter_file){
        vt_cudart_filter = RFG_Filter_init();

        RFG_Filter_setDefFile(vt_cudart_filter, filter_file);
        if(!RFG_Filter_readDefFile(vt_cudart_filter, 0, NULL)){
          vt_error_msg("[CUDART] Could not read region filter specification file");
        }
      }
    }

    /* initialize CUDA functions for not traced internal use */
    {
      static int func_id = VT_LIBWRAP_NOID;

      VTLibwrap_func_init(vt_cudart_lw, "cudaGetDeviceCount", NULL, 0,
                          (void**)(&cudaGetDeviceCount_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaGetDevice", NULL, 0,
                          (void**)(&cudaGetDevice_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaGetDeviceProperties", NULL, 0,
                          (void**)(&cudaGetDeviceProperties_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaEventCreate", NULL, 0,
                          (void**)(&cudaEventCreate_ptr), &func_id);
      /*VTLibwrap_func_init(lw, "cudaEventCreateWithFlags", NULL, 0,
                          (void**)(&cudaEventCreateWithFlags_ptr), &func_id);*/
      VTLibwrap_func_init(vt_cudart_lw, "cudaEventRecord", NULL, 0,
                          (void**)(&cudaEventRecord_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaEventSynchronize", NULL, 0,
                          (void**)(&cudaEventSynchronize_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaEventElapsedTime", NULL, 0,
                          (void**)(&cudaEventElapsedTime_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaEventDestroy", NULL, 0,
                          (void**)(&cudaEventDestroy_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaEventQuery", NULL, 0,
                          (void**)(&cudaEventQuery_ptr), &func_id);
      /*VTLibwrap_func_init(lw, "cudaStreamSynchronize", NULL, 0,
                          (void**)(&cudaStreamSynchronize_ptr), &func_id);*/
      VTLibwrap_func_init(vt_cudart_lw, "cudaStreamQuery", NULL, 0,
                          (void**)(&cudaStreamQuery_ptr), &func_id);
      
      VTLibwrap_func_init(vt_cudart_lw, "cudaGetErrorString", NULL, 0,
                          (void**)(&cudaGetErrorString_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaGetLastError", NULL, 0,
                          (void**)(&cudaGetLastError_ptr), &func_id);
      
      /* CUDA runtime API changes in CUDA 4.0 */
# if (defined(CUDART_VERSION) && (CUDART_VERSION < 4000))
      VTLibwrap_func_init(vt_cudart_lw, "cudaThreadSynchronize", NULL, 0,
                          (void**)(&cudaThreadSynchronize_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaThreadSynchronize", NULL, 0,
                          (void**)(&cudaDeviceSynchronize_ptr), &func_id);
# else
      VTLibwrap_func_init(vt_cudart_lw, "cudaDeviceSynchronize", NULL, 0,
                          (void**)(&cudaDeviceSynchronize_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaDeviceSynchronize", NULL, 0,
                          (void**)(&cudaThreadSynchronize_ptr), &func_id);
      
      /* used since CUDA 4.0 */
      VTLibwrap_func_init(vt_cudart_lw, "cudaSetDevice", NULL, 0,
                          (void**)(&cudaSetDevice_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaPointerGetAttributes", NULL, 0,
                          (void**)(&cudaPointerGetAttributes_ptr), &func_id);
      
# endif /* defined(CUDART_VERSION) && (CUDART_VERSION >= 4000) */
    }

#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
#endif

    vt_gpu_init(); /* initialize GPU common stuff */

    rid_check = vt_def_region(VT_MASTER_THREAD, "vtcudaCheckThread", VT_NO_ID,
                              VT_NO_LNO, VT_NO_LNO, "VT_CUDA", VT_FUNCTION);
    rid_create = vt_def_region(VT_MASTER_THREAD, "vtcudaCreateDevice", VT_NO_ID,
                               VT_NO_LNO, VT_NO_LNO, "VT_CUDA", VT_FUNCTION);
    rid_sync = vt_def_region(VT_MASTER_THREAD, "cudaSynchronize", VT_NO_ID,
                             VT_NO_LNO, VT_NO_LNO, "CUDA_SYNC", VT_FUNCTION);
    rid_flush = vt_def_region(VT_MASTER_THREAD, "vtcudaFlush", VT_NO_ID,
                              VT_NO_LNO, VT_NO_LNO, "VT_CUDA", VT_FUNCTION);

    /* get global counter group IDs */
    {
      uint32_t cgid_kn = vt_def_counter_group(VT_MASTER_THREAD, "CUDA_KERNEL");

      cid_blocksPerGrid = vt_def_counter(VT_MASTER_THREAD, "blocks_per_grid", "#",
                    VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, cgid_kn, 0);
      cid_threadsPerBlock = vt_def_counter(VT_MASTER_THREAD, "threads_per_block", "#",
                    VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, cgid_kn, 0);
      cid_threadsPerKernel = vt_def_counter(VT_MASTER_THREAD, "threads_per_kernel", "#",
                    VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, cgid_kn, 0);
    }

    if(trace_gpumem > 0){
      cid_cudaMalloc = vt_def_counter(VT_MASTER_THREAD, "gpu_mem_usage", "Bytes",
                        VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED,
                        vt_def_counter_group(VT_MASTER_THREAD, "CUDA_MEMORY_USAGE"),
                        0);
    }

#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_UNLOCK_IDS();
#endif
    
#if (defined(VT_CUPTI_EVENTS))
    if(trace_kernels){
      if(trace_cupti_events){
        vt_cudart_bufSize = sizeof(VTCUDAKernel) + sizeof(VTCUDAknconf);
        vt_cntl_msg(2, "[CUDART] Current CUDA buffer size: %d bytes", 
                       vt_cudart_bufSize);
      }
    }
#endif
    
    /*
     * Register the finalize function of the CUDA wrapper to be called before
     * the program exits and CUDA has done its implicit clean-up.
     * A CUDA function (any) has to be called before, as VampirTrace CUDA
     * wrapper has to finalize before CUDA does its clean-up!!!
     */
    atexit(vt_cudartwrap_finalize);

    vt_cudart_initialized = 1;
  }
}

/*
 * Free the local allocated VampirTrace structure and set the VT device to NULL.
 * Has to be locked!!!
 *
 * @param vtDev pointer to the VampirTrace CUDA device
 */
static void VTCUDAremoveDevice(VTCUDADevice *vtDev)
{
  VTCUDADevice *curDev = cudaDevices;
  VTCUDADevice *lastDev = NULL;

  while(curDev != NULL){
    if(vtDev == curDev){
      /* remove CUDA device from global list */
      /* first element */
      if(curDev == cudaDevices){
        cudaDevices = cudaDevices->next;
      }else{/* has to be at least the second loop */
        lastDev->next = curDev->next;
      }

      free(curDev);
      curDev = NULL;
      return;
    }
    lastDev = curDev;
    curDev = curDev->next;
  }
}

/*
 * Cleans up the structure of a VampirTrace CUDA device.
 *
 * @param ptid the VampirTrace thread, which executes this cleanup
 * @param vtDev pointer to VampirTrace CUDA device structure to be cleaned up
 * @param cleanEvents cleanup CUDA events? 1 - yes, 0 - no
 */
static void VTCUDAcleanupDevice(uint32_t ptid, VTCUDADevice *vtDev,
                                uint8_t cleanEvents)
{
  /* check if device already cleanup (e.g. with cudaThreadExit() call) */
  if(vtDev == NULL) return;

  vt_cntl_msg(2, "[CUDART] Cleanup device %d (tid: %d)", vtDev->device, ptid);

  /* flush kernels, if possible */
  if(trace_events && cleanEvents){
    cudaError_t ret = cudaSuccess;
    ret = cudaEventQuery_ptr(vtDev->sync.strtEvt);

    if(ret == cudaErrorInvalidResourceHandle){
      cleanEvents = 0;
      vt_warning("[CUDART] Events are invalid. Context has been destroyed, \n"
                 "before asynchronous tasks could be flushed! "
                 "Traces might be incomplete!");
    }else{
      /* if there is another error than invalid resource handle, just cleanup
       * wrapper structures and try to write, what is traced so far*/
      if(ret != cudaSuccess){
        cleanEvents = 0;
        vt_warning("[CUDART] CUDA error '%s' in device cleanup. (pid=%d)\n"
                   "Traces might be incomplete!",
                   cudaGetErrorString_ptr(ret), ptid);
      }else{
        /* flush remaining asynchronous tasks */
        VTCUDAflush(vtDev, ptid);
      }
    }
  }

#if (defined(VT_CUPTI_EVENTS))
  if(trace_cupti_events && cleanEvents && vt_gpu_debug == 0){
    uint64_t time = vt_pform_wtime();
    VTCUDAStrm *curStrm = vtDev->strmList;
    vt_cuptievt_ctx_t* vtcuptiCtx = vt_cuptievt_getCurrentContext(ptid);

    while(curStrm != NULL){
      vt_cuptievt_resetCounter(vtcuptiCtx, curStrm->tid, &time);
      curStrm = curStrm->next;
    }

    vt_cuptievt_finalize_device(ptid, cleanEvents);
  }
#endif

  /* write idle end time to CUDA stream 0 */
  if(vt_gpu_trace_idle == 1){
    uint64_t idle_end = vt_pform_wtime();
    vt_exit(vtDev->strmList->tid, &idle_end);
  }

  /* cleanup stream list */
  if(vtDev->strmList != NULL){
    free(vtDev->strmList);
    vtDev->strmList = NULL;
  }

  if(trace_events){
    /* destroy CUDA events (cudaThreadExit() implicitly destroys events) */
    if(vtDev->evtbuf != NULL){
      /* destroy CUDA events for asynchronous task measurement */
      if(cleanEvents){
        size_t k;
        cudaError_t ret = cudaSuccess;
        VT_CUDART_CALL(cudaGetLastError_ptr(), "Error check before event destroy");
        for(k = 0; k < maxEvtNum; k++){
          cudaEventDestroy_ptr((vtDev->evtbuf[k]).strt);
          ret = cudaEventDestroy_ptr((vtDev->evtbuf[k]).stop);
        }
        VT_CUDART_CALL(ret, "cudaEventDestroy failed");
      }

      /* free the event buffer */
      free(vtDev->evtbuf);
      vtDev->evtbuf = NULL;
      vtDev->evtbuf_pos = NULL;
    }

    /* destroy synchronization events */
    if(cleanEvents){
      VT_CUDART_CALL(cudaEventDestroy_ptr(vtDev->sync.strtEvt),
                    "cudaEventDestroy(syncStrtEvt) failed!");
      VT_CUDART_CALL(cudaEventDestroy_ptr(vtDev->sync.stopEvt),
                    "cudaEventDestroy(syncStopEvt) failed!");
    }

    /* cleanup entry buffer */
    if(vtDev->asyncbuf != NULL){
      free(vtDev->asyncbuf);
      vtDev->asyncbuf = NULL;
    }
  }

  /* free cuda malloc entries, if application didn't do this yet */
  while(vtDev->mallocList != NULL){
    VTCUDAmalloc *tmpM =  vtDev->mallocList;
    
    if(trace_gpumem > 1)
      vt_cntl_msg(1, "[CUDART] cudaFree* of %d bytes missing!", tmpM->size);
    
    vtDev->mallocList = tmpM->next;
    free(tmpM);
    tmpM = NULL;
  }

  /* free malloc of VTCUDADevice, set pointer to this VT device NULL */
  VTCUDAremoveDevice(vtDev);
}

/*
 * Cleanup the CUDA runtime API wrapper
 */
void vt_cudartwrap_finalize(void)
{
  if(!finalized){
    CUDARTWRAP_LOCK();
    if(!finalized){
      if(vt_cudart_trace_enabled){
        uint32_t ptid;

        vt_cntl_msg(2, "[CUDART] Finalizing wrapper.");

        vt_gpu_finalize();

        VT_CHECK_THREAD;
        ptid = VT_MY_THREAD;

        /* cleanup CUDA device list */
        while(cudaDevices != NULL){
          int device;
          VTCUDADevice *vtDev = cudaDevices;

          /* get the next list element, before current will be freed */
          cudaDevices = cudaDevices->next;

          VT_CUDART_CALL(cudaGetDevice_ptr(&device), "cudaGetDevice(device) failed!");
          if(vtDev->device == device && vtDev->ptid == ptid){
            VTCUDAcleanupDevice(ptid, vtDev, 1);
          }else{
            if(vtDev->buf_pos != vtDev->asyncbuf){
              vt_warning("[CUDART] Current device is %d (Thread %d).\n"
                         "Can neither flush asynchronous tasks nor cleanup resources for device %d with thread %d!\n"
                         "cudaThreadExit() has been implicitly called by the CUDA runtime on host thread exit.\n"
                         "To avoid this warning call cudaThreadExit() before the host thread, using CUDA, exits.",
                         device, ptid, vtDev->device, vtDev->ptid);
            }
            VTCUDAcleanupDevice(ptid, vtDev, 0);
          }
        }
        
#if (defined(VT_CUPTI_EVENTS))
        if(trace_cupti_events) vt_cupti_events_finalize();
#endif

        /* cleanup GPU device list */
        if(cudaDevices != NULL){
          free(cudaDevices);
          cudaDevices = NULL;
        }
        
        /* free filter for CUDA kernel filtering */
        RFG_Filter_free(vt_cudart_filter);

      }
      finalized = 1;
      CUDARTWRAP_UNLOCK();
#if (defined(VT_MT) || defined (VT_HYB))
      VTTHRD_LOCK_ENV();
      VTThrd_deleteMutex(&VTThrdMutexCudart);
      VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB */
    }
  }
}

/*
 * Create a synchronization point for CUDA and VampirTrace time.
 *
 * @param syncEvt the CUDA event to be synchronized
 *
 * @return the corresponding VampirTrace timestamp
 */
static uint64_t VTCUDAsynchronizeEvt(cudaEvent_t syncEvt)
{
  uint64_t syncTime;
  cudaError_t ret;

  {
    /* Record and synchronization events on stream 0 (prior to FERMI)
       see NVIDIA CUDA Programming Guide (4.0), Chapter 3. -> CUDA C Runtime -> 
       subsection Events (3.2.5.6)
       -> "Events in stream zero are recorded after all preceding tasks/commands
           from all streams are completed by the device." */
    cudaEventRecord_ptr(syncEvt, 0);
    /*ret = cudaThreadSynchronize_ptr();*/
    ret = cudaEventSynchronize_ptr(syncEvt);
    syncTime = vt_pform_wtime();

    /* error handling */
    if(cudaSuccess != ret){
      if(cudaErrorInvalidResourceHandle == ret){
        vt_warning("[CUDART] Synchronization stop event is invalid. Context has "
                 "been destroyed, \nbefore asynchronous tasks could be flushed! "
                   "Traces might be incomplete!");
      }else{
        VT_CUDART_CALL(ret, NULL);
      }
      return (uint64_t)-1;
    }
  }

  return syncTime;
}

/*
 * Write asynchronous CUDA tasks to VampirTrace CUDA thread/stream
 *
 * @param vtDev pointer to the VTCUDADevice to be flushed
 * @param ptid the VampirTrace thread ID of the calling thread
 */
static void VTCUDAflush(VTCUDADevice *vtDev, uint32_t ptid)
{
  uint64_t flush_time, syncStopTime;
  float diff_flush_ms;
  VTCUDAsync *sync;

  /* check if device available */
  if(vtDev == NULL) return;
  /* check if buffer entries available */
  if(vtDev->buf_pos == vtDev->asyncbuf) return;

  sync = &(vtDev->sync);

  /* trace the synchronization (this is no VampirTrace overhead!!!) */
  flush_time = vt_pform_wtime();
  vt_enter(ptid, &flush_time, rid_sync);
  syncStopTime = VTCUDAsynchronizeEvt(sync->stopEvt);
  if(syncStopTime == (uint64_t)-1) return;
  vt_exit(ptid, &syncStopTime);

  /* trace the flush itself (VampirTrace overhead) */
  vt_enter(ptid, &syncStopTime, rid_flush);

  /* get time between syncStrtEvt and syncStopEvt */
  VT_CUDART_CALL(cudaEventElapsedTime_ptr(&diff_flush_ms, sync->strtEvt,
                                         sync->stopEvt),
                "cudaEventElapsedTime(float *, cudaEvent_t syncStrtEvt, "
                "cudaEvent_t syncStopEvt) failed!");

  vt_cntl_msg(3, "[CUDART] Time between syncEvts: %f ms (%llu ticks)",
                 diff_flush_ms, syncStopTime-sync->strtTime);

  /* if copy time not yet set, e.g. flush limit reached */
  if(sync->strtTime > sync->lastTime){
    sync->lastTime = sync->strtTime;
    vt_cntl_msg(1,"This should not appear!");
  }

  /* set the synchronization start point (for all streams), no asynchronous
   * task may begin before this time */
  {
    VTCUDAStrm *curStrm = vtDev->strmList;
    do{
      curStrm->lastEvt = sync->strtEvt;
      curStrm->lastVTTime = sync->strtTime;
      curStrm = curStrm->next;
    }while(curStrm != NULL);
  }

  {
    uint64_t serialKernelTime = 0;
    uint8_t idleOn = 1; /* GPU idle region is initially entered */

    /* conversion factor between VampirTrace and CUDA time */
    const double factorX = (double)(syncStopTime - sync->strtTime)/
                           (double)diff_flush_ms;

    /* write events for all recorded asynchronous calls */
    buffer_t entry = vtDev->asyncbuf;
    while(entry < vtDev->buf_pos){
      uint64_t strttime, stoptime; /* will be written in vt_enter/vt_exit */
      VTCUDAbufEntry *bufEntry = (VTCUDAbufEntry*)entry;
      uint32_t tid = bufEntry->strm->tid;

      /* get VampirTrace start and stop timestamp (in: bufEntry, minStrtTS, factorX)*/
      {
        VTCUDAStrm *strm = bufEntry->strm;
        float diff_ms;

        /* time between synchronize start event and kernel start event */
        VT_CUDART_CALL(cudaEventElapsedTime_ptr(&diff_ms, strm->lastEvt, bufEntry->evt->strt),
                      "cudaEventElapsedTime(diff, tmpStrtEvt, knStrtEvt) failed!");

        /* convert CUDA kernel start event to VampirTrace timestamp */
        strttime = strm->lastVTTime + (uint64_t)((double)diff_ms * factorX);

        /* check if kernel start time is before last synchronous CUDA call */
        if(strttime < sync->lastTime){
          strttime = sync->lastTime;
          vt_warning("[CUDART] event before last synchronous CUDA call measured!");
        }

        /* time between kernel start event and kernel stop event */
        VT_CUDART_CALL(cudaEventElapsedTime_ptr(&diff_ms, bufEntry->evt->strt, bufEntry->evt->stop),
                      "cudaEventElapsedTime(diff, knStrtEvt, knStopEvt) failed!");

        /* convert CUDA kernel stop event to VampirTrace timestamp */
        stoptime = strttime + (uint64_t)((double)diff_ms * factorX);

        if(stoptime > syncStopTime){
          stoptime = syncStopTime;
          if(strttime > syncStopTime){
            strttime = syncStopTime;
          }
          vt_warning("[CUDART] time measurement of kernel or memcpyAsync failed!");
        }

        /* set new synchronized CUDA start event and VampirTrace start timestamp,
           which keeps period small and reduces conversion errors (casts) */
        strm->lastVTTime = stoptime;
        strm->lastEvt = bufEntry->evt->stop;
      }

      if(bufEntry->type == VTCUDABUF_ENTRY_TYPE__Kernel){
        VTCUDAKernel *kn = (VTCUDAKernel*)entry;

        /* CUDA devices prior to FERMI only allow execution of one kernel */
        if(strttime < serialKernelTime && vtDev->concurrentKernels == 0){
          strttime = serialKernelTime;
        }

        /* GPU idle time will be written to first CUDA stream in list */
        if(vt_gpu_trace_idle){
          if(idleOn){
            vt_exit(vtDev->strmList->tid, &strttime);
            idleOn = 0;
          }else if(strttime > serialKernelTime){
            /* idle is off and kernels are consecutive */
            vt_enter(vtDev->strmList->tid, &serialKernelTime, vt_gpu_rid_idle);
            vt_exit(vtDev->strmList->tid, &strttime);
          }
        }
        
        /* write VampirTrace events to CUDA threads */
        vt_enter(tid, &strttime, kn->rid);
        vt_count(tid, &strttime, cid_blocksPerGrid, kn->blocksPerGrid);
        vt_count(tid, &strttime, cid_threadsPerBlock, kn->threadsPerBlock);
        vt_count(tid, &strttime, cid_threadsPerKernel,
                 kn->threadsPerBlock * kn->blocksPerGrid);
        vt_count(tid, &stoptime, cid_blocksPerGrid, 0);
        vt_count(tid, &stoptime, cid_threadsPerBlock, 0);
        vt_count(tid, &stoptime, cid_threadsPerKernel, 0);
        vt_exit(tid, &stoptime);
        
        if(serialKernelTime < stoptime) serialKernelTime = stoptime;

        /* go to next entry in buffer */
        entry += sizeof(VTCUDAKernel);
      }else if(bufEntry->type == VTCUDABUF_ENTRY_TYPE__Memcpy){
        /* write communication (in: mcpy, strttime, stoptime) */
        VTCUDAMemcpy *mcpy = (VTCUDAMemcpy*)entry;

        /* enter GPU idle region after last kernel, if exited before 
        if(idleOn == 0){
          vt_enter(vtDev->strmList->tid, &serialKernelTime, vt_gpu_rid_idle);
          idleOn = 1;
        }*/
        
        if(mcpy->kind == cudaMemcpyHostToDevice){
          vt_mpi_rma_get(tid, &strttime, VT_GPU_RANK_ID(mcpy->pid),
                          vt_gpu_commCID, 0, mcpy->byteCount);
        }else if(mcpy->kind == cudaMemcpyDeviceToHost){
          vt_mpi_rma_put(tid, &strttime, VT_GPU_RANK_ID(mcpy->pid),
                          vt_gpu_commCID, 0, mcpy->byteCount);
        }else if(mcpy->kind == cudaMemcpyDeviceToDevice){
          vt_mpi_rma_get(tid, &strttime, VT_GPU_RANK_ID(tid),
                          vt_gpu_commCID, 0, mcpy->byteCount);
       }

        vt_mpi_rma_end(tid, &stoptime, vt_gpu_commCID, 0);
        
        /* go to next entry in buffer */
        entry += sizeof(VTCUDAMemcpy);
      }
    } /* while(entry < vtDev->buf_pos) */
    
    /* enter GPU idle region after last kernel, if exited before */
    if(idleOn == 0){
      vt_enter(vtDev->strmList->tid, &serialKernelTime, vt_gpu_rid_idle);
    }
  }
  
  /* set new syncStrtTime and syncStrtEvt */
  {
    cudaEvent_t tmp_Evt = sync->strtEvt;
    sync->strtEvt = sync->stopEvt;
    sync->stopEvt = tmp_Evt;
  }
  sync->strtTime = syncStopTime;
  sync->lastTime = syncStopTime;

  /* reset entry and event buffer */
  vtDev->buf_pos = vtDev->asyncbuf;
  vtDev->evtbuf_pos = vtDev->evtbuf;

  flush_time = vt_pform_wtime();
  vt_exit(ptid, &flush_time);
}

/**
 * Creates a VampirTrace CUDA stream object and returns it.
 *
 *  @param device the CUDA device id this stream is created for
 *  @param stream the CUDA stream id
 *  @param ptid the VampirTrace thread ID of the calling thread
 *
 *  @return the created stream object
 */
static VTCUDAStrm* VTCUDAcreateStream(VTCUDADevice* vtDev, cudaStream_t cuStrm)
{
  uint32_t gpu_tid = 0;
  char thread_name[16];
  VTCUDAStrm *nstrm;
  uint32_t strmNum = vtDev->strmNum;

  /* allocate memory for stream */
  nstrm = (VTCUDAStrm*) malloc(sizeof (VTCUDAStrm));
  if(nstrm == NULL) vt_error_msg("malloc(sizeof(VTCUDAStrm)) failed!");

  nstrm->stream = cuStrm;
  nstrm->lastVTTime = 0;
  nstrm->next = NULL;
  
  /* set the stream number */
  if(cuStrm == NULL){
    strmNum = 1;
  }else{
    vtDev->strmNum++;
  }

  /* create VT-User-Thread with name and parent id and get its id */
  if(-1 == snprintf(thread_name, 15, "CUDA[%d:%d]", vtDev->device, strmNum))
    vt_cntl_msg(1, "Could not create thread name for CUDA thread!");
  vt_gpu_registerThread(thread_name, vtDev->ptid, &gpu_tid);
  nstrm->tid = gpu_tid;
  
  /* if first stream created for this device, make it the default stream */
  if(vtDev->strmList == NULL){
    /* write enter event for GPU_IDLE on first stream */
    if(vt_gpu_trace_idle == 1) 
      vt_enter(gpu_tid, &vt_start_time, vt_gpu_rid_idle);

    /* set the counter value for cudaMalloc to 0 on first stream */
    if(trace_gpumem > 0) 
      vt_count(gpu_tid, &vt_start_time, cid_cudaMalloc, 0);
  }
  
  return nstrm;
}

/*
 * Creates a VTCUDADevice and returns a pointer to the created object.
 *
 * @param device the CUDA device id
 *
 * @return VampirTrace CUDA device object
 */
static VTCUDADevice* VTCUDAcreateDevice(uint32_t ptid, int device)
{
  VTCUDADevice *vtDev = (VTCUDADevice*)malloc(sizeof(VTCUDADevice));
  if(vtDev == NULL) vt_error_msg("Could not allocate memory for VTCUDADevice!");
  vtDev->device = device;
  vtDev->ptid = ptid;
  vtDev->mallocList = NULL;
  vtDev->mallocated = 0;
  vtDev->asyncbuf = NULL;
  vtDev->buf_pos = NULL;
  vtDev->buf_size = NULL;
  vtDev->conf_stack = NULL;
  vtDev->evtbuf = NULL;
  vtDev->evtbuf_pos = NULL;
  vtDev->strmList = NULL;
  vtDev->strmNum = 2;
  vtDev->next = NULL;

#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 3000))
  /* get compute capability of CUDA device */
  {
    struct cudaDeviceProp deviceProp;
    cudaGetDeviceProperties_ptr(&deviceProp, device);
    vtDev->concurrentKernels = (uint8_t)deviceProp.concurrentKernels;
  }
#else
  vtDev->concurrentKernels = 0;
#endif

  /* async buffer or events may not be used */
  if(trace_events){
    int cuDev_save = 0;
    
    /* set the device to be created, if it is not the current yet 
       (needed for peer2peer copy only) */
    VT_CUDART_CALL(cudaGetDevice_ptr(&cuDev_save),"cudaGetDevice()");
    if(cuDev_save != device){
      VT_CUDART_CALL(cudaSetDevice(device), "cudaSetDevice()");
    }
    
    /* --- set VampirTrace - CUDA time synchronization --- */
    VT_CUDART_CALL(cudaEventCreate_ptr(&(vtDev->sync.strtEvt)),
                  "cudaEventCreate(syncStrtEvt) failed!");

    VT_CUDART_CALL(cudaEventCreate_ptr(&(vtDev->sync.stopEvt)),
                  "cudaEventCreate(syncStopEvt) failed!");

    /* record init event for later synchronization with VampirTrace time */
    vtDev->sync.strtTime = VTCUDAsynchronizeEvt(vtDev->sync.strtEvt);

    /* set initial memory copy timestamp, if no memory copies are done */
    vtDev->sync.lastTime = vtDev->sync.strtTime;

    /* allocate buffers for asynchronous entries */
    vtDev->asyncbuf = malloc(vt_cudart_bufSize);
    if(vtDev->asyncbuf == NULL){
      vt_error_msg("malloc of asynchronous CUDA call buffer failed! "
                  "Reduce buffer size with VT_BUFFER_SIZE!");
    }
    vtDev->buf_pos = vtDev->asyncbuf;
    vtDev->buf_size = vtDev->asyncbuf + vt_cudart_bufSize;
    vtDev->conf_stack = vtDev->buf_size;

    vtDev->evtbuf = (VTCUDABufEvt*)malloc(maxEvtNum*sizeof(VTCUDABufEvt));
    if(vtDev->evtbuf == NULL)
      vt_error_msg("Could not allocate memory for VTCUDABufEvt!");
    vtDev->evtbuf_pos = vtDev->evtbuf;

    {/* create CUDA events */
      size_t i;
      cudaError_t ret = cudaSuccess;
      for(i = 0; i < maxEvtNum; i++){
        cudaEventCreate_ptr(&((vtDev->evtbuf[i]).strt));
        ret = cudaEventCreate_ptr(&((vtDev->evtbuf[i]).stop));
      }
      VT_CUDART_CALL(ret, "cudaEventCreate failed");
    }
    
    if(cuDev_save != device){
      VT_CUDART_CALL(cudaSetDevice(cuDev_save), "cudaSetDevice()");
    }
  }

#if (defined(VT_CUPTI_EVENTS))
  if(trace_cupti_events){
    vtDev->asyncbuf = malloc(vt_cudart_bufSize);
    vtDev->buf_pos = vtDev->asyncbuf;
    vtDev->buf_size = vtDev->asyncbuf + vt_cudart_bufSize;
    vtDev->conf_stack = vtDev->buf_size;
  }
#endif

  return vtDev;
}

/*
 * Invokes the device creation for VTCUDA.
 *
 * @param ptid the host process/thread id
 * @param cudaDev the CUDA device identifier
 *
 * @return the VampirTrace CUDA device structure
 */
static VTCUDADevice* VTCUDAinitDevice(uint32_t ptid, int cudaDev, 
                                      cudaStream_t cuStrm)
{
  uint64_t time;
  VTCUDADevice *vtDev = NULL;
  
  /*vt_cntl_msg(1, "Init CUDA device %d", cudaDev);*/

  /* cuda device not found, create new cuda device node */
  time = vt_pform_wtime();
  vt_enter(ptid, &time, rid_create);
    vtDev = VTCUDAcreateDevice(ptid, cudaDev);
  time = vt_pform_wtime();
  vt_exit(ptid, &time);

  /* the first stream of the device is not the default stream, IDLE time 
     is enabled and asynchronous memory copy tracing is enabled */
  if(cuStrm != NULL && vtDev->strmList == NULL && 
     vt_gpu_trace_idle == 1 && trace_memcpyAsync == 1){
     vtDev->strmList = VTCUDAcreateStream(vtDev, NULL);
     vtDev->strmList->next = VTCUDAcreateStream(vtDev, cuStrm);
  }else{
    /* set the current stream (stream 0) */
    vtDev->strmList = VTCUDAcreateStream(vtDev, cuStrm);
  }

  /* add thread and CUDA device to list */
  CUDARTWRAP_LOCK();
    /* prepend */
    vtDev->next = cudaDevices;
    cudaDevices = vtDev;
  CUDARTWRAP_UNLOCK();
  
  return vtDev;
}

/*
 * Check if the active CUDA device with the given CUDA stream is registered.
 * Creates a CUDA device object or VampirTrace CUDA stream if not registered.
 *
 * @param stream the CUDA stream, which should be used after this function call.
 * @param ptid the VampirTrace thread ID of the calling thread
 * @param vtStrm pointer to a pointer of a CUDA stream (caller needs stream id)
 *
 * @return VampirTrace CUDA device object
 */
static VTCUDADevice* VTCUDAcheckThread(cudaStream_t cuStrm, uint32_t ptid,
                                       VTCUDAStrm **vtStrm)
{
    VTCUDADevice *vtDev;
    uint64_t time_check;
    int device;

    time_check = vt_pform_wtime();
    vt_enter(ptid, &time_check, rid_check);

    /* get the device to set the gpu_tid */
    VT_CUDART_CALL(cudaGetDevice_ptr(&device), "cudaGetDevice(device) failed!");

    CUDARTWRAP_LOCK();
    vt_cntl_msg(3, "Using CUDA device %d", device);

    /* check if this device+stream has been registered as VampirTrace thread */
    vtDev = cudaDevices;
    while(vtDev != NULL){
      if(vtDev->device == device && vtDev->ptid == ptid){
        /* the CUDA device is already listed -> stream 0 exists */
        VTCUDAStrm *curStrm, *ptrLastStrm;
        curStrm = vtDev->strmList;

        CUDARTWRAP_UNLOCK();
        do{
          if(cuStrm == curStrm->stream){
            *vtStrm = curStrm;
            time_check = vt_pform_wtime();
            vt_exit(ptid, &time_check);
            return vtDev;
          }
          ptrLastStrm = curStrm;
          curStrm = curStrm->next;
        }while(curStrm != NULL);
        /* stream not found */

        /* append newly created stream (stream 0 is probably used most, will
           therefore always be the first element in the list */
        ptrLastStrm->next = VTCUDAcreateStream(vtDev, cuStrm);
        *vtStrm = ptrLastStrm->next;

        time_check = vt_pform_wtime();
        vt_exit(ptid, &time_check);
        return vtDev;
      }

      vtDev = vtDev->next;
    }
    CUDARTWRAP_UNLOCK();

    /* CUDA device not found, create new CUDA device node */
   vtDev = VTCUDAinitDevice(ptid, device, cuStrm);

   time_check = vt_pform_wtime();
   vt_exit(ptid, &time_check);
  
   /* the return values */
   *vtStrm = vtDev->strmList;
   return vtDev;
}

/*
 * Retrieves the VampirTrace CUDA device object for the current CUDA device.
 *
 * @param ptid the VampirTrace thread id of the active thread
 *
 * @return the VampirTrace CUDA device structure for current CUDA device or NULL if not found
 */
static VTCUDADevice* VTCUDAgetDevice(uint32_t ptid)
{
  int device;
  VTCUDADevice *vtDev = NULL;

  /* get the device to set the gpu_tid */
  VT_CUDART_CALL(cudaGetDevice_ptr(&device),
                "cudaGetDevice(int *device) failed!");

  vt_cntl_msg(3, "Lookup CUDA device %d", device);

  CUDARTWRAP_LOCK();
  vtDev = cudaDevices;
  while(vtDev != NULL){
    if(vtDev->device == device && vtDev->ptid == ptid){
      /* the cuda device is already listed -> stream 0 exists */
      CUDARTWRAP_UNLOCK();
      return vtDev;
    }
    vtDev = vtDev->next;
  }
  CUDARTWRAP_UNLOCK();

  /* This may happen, if user does e.g. cudaThreadSynchronize before actually
     using the GPU
  vt_cntl_msg(2, "[CUDART] Device not created yet! Creating ... ");
  return VTCUDAinitDevice(ptid, device);*/
  vt_cntl_msg(2, "[CUDART] Useless or wrong cuda*-function call?");
  return NULL;
}

/*
 * Add memory copy to entry buffer.
 *
 * @param kind kind/direction of memory copy
 * @param count number of bytes for this data transfer
 * @param stream the CUDA stream
 *
 * @return pointer to the VampirTrace CUDA memory copy structure
 */
static VTCUDAMemcpy* addMemcpy2Buf(enum cudaMemcpyKind kind, int count,
                                   cudaStream_t stream)
{
  VTCUDADevice *vtDev;
  VTCUDAStrm *ptrStrm;
  VTCUDAMemcpy *mcpy;
  uint32_t ptid;

  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;
  vtDev = VTCUDAcheckThread(stream, ptid, &ptrStrm);


  /* check if there is enough buffer space */
  if(vtDev->buf_pos + sizeof(VTCUDAMemcpy) > vtDev->conf_stack){
    VTCUDAflush(vtDev, ptid);
    if(vtDev->buf_pos + sizeof(VTCUDAMemcpy) > vtDev->conf_stack){
      vt_error_msg("[CUDART] Not enough buffer space for asynchronous memory copy!");
    }
  }

  /* get and increase entry buffer position */
  mcpy = (VTCUDAMemcpy*)vtDev->buf_pos;
  vtDev->buf_pos += sizeof(VTCUDAMemcpy);

  /* initialize asynchronous memory copy entry */
  mcpy->type = VTCUDABUF_ENTRY_TYPE__Memcpy;
  mcpy->strm = ptrStrm;
  mcpy->byteCount = count;
  mcpy->pid = ptid;
  mcpy->kind = kind;

  /* get and increase event buffer position */
  mcpy->evt = vtDev->evtbuf_pos;
  vtDev->evtbuf_pos++;

  /* both threads are involved in GPU communication */
  CUDARTWRAP_LOCK();
    vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;
    vt_gpu_prop[ptrStrm->tid] |= VTGPU_GPU_COMM;
  CUDARTWRAP_UNLOCK();

  return mcpy;
}

#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 4000))
/*
 * Retrieves the VampirTrace CUDA device object for the current CUDA device.
 *
 * @param ptid the VampirTrace thread id of the active thread
 *
 * @return the VampirTrace CUDA device structure for current CUDA device or NULL if not found
 */
static VTCUDADevice* VTCUDAcheckDeviceByID(int cuDev)
{
  VTCUDADevice *vtDev = NULL;

  /*vt_cntl_msg(1, "Lookup CUDA device %d", cuDev);*/

  CUDARTWRAP_LOCK();
  vtDev = cudaDevices;
  while(vtDev != NULL){
    if(vtDev->device == cuDev){
      /* the cuda device is already listed -> stream 0 exists */
      CUDARTWRAP_UNLOCK();
      return vtDev;
    }
    vtDev = vtDev->next;
  }
  CUDARTWRAP_UNLOCK();
  
  return VTCUDAinitDevice(VT_MY_THREAD, cuDev, 0);
}

/*
 * Setup a synchronous peer-to-peer memory copy. Synchronization, flush, etc.
 * 
 * @param ptid host process/thread id
 * @param vtSrcDev VampirTrace source device
 * @param vtDstDev VampirTrace destination device
 * 
 * @return last written VampirTrace timestamp (only needed for syncLevel 1)
 */
static uint64_t vt_cudart_setupMemcpyPeer2Peer(uint32_t ptid, 
                                               VTCUDADevice *vtSrcDev, 
                                               VTCUDADevice *vtDstDev)
{
  uint64_t time = 0;
  int currentCuDev = 0;
  VTCUDADevice *vtCurrDev = NULL;

  /* get the current device */
  VT_CUDART_CALL(cudaGetDevice_ptr(&currentCuDev), "cudaGetDevice()");
    
  if(syncLevel > 2){
    /* flush the current device first */
    if(currentCuDev == vtSrcDev->device){
      VTCUDAflush(vtSrcDev, ptid);
      VT_CUDART_CALL(cudaSetDevice_ptr(vtDstDev->device), "cudaSetDevice()");
      vtCurrDev = vtDstDev;
      VTCUDAflush(vtDstDev, ptid);
    }else{
      VTCUDAflush(vtDstDev, ptid);
      VT_CUDART_CALL(cudaSetDevice_ptr(vtSrcDev->device), "cudaSetDevice()");
      vtCurrDev = vtSrcDev;
      VTCUDAflush(vtSrcDev, ptid);
    }
  }else if(syncLevel > 0){
    time = vt_pform_wtime();

    if(syncLevel > 1) vt_enter(ptid, &time, rid_sync);

    /* synchronize the current device */
    VT_CUDART_CALL(cudaThreadSynchronize_ptr(),"vtcudaSync() failed!");

    /* synchronize the other device */
    if(currentCuDev == vtSrcDev->device){
      VT_CUDART_CALL(cudaSetDevice_ptr(vtDstDev->device), "cudaSetDevice()");
      vtCurrDev = vtDstDev;
    }else{
      VT_CUDART_CALL(cudaSetDevice_ptr(vtSrcDev->device), "cudaSetDevice()");
      vtCurrDev = vtSrcDev;
    }

    VT_CUDART_CALL(cudaThreadSynchronize_ptr(),"vtcudaSync() failed!");

    if(syncLevel > 1){
      time = vt_pform_wtime(); 
      vt_exit(ptid, &time);
    }
  }
  
  /* set saved device as current, if necessary */
  if(currentCuDev != vtCurrDev->device){
    VT_CUDART_CALL(cudaSetDevice_ptr(currentCuDev), "cudaSetDevice()");
  }

  /* add GPU communication property */
  CUDARTWRAP_LOCK();
    vt_gpu_prop[vtSrcDev->strmList->tid] |= VTGPU_GPU_COMM;
    vt_gpu_prop[vtDstDev->strmList->tid] |= VTGPU_GPU_COMM;
  CUDARTWRAP_UNLOCK();
  
  return time;
}

/*
 * Retrieve the kind of CUDA memory copy by source and destination device 
 * pointers, if enum cudaMemcpyKind is cudaMemcpyDefault. Return the device 
 * IDs additionally for peer-to-peer memory copies.
 * 
 * @param src source device pointer (input)
 * @param dst destination device pointer (input)
 * @param kind the cudaMemcpyKind (inout)
 * @param cuSrcDev the CUDA source device number (output)
 * @param cuDstDev the CUDA destination device number (output)
 */
static void vt_cudart_getMemcpyKind(void *src, void *dst, 
                                    enum cudaMemcpyKind *kind,
                                    int *cuSrcDev,
                                    int *cuDstDev)
{
  if(*kind == cudaMemcpyDefault){
    struct cudaPointerAttributes attr_src;
    struct cudaPointerAttributes attr_dst;
    
    /* get source and destination pointer attributes */
    VT_CUDART_CALL(cudaPointerGetAttributes(&attr_src, src), 
                   "cudaPointerGetAttributes()");
    VT_CUDART_CALL(cudaPointerGetAttributes(&attr_dst, dst), 
                   "cudaPointerGetAttributes() for destination");
    
    if(attr_src.memoryType == cudaMemoryTypeHost){
      if(attr_dst.memoryType == cudaMemoryTypeDevice){
        *kind = cudaMemcpyHostToDevice;
      }else{
        *kind = cudaMemcpyHostToHost;
      }
    }else{      
      if(attr_dst.memoryType == cudaMemoryTypeDevice){
        /* same device */
        if(attr_dst.device == attr_src.device){
          *kind = cudaMemcpyDeviceToDevice;
        }else{
          *cuSrcDev = attr_src.device;
          *cuDstDev = attr_dst.device;
        }
      }else{
        *kind = cudaMemcpyDeviceToHost;
      }
    }
  }
}

#endif /* defined(CUDART_VERSION) && (CUDART_VERSION >= 4000) */

/*
 * Increases the "Allocated CUDA memory" counter.
 *
 * @param devPtr pointer to the allocated memory (needed for vtcudaFree())
 * @param size the number of bytes allocated
 */
static void vtcudaMalloc(void *devPtr, size_t size)
{
  uint32_t ptid;
  uint64_t vtTime;
  VTCUDADevice *vtDev = NULL;
  VTCUDAStrm *vtStrm = NULL;
  VTCUDAmalloc *vtMalloc = (VTCUDAmalloc*)malloc(sizeof(VTCUDAmalloc));

  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;
  vtDev = VTCUDAcheckThread(NULL, ptid, &vtStrm);

  vtMalloc->memPtr = devPtr;
  vtMalloc->size = size;
  vtMalloc->next = vtDev->mallocList;
  vtDev->mallocList = vtMalloc;
  vtDev->mallocated += size;

  /* flush before writing counter to cuda thread necessary */
  VTCUDAflush(vtDev, ptid);

  /* write counter value */
  vtTime = vt_pform_wtime();
  vt_count(vtStrm->tid, &vtTime, cid_cudaMalloc, (uint64_t)(vtDev->mallocated));
  REGISTER_FINALIZE;
}

/*
 * Decreases the "Allocated CUDA memory" counter.
 *
 * @param devPtr pointer to the allocated memory
 */
static void vtcudaFree(void *devPtr)
{
  uint64_t vtTime;
  uint32_t ptid;
  VTCUDADevice *vtDev = NULL;
  VTCUDAmalloc *curMalloc;
  VTCUDAmalloc *lastMalloc;

  VT_CHECK_THREAD;
  ptid = VT_MY_THREAD;
  vtDev = VTCUDAgetDevice(ptid);

  if(vtDev == NULL || devPtr == NULL) return;

  curMalloc = vtDev->mallocList;
  lastMalloc = vtDev->mallocList;

  while(curMalloc != NULL){
    if(devPtr == curMalloc->memPtr){
      /* Flush CUDA thread before writing the counter */
      VTCUDAflush(vtDev, ptid);

      /* decrease allocated counter value and write it */
      vtTime = vt_pform_wtime();
      vtDev->mallocated -= curMalloc->size;
      vt_count(vtDev->strmList->tid, &vtTime, cid_cudaMalloc,
               (uint64_t)(vtDev->mallocated));


      /* set pointer over current element to next one */
      lastMalloc->next = curMalloc->next;

      /* if current element is the first list entry, set the list entry */
      if(curMalloc == vtDev->mallocList){
        vtDev->mallocList = curMalloc->next;
      }

      /* free VT memory of cuda malloc */
      curMalloc->next = NULL;
      free(curMalloc);
      curMalloc = NULL;

      /* set mallocList to NULL, if last element freed */
      if(vtDev->mallocated == 0) {
        vtDev->mallocList = NULL;
      }
      return;
    }

    lastMalloc = curMalloc;
    curMalloc = curMalloc->next;
  }

  vt_warning("[CUDART] free cuda memory, which has not been allocated!");
}

/*
 * Parse the device function name:
 * "_Z<kernel_length><kernel_name><templates>..." (no namespace)
 * "_ZN<ns_length><ns_name>...<ns_length><ns_name><kernel_length>..." (with namespace)
 *
 * @param elem pointer to the kernel element
 * @param devFunc the CUDA internal kernel function name
 */
static void extractKernelName(VTCUDAkernelSymbol *e, const char* devFunc)
{
  int i = 0;       /* position in device function (source string) */
  int nlength = 0; /* length of namespace or kernel */
  int ePos = 0;    /* position in final kernel string */
  char *curr_elem, kn_templates[VTGPU_KERNEL_STRING_SIZE];
  char *tmpEnd, *tmpElemEnd;

  /*vt_cntl_msg(1,"[CUDART] device funtion name: %s'", devFunc);*/

  /* init for both cases: namespace available or not */
  if(devFunc[2] == 'N'){
    nlength = atoi(&devFunc[3]); /* get length of first namespace */
    i = 4;
  }else{
    nlength = atoi(&devFunc[2]); /* get length of kernel */
    i = 3;
  }

  /* unless string null termination */
  while(devFunc[i] != '\0'){
    /* found either namespace or kernel name (no digits) */
    if(devFunc[i] < '0' || devFunc[i] > '9'){
      /* copy name to kernel function */
      if((ePos + nlength) < VTGPU_KERNEL_STRING_SIZE){
        (void)strncpy(&e->name[ePos], &devFunc[i], nlength);
        ePos += nlength; /* set next position to write */
      }else{
        nlength = VTGPU_KERNEL_STRING_SIZE - ePos;
        (void)strncpy(&e->name[ePos], &devFunc[i], nlength);
        vt_cntl_msg(1,"[CUDART]: kernel name '%s' contains more than %d chars!",
                      devFunc, VTGPU_KERNEL_STRING_SIZE);
        return;
      }

      i += nlength; /* jump over name */
      nlength = atoi(&devFunc[i]); /* get length of next namespace or kernel */

      /* finish if no digit after namespace or kernel */
      if(nlength == 0){
        e->name[ePos] = '\0'; /* set string termination */
        break;
      }else{
        if((ePos + 3) < VTGPU_KERNEL_STRING_SIZE){
          (void)strncpy(&e->name[ePos], "::\0", 3);
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

    /* write at postion 'I' with '<' */
    /* elem->name[ePos]='<'; */
    if(-1 == snprintf(&(e->name[ePos]),VTGPU_KERNEL_STRING_SIZE-ePos,"<"))
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
      if(-1 == (res = snprintf(&(e->name[ePos]),
                               VTGPU_KERNEL_STRING_SIZE-ePos,"%s,",curr_elem)))
        vt_cntl_msg(1,"[CUDART]: Parsing templates of kernel '%s' crashed!", devFunc);
      ePos += res; /* continue after template value */
      curr_elem =tmpElemEnd + 1; /* set current element to begin of next template */
    }while(tmpElemEnd < tmpEnd);
    if((ePos-1) < VTGPU_KERNEL_STRING_SIZE) (void)strncpy(&e->name[ePos-1], ">\0", 2);
    else vt_cntl_msg(1,"[CUDART]: Templates of '%s' too long for internal buffer!", devFunc);
  } /* else: kernel has no templates */
  /*vt_cntl_msg(1,"[CUDART] funtion name: %s'",e->name);*/
}

/*
 * Inserts a new element in the kernel list (LIFO).
 * (Called by __cudaRegisterFunction)
 *
 *  @param hostFun the name of the host function
 * @param devFunc the name of kernel (device function)
 */
static void insertKernelSymbol(const char* hostFun, const char* devFunc)
{
  VTCUDAkernelSymbol* e = (VTCUDAkernelSymbol*) malloc(sizeof(VTCUDAkernelSymbol));
  e->pointer = hostFun;
  e->knSymbolName = devFunc;
  
  /*strncpy(e->deviceName, deviceName, DEVICE_NAME_SIZE);*/
  extractKernelName(e, devFunc);
  
  if(vt_cudart_filter){
    int32_t climit;
    
    RFG_Filter_get(vt_cudart_filter, e->name, &climit);

    if(climit == 0) return;
  }

#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
   e->rid = vt_def_region(VT_MASTER_THREAD, e->name, VT_NO_ID, VT_NO_LNO,
                           VT_NO_LNO, "CUDA_KERNEL", VT_FUNCTION);
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif

  /* lock list operation if multi-threaded */
  CUDARTWRAP_LOCK();
    e->next = kernelListHead;
    kernelListHead = e;
  CUDARTWRAP_UNLOCK();
}

/*
 * Get kernel element from host function pointer (to lookup name and token).
 *
 * @param hostFun the identifier string of the CUDA kernel
 * @return the kernel or NULL, if nothing was found
 * @todo linear search could be replaced with hash
 */
static VTCUDAkernelSymbol* getKernelByHostFunction(const char* hostFun)
{
  VTCUDAkernelSymbol *actual = NULL;

  /* lock list operation if multi-threaded */
  CUDARTWRAP_LOCK();

  actual = kernelListHead;

  while(actual != NULL) {
    if(hostFun == actual->pointer) {
      CUDARTWRAP_UNLOCK();
      return actual;
    }
    actual = actual->next;
  }

  CUDARTWRAP_UNLOCK();

  return NULL; /* not found */
}

/*
 * This function is being called before execution of a CUDA program for every
 * CUDA kernel (host_runtime.h)
 */
void __cudaRegisterFunction(void **, const char *, char *, const char *, int,
                            uint3 *, uint3 *, dim3 *, dim3 *, int *);
void  __cudaRegisterFunction(void   **fatCubinHandle,
  const char    *hostFun,
        char    *deviceFun,
  const char    *deviceName,
        int      thread_limit,
        uint3   *tid,
        uint3   *bid,
        dim3    *bDim,
        dim3    *gDim,
        int     *wSize )
{

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "__cudaRegisterFunction",
      cudaError_t , (void   **,
            const char    *,
                  char    *,
            const char    *,
                  int      ,
                  uint3   *,
                  uint3   *,
                  dim3    *,
                  dim3    *,
                  int     *),
      NULL, 0);

    VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (fatCubinHandle,hostFun,deviceFun,deviceName,
        thread_limit,tid,bid,bDim,gDim,wSize));

    if(vt_cudart_trace_enabled && trace_kernels){
      insertKernelSymbol(hostFun, deviceFun/*, deviceName*/);
    }
}

/* -- cuda_runtime_api.h:cudaMalloc3D -- */

cudaError_t  cudaMalloc3D(struct cudaPitchedPtr *pitchedDevPtr, struct cudaExtent extent)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMalloc3D",
    cudaError_t , (struct cudaPitchedPtr *, struct cudaExtent ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pitchedDevPtr, extent));

  if(vt_cudart_trace_enabled){
    VT_LIBWRAP_FUNC_END(vt_cudart_lw);
    if(trace_gpumem > 0){
      vtcudaMalloc(pitchedDevPtr->ptr,
                   pitchedDevPtr->pitch * extent.height * extent.depth);
    }
  }

  return ret;
}

/* if  < CUDA 3.1 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION < 3010))

/* -- cuda_runtime_api.h:cudaMalloc3DArray -- */

cudaError_t  cudaMalloc3DArray(struct cudaArray **arrayPtr, const struct cudaChannelFormatDesc *desc, struct cudaExtent extent)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMalloc3DArray",
    cudaError_t , (struct cudaArray **, const struct cudaChannelFormatDesc *, struct cudaExtent ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (arrayPtr, desc, extent));

  if(vt_cudart_trace_enabled){
    VT_LIBWRAP_FUNC_END(vt_cudart_lw);
    if(trace_gpumem > 0){
      vtcudaMalloc(*arrayPtr, extent.width * extent.height * extent.depth);
    }
  }

  return ret;
}

/* -- cuda_runtime_api.h:cudaMallocArray -- */

cudaError_t  cudaMallocArray(struct cudaArray **array, const struct cudaChannelFormatDesc *desc, size_t width, size_t height)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMallocArray",
    cudaError_t , (struct cudaArray **, const struct cudaChannelFormatDesc *, size_t , size_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (array, desc, width, height));

  if(vt_cudart_trace_enabled){
    VT_LIBWRAP_FUNC_END(vt_cudart_lw);
    if(trace_gpumem > 0) vtcudaMalloc(*array, width * height);
  }

  return ret;
}

#endif

/* -- cuda_runtime_api.h:cudaMemcpy3D -- */
cudaError_t  cudaMemcpy3D(const struct cudaMemcpy3DParms *p)
{
  cudaError_t  ret;

  enum cudaMemcpyKind kind = p->kind;
  struct cudaExtent extent = p->extent;
  int count = extent.height * extent.width * extent.depth;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy3D",
    cudaError_t , (const struct cudaMemcpy3DParms *),
    NULL, 0);

  VT_CUDART_MEMCPY(p->dstArray, p->srcArray, kind, count,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (p));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy3DAsync -- */
cudaError_t  cudaMemcpy3DAsync(const struct cudaMemcpy3DParms *p, cudaStream_t stream)
{
  cudaError_t  ret;
  enum cudaMemcpyKind kind = p->kind;
  struct cudaExtent extent = p->extent;
  size_t count = extent.height * extent.width * extent.depth;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy3DAsync",
    cudaError_t , (const struct cudaMemcpy3DParms *, cudaStream_t ),
    NULL, 0);

  CUDA_MEMCPY_ASYNC(kind, count, stream,
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (p, stream));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMalloc -- */
cudaError_t  cudaMalloc(void **devPtr, size_t size)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMalloc",
    cudaError_t , (void **, size_t ),
    NULL, 0);

  if(vt_cudart_trace_enabled) VT_LIBWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (devPtr, size));

  if(vt_cudart_trace_enabled){
    VT_LIBWRAP_FUNC_END(vt_cudart_lw);
    if(trace_gpumem > 0) vtcudaMalloc(*devPtr, size);
  }

  return ret;
}

/* -- cuda_runtime_api.h:cudaMallocPitch -- */
cudaError_t  cudaMallocPitch(void **devPtr, size_t *pitch, size_t width, size_t height)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMallocPitch",
    cudaError_t , (void **, size_t *, size_t , size_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (devPtr, pitch, width, height));

  if(vt_cudart_trace_enabled){
    VT_LIBWRAP_FUNC_END(vt_cudart_lw);
    if(trace_gpumem > 0) vtcudaMalloc(*devPtr, (*pitch) * height);
  }

  return ret;
}

/* -- cuda_runtime_api.h:cudaFree -- */
cudaError_t  cudaFree(void *devPtr)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaFree",
    cudaError_t , (void *),
    NULL, 0);

  if(vt_cudart_trace_enabled){
    if(trace_gpumem > 0) vtcudaFree(devPtr);
    VT_LIBWRAP_FUNC_START(vt_cudart_lw);
  }

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (devPtr));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaFreeArray -- */
cudaError_t  cudaFreeArray(struct cudaArray *array)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaFreeArray",
    cudaError_t , (struct cudaArray *),
    NULL, 0);

  if(vt_cudart_trace_enabled){
    if(trace_gpumem > 0) vtcudaFree(array);
    VT_LIBWRAP_FUNC_START(vt_cudart_lw);
  }

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (array));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy -- */
cudaError_t  cudaMemcpy(void *dst, const void *src, size_t count, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy",
    cudaError_t , (void *, const void *, size_t , enum cudaMemcpyKind ),
    NULL, 0);
  
  VT_CUDART_MEMCPY(dst, src, kind, count,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, src, count, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyToArray -- */
cudaError_t  cudaMemcpyToArray(struct cudaArray *dst, size_t wOffset, size_t hOffset, const void *src, size_t count, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyToArray",
    cudaError_t , (struct cudaArray *, size_t , size_t , const void *, size_t , enum cudaMemcpyKind ),
    NULL, 0);

  VT_CUDART_MEMCPY(dst, src, kind, count,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, wOffset, hOffset, src, count, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyFromArray -- */
cudaError_t  cudaMemcpyFromArray(void *dst, const struct cudaArray *src, size_t wOffset, size_t hOffset, size_t count, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyFromArray",
    cudaError_t , (void *, const struct cudaArray *, size_t , size_t , size_t , enum cudaMemcpyKind ),
    NULL, 0);

  VT_CUDART_MEMCPY(dst, src, kind, count,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, src, wOffset, hOffset, count, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyArrayToArray -- */
cudaError_t  cudaMemcpyArrayToArray(struct cudaArray *dst, size_t wOffsetDst, size_t hOffsetDst, const struct cudaArray *src, size_t wOffsetSrc, size_t hOffsetSrc, size_t count, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyArrayToArray",
    cudaError_t , (struct cudaArray *, size_t , size_t , const struct cudaArray *, size_t , size_t , size_t , enum cudaMemcpyKind ),
    NULL, 0);

  VT_CUDART_MEMCPY(dst, src, kind, count,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, wOffsetDst, hOffsetDst, src, wOffsetSrc, hOffsetSrc, count, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy2D -- */
cudaError_t  cudaMemcpy2D(void *dst, size_t dpitch, const void *src, size_t spitch, size_t width, size_t height, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy2D",
    cudaError_t , (void *, size_t , const void *, size_t , size_t , size_t , enum cudaMemcpyKind ),
    NULL, 0);

  VT_CUDART_MEMCPY(dst, src, kind, width*height,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, dpitch, src, spitch, width, height, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy2DToArray -- */
cudaError_t  cudaMemcpy2DToArray(struct cudaArray *dst, size_t wOffset, size_t hOffset, const void *src, size_t spitch, size_t width, size_t height, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy2DToArray",
    cudaError_t , (struct cudaArray *, size_t , size_t , const void *, size_t , size_t , size_t , enum cudaMemcpyKind ),
    NULL, 0);

  VT_CUDART_MEMCPY(dst, src, kind, width*height,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, wOffset, hOffset, src, spitch, width, height, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy2DFromArray -- */
cudaError_t  cudaMemcpy2DFromArray(void *dst, size_t dpitch, const struct cudaArray *src, size_t wOffset, size_t hOffset, size_t width, size_t height, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy2DFromArray",
    cudaError_t , (void *, size_t , const struct cudaArray *, size_t , size_t , size_t , size_t , enum cudaMemcpyKind ),
    NULL, 0);

  VT_CUDART_MEMCPY(dst, src, kind, width*height,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, dpitch, src, wOffset, hOffset, width, height, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy2DArrayToArray -- */
cudaError_t  cudaMemcpy2DArrayToArray(struct cudaArray *dst, size_t wOffsetDst, size_t hOffsetDst, const struct cudaArray *src, size_t wOffsetSrc, size_t hOffsetSrc, size_t width, size_t height, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy2DArrayToArray",
    cudaError_t , (struct cudaArray *, size_t , size_t , const struct cudaArray *, size_t , size_t , size_t , size_t , enum cudaMemcpyKind ),
    NULL, 0);

  VT_CUDART_MEMCPY(dst, src, kind, width*height,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, wOffsetDst, hOffsetDst, src, wOffsetSrc, hOffsetSrc, width, height, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyToSymbol -- */
cudaError_t  cudaMemcpyToSymbol(const char *symbol, const void *src, size_t count, size_t offset, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyToSymbol",
    cudaError_t , (const char *, const void *, size_t , size_t , enum cudaMemcpyKind ),
    NULL, 0);

  VT_CUDART_MEMCPY(symbol, src, kind, count,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (symbol, src, count, offset, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyFromSymbol -- */
cudaError_t  cudaMemcpyFromSymbol(void *dst, const char *symbol, size_t count, size_t offset, enum cudaMemcpyKind kind)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyFromSymbol",
    cudaError_t , (void *, const char *, size_t , size_t , enum cudaMemcpyKind ),
    NULL, 0);

  VT_CUDART_MEMCPY(dst, symbol, kind, count,
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, symbol, count, offset, kind));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyAsync -- */
cudaError_t  cudaMemcpyAsync(void *dst, const void *src, size_t count, enum cudaMemcpyKind kind, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyAsync",
    cudaError_t , (void *, const void *, size_t , enum cudaMemcpyKind , cudaStream_t ),
    NULL, 0);

  CUDA_MEMCPY_ASYNC(kind, count, stream,
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, src, count, kind, stream));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyToArrayAsync -- */
cudaError_t  cudaMemcpyToArrayAsync(struct cudaArray *dst, size_t wOffset, size_t hOffset, const void *src, size_t count, enum cudaMemcpyKind kind, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyToArrayAsync",
    cudaError_t , (struct cudaArray *, size_t , size_t , const void *, size_t , enum cudaMemcpyKind , cudaStream_t ),
    NULL, 0);

  CUDA_MEMCPY_ASYNC(kind, count, stream,
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, wOffset, hOffset, src, count, kind, stream));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyFromArrayAsync -- */
cudaError_t  cudaMemcpyFromArrayAsync(void *dst, const struct cudaArray *src, size_t wOffset, size_t hOffset, size_t count, enum cudaMemcpyKind kind, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyFromArrayAsync",
    cudaError_t , (void *, const struct cudaArray *, size_t , size_t , size_t , enum cudaMemcpyKind , cudaStream_t ),
    NULL, 0);

  CUDA_MEMCPY_ASYNC(kind, count, stream,
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, src, wOffset, hOffset, count, kind, stream));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy2DAsync -- */
cudaError_t  cudaMemcpy2DAsync(void *dst, size_t dpitch, const void *src, size_t spitch, size_t width, size_t height, enum cudaMemcpyKind kind, cudaStream_t stream)
{
  cudaError_t  ret;


  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy2DAsync",
    cudaError_t , (void *, size_t , const void *, size_t , size_t , size_t , enum cudaMemcpyKind , cudaStream_t ),
    NULL, 0);

  CUDA_MEMCPY_ASYNC(kind, width*height, stream,
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, dpitch, src, spitch, width, height, kind, stream));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy2DToArrayAsync -- */
cudaError_t  cudaMemcpy2DToArrayAsync(struct cudaArray *dst, size_t wOffset, size_t hOffset, const void *src, size_t spitch, size_t width, size_t height, enum cudaMemcpyKind kind, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy2DToArrayAsync",
    cudaError_t , (struct cudaArray *, size_t , size_t , const void *, size_t , size_t , size_t , enum cudaMemcpyKind , cudaStream_t ),
    NULL, 0);

  CUDA_MEMCPY_ASYNC(kind, width*height, stream,
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, wOffset, hOffset, src, spitch, width, height, kind, stream));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy2DFromArrayAsync -- */
cudaError_t  cudaMemcpy2DFromArrayAsync(void *dst, size_t dpitch, const struct cudaArray *src, size_t wOffset, size_t hOffset, size_t width, size_t height, enum cudaMemcpyKind kind, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy2DFromArrayAsync",
    cudaError_t , (void *, size_t , const struct cudaArray *, size_t , size_t , size_t , size_t , enum cudaMemcpyKind , cudaStream_t ),
    NULL, 0);

  CUDA_MEMCPY_ASYNC(kind, width*height, stream,
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, dpitch, src, wOffset, hOffset, width, height, kind, stream));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyToSymbolAsync -- */
cudaError_t  cudaMemcpyToSymbolAsync(const char *symbol, const void *src, size_t count, size_t offset, enum cudaMemcpyKind kind, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyToSymbolAsync",
    cudaError_t , (const char *, const void *, size_t , size_t , enum cudaMemcpyKind , cudaStream_t ),
    NULL, 0);

  CUDA_MEMCPY_ASYNC(kind, count, stream,
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (symbol, src, count, offset, kind, stream));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyFromSymbolAsync -- */
cudaError_t  cudaMemcpyFromSymbolAsync(void *dst, const char *symbol, size_t count, size_t offset, enum cudaMemcpyKind kind, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyFromSymbolAsync",
    cudaError_t , (void *, const char *, size_t , size_t , enum cudaMemcpyKind , cudaStream_t ),
    NULL, 0);

  CUDA_MEMCPY_ASYNC(kind, count, stream,
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, symbol, count, offset, kind, stream));
  );

  return ret;
}

/* -- cuda_runtime_api.h:cudaConfigureCall -- */
cudaError_t  cudaConfigureCall(dim3 gridDim, dim3 blockDim, size_t sharedMem, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaConfigureCall",
    cudaError_t , (dim3 , dim3 , size_t , cudaStream_t),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (gridDim, blockDim, sharedMem, stream));

  if(vt_cudart_trace_enabled){
    VT_LIBWRAP_FUNC_END(vt_cudart_lw);  /* no extra if(trace_enabled) */

    if(trace_kernels){
        VTCUDADevice* vtDev;
        VTCUDAStrm *ptrStrm;
        uint32_t ptid;

        VT_CHECK_THREAD;
        ptid = VT_MY_THREAD;

        if(vt_is_trace_on(ptid)){
          vtDev = VTCUDAcheckThread(stream, ptid, &ptrStrm);

          /* get kernel configure position */
          vtDev->conf_stack = vtDev->conf_stack - sizeof(VTCUDAknconf);
          
          /* check if there is enough buffer space */
          if(vtDev->buf_pos + sizeof(VTCUDAKernel) > vtDev->conf_stack){
#if defined(VT_CUPTI_EVENTS)
            if(trace_cupti_events){
              size_t new_size = sizeof(VTCUDAKernel) + 16*sizeof(VTCUDAknconf);
              size_t conf_stack_size = 
                   vtDev->buf_size - (vtDev->conf_stack + sizeof(VTCUDAknconf));
              buffer_t new_buf = (buffer_t)realloc(vtDev->asyncbuf, new_size);

              /* copy stacked kernel configurations */
              memcpy(new_buf + new_size - conf_stack_size, 
                     vtDev->conf_stack + sizeof(VTCUDAknconf), 
                     conf_stack_size);
              
              /* set the new buffer values */
              vtDev->asyncbuf = new_buf;
              vtDev->buf_pos = new_buf;
              vtDev->buf_size = vtDev->asyncbuf + new_size;
              vtDev->conf_stack = 
                     vtDev->buf_size - (conf_stack_size + sizeof(VTCUDAknconf));
              
              vt_cntl_msg(2, "[CUDART] Reallocated buffer due to stacked kernel"
                             " calls! New buffer size: %d bytes", new_size);
            }else{
#endif
              VTCUDAflush(vtDev, ptid);
#if defined(VT_CUPTI_EVENTS)
            }
#endif
            if(vtDev->buf_pos + sizeof(VTCUDAKernel) > vtDev->conf_stack){
              vt_error_msg("[CUDART] Not enough buffer space to configure kernel!");
            }
          }

          /* add kernel configure to stack */
          {
            VTCUDAknconf *vtKnconf = (VTCUDAknconf*) vtDev->conf_stack;

            vtKnconf->strm = ptrStrm;
            vtKnconf->blocksPerGrid = gridDim.x * gridDim.y * gridDim.z;
            vtKnconf->threadsPerBlock = blockDim.x * blockDim.y * blockDim.z;
          }
        }
    }
  }

  return ret;
}

/* -- cuda_runtime_api.h:cudaLaunch -- */
cudaError_t  cudaLaunch(const char *entry)
{
  cudaError_t  ret;
  VTCUDADevice *vtDev = NULL;
  VTCUDAKernel *kernel = NULL;
  VTCUDAkernelSymbol* e = NULL;
  uint8_t do_trace = 0;
  uint32_t ptid = 0;
  uint64_t time;

#if defined(VT_CUPTI_EVENTS)
  vt_cuptievt_ctx_t* vtcuptiCtx = NULL;
#endif

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaLaunch",
    cudaError_t , (const char *), NULL, 0);

  if(vt_cudart_trace_enabled){
    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;

    /*do_trace = vt_is_trace_on(ptid);*/

    time = vt_pform_wtime();
    do_trace = vt_enter(ptid, &time, VT_LIBWRAP_FUNC_ID);

    if(trace_kernels && do_trace){

      /* get kernel element */
      e = getKernelByHostFunction(entry);
      if(e != NULL){
        
          /* check if the kernel will be traced on the correct thread */
          vtDev = VTCUDAgetDevice(ptid);

          /* check the kernel configure stack for last configured kernel */
          if(vtDev->conf_stack == vtDev->buf_size){
            ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (entry));
            vt_warning("[CUDART] No kernel configure call found for "
                       "'%s' (device %d, ptid %d)", e->name, vtDev->device, ptid);
            return ret;
          }

          /* get buffer space for kernel */
          kernel = (VTCUDAKernel*) vtDev->buf_pos;
          
          /* set configure information */
          {
            VTCUDAknconf *vtKnconf = (VTCUDAknconf*) vtDev->conf_stack;
            
            kernel->blocksPerGrid = vtKnconf->blocksPerGrid;
            kernel->threadsPerBlock = vtKnconf->threadsPerBlock;
            kernel->strm = vtKnconf->strm;
            
            vtDev->conf_stack = vtDev->conf_stack + sizeof(VTCUDAknconf);
          }

          vt_cntl_msg(3, "[CUDART] Launch '%s' (device %d, tid %d, rid %d, strm %d)",
                        e->name, vtDev->device, vtDev->ptid,
                        e->rid, (uint64_t)kernel->strm->stream);

          kernel->rid = e->rid;

          /* set type of buffer entry */
          kernel->type = VTCUDABUF_ENTRY_TYPE__Kernel;

          /*  get an already created unused event */
          kernel->evt = vtDev->evtbuf_pos;

          if(!trace_cupti_events){
            /* increment buffers */
            vtDev->evtbuf_pos++;
            vtDev->buf_pos += sizeof(VTCUDAKernel);
          }

#if defined(VT_CUPTI_EVENTS)
        
        /* zero CUPTI counter */
        if(trace_cupti_events){
          uint32_t tid = kernel->strm->tid;

          VT_CUDART_CALL(cudaThreadSynchronize_ptr(), NULL);

          /* write VT kernel start events */
          time = vt_pform_wtime();
          if(vt_gpu_trace_idle) vt_exit(vtDev->strmList->tid, &time);
          vt_enter(tid, &time, e->rid);
          vt_count(tid, &time, cid_blocksPerGrid, kernel->blocksPerGrid);
          vt_count(tid, &time, cid_threadsPerBlock, kernel->threadsPerBlock);
          vt_count(tid, &time, cid_threadsPerKernel,
                   kernel->threadsPerBlock * kernel->blocksPerGrid);

          vtcuptiCtx = vt_cuptievt_getCurrentContext(ptid);
          vt_cuptievt_resetCounter(vtcuptiCtx, tid, &time);
        }else
#endif
            VT_CUDART_CALL(cudaEventRecord_ptr(kernel->evt->strt, kernel->strm->stream),
                          "cudaEventRecord(startEvt, strmOfLastKernel) failed!");
      }else{ /* e != NULL */
        /* kernel is filtered -> correct configure stack */
        vtDev = VTCUDAgetDevice(ptid);
        vtDev->conf_stack = vtDev->conf_stack + sizeof(VTCUDAknconf);
      } /* e != NULL */
    } /* trace_kernels && do_trace */
  }

  /* call cudaLaunch itself */
  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (entry));

  if(vt_cudart_trace_enabled){
    time = vt_pform_wtime();
    vt_exit(ptid, &time);

    if(do_trace && e != NULL && trace_kernels){
      REGISTER_FINALIZE;

#if defined(VT_CUPTI_EVENTS)
      /* synchronize after kernel launch to get CUPTI counter values */
      if(trace_cupti_events){
        cudaError_t ret;
        uint32_t tid = kernel->strm->tid;

        vt_enter(ptid, &time, rid_sync);

        if(cupti_event_sampling){
          /* sampling of CUPTI counter values */
          do{
            time = vt_pform_wtime();
            vt_cuptievt_writeCounter(vtcuptiCtx, tid, &time);
            /*ret = cudaEventQuery_ptr(kernel->evt->stop);*/
            ret = cudaStreamQuery_ptr(kernel->strm->stream);
          }while(ret != cudaSuccess);
        }else{
          /*ret = cudaEventSynchronize_ptr(kernel->evt->stop);*/
          VT_CUDART_CALL(cudaThreadSynchronize_ptr(), NULL);
        }

        time = vt_pform_wtime();
        vt_cuptievt_writeCounter(vtcuptiCtx, tid, &time);
        vt_exit(ptid, &time);

        /* write VT kernel stop events */
        vt_count(tid, &time, cid_blocksPerGrid, 0);
        vt_count(tid, &time, cid_threadsPerBlock, 0);
        vt_count(tid, &time, cid_threadsPerKernel, 0);
        vt_exit(tid, &time);
        if(vt_gpu_trace_idle) 
          vt_enter(vtDev->strmList->tid, &time, vt_gpu_rid_idle);
      }else
#endif
          VT_CUDART_CALL(cudaEventRecord_ptr(kernel->evt->stop, kernel->strm->stream),
                        "cudaEventRecord(stopEvt, streamOfCurrentKernel) failed!");
    } /* do_trace && e != NULL && trace_kernels */
  }

  return ret;
}

/* -- cuda_runtime_api.h:cudaThreadExit -- */

cudaError_t  cudaThreadExit()
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaThreadExit",
    cudaError_t , (void), NULL, 0);

  if(vt_cudart_trace_enabled){
    uint32_t ptid;
    VTCUDADevice *vtDev;

    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;

    vtDev = VTCUDAgetDevice(ptid);

    vt_cntl_msg(2, "cudaThreadExit called (thread; %d)", ptid);
    /* cleanup the CUDA device associated to this thread */
    CUDARTWRAP_LOCK();
      VTCUDAcleanupDevice(ptid, vtDev, 1);
    CUDARTWRAP_UNLOCK();
    VT_LIBWRAP_FUNC_START(vt_cudart_lw); /* no extra if(trace_enabled) */
  }

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, ());

  if(vt_cudart_trace_enabled) VT_LIBWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaThreadSynchronize -- */

cudaError_t  cudaThreadSynchronize()
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaThreadSynchronize",
    cudaError_t , (void),
    NULL, 0);

  if(vt_cudart_trace_enabled){
    if(syncLevel > 2){
      VTCUDADevice *node = NULL;
      uint32_t ptid;

      VT_CHECK_THREAD;
      ptid = VT_MY_THREAD;
      node = VTCUDAgetDevice(ptid);
      VTCUDAflush(node, ptid);
    }
    VT_LIBWRAP_FUNC_START(vt_cudart_lw); /* no extra if(trace_enabled) */
  }

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, ());

  if(vt_cudart_trace_enabled) VT_LIBWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* CUDA 3.1 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 3010))

/* -- cuda_runtime_api.h:cudaMallocArray -- */
cudaError_t  cudaMallocArray(struct cudaArray **array, const struct cudaChannelFormatDesc *desc, size_t width, size_t height, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMallocArray",
    cudaError_t , (struct cudaArray **, const struct cudaChannelFormatDesc *, size_t , size_t , unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (array, desc, width, height, flags));

  if(vt_cudart_trace_enabled){
    VT_LIBWRAP_FUNC_END(vt_cudart_lw);
    if(trace_gpumem > 0) vtcudaMalloc(*array, width * height);
  }

  return ret;
}

/* -- cuda_runtime_api.h:cudaMalloc3DArray -- */
cudaError_t  cudaMalloc3DArray(struct cudaArray **arrayPtr, const struct cudaChannelFormatDesc *desc, struct cudaExtent extent, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMalloc3DArray",
    cudaError_t , (struct cudaArray **, const struct cudaChannelFormatDesc *, struct cudaExtent , unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (arrayPtr, desc, extent, flags));

  if(vt_cudart_trace_enabled){
    VT_LIBWRAP_FUNC_END(vt_cudart_lw);
    if(trace_gpumem > 0){
      vtcudaMalloc(*arrayPtr, extent.width * extent.height * extent.depth);
    }
  }

  return ret;
}

#endif /* CUDA 3.1 */


/*
 *  Adaptions for CUDA 4.0
 */

#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 4000))

/* -- cuda_runtime_api.h:cudaDeviceReset -- */
cudaError_t  cudaDeviceReset()
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceReset",
    cudaError_t , (void), NULL, 0);

  if(vt_cudart_trace_enabled){
    uint32_t ptid;
    VTCUDADevice *vtDev;

    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;

    vtDev = VTCUDAgetDevice(ptid);

    vt_cntl_msg(2, "cudaDeviceReset called (thread; %d)", ptid);
    /* cleanup the CUDA device associated to this thread */
    CUDARTWRAP_LOCK();
      VTCUDAcleanupDevice(ptid, vtDev, 1);
    CUDARTWRAP_UNLOCK();
    VT_LIBWRAP_FUNC_START(vt_cudart_lw); /* no extra if(trace_enabled) */
  }

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, ());

  if(vt_cudart_trace_enabled) VT_LIBWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDeviceSynchronize -- */
cudaError_t  cudaDeviceSynchronize()
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceSynchronize",
    cudaError_t , (void), NULL, 0);

  if(vt_cudart_trace_enabled){
    if(syncLevel > 2){
      VTCUDADevice *node = NULL;
      uint32_t ptid;

      VT_CHECK_THREAD;
      ptid = VT_MY_THREAD;
      node = VTCUDAgetDevice(ptid);
      VTCUDAflush(node, ptid);
    }
    VT_LIBWRAP_FUNC_START(vt_cudart_lw); /* no extra if(trace_enabled) */
  }

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, ());

  if(vt_cudart_trace_enabled) VT_LIBWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpyPeer -- */
cudaError_t  cudaMemcpyPeer(void *dst, int dstDevice, const void *src, int srcDevice, size_t count)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyPeer",
    cudaError_t , (void *, int , const void *, int , size_t ), NULL, 0);
  
  if(vt_cudart_trace_enabled){
    uint32_t ptid;
    
    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;
    
    if(vt_is_trace_on(ptid)){
      VT_CUDART_MEMCPY_PEER2PEER(ptid, srcDevice, dstDevice, count, 
        ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, dstDevice, src, srcDevice, count));
     )
    }else{
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, dstDevice, src, srcDevice, count));
    }
  }else{
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, dstDevice, src, srcDevice, count));
  }
          
  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy3DPeer -- */
cudaError_t  cudaMemcpy3DPeer(const struct cudaMemcpy3DPeerParms *p)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy3DPeer",
    cudaError_t , (const struct cudaMemcpy3DPeerParms *), NULL, 0);
  
  if(vt_cudart_trace_enabled){
    struct cudaExtent extent = p->extent;
    size_t count = extent.height * extent.width * extent.depth;
    uint32_t ptid;
    
    VT_CHECK_THREAD;
    ptid = VT_MY_THREAD;
    
    if(vt_is_trace_on(ptid)){
      VT_CUDART_MEMCPY_PEER2PEER(ptid, p->srcDevice, p->dstDevice, count, 
        ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (p));
     )
    }else{
      ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (p));
    }
  }else{
    ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (p));
  }

  return ret;
}

/* TODO: has to be implemented as communication */
/* -- cuda_runtime_api.h:cudaMemcpyPeerAsync -- */
cudaError_t  cudaMemcpyPeerAsync(void *dst, int dstDevice, const void *src, int srcDevice, size_t count, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpyPeerAsync",
    cudaError_t , (void *, int , const void *, int , size_t , cudaStream_t ),
    NULL, 0);
  
  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (dst, dstDevice, src, srcDevice, count, stream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);
  
  return ret;
}

/* -- cuda_runtime_api.h:cudaMemcpy3DPeerAsync -- */
cudaError_t  cudaMemcpy3DPeerAsync(const struct cudaMemcpy3DPeerParms *p, cudaStream_t stream)
{
  cudaError_t  ret;
  /*struct cudaExtent extent = p->extent;
  size_t count = extent.height * extent.width * extent.depth;*/

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy3DPeerAsync",
    cudaError_t , (const struct cudaMemcpy3DPeerParms *, cudaStream_t ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (p, stream));
  
  CUDARTWRAP_FUNC_END(vt_cudart_lw);
  
  return ret;
}

#endif /* CUDA 4.0 */

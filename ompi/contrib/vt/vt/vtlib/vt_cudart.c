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
#include "vt_libwrap.h"     /* wrapping of CUDA Runtime API functions */
#include "vt_cudartwrap.h"  /* CUDA wrapper functions for external use */
#include "vt_gpu.h"         /* common for GPU */
#include "vt_memhook.h"     /* Switch memory tracing on/off */

/* CUDA counter support */
#include "vt_cudacupti.h"

#include <stdio.h>
#include <string.h>

/* mutexes for locking the CUDA runtime wrap environment */
#if (defined(VT_MT) || defined(VT_HYB))
VTThrdMutex* VTThrdMutexCudart = NULL;
#endif /* VT_MT || VT_HYB */

/*
 * Register the finalize function of the CUDA wrapper to be called before
 * the program exits and CUDA has done its implizit clean-up.
 * A CUDA function (context creating???) has to be called before, as
 * VampirTrace CUDA wrapper has to finalize before CUDA does its clean-up!!!
 */
#define REGISTER_FINALIZE                              \
  if(!finalize_registered){                            \
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
#define CUDA_SEND_RECV(_kind, _bytes, _call){                                  \
  uint64_t time = 0;                                                           \
  uint8_t do_trace = 0; /* is trace on */                                      \
  uint8_t do_traceE = 0; /* is call limit reached */                           \
  VTCUDADevice* vtDev = NULL;                                                  \
  uint32_t strmID = 0;                                                         \
  uint32_t ptid = 0;                                                           \
  if(vt_cudart_trace_enabled){                                                 \
    VTCUDAStrm *strm;                                                          \
    VT_CHECK_THREAD;                                                           \
    ptid = VT_MY_THREAD;                                                       \
    do_trace = vt_is_trace_on(ptid);                                           \
    if(do_trace){                                                              \
      vtDev = VTCUDAcheckThread(0, ptid, &strm);                               \
      strmID = strm->tid;                                                      \
      if(_kind != cudaMemcpyHostToHost){                                       \
        if(syncLevel > 2) VTCUDAflush(vtDev, ptid);                            \
        else if(syncLevel > 0){                                                \
          time = vt_pform_wtime();                                             \
          if(syncLevel > 1) vt_enter(ptid, &time, rid_sync);                   \
          checkCUDACall(cudaThreadSynchronize_ptr(), "vtcudaSync() failed!");  \
          if(syncLevel > 1){time = vt_pform_wtime(); vt_exit(ptid, &time);}    \
        }                                                                      \
        CUDARTWRAP_LOCK();                                                     \
          if(_kind != cudaMemcpyDeviceToDevice)                                \
            vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;                               \
          vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;                               \
        CUDARTWRAP_UNLOCK();                                                   \
      }                                                                        \
      if(syncLevel == 1 && time != 0){ /* no hostTohost and sync==1 */         \
        do_traceE = vt_enter(ptid, &time, VT_LIBWRAP_FUNC_ID);                 \
        time = vt_pform_wtime();                                               \
      }else{                                                                   \
        time = vt_pform_wtime();                                               \
        do_traceE = vt_enter(ptid, &time, VT_LIBWRAP_FUNC_ID);                 \
      }                                                                        \
      if(do_traceE){                                                           \
        if(_kind == cudaMemcpyHostToDevice){                                   \
          vt_mpi_rma_put(ptid, &time, strmID * 65536 + vt_my_trace,            \
                         vt_gpu_commCID, 0, _bytes);                           \
        }else if(_kind == cudaMemcpyDeviceToHost){                             \
          vt_mpi_rma_get(ptid, &time, strmID * 65536 + vt_my_trace,            \
                         vt_gpu_commCID, 0, _bytes);                           \
        }else if(_kind == cudaMemcpyDeviceToDevice && syncLevel > 2){          \
          vt_mpi_rma_get(strmID, &time, strmID * 65536 + vt_my_trace,          \
                         vt_gpu_commCID, 0, _bytes);                           \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  }                                                                            \
  _call  /* the CUDA memcpy call itself */                                     \
  if(vt_cudart_trace_enabled && do_trace){                                     \
    time = vt_pform_wtime();                                                   \
    if(do_traceE){                                                             \
      if(_kind == cudaMemcpyDeviceToDevice && syncLevel > 2){                  \
        vt_mpi_rma_end(strmID, &time, vt_gpu_commCID, 0);                      \
      }else if(_kind != cudaMemcpyHostToHost){                                 \
        vt_mpi_rma_end(ptid, &time, vt_gpu_commCID, 0);                        \
      }                                                                        \
    }                                                                          \
    if(syncLevel > 2) vtDev->sync.lastTime = time;                             \
    REGISTER_FINALIZE;                                                         \
    vt_exit(ptid, &time);                                                      \
  }                                                                            \
}

#if (defined(VT_CUDACUPTI))
#define CUDA_MEMCPY_ASYNC(kind, bytes, stream, _call) \
  if(trace_cupti) CUDA_MEMCPY_ASYNC_CUPTI(kind, bytes, stream, _call) \
  else CUDA_MEMCPY_ASYNC_EVT(kind, bytes, stream, _call)
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
#define CUDA_MEMCPY_ASYNC_EVT(kind, bytes, stream, _call){                     \
  VTCUDAMemcpy *mcpy = NULL;                                                   \
  if(vt_cudart_trace_enabled){                                                 \
    if((kind != cudaMemcpyHostToHost) && trace_memcpyAsync){                   \
      mcpy = addMemcpy2Buf(kind, bytes, stream);                               \
      /*vt_cntl_msg(1,"[CUDART] cudaMemcpyAsync on stream %llu ", (uin64_t)stream);*/\
      if(mcpy != NULL)                                                         \
        checkCUDACall(cudaEventRecord_ptr(mcpy->evt->strt, stream), NULL);     \
    }                                                                          \
    VT_LIBWRAP_FUNC_START(vt_cudart_lw);                                       \
  }                                                                            \
  _call  /* the CUDA memCpy call itself */                                     \
  if(vt_cudart_trace_enabled){                                                 \
    VT_LIBWRAP_FUNC_END(vt_cudart_lw);                                         \
    if((kind != cudaMemcpyHostToHost) && trace_memcpyAsync && mcpy != NULL){   \
      checkCUDACall(cudaEventRecord_ptr(mcpy->evt->stop, stream), NULL);       \
    }                                                                          \
    REGISTER_FINALIZE;                                                         \
  }                                                                            \
}

#define CUDA_MEMCPY_ASYNC_CUPTI(_kind, _bytes, _stream, _call){                 \
  uint8_t do_trace = 0; /* is trace on */                                      \
  uint64_t time = 0;                                                           \
  uint32_t ptid = 0;                                                           \
  uint32_t strmID = 0;                                                         \
  if(vt_cudart_trace_enabled){                                                 \
    VTCUDAStrm *strm;                                                          \
    VT_CHECK_THREAD;                                                           \
    ptid = VT_MY_THREAD;                                                       \
    (void)VTCUDAcheckThread(_stream, ptid, &strm);                             \
    strmID = strm->tid;                                                        \
    time = vt_pform_wtime();                                                   \
    do_trace = vt_enter(ptid, &time, VT_LIBWRAP_FUNC_ID);                      \
    if(do_trace){                                                              \
      if(syncLevel > 1) vt_enter(ptid, &time, rid_sync);                       \
      checkCUDACall(cudaDeviceSynchronize_ptr(),"vtcudaSync() failed!");       \
      if(syncLevel > 1){time = vt_pform_wtime(); vt_exit(ptid, &time);}        \
      if(_kind == cudaMemcpyHostToDevice){                                     \
        vt_mpi_rma_put(ptid, &time, strmID * 65536 + vt_my_trace,              \
                       vt_gpu_commCID, 0, _bytes);                             \
      }else if(_kind == cudaMemcpyDeviceToHost){                               \
        vt_mpi_rma_get(ptid, &time, strmID * 65536 + vt_my_trace,              \
                       vt_gpu_commCID, 0, _bytes);                             \
      }else if(_kind == cudaMemcpyDeviceToDevice && syncLevel > 2){            \
        vt_mpi_rma_get(strmID, &time, strmID * 65536 + vt_my_trace,            \
                       vt_gpu_commCID, 0, _bytes);                             \
      }                                                                        \
    }                                                                          \
  }                                                                            \
  _call  /* the CUDA memCpy call itself */                                     \
  if(vt_cudart_trace_enabled){                                                 \
    checkCUDACall(cudaDeviceSynchronize_ptr(),"vtcudaSync() failed!");         \
    time = vt_pform_wtime();                                                   \
    if(do_trace){                                                              \
      if(_kind == cudaMemcpyDeviceToDevice){                                   \
        vt_mpi_rma_end(strmID, &time, vt_gpu_commCID, 0);                      \
        CUDARTWRAP_LOCK();                                                     \
        vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;                                 \
        CUDARTWRAP_UNLOCK();                                                   \
      }else if(_kind != cudaMemcpyHostToHost){                                 \
        vt_mpi_rma_end(ptid, &time, vt_gpu_commCID, 0);                        \
        CUDARTWRAP_LOCK();                                                     \
        vt_gpu_prop[ptid] |= VTGPU_GPU_COMM;                                   \
        vt_gpu_prop[strmID] |= VTGPU_GPU_COMM;                                 \
        CUDARTWRAP_UNLOCK();                                                   \
      }                                                                        \
    }                                                                          \
    vt_exit(ptid, &time);                                                      \
    REGISTER_FINALIZE;                                                         \
  }                                                                            \
}

#define checkCUDACall(ecode, msg) __checkCUDACall(ecode, msg, __FILE__,__LINE__)

/* minimum size of an asynchronous task (in bytes) */
#define MIN_ASYNC_ENTRY sizeof(VTCUDAMemcpy)

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

/* flag: write GPU idle time as region in CUDA stream 0? */
static uint8_t show_gpu_idle = 0;

/* flag: synchronization and flush points during runtime enabled? */
static uint8_t syncLevel = 3;

/* flag: tracing of kernels enabled? */
static uint8_t trace_kernels = 1;

/* region filter for kernel filtering */
static RFG_Filter* vt_cudart_filter = NULL;

/* flag: abort program on CUDA error, if enabled */
static uint8_t vt_cudart_error = 0;

/* flag: tracing of asynchronous memory copies enabled? */
static uint8_t trace_memcpyAsync = 1;

/* flag: tracing of cudaMalloc*() and cudaFree*() enabled? */
static uint8_t trace_gpumem = 0;

/* flag: trace nvidia cupti counters */
static uint8_t trace_cupti = 0;

/* flag: sampling for CUPTI counter values enabled? */
static uint8_t cupti_sampling = 0;

/* flag: event based tracing (kernels, memcpyAsync) enabled? */
static uint8_t trace_events = 1;

/* number of bytes used to buffer asynchronous tasks */
static size_t asyncBufSize = VTGPU_MAX_BSIZE;

/* flag: CUDA wrapper already finalized? */
static uint8_t finalized = 0;

/* flag: has CUDA Runtime API been used */
uint8_t vt_cudartwrap_used = 0;

/* global region IDs for wrapper internal tracing */
static uint32_t rid_check, rid_create, rid_sync, rid_flush;
static uint32_t rid_idle = VT_NO_ID;

/* global counter IDs */
static uint32_t cid_blocksPerGrid; /* number of blocks per grid */
static uint32_t cid_threadsPerBlock; /* number of threads per block */
static uint32_t cid_threadsPerKernel; /* number of threads per kernel */
static uint32_t cid_cudaMalloc; /* CUDA GPU memory allocation counter */

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
  VTCUDAmalloc *mallocList;  /**< list of not yet freed cudaMalloc* calls */
  size_t mallocated;         /**< memory allocated on CUDA device */
  VTCUDAsync sync;           /**< synchronization time and events */
  buffer_t asyncbuf;         /**< points to the first byte in buffer */
  buffer_t buf_pos;          /**< current buffer position */
  buffer_t buf_size;         /**< buffer size (in bytes) */
  uint8_t kn_conf;           /**< flag: kernel configured? */
  VTCUDABufEvt *evtbuf;      /**< the preallocated cuda event list */
  VTCUDABufEvt *evtbuf_pos;  /**< current unused event space */
  struct vtcudaDev_st *next; /**< pointer to next element in list */
}VTCUDADevice;

/* list of CUDA devices */
static VTCUDADevice* cudaDevices = NULL;

/* maximum events needed for task buffer size */
static size_t maxEvtNum = VTGPU_MAX_BSIZE / sizeof(VTCUDAKernel);

/* pointer to cuda functions which should not be traced */
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

/*
 * CUDA wrapper function declarations
 */
static void VTCUDAflush(VTCUDADevice*, uint32_t);
static VTCUDADevice* VTCUDAgetDevice(uint32_t ptid);

/* Checks if a CUDA runtime API call returns successful and respectively prints
 * the error.
 * @param ecode the CUDA error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
static void __checkCUDACall(cudaError_t ecode, const char* msg,
                            const char *file, const int line)
{
  if(cudaSuccess != ecode){
    if(msg != NULL) vt_cntl_msg(1, msg);
    if(vt_cudart_error)
      vt_error_msg("[CUDA Error <%s>:%i] %s", file, line,
                   cudaGetErrorString_ptr(ecode));
    else
      vt_warning("[CUDA <%s>:%i] %s", file, line,
                 cudaGetErrorString_ptr(ecode));
  }
  /*vt_cntl_msg(1, "[<%s>:%i] '%s'",
                 cudaGetErrorString_ptr(cudaGetLastError_ptr()));*/
}

/*
 * initializer function for library wrapper attributes
   (called from first triggered wrapper event)
 */
void vt_cudartwrap_lw_attr_init(VTLibwrapAttr* attr)
{
  /* initialize library wrapper attributes */
  attr->shlibs_num = 0;
#ifdef DEFAULT_CUDARTLIB_PATHNAME
  attr->shlibs_num = 1;
  attr->shlibs[0] = DEFAULT_CUDARTLIB_PATHNAME;
#endif /* DEFAULT_CUDARTLIB_PATHNAME */
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

  if(vt_cudart_trace_enabled){
    size_t minTaskSize = sizeof(VTCUDAKernel) + sizeof(VTCUDAMemcpy);

    syncLevel = (uint8_t)vt_env_cudatrace_sync();
    trace_kernels = (uint8_t)vt_env_cudatrace_kernel();
    trace_memcpyAsync = (uint8_t)vt_env_cudatrace_memcpyasync();
    
    vt_cudart_error = (uint8_t)vt_env_cudatrace_error();

#if (defined(VT_CUDACUPTI))
    if(vt_env_cupti_metrics() == NULL){
      trace_cupti = 0;
    }else{
      trace_cupti = 1;
      cupti_sampling = (uint8_t)vt_env_cupti_sampling();
    }
#endif

    trace_events = 0;
    if(trace_kernels){
      minTaskSize = sizeof(VTCUDAKernel);
      trace_events = 1;
    }

    if(trace_memcpyAsync){
      if(sizeof(VTCUDAMemcpy) < minTaskSize) minTaskSize = sizeof(VTCUDAMemcpy);
      trace_events = 1;
    }

    /* if events are used */
    if(trace_events){
      /* get user-defined task buffer size and check it */
      asyncBufSize = vt_env_cudatrace_bsize();
      if(asyncBufSize < MIN_ASYNC_ENTRY){
        if(asyncBufSize > 0){
          vt_warning("[CUDART] Minimal buffer size is %d bytes", MIN_ASYNC_ENTRY);
        }
        asyncBufSize = VTGPU_DEFAULT_BSIZE;
      }else if(VTGPU_MAX_BSIZE < asyncBufSize){
        vt_warning("[CUDART] Current CUDA buffer size requires %d CUDA events.\n"
                   "The recommended max. CUDA buffer size is %d. "
                   "(export VT_CUDA_BUFFER_SIZE=2097152)",
                   2*asyncBufSize/sizeof(VTCUDAKernel), VTGPU_MAX_BSIZE);
        /* TODO: dynamic event creation for more than 2097152 bytes cuda buffer size */
      }

      /* determine maximum necessary VT-events (=2 CUDA events) */
      maxEvtNum = asyncBufSize / minTaskSize;

#if (defined(VT_CUDACUPTI))
      if(trace_cupti){
        maxEvtNum = 0;
        trace_events = 0;
        /*asyncBufSize = (sizeof(VTCUDAKernel) > sizeof(VTCUDAMemcpy)) ? sizeof(VTCUDAKernel) : sizeof(VTCUDAMemcpy);*/
        asyncBufSize = sizeof(VTCUDAKernel);
      }
#endif

      vt_cntl_msg(2,"[CUDART] Current CUDA buffer size: %d bytes \n"
                    "(Kernel: %d bytes, MemcpyAsync: %d bytes, "
                    "Pre-created events: %d)", asyncBufSize,
                    sizeof(VTCUDAKernel), sizeof(VTCUDAMemcpy), maxEvtNum);

    }

    show_gpu_idle = (uint8_t)vt_env_cudatrace_idle() & trace_kernels;
    trace_gpumem = (uint8_t)vt_env_cudatrace_gpumem();
    
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
# if (defined(CUDA_VERSION) && (CUDA_VERSION < 4000))
      VTLibwrap_func_init(vt_cudart_lw, "cudaThreadSynchronize", NULL, 0,
                          (void**)(&cudaThreadSynchronize_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaThreadSynchronize", NULL, 0,
                          (void**)(&cudaDeviceSynchronize_ptr), &func_id);
# else
      VTLibwrap_func_init(vt_cudart_lw, "cudaDeviceSynchronize", NULL, 0,
                          (void**)(&cudaDeviceSynchronize_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaDeviceSynchronize", NULL, 0,
                          (void**)(&cudaThreadSynchronize_ptr), &func_id);
# endif
      VTLibwrap_func_init(vt_cudart_lw, "cudaGetErrorString", NULL, 0,
                          (void**)(&cudaGetErrorString_ptr), &func_id);
      VTLibwrap_func_init(vt_cudart_lw, "cudaGetLastError", NULL, 0,
                          (void**)(&cudaGetLastError_ptr), &func_id);
    }

#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_LOCK_IDS();
#endif

    vt_gpu_init(); /* initialize GPU common stuff */

    /* get region IDs for this CUDA Runtime API wrapper (internal tracing) */
    if(show_gpu_idle){
      rid_idle = vt_def_region(VT_MASTER_THREAD, "gpu_idle", VT_NO_ID,
                               VT_NO_LNO, VT_NO_LNO, "CUDA_IDLE", VT_FUNCTION);
    }
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

      cid_blocksPerGrid = vt_def_counter(VT_MASTER_THREAD, "blocks_per_grid",
                    VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, cgid_kn, "");
      cid_threadsPerBlock = vt_def_counter(VT_MASTER_THREAD, "threads_per_block",
                    VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, cgid_kn, "");
      cid_threadsPerKernel = vt_def_counter(VT_MASTER_THREAD, "threads_per_kernel",
                    VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED, cgid_kn, "");
    }

    if(trace_gpumem){
      cid_cudaMalloc = vt_def_counter(VT_MASTER_THREAD, "gpu_mem_usage",
                        VT_CNTR_ABS | VT_CNTR_NEXT | VT_CNTR_UNSIGNED,
                        vt_def_counter_group(VT_MASTER_THREAD, "CUDA_MEMORY_USAGE"),
                        "byte");
    }

#if (defined(VT_MT) || defined(VT_HYB))
    VTTHRD_UNLOCK_IDS();
#endif

    /*
     * Register the finalize function of the CUDA wrapper to be called before
     * the program exits and CUDA has done its implicit clean-up.
     * A CUDA function (any) has to be called before, as VampirTrace CUDA
     * wrapper has to finalize before CUDA does its clean-up!!!
     */
    atexit(vt_cudartwrap_finalize);

    /* show CUDA Runtime API, that CUDA Driver API is used as well */
    vt_cudartwrap_used = 1;
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

#if (defined(VT_CUDACUPTI))
  if(trace_cupti && cleanEvents && vt_gpu_debug == 0){
    uint64_t time = vt_pform_wtime();
    VTCUDAStrm *curStrm = vtDev->strmList;
    vt_cupti_ctx_t* vtcuptiCtx = vt_cupti_getCurrentContext(ptid);

    while(curStrm != NULL){
      vt_cupti_resetCounter(vtcuptiCtx, curStrm->tid, &time);
      curStrm = curStrm->next;
    }

    vt_cupti_finalize_device(ptid, cleanEvents);
  }
#endif

  /* write idle end time to CUDA stream 0 */
  if(show_gpu_idle == 1){
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
        checkCUDACall(cudaGetLastError_ptr(), "Error check before event destroy");
        for(k = 0; k < maxEvtNum; k++){
          cudaEventDestroy_ptr((vtDev->evtbuf[k]).strt);
          ret = cudaEventDestroy_ptr((vtDev->evtbuf[k]).stop);
        }
        checkCUDACall(ret, "cudaEventDestroy failed");
      }

      /* free the event buffer */
      free(vtDev->evtbuf);
      vtDev->evtbuf = NULL;
      vtDev->evtbuf_pos = NULL;
    }

    /* destroy synchronization events */
    if(cleanEvents){
      checkCUDACall(cudaEventDestroy_ptr(vtDev->sync.strtEvt),
                    "cudaEventDestroy(syncStrtEvt) failed!");
      checkCUDACall(cudaEventDestroy_ptr(vtDev->sync.stopEvt),
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
    vt_cntl_msg(1, "[CUDART] cudaFree* of %d bytes missing!", tmpM->size);
    vtDev->mallocList = tmpM->next;
    free(tmpM);
    tmpM = NULL;
  }

  /* free malloc of VTCUDADevice, set pointer to this VT device NULL */
  VTCUDAremoveDevice(vtDev);
}

/*
 * Cleanup the current CUDA device thread.
 *
 * @param ptid the VampirTrace process/thread id
 */
void vt_cudartwrap_cleanThread(uint32_t ptid)
{
  VTCUDADevice *vtDev;

  vtDev = VTCUDAgetDevice(ptid);
  CUDARTWRAP_LOCK();
    VTCUDAcleanupDevice(ptid, vtDev, 1);
  CUDARTWRAP_UNLOCK();
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

          checkCUDACall(cudaGetDevice_ptr(&device), "cudaGetDevice(device) failed!");
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

#if (defined(VT_CUDACUPTI))
        if(trace_cupti) vt_cupti_finalize();
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
       see NVIDIA CUDA Programming Guide 2.3, sections 3.2.6.1 and 3.2.6.2
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
        return (uint64_t)-1;
      }else{
        vt_error_msg("[CUDA Error <%s>:%i] %s",  __FILE__,__LINE__,
                     cudaGetErrorString_ptr(ret));
      }
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
  checkCUDACall(cudaEventElapsedTime_ptr(&diff_flush_ms, sync->strtEvt,
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
        checkCUDACall(cudaEventElapsedTime_ptr(&diff_ms, strm->lastEvt, bufEntry->evt->strt),
                      "cudaEventElapsedTime(diff, tmpStrtEvt, knStrtEvt) failed!");

        /* convert CUDA kernel start event to VampirTrace timestamp */
        strttime = strm->lastVTTime + (uint64_t)((double)diff_ms * factorX);

        /* check if kernel start time is before last synchronous CUDA call */
        if(strttime < sync->lastTime){
          strttime = sync->lastTime;
          vt_warning("[CUDART] event before last synchronous CUDA call measured!");
        }

        /* time between kernel start event and kernel stop event */
        checkCUDACall(cudaEventElapsedTime_ptr(&diff_ms, bufEntry->evt->strt, bufEntry->evt->stop),
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
        serialKernelTime = stoptime;

        /* write VampirTrace events to CUDA threads */
        /* gpu idle time will be written to first cuda stream in list */
        if(show_gpu_idle) vt_exit(vtDev->strmList->tid, &strttime);
        vt_enter(tid, &strttime, kn->rid);
        vt_count(tid, &strttime, cid_blocksPerGrid, kn->blocksPerGrid);
        vt_count(tid, &strttime, cid_threadsPerBlock, kn->threadsPerBlock);
        vt_count(tid, &strttime, cid_threadsPerKernel,
                 kn->threadsPerBlock * kn->blocksPerGrid);
        vt_count(tid, &stoptime, cid_blocksPerGrid, 0);
        vt_count(tid, &stoptime, cid_threadsPerBlock, 0);
        vt_count(tid, &stoptime, cid_threadsPerKernel, 0);
        vt_exit(tid, &stoptime);
        if(show_gpu_idle) vt_enter(vtDev->strmList->tid, &stoptime, rid_idle);

        /* go to next entry in buffer */
        entry += sizeof(VTCUDAKernel);
      }else if(bufEntry->type == VTCUDABUF_ENTRY_TYPE__Memcpy){
        /* write communication (in: mcpy, strttime, stoptime) */
        VTCUDAMemcpy *mcpy = (VTCUDAMemcpy*)entry;

        if(mcpy->kind == cudaMemcpyHostToDevice){
          vt_mpi_rma_get(tid, &strttime, mcpy->pid * 65536 + vt_my_trace,
                          vt_gpu_commCID, 0, mcpy->byteCount);
        }else if(mcpy->kind == cudaMemcpyDeviceToHost){
          vt_mpi_rma_put(tid, &strttime, mcpy->pid * 65536 + vt_my_trace,
                          vt_gpu_commCID, 0, mcpy->byteCount);
        }else if(mcpy->kind == cudaMemcpyDeviceToDevice){
          vt_mpi_rma_get(tid, &strttime, tid * 65536 + vt_my_trace,
                          vt_gpu_commCID, 0, mcpy->byteCount);
       }

        vt_mpi_rma_end(tid, &stoptime, vt_gpu_commCID, 0);

        /* go to next entry in buffer */
        entry += sizeof(VTCUDAMemcpy);
      }

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
static VTCUDAStrm* VTCUDAcreateStream(int device, cudaStream_t stream,
                                      uint32_t ptid)
{
  uint32_t gpu_tid = 0;
  char thread_name[16];
  VTCUDAStrm *nstrm;

  /* allocate memory for stream */
  nstrm = (VTCUDAStrm*) malloc(sizeof (VTCUDAStrm));
  if(nstrm == NULL) vt_error_msg("malloc(sizeof(VTCUDAStrm)) failed!");

  nstrm->stream = stream;
  /*nstrm->lastEvt = -1;*/
  nstrm->lastVTTime = 0;
  nstrm->next = NULL;

  /* create VT-User-Thread with name and parent id and get its id */
  /*if(-1 == snprintf(thread_name, 15, "CUDA[%d:%d]", device, (int)stream))*/
  if(-1 == snprintf(thread_name, 15, "CUDA[%d]", device))
    vt_cntl_msg(1, "Could not create thread name for CUDA thread!");
  vt_gpu_registerThread(thread_name, ptid, &gpu_tid);
  nstrm->tid = gpu_tid;

  /* set the threads property to GPU */
  CUDARTWRAP_LOCK();
    vt_gpu_prop[gpu_tid] = VTGPU_GPU;
  CUDARTWRAP_UNLOCK();

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
  vtDev->kn_conf = 0;
  vtDev->evtbuf = NULL;
  vtDev->evtbuf_pos = NULL;
  vtDev->strmList = NULL;
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
    /* --- set VampirTrace - CUDA time synchronization --- */
    checkCUDACall(cudaEventCreate_ptr(&(vtDev->sync.strtEvt)),
                  "cudaEventCreate(syncStrtEvt) failed!");

    checkCUDACall(cudaEventCreate_ptr(&(vtDev->sync.stopEvt)),
                  "cudaEventCreate(syncStopEvt) failed!");

    /* record init event for later synchronization with VampirTrace time */
    vtDev->sync.strtTime = VTCUDAsynchronizeEvt(vtDev->sync.strtEvt);

    /* set initial memory copy timestamp, if no memory copies are done */
    vtDev->sync.lastTime = vtDev->sync.strtTime;

    /* allocate buffers for asynchronous entries */
    vtDev->asyncbuf = malloc(asyncBufSize);
    if(vtDev->asyncbuf == NULL){
      vt_error_msg("malloc of asynchronous CUDA call buffer failed! "
                  "Reduce buffer size with VT_BUFFER_SIZE!");
    }
    vtDev->buf_pos = vtDev->asyncbuf;
    vtDev->buf_size = vtDev->asyncbuf + asyncBufSize;

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
      checkCUDACall(ret, "cudaEventCreate failed");
    }
  }

#if (defined(VT_CUDACUPTI))
  if(trace_cupti){
    vtDev->asyncbuf = malloc(asyncBufSize);
    vtDev->buf_pos = vtDev->asyncbuf;
    vtDev->buf_size = vtDev->asyncbuf + asyncBufSize;
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
static VTCUDADevice* VTCUDAinitDevice(uint32_t ptid, int cudaDev)
{
  uint64_t time;
  VTCUDADevice *vtDev = NULL;

  /* cuda device not found, create new cuda device node */
  time = vt_pform_wtime();
  vt_enter(ptid, &time, rid_create);
  vtDev = VTCUDAcreateDevice(ptid, cudaDev);

  time = vt_pform_wtime();
  vt_exit(ptid, &time);

  /* set the current stream (stream 0) */
  vtDev->strmList = VTCUDAcreateStream(cudaDev, 0, ptid);

  /* write enter event for GPU_IDLE on stream 0 (has to be written first */
  if(show_gpu_idle == 1) vt_enter(vtDev->strmList->tid, &vt_start_time, rid_idle);

  /* set the counter value for cudaMalloc to 0  in stream 0 */
  if(trace_gpumem) vt_count(vtDev->strmList->tid, &time, cid_cudaMalloc, 0);

  /* add thread and CUDA device to list */
  CUDARTWRAP_LOCK();
    /* prepend */
    vtDev->next = cudaDevices;
    cudaDevices = vtDev;
  CUDARTWRAP_UNLOCK();

  time = vt_pform_wtime();
  vt_exit(ptid, &time);

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
static VTCUDADevice* VTCUDAcheckThread(cudaStream_t stream, uint32_t ptid,
                                       VTCUDAStrm **vtStrm)
{
    VTCUDADevice *vtDev;
    uint64_t time_check;
    int device;

    time_check = vt_pform_wtime();
    vt_enter(ptid, &time_check, rid_check);

    /* get the device to set the gpu_tid */
    checkCUDACall(cudaGetDevice_ptr(&device), "cudaGetDevice(device) failed!");

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
          if(stream == curStrm->stream){
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
        ptrLastStrm->next = VTCUDAcreateStream(device, stream, ptid);
        *vtStrm = ptrLastStrm->next;

        time_check = vt_pform_wtime();
        vt_exit(ptid, &time_check);
        return vtDev;
      }

      vtDev = vtDev->next;
    }
    CUDARTWRAP_UNLOCK();

    /* CUDA device not found, create new CUDA device node */
   vtDev = VTCUDAinitDevice(ptid, device);

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
  checkCUDACall(cudaGetDevice_ptr(&device),
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
 * @param stream the cuda stream
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

  if(vtDev->kn_conf) return NULL;

  /* check if there is enough buffer space */
  if(vtDev->buf_pos + sizeof(VTCUDAMemcpy) > vtDev->buf_size){
    VTCUDAflush(vtDev, ptid);
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
  vtDev = VTCUDAcheckThread(0, ptid, &vtStrm);

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

/* The structure of a cuda kernel element. The list will be filled in
 * __cudaRegisterFunction() and used in cudaLaunch() to get function name from
 * function pointer.
 */
typedef struct kernelele {
  const char* pointer;                /**< the host function */
  struct kernelele *next;             /**< pointer to next kernel element */
  char name[VTGPU_KERNEL_STRING_SIZE];         /**< name of the cuda kernel */
  /*char deviceName[DEVICE_NAME_SIZE];  *< name of the cuda device */
  uint32_t rid;                       /**< region id for this kernel */
}kernelelement;
static kernelelement *kernelListHead = NULL;

/*
 * Parse the device function name:
 * "_Z<kernel_length><kernel_name><templates>..." (no namespace)
 * "_ZN<ns_length><ns_name>...<ns_length><ns_name><kernel_length>..." (with namespace)
 *
 * @param elem pointer to the kernel element
 * @param devFunc the CUDA internal kernel function name
 */
static void extractKernelName(kernelelement *e, const char* devFunc)
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
static void insertKernelElement(const char* hostFun, const char* devFunc
                                /*, const char *deviceName*/)
{
  kernelelement* e = (kernelelement*) malloc(sizeof(kernelelement));
  e->pointer = hostFun;
  /*strncpy(e->deviceName, deviceName, DEVICE_NAME_SIZE);*/
  extractKernelName(e,devFunc);
  
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
 * Get kernel element from kernel pointer (to lookup name and token).
 *
 * @param hostFun the identifier string of the cuda kernel
 * @return the kernelNULL, if nothing was found
 * @todo linear search could be replaced with hash
 */
static kernelelement* getKernelElement(const char* hostFun)
{
  kernelelement *actual = NULL;

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
 * This function is being called before execution of a cuda program for every
 * cuda kernel (host_runtime.h)
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
      insertKernelElement(hostFun, deviceFun/*, deviceName*/);
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
    if(trace_gpumem){
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
    if(trace_gpumem){
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
    if(trace_gpumem) vtcudaMalloc(*array, width * height);
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
  int count = extent.height*extent.width*extent.depth;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemcpy3D",
    cudaError_t , (const struct cudaMemcpy3DParms *),
    NULL, 0);

  CUDA_SEND_RECV(kind, count,
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
  int count = extent.height*extent.width*extent.depth;

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
    if(trace_gpumem) vtcudaMalloc(*devPtr, size);
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
    if(trace_gpumem) vtcudaMalloc(*devPtr, (*pitch) * height);
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
    if(trace_gpumem) vtcudaFree(devPtr);
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
    if(trace_gpumem) vtcudaFree(array);
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

  CUDA_SEND_RECV(kind, count,
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

  CUDA_SEND_RECV(kind, count,
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

  CUDA_SEND_RECV(kind, count,
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

  CUDA_SEND_RECV(kind, count,
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

  CUDA_SEND_RECV(kind, width*height,
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

  CUDA_SEND_RECV(kind, width*height,
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

  CUDA_SEND_RECV(kind, width*height,
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

  CUDA_SEND_RECV(kind, width*height,
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

  CUDA_SEND_RECV(kind, count,
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

  CUDA_SEND_RECV(kind, count,
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

        /* avoid configure calls one after another without cudaLaunch */
        if(vtDev->kn_conf) return ret;
        vtDev->kn_conf = 1;

        /* check if there is enough buffer space */
        if(vtDev->buf_pos + sizeof(VTCUDAKernel) > vtDev->buf_size){
          VTCUDAflush(vtDev, ptid);
        }

        /* set already available values of kernel */
        {
          VTCUDAKernel* vtKernel = (VTCUDAKernel*) vtDev->buf_pos;

          vtKernel->strm = ptrStrm;
          vtKernel->blocksPerGrid = gridDim.x * gridDim.y * gridDim.z;
          vtKernel->threadsPerBlock = blockDim.x * blockDim.y * blockDim.z;
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
  kernelelement* e = NULL;
  uint8_t do_trace = 0;
  uint32_t ptid = 0;
  uint64_t time;

#if (defined(VT_CUDACUPTI))
  vt_cupti_ctx_t* vtcuptiCtx = NULL;
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
      e = getKernelElement(entry);
      if(e != NULL){       

        /* check if the kernel will be traced on the correct thread */
        vtDev = VTCUDAgetDevice(ptid);

        /* check if this kernel has been configured */
        if(vtDev->kn_conf == 0){
          ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (entry));
          vt_warning("[CUDART] No stacked configure call before launch of kernel "
                     "'%s' (device %d, ptid %d)", e->name, vtDev->device, ptid);
          return ret;
        }
        vtDev->kn_conf = 0;

        /* get the kernel, which has been partly filled in configure call */
        kernel = (VTCUDAKernel*)vtDev->buf_pos;

        vt_cntl_msg(3, "[CUDART] Launch '%s' (device %d, tid %d, rid %d, strm %d)",
                      e->name, vtDev->device, vtDev->ptid,
                      e->rid, (uint64_t)kernel->strm->stream);

        kernel->rid = e->rid;

        /* set type of buffer entry */
        kernel->type = VTCUDABUF_ENTRY_TYPE__Kernel;

        /*  get an already created unused event */
        kernel->evt = vtDev->evtbuf_pos;

        if(!trace_cupti){
          /* increment buffers */
          vtDev->evtbuf_pos++;
          vtDev->buf_pos += sizeof(VTCUDAKernel);
        }

#if (defined(VT_CUDACUPTI))
        /* zero CUPTI counter */
        if(trace_cupti){
          uint32_t tid = kernel->strm->tid;
          
          checkCUDACall(cudaDeviceSynchronize_ptr(), NULL);

          /* write VT kernel start events */
          time = vt_pform_wtime();
          if(show_gpu_idle) vt_exit(vtDev->strmList->tid, &time);
          vt_enter(tid, &time, e->rid);
          vt_count(tid, &time, cid_blocksPerGrid, kernel->blocksPerGrid);
          vt_count(tid, &time, cid_threadsPerBlock, kernel->threadsPerBlock);
          vt_count(tid, &time, cid_threadsPerKernel,
                   kernel->threadsPerBlock * kernel->blocksPerGrid);

          vtcuptiCtx = vt_cupti_getCurrentContext(ptid);
          vt_cupti_resetCounter(vtcuptiCtx, tid, &time);
        }else
#endif
          checkCUDACall(cudaEventRecord_ptr(kernel->evt->strt, kernel->strm->stream),
                      "cudaEventRecord(startEvt, strmOfLastKernel) failed!");
      }
    }
  }

  /* call cudaLaunch itself */
  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (entry));

  if(vt_cudart_trace_enabled){
    time = vt_pform_wtime();
    vt_exit(ptid, &time);

    if(do_trace){
      if(e != NULL && trace_kernels){
        REGISTER_FINALIZE;

#if (defined(VT_CUDACUPTI))
        /* synchronize after kernels to get cupti counter values */
        if(trace_cupti){
          cudaError_t ret;
          uint32_t tid = kernel->strm->tid;

          vt_enter(ptid, &time, rid_sync);

          if(cupti_sampling){
            /* sampling of CUPTI counter values */
            do{
              time = vt_pform_wtime();
              vt_cupti_writeCounter(vtcuptiCtx, tid, &time);
              /*ret = cudaEventQuery_ptr(kernel->evt->stop);*/
              ret = cudaStreamQuery_ptr(kernel->strm->stream);
            }while(ret != cudaSuccess);
          }else{
            /*ret = cudaEventSynchronize_ptr(kernel->evt->stop);*/
            checkCUDACall(cudaDeviceSynchronize_ptr(), NULL);
          }

          time = vt_pform_wtime();
          vt_cupti_writeCounter(vtcuptiCtx, tid, &time);
          vt_exit(ptid, &time);

          /* write VT kernel stop events */
          vt_count(tid, &time, cid_blocksPerGrid, 0);
          vt_count(tid, &time, cid_threadsPerBlock, 0);
          vt_count(tid, &time, cid_threadsPerKernel, 0);
          vt_exit(tid, &time);
          if(show_gpu_idle) vt_enter(vtDev->strmList->tid, &time, rid_idle);
        }else
#endif
          checkCUDACall(cudaEventRecord_ptr(kernel->evt->stop, kernel->strm->stream),
                      "cudaEventRecord(stopEvt, streamOfCurrentKernel) failed!");
      } /* e != NULL && trace_kernels */
    }
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
    if(trace_gpumem) vtcudaMalloc(*array, width * height);
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
    if(trace_gpumem){
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

#endif /* CUDA 4.0 */

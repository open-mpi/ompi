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
#include "vt_defs.h"        /* VampirTrace constants */
#include "vt_env.h"         /* get environment variables */
#include "vt_pform.h"       /* VampirTrace time measurement */
#include "vt_trc.h"         /* VampirTrace events */
#include "vt_cupti_events.h"
#include "vt_gpu.h"

#include <string.h>

#define PRINT_CUPTI_ERROR(err, _msg){                    \
    const char *errstr;                                  \
    cuptiGetResultString(err, &errstr);                  \
    vt_warning("[CUPTI EVENTS] %s:%d:%s:'%s'",           \
                 __FILE__, __LINE__, _msg, errstr);      \
  }

/* mutexes for locking the CUPTI environment */
#if (defined(VT_MT) || defined(VT_HYB))
static VTThrdMutex* VTThrdMutexCupti = NULL;
# define VT_CUPTIEVT_LOCK() VTThrd_lock(&VTThrdMutexCupti)
# define VT_CUPTIEVT_UNLOCK() VTThrd_unlock(&VTThrdMutexCupti)
#else /* VT_MT || VT_HYB */
# define VT_CUPTIEVT_LOCK()
# define VT_CUPTIEVT_UNLOCK()
#endif /* VT_MT || VT_HYB */

/* some of CUPTI API functions have changed */
#if (defined(CUPTI_API_VERSION) && (CUPTI_API_VERSION >= 2))

# define VTCUPTIEVENTGETATTRIBUTE(_cuDev, _cuptiEvtID, _cuptiAttr, _valueSize, \
                                 _value) \
  VT_CUPTI_CALL(\
    cuptiEventGetAttribute(_cuptiEvtID, _cuptiAttr, _valueSize, _value), \
    "cuptiEventGetAttribute")

# define VTCUPTIEVENTDOMAINGETNUMEVENTS(_cuDev, _cuptiDomain, _numEvts) \
  VT_CUPTI_CALL(\
    cuptiEventDomainGetNumEvents(_cuptiDomain, _numEvts), \
    "cuptiEventDomainGetNumEvents")

# define VTCUPTIEVENTDOMAINENUMEVENTS(_cuDev, _cuptiDomain, _valueSize, _value)\
  VT_CUPTI_CALL(\
    cuptiEventDomainEnumEvents(_cuptiDomain, _valueSize, _value), \
    "cuptiEventDomainEnumEvents")

#else

# define VTCUPTIEVENTGETATTRIBUTE(_cuDev, _cuptiEvtID, _cuptiAttr, _valueSize, \
                                 _value) \
  VT_CUPTI_CALL(\
    cuptiEventGetAttribute(_cuDev, _cuptiEvtID, _cuptiAttr, _valueSize,_value),\
    "cuptiEventGetAttribute")

# define VTCUPTIEVENTDOMAINGETNUMEVENTS(_cuDev, _cuptiDomain, _numEvts) \
  VT_CUPTI_CALL(\
    cuptiEventDomainGetNumEvents(_cuDev, _cuptiDomain, _numEvts), \
    "cuptiEventDomainGetNumEvents")

# define VTCUPTIEVENTDOMAINENUMEVENTS(_cuDev, _cuptiDomain, _valueSize, _value)\
  VT_CUPTI_CALL(\
    cuptiEventDomainEnumEvents(_cuDev, _cuptiDomain, _valueSize, _value), \
    "cuptiEventDomainEnumEvents")

#endif

static uint32_t vt_cuptievt_rid_init;
static uint8_t vt_cuptievt_initialized = 0;
static uint8_t vt_cuptievt_finalized = 0;

/* VampirTrace counter group ID */
static uint32_t vt_cuptievt_cgid;

static vt_cuptievt_dev_t *vtcuptievtCapList = NULL;
static vt_cuptievt_ctx_t *vtcuptievtCtxList = NULL;

/***** --- Declaration of internally used functions --- *****/

/*
 * Enables the recording of CUPTI counters. Either thread if or pointer to the
 * host thread structure has to be given.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 */
static void vt_cuptievt_start(vt_cuptievt_ctx_t *vtcuptiCtx);

/*
 * Disables recording of CUPTI counters.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 */
static void vt_cuptievt_stop(vt_cuptievt_ctx_t *vtcuptiCtx);

/*
 * Initialize a VampirTrace CUPTI context.
 * 
 * @param ptid the VampirTrace process/thread id
 * @param cuCtx the CUDA context
 * 
 * @return pointer to the created VampirTrace CUPTI context
 */
static vt_cuptievt_ctx_t* vt_cuptievt_initCtx(uint32_t ptid, CUcontext cuCtx);

/*
 * Get to current VampirTrace CUPTI context or create a new one, if CUDA context
 * is not registered yet.
 *
 * @param cuCtx the CUDA context to lookup the VampirTrace CUPTI context
 * @param ptid the VampirTrace thread id of current running thread
 *
 * @return the corresponding VampirTrace host thread structure.
 */
static vt_cuptievt_ctx_t* vt_cuptievt_getCtx(CUcontext cuCtx, uint32_t ptid);

/*
 * Free the memory allocated for the given VampirTrace CUPTI context.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 */
static void vt_cuptievt_freeCtx(vt_cuptievt_ctx_t *vtcuptiCtx);

/*
 * Remove the given CUDA context from the global VampirTrace CUPTI context list.
 * 
 * @param cuCtx pointer to the CUDA context
 * 
 * @return the removed VampirTrace CUPTI context entry
 */
static vt_cuptievt_ctx_t* vt_cupti_takeCtxFromList(CUcontext *cuCtx);

/* 
 * De-initialize the VampirTrace CUPTI context without destroying it.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 */
static void vt_cuptievt_finish(vt_cuptievt_ctx_t *vtcuptiCtx);

/*
 * Create a VampirTrace CUPTI event group.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 * 
 * @return the created VampirTrace CUPTI event group
 */
static vt_cuptievt_grp_t* vt_cuptievt_createEvtGrp(vt_cuptievt_ctx_t *vtcuptiCtx);

/*
 * Setup a list of devices with different device capabilities and add the 
 * metrics, which are specified by the user.
 * 
 * @return a list of CUDA devices with different device capabilities
 */
static vt_cuptievt_dev_t* vt_cuptievt_setupMetricList(void);

/*
 * Parse the environment variable for CUPTI metrics (including CUDA device
 * capabilities) and fill the capability metric list.
 *
 * @param capList points to the first element of the capability metric list
 */
static void vt_cupti_fillMetricList(vt_cuptievt_dev_t *capList);

/*
 * Check whether the CUDA device capability is already listed.
 *
 * @param capList IN: list containing the CUDA device capabilities
 * @param major the major CUDA device capability
 * @param minor the minor CUDA device capability
 *
 * @return pointer to the list entry (NULL if not found)
 */
static vt_cuptievt_dev_t* vt_cupti_checkMetricList(vt_cuptievt_dev_t *capList,
                                                   int major, int minor);

/*
 * Print all available counters to stdout.
 *
 * @param capList list of CUDA devices with different capabilities
 */
static void vt_cupti_showAllCounters(vt_cuptievt_dev_t *capList);

/*
 * Print all events for a given CUDA device and CUPTI event domain with name 
 * and ID.
 * 
 * @param cuDev the CUDA device
 * @param domainId the CUPTI event domain ID
 */
static void vt_cuptievt_enumEvents(CUdevice cuDev, CUpti_EventDomainID domainId);
/* ------ */

/* ----------------------- internally used functions ----------------------- */

static vt_cuptievt_grp_t* vt_cuptievt_createEvtGrp(vt_cuptievt_ctx_t *vtcuptiCtx)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cuptievt_grp_t *vtcuptiGrp = NULL;

  vtcuptiGrp = (vt_cuptievt_grp_t*)malloc(sizeof(vt_cuptievt_grp_t));
  vtcuptiGrp->evtNum = 0;
  vtcuptiGrp->enabled = 0;
  vtcuptiGrp->next = NULL;

  /* create initial CUPTI counter group */
  cuptiErr = cuptiEventGroupCreate(vtcuptiCtx->cuCtx, &(vtcuptiGrp->evtGrp), 0);
  VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupCreate");

  vtcuptiGrp->cuptiEvtIDs = (CUpti_EventID *)malloc(
                            vtcuptiCtx->vtDevCap->evtNum*sizeof(CUpti_EventID));
  vtcuptiGrp->vtCIDs = (uint32_t *)malloc(
                       vtcuptiCtx->vtDevCap->evtNum*sizeof(uint32_t));

  return vtcuptiGrp;
}

static void vt_cupti_addEvtGrpsToCtx(vt_cuptievt_ctx_t *vtcuptiCtx)
{
    CUptiResult cuptiErr = CUPTI_SUCCESS;
    vt_cuptievt_grp_t *vtcuptiGrp = vt_cuptievt_createEvtGrp(vtcuptiCtx);
    vt_cuptievt_evt_t *vtcuptiEvt = vtcuptiCtx->vtDevCap->vtcuptiEvtList;

    /* try to add all events for current context/device */
    while(vtcuptiEvt != NULL && vtcuptiGrp->evtNum < vtcuptiCtx->vtDevCap->evtNum){
      cuptiErr = cuptiEventGroupAddEvent(vtcuptiGrp->evtGrp,
                                         vtcuptiEvt->cuptiEvtID);

      /* everything is fine */
      if(cuptiErr == CUPTI_SUCCESS){
        vtcuptiGrp->cuptiEvtIDs[vtcuptiGrp->evtNum] = vtcuptiEvt->cuptiEvtID;
        vtcuptiGrp->vtCIDs[vtcuptiGrp->evtNum] = vtcuptiEvt->vtCID;
        vtcuptiGrp->evtNum++;
      }else{
        /* we can at least try to put the event in another group */

        /* too many events in this group or
           event is in different domain or device limitation*/
        if(cuptiErr == CUPTI_ERROR_MAX_LIMIT_REACHED ||
           cuptiErr == CUPTI_ERROR_NOT_COMPATIBLE){

          vt_cntl_msg(2, "[CUPTI EVENTS] Create new event group for event %d",
                         vtcuptiEvt->cuptiEvtID);

          /* prepend last group to list, if it is not empty */
          if(vtcuptiGrp->evtNum > 0){
            vtcuptiGrp->next = vtcuptiCtx->vtGrpList;
            vtcuptiCtx->vtGrpList = vtcuptiGrp;
          }

          /* create new VampirTrace CUPTI event group */
          vtcuptiGrp = vt_cuptievt_createEvtGrp(vtcuptiCtx);

          /* try to add the same event to the just created group */
          continue;
        }

        PRINT_CUPTI_ERROR(cuptiErr, "cuptiEventGroupAddEvent");
      }

      vtcuptiEvt = vtcuptiEvt->next;
    }

    /* prepend last group to list, if it is not empty */
    if(vtcuptiGrp->evtNum > 0){
      vtcuptiGrp->next = vtcuptiCtx->vtGrpList;
      vtcuptiCtx->vtGrpList = vtcuptiGrp;
    }
}

/*
 * Initializes a CUPTI host thread and create the event group.
 *
 * @param ptid the VampirTrace thread id
 * @param cuCtx optionally given CUDA context
 *
 * @return the created VampirTrace CUPTI host thread structure
 */
static vt_cuptievt_ctx_t* vt_cuptievt_initCtx(uint32_t ptid, CUcontext cuCtx)
{
  vt_cuptievt_ctx_t *vtcuptiCtx = NULL;
  uint64_t time;

  vt_cntl_msg(2, "[CUPTI EVENTS] Initializing VampirTrace CUPTI context (ptid=%d)",
              ptid);
  
  time = vt_pform_wtime();
  vt_enter(ptid, &time, vt_cuptievt_rid_init);

  /* initialize CUDA driver API, if necessary and get context handle */
  if(cuCtx == NULL){
#if (defined(CUDA_VERSION) && (CUDA_VERSION < 4000))
    CHECK_CU_ERROR(cuCtxPopCurrent(&cuCtx), "cuCtxPopCurrent");
    CHECK_CU_ERROR(cuCtxPushCurrent(cuCtx), "cuCtxPushCurrent");
#else
    CHECK_CU_ERROR(cuCtxGetCurrent(&cuCtx), "cuCtxGetCurrent");
#endif
  }

  /* get a pointer to eventIDArray */
  {
    CUresult cuErr = CUDA_SUCCESS;
    int dev_major, dev_minor;
    CUdevice cuDev = 0;
    vt_cuptievt_dev_t *cuptiDev;

    CHECK_CU_ERROR(cuCtxGetDevice(&cuDev), "cuCtxGetDevice");

    cuErr = cuDeviceComputeCapability(&dev_major, &dev_minor, cuDev);
    CHECK_CU_ERROR(cuErr, "cuDeviceComputeCapability");

    /* check if device capability already listed */
    VT_CUPTIEVT_LOCK();
      cuptiDev = vtcuptievtCapList;
    VT_CUPTIEVT_UNLOCK();
    
    cuptiDev = vt_cupti_checkMetricList(cuptiDev, dev_major, dev_minor);
    if(cuptiDev){
      vtcuptiCtx = (vt_cuptievt_ctx_t*)malloc(sizeof(vt_cuptievt_ctx_t));
      if(vtcuptiCtx == NULL)
        vt_error_msg("malloc(sizeof(VTCUPTIhostThrd)) failed!");
      vtcuptiCtx->cuCtx = cuCtx;
      vtcuptiCtx->vtDevCap = cuptiDev;
      vtcuptiCtx->vtGrpList = NULL;
      vtcuptiCtx->counterData = NULL;
      vtcuptiCtx->cuptiEvtIDs = NULL;
      vtcuptiCtx->next = NULL;
    }else{
      time = vt_pform_wtime();
      vt_exit(ptid, &time);
      return NULL;
    }
  }

  /* create and add the VampirTrace CUPTI groups to the context */
  vt_cupti_addEvtGrpsToCtx(vtcuptiCtx);

  /* allocate memory for CUPTI counter reads */
  {
    size_t allocSize = vtcuptiCtx->vtGrpList->evtNum;
    
    vtcuptiCtx->counterData = (uint64_t *)malloc(allocSize*sizeof(uint64_t));
    vtcuptiCtx->cuptiEvtIDs = (CUpti_EventID *)malloc(allocSize*sizeof(CUpti_EventID));
  }

  /* add VampirTrace CUPTI context entry to list (as first element) */
  VT_CUPTIEVT_LOCK();
    vtcuptiCtx->next = vtcuptievtCtxList;
    vtcuptievtCtxList = vtcuptiCtx;
  VT_CUPTIEVT_UNLOCK();

  time = vt_pform_wtime();
  vt_exit(ptid, &time);

  return vtcuptiCtx;
}

static void vt_cuptievt_freeCtx(vt_cuptievt_ctx_t *vtcuptiCtx)
{
  vt_cuptievt_grp_t *vtcuptiGrp = vtcuptiCtx->vtGrpList;

  while(vtcuptiGrp != NULL){
    free(vtcuptiGrp->cuptiEvtIDs);
    free(vtcuptiGrp->vtCIDs);
    
    vtcuptiGrp = vtcuptiGrp->next;
  }

  /* free memory for CUPTI counter reads */
  free(vtcuptiCtx->counterData);
  free(vtcuptiCtx->cuptiEvtIDs);

  /* free the VampirTrace CUPTI context structure itself */
  free(vtcuptiCtx);
}

/*
 * Retrieve the VampirTrace CUPTI context from the CUDA context.
 * 
 * @param cuCtx the CUDA context
 * @param ptid the active VampirTrace thread id
 */
static vt_cuptievt_ctx_t* vt_cuptievt_getCtx(CUcontext cuCtx, uint32_t ptid)
{
  vt_cuptievt_ctx_t *vtcuptiCtx = NULL;

  /* check, if there has been at least one VampirTrace CUPTI context created */
  if(vtcuptievtCtxList == NULL) vt_cupti_events_init();

  /* check, if the current VampirTrace thread is enabled for GPU counters */
  if((vt_gpu_prop[ptid] & VTGPU_NO_PC) == VTGPU_NO_PC)
    return NULL;

  /* check if CUDA context is listed (linear search) */
  VT_CUPTIEVT_LOCK();
  vtcuptiCtx = vtcuptievtCtxList;
  while(vtcuptiCtx != NULL){
    if(vtcuptiCtx->cuCtx == cuCtx){
      VT_CUPTIEVT_UNLOCK();
      /*vt_cntl_msg(1, "[CUPTI EVENTS] host thread %d (MPI rank %d)", ptid, vt_my_trace);*/
      return vtcuptiCtx;
    }
    vtcuptiCtx = vtcuptiCtx->next;
  }
  VT_CUPTIEVT_UNLOCK();

  vt_cntl_msg(2, "[CUPTI EVENTS] Context for VT tid=%d unknown! Creating ... ", 
              ptid);

  vtcuptiCtx = vt_cuptievt_initCtx(ptid, NULL);
  if(vtcuptiCtx != NULL){
    vt_cuptievt_start(vtcuptiCtx);
  }else{
    /* no performance counters for this thread available */
    vt_gpu_prop[ptid] |= VTGPU_NO_PC;
    vt_cntl_msg(2, "[CUPTI EVENTS] Could not initialize!");
  }

  return vtcuptiCtx;
}

/*
 * Parse the environment variable for CUPTI metrics (including CUDA device
 * capabilities) and fill the capability metric list.
 *
 * @param capList points to the first element of the capability metric list
 */
static void vt_cupti_fillMetricList(vt_cuptievt_dev_t *capList)
{
  char *metricString = vt_env_cupti_events();
  char *metric_sep = vt_env_metrics_sep();
  char *metric, *metric_cap;

  metric = strtok(metricString, metric_sep);

  while (metric != NULL){
    CUptiResult cuptiErr = CUPTI_SUCCESS;
    vt_cuptievt_dev_t *cuptiDev = NULL;
    vt_cuptievt_evt_t *vtcuptiEvt = NULL;
    int metr_major = 0;
    int metr_minor = 0;

    /* try to get CUDA device capability parsed from metric */
    metr_major = atoi(metric);
    metric_cap = strchr(metric+1, '.');
    if(metric_cap){
      metr_minor = atoi(metric_cap+1);
      metric_cap = strchr(metric_cap+1, '_');
    }
    
    /* check whether device capability is given or not */
    if(metric_cap){
      metric = metric_cap + 1;

      vt_cntl_msg(2, "Metric '%s', %d.%d", metric, metr_major, metr_minor);

      cuptiDev = vt_cupti_checkMetricList(capList, metr_major, metr_minor);
      if(cuptiDev == NULL){
        metric = strtok(NULL, metric_sep);
        continue;
      }
      
      vtcuptiEvt = (vt_cuptievt_evt_t*)malloc(sizeof(vt_cuptievt_evt_t));
      cuptiErr = cuptiEventGetIdFromName(cuptiDev->cuDev, metric,
                                         &vtcuptiEvt->cuptiEvtID);
      if(cuptiErr != CUPTI_SUCCESS){
        if(!strncmp(metric, "help", 4)) vt_cupti_showAllCounters(capList);
        vt_warning("[CUPTI EVENTS] Skipping invalid event '%s' for device %d",
                   metric, cuptiDev->cuDev);
        metric = strtok(NULL, metric_sep);
        continue;
      }

      /* create VampirTrace counter ID */
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
      vtcuptiEvt->vtCID = vt_def_counter(VT_MASTER_THREAD, metric, "#",
            VT_CNTR_ABS | VT_CNTR_LAST | VT_CNTR_UNSIGNED, vt_cuptievt_cgid, 0);
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif

      cuptiDev->evtNum++;
      vtcuptiEvt->next = cuptiDev->vtcuptiEvtList;
      cuptiDev->vtcuptiEvtList = vtcuptiEvt;
    }else{ 
      /* device capability is not given. Try to add metric to all devices */
      uint32_t cid_metric = VT_NO_ID;

      cuptiDev = capList;
      while(cuptiDev != NULL){
        vtcuptiEvt = (vt_cuptievt_evt_t*)malloc(sizeof(vt_cuptievt_evt_t));
        cuptiErr = cuptiEventGetIdFromName(cuptiDev->cuDev, metric,
                                           &vtcuptiEvt->cuptiEvtID);

        if(cuptiErr != CUPTI_SUCCESS){
          if(!strncmp(metric, "help", 4)) vt_cupti_showAllCounters(capList);
          vt_warning("[CUPTI EVENTS] Skipping invalid event '%s' for device %d",
                     metric, cuptiDev->cuDev);
        }else{
          /* create VampirTrace counter ID, if not yet done for other device */
          if(cid_metric == VT_NO_ID){
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
      cid_metric = vt_def_counter(VT_MASTER_THREAD, metric, "#", 
            VT_CNTR_ABS | VT_CNTR_LAST | VT_CNTR_UNSIGNED, vt_cuptievt_cgid, 0);
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif
          }
          
          cuptiDev->evtNum++;
          vtcuptiEvt->vtCID = cid_metric;
          vtcuptiEvt->next = cuptiDev->vtcuptiEvtList;
          cuptiDev->vtcuptiEvtList = vtcuptiEvt;
        }

        cuptiDev = cuptiDev->next;
      }
    }

    metric = strtok(NULL, metric_sep);
  }
}

/*
 * Check whether the CUDA device capability is already listed.
 *
 * @param capList IN: list containing the CUDA device capabilities
 * @param major the major CUDA device capability
 * @param minor the minor CUDA device capability
 *
 * @return pointer to the list entry (NULL if not found)
 */
static vt_cuptievt_dev_t* vt_cupti_checkMetricList(vt_cuptievt_dev_t *capList,
                                                int major, int minor)
{
  vt_cuptievt_dev_t *cuptiDev;

  /* check if device capability is already listed and return it if found */
  cuptiDev = capList;
  while(cuptiDev != NULL){
    if(cuptiDev->dev_major == major && cuptiDev->dev_minor == minor){
      return cuptiDev;
    }
    cuptiDev = cuptiDev->next;
  }

  return NULL;
}

/*
 * Setup a list of devices with different device capabilities and add the 
 * metrics, which are specified by the user.
 * 
 * @return a list of CUDA devices with different device capabilities
 */
static vt_cuptievt_dev_t* vt_cuptievt_setupMetricList(void)
{
  CUresult err;
  int deviceCount, id;
  vt_cuptievt_dev_t *capList = NULL;
  
  /* CUDA initialization */
	CHECK_CU_ERROR(cuInit(0), "cuInit");
  
  /* How many GPGPU devices do we have? */
	err = cuDeviceGetCount( &deviceCount );
	CHECK_CU_ERROR(err, "cuDeviceGetCount");
	if(deviceCount == 0){
		vt_error_msg("[CUPTI EVENTS] There is no device supporting CUDA.");
	}

  /* create list with available compute capabilities */
  for(id = 0; id < deviceCount; id++){
    CUdevice cuDev;
    vt_cuptievt_dev_t *cuptiDev;
    int dev_major, dev_minor;

    err = cuDeviceGet(&cuDev, id);
		CHECK_CU_ERROR(err, "cuDeviceGet");

    err = cuDeviceComputeCapability(&dev_major, &dev_minor, cuDev);
    CHECK_CU_ERROR(err, "cuDeviceComputeCapability");

    /* check if device capability already listed */
    cuptiDev = vt_cupti_checkMetricList(capList, dev_major, dev_minor);

    if(cuptiDev == NULL){
      /* allocate memory for device list entry */
      cuptiDev = (vt_cuptievt_dev_t *)malloc(sizeof(vt_cuptievt_dev_t));
      cuptiDev->dev_major = dev_major;
      cuptiDev->dev_minor = dev_minor;
      cuptiDev->cuDev = cuDev;
      cuptiDev->vtcuptiEvtList = NULL;
      cuptiDev->evtNum = 0;
      cuptiDev->next = NULL;

      /* prepend to list */
      cuptiDev->next = capList;
      capList = cuptiDev;
    }
  }

  vt_cupti_fillMetricList(capList);

  /* cleanup list: remove entries, which don't have metrics */
  {
    vt_cuptievt_dev_t *curr = capList;
    vt_cuptievt_dev_t *last = capList;

    while(curr != NULL){
      vt_cuptievt_dev_t *freeDev = curr;
      curr = curr->next;

      if(freeDev->evtNum == 0){
        /* first element */
        if(freeDev == capList){
          capList = capList->next;
        }else{
          last->next = freeDev->next;
        }
        free(freeDev);
      }else last = freeDev;
    }
  }

  return capList;
}

/*
 * Print all events for a given CUDA device and CUPTI event domain with name 
 * and ID.
 * 
 * @param cuDev the CUDA device
 * @param domainId the CUPTI event domain ID
 */
static void vt_cuptievt_enumEvents(CUdevice cuDev, CUpti_EventDomainID domainId)
{
  CUpti_EventID *eventId = NULL;
  uint32_t maxEvents = 0;
  uint32_t i = 0;
  size_t size = 0;
  uint8_t desc_on = 0;
  char *help = vt_env_cupti_events();
  
  if(!strncmp(&help[4], "_l", 2)) desc_on = 1;
  
  /*vt_cntl_msg(1, "############ %s", &help[5]);*/

  /* query num of events available in the domain */
  VTCUPTIEVENTDOMAINGETNUMEVENTS(cuDev,
                                 (CUpti_EventDomainID)domainId,
                                 &maxEvents);

  size = sizeof(CUpti_EventID) * maxEvents;
  eventId = (CUpti_EventID*)malloc(size);
  if(eventId == NULL) vt_error_msg("Failed to allocate memory for event ID");
  memset(eventId, 0, size);

  VTCUPTIEVENTDOMAINENUMEVENTS(cuDev,
                               (CUpti_EventDomainID)domainId,
                               &size,
                               eventId);

  /* query event info */
  {
    size_t NAME_SHORT = 32;
    size_t DESC_SHORT = 2048;
    char *eventname = (char*)malloc(NAME_SHORT*sizeof(char)); /* event name */
    char *shortdesc = NULL; /* short desc of the event */
    
    if(desc_on) shortdesc = malloc(DESC_SHORT*sizeof(char));
    
    for(i = 0; i < maxEvents; i++){
      NAME_SHORT = 32;
      DESC_SHORT = 2048;
      VTCUPTIEVENTGETATTRIBUTE(cuDev,
                               eventId[i],
                               CUPTI_EVENT_ATTR_NAME,
                               &NAME_SHORT,
                               eventname);

      if(desc_on){
        VTCUPTIEVENTGETATTRIBUTE(cuDev,
                                 eventId[i],
                                 CUPTI_EVENT_ATTR_LONG_DESCRIPTION,
                                 &DESC_SHORT,
                                 (uint8_t*)shortdesc);
      }

      vt_cntl_msg(1, "%d:%s", eventId[i], eventname);
      if(desc_on) vt_cntl_msg(1, "%s\n", shortdesc);
    }

    free(eventname);
    if(desc_on) free(shortdesc);
  }

  free(eventId);
}

/*
 * Print all available counters to stdout.
 *
 * @param capList list of CUDA devices with different capabilities
 */
static void vt_cupti_showAllCounters(vt_cuptievt_dev_t *capList)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  CUpti_EventDomainID *domainId = NULL;
  uint32_t maxDomains = 0;
  uint32_t i;
  size_t size = 0;
  
  while(capList != NULL){
    CUdevice cuDev = capList->cuDev;
    vt_cntl_msg(1, "[CUPTI EVENTS] Available events for device %d (SM %d.%d):", 
                   cuDev, capList->dev_major, capList->dev_minor);
    vt_cntl_msg(1, "Id:Name");
    vt_cntl_msg(1, "Description\n"
         "-------------------------------------------------------------------");
    
    cuptiErr = cuptiDeviceGetNumEventDomains(cuDev, &maxDomains);
    VT_CUPTI_CALL(cuptiErr, "cuptiDeviceGetNumEventDomains");

    if(maxDomains == 0){
      vt_cntl_msg(1, "[CUPTI EVENTS] No domain is exposed by dev = %d\n", cuDev);
      return;
    }

    size = sizeof(CUpti_EventDomainID) * maxDomains;
    domainId = (CUpti_EventDomainID*)malloc(size);
    if(domainId == NULL){
      vt_cntl_msg(1, "[CUPTI EVENTS] Failed to allocate memory to domain ID");
      return;
    }
    memset(domainId, 0, size);

    cuptiErr = cuptiDeviceEnumEventDomains(cuDev, &size, domainId);
    VT_CUPTI_CALL(cuptiErr, "cuptiDeviceEnumEventDomains");

    /* enum domains */
    for(i = 0; i < maxDomains; i++) vt_cuptievt_enumEvents(cuDev, domainId[i]);

    vt_cntl_msg(1, "------------------------------------------------------");
    
    free(domainId);
    
    capList = capList->next;
  }
  
  /* as this function is in the call-path of the initialize functions
   * -> vt_cupti_setupMetrics 
   * -> vt_cupti_fillMetricList 
   * -> vt_cupti_showAllCounters
   */
  vt_cuptievt_initialized = 1;
  VT_CUPTIEVT_UNLOCK();
  exit(0);
}


static void vt_cuptievt_start(vt_cuptievt_ctx_t *vtcuptiCtx)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cuptievt_grp_t *vtcuptiGrp = NULL;
  vt_cuptievt_grp_t *lastGrp = NULL;

  if(vtcuptiCtx == NULL) return;

  /* start all groups */
  vtcuptiGrp = vtcuptiCtx->vtGrpList;
  lastGrp = vtcuptiCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    cuptiErr = cuptiEventGroupEnable(vtcuptiGrp->evtGrp);
    
    /* if the event group could not be enabled, remove it */
    if(cuptiErr != CUPTI_SUCCESS){
      size_t i;
      vt_cuptievt_grp_t *freeGrp = vtcuptiGrp;
      size_t valueSize = 32;
      char name[32];

      vtcuptiGrp = vtcuptiGrp->next;

      /* give user information about the group, which cannot be enabled */
      for(i = 0; i < freeGrp->evtNum; i++){
        VTCUPTIEVENTGETATTRIBUTE(vtcuptiCtx->vtDevCap->cuDev,
                                 *(freeGrp->cuptiEvtIDs)+i,
                                 CUPTI_EVENT_ATTR_NAME,
                                 &valueSize, (char*)name);
        vt_warning("[CUPTI EVENTS] Event '%s' (%d) cannot be enabled",
                   name, *(freeGrp->cuptiEvtIDs)+i);
      }

      /* group is first element in linked list */
      if(vtcuptiCtx->vtGrpList == freeGrp){
        vtcuptiCtx->vtGrpList = vtcuptiCtx->vtGrpList->next;
      }else{/* has to be at least the second group in linked list */
        lastGrp->next = freeGrp->next;
      }

      free(freeGrp);
      freeGrp = NULL;
    }else{
      vtcuptiGrp->enabled = 1;
      lastGrp= vtcuptiGrp;
      vtcuptiGrp = vtcuptiGrp->next;
    }
  }
  
}

/*
 * Stop CUPTI counter capturing by disabling the CUPTI event groups.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 */
static void vt_cuptievt_stop(vt_cuptievt_ctx_t *vtcuptiCtx)
{
  vt_cuptievt_grp_t *vtcuptiGrp = NULL;
  /*vt_cntl_msg(1, "[CUPTI EVENTS] vt_cupti_stop() ... ");*/

  if(vtcuptiCtx == NULL || vt_gpu_debug) return;

  /* stop counter reading for all groups */
  vtcuptiGrp = vtcuptiCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    if(vtcuptiGrp->enabled){
      CUptiResult cuptiErr = CUPTI_SUCCESS;
      
      cuptiErr = cuptiEventGroupDisable(vtcuptiGrp->evtGrp);
      VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupDisable");

      vtcuptiGrp->enabled = 0;
    }

    vtcuptiGrp = vtcuptiGrp->next;
  }
}

/* 
 * De-initialize the VampirTrace CUPTI context without destroying it.
 * 
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 */
static void vt_cuptievt_finish(vt_cuptievt_ctx_t *vtcuptiCtx)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;

  if(vtcuptiCtx == NULL || vt_gpu_debug) return;

  /*uint64_t time = vt_pform_wtime();
  vt_cupti_resetCounter(vtcuptiCtx, 0, &time);*/

  /* stop CUPTI counter capturing */
  vt_cuptievt_stop(vtcuptiCtx);

  /* destroy all CUPTI event groups, which have been created */
  {
    vt_cuptievt_grp_t *vtcuptiGrp = vtcuptiCtx->vtGrpList;
    
    while(vtcuptiGrp != NULL){
      cuptiErr = cuptiEventGroupRemoveAllEvents(vtcuptiGrp->evtGrp);
      VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupRemoveAllEvents");

      cuptiErr = cuptiEventGroupDestroy(vtcuptiGrp->evtGrp);
      VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupDestroy");

      vtcuptiGrp = vtcuptiGrp->next;
    }
  }
}

/*
 * Remove the given CUDA context from the global VampirTrace CUPTI context list.
 * 
 * @param cuCtx pointer to the CUDA context
 * 
 * @return the removed VampirTrace CUPTI context entry
 */
static vt_cuptievt_ctx_t* vt_cupti_takeCtxFromList(CUcontext *cuCtx)
{
  vt_cuptievt_ctx_t *currCtx = NULL;
  vt_cuptievt_ctx_t *lastCtx = NULL;

  VT_CUPTIEVT_LOCK();
  currCtx = vtcuptievtCtxList;
  lastCtx = vtcuptievtCtxList;
  while(currCtx != NULL){
    if(currCtx->cuCtx == *cuCtx){
      /* if first element in list */
      if(currCtx == vtcuptievtCtxList){
        vtcuptievtCtxList = vtcuptievtCtxList->next;
      }else{
        lastCtx->next = currCtx->next;
      }
      VT_CUPTIEVT_UNLOCK();
      return currCtx;
    }
    lastCtx = currCtx;
    currCtx = currCtx->next;
  }
  VT_CUPTIEVT_UNLOCK();

  vt_cntl_msg(2, "[CUPTI EVENTS] Context structure not found!");
  return NULL;
}


/* -------------START: Implementation of public functions ------------------ */
/* ------------------------------------------------------------------------- */

/*
 * Initialize Mutex, VampirTrace IDs and registers the finalize function.
 * This may be done implicitly by vt_cupti_count().
 */
void vt_cupti_events_init()
{
  if(!vt_cuptievt_initialized){
#if (defined(VT_MT) || defined(VT_HYB))
    VTThrd_createMutex(&VTThrdMutexCupti);
#endif
    VT_CUPTIEVT_LOCK();
    if(!vt_cuptievt_initialized){
      vt_cntl_msg(2, "[CUPTI EVENTS] Initializing ... ");

      /* create VampirTrace counter group ID only once */
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
      vt_cuptievt_rid_init = vt_def_region(VT_MASTER_THREAD, "vtcuptiHostThreadInit",
                      VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "VT_CUPTI", VT_FUNCTION);

      vt_cuptievt_cgid = vt_def_counter_group(VT_MASTER_THREAD, "CUPTI");
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif

      vtcuptievtCapList = vt_cuptievt_setupMetricList();

      /* register the finalize function of VampirTrace CUPTI to be called before
       * the program exits */
      atexit(vt_cupti_events_finalize);

      vt_cuptievt_initialized = 1;
      VT_CUPTIEVT_UNLOCK();
    }
  }
}

/*
 * Finalizes the VampirTrace CUPTI implementation.
 */
void vt_cupti_events_finalize()
{
  if(!vt_cuptievt_finalized){

    VT_CUPTIEVT_LOCK();
    if(!vt_cuptievt_finalized){

      vt_cntl_msg(2, "[CUPTI EVENTS] Finalizing ...");

      /* free VampirTrace CUPTI context structures (should already be freed) */
      while(vtcuptievtCtxList != NULL){
        vt_cuptievt_ctx_t *tmp =  vtcuptievtCtxList;

        vt_cuptievt_finish(vtcuptievtCtxList);

        vtcuptievtCtxList = vtcuptievtCtxList->next;

        free(tmp);
        tmp = NULL;
      }

      /* free capability metric list */
      while(vtcuptievtCapList != NULL){
        vt_cuptievt_dev_t *tmp = vtcuptievtCapList;
        vtcuptievtCapList = vtcuptievtCapList->next;
        
        /* free VampirTrace CUPTI events */
        while(tmp->vtcuptiEvtList != NULL){
          vt_cuptievt_evt_t *tmpEvt = tmp->vtcuptiEvtList;
          tmp->vtcuptiEvtList = tmp->vtcuptiEvtList->next;
          free(tmpEvt);
          tmpEvt = NULL;
        }

        free(tmp);
        tmp = NULL;
      }

      vt_cuptievt_finalized = 1;
      VT_CUPTIEVT_UNLOCK();

#if (defined(VT_MT) || defined (VT_HYB))
      VTTHRD_LOCK_ENV();
      VTThrd_deleteMutex(&VTThrdMutexCupti);
      VTTHRD_UNLOCK_ENV();
#endif /* VT_MT || VT_HYB */
    }
  }
}

/*
 * Returns the VampirTrace CUPTI context for the CUDA context associated with
 * the calling host thread.
 *
 * @param ptid the VampirTrace thread id of the calling host thread
 */
vt_cuptievt_ctx_t* vt_cuptievt_getCurrentContext(uint32_t ptid)
{
  CUcontext cuCtx = NULL;
  
  if(!vt_cuptievt_initialized) vt_cupti_events_init();

# if (defined(CUDA_VERSION) && (CUDA_VERSION < 4000))
  CHECK_CU_ERROR(cuCtxPopCurrent(&cuCtx), "cuCtxPopCurrent");
  CHECK_CU_ERROR(cuCtxPushCurrent(cuCtx), "cuCtxPushCurrent");
# else
  CHECK_CU_ERROR(cuCtxGetCurrent(&cuCtx), "cuCtxGetCurrent");
# endif
  
  if(cuCtx == NULL){
    vt_cntl_msg(2, "[CUPTI EVENTS] No context is bound to the calling CPU thread!");
    return NULL;
  }
  
  return vt_cuptievt_getCtx(cuCtx, ptid);
}

/*
 * Request the CUTPI counter values and write it to the given VampirTrace
 * stream with the given timestamps.
 *
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 * @param strmid the stream id for the counter values
 * @param time the VampirTrace timestamps
 */
void vt_cuptievt_writeCounter(vt_cuptievt_ctx_t *vtcuptiCtx, uint32_t strmid,
                              uint64_t *time)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cuptievt_grp_t *vtcuptiGrp = NULL;

  size_t bufferSizeBytes;
  size_t arraySizeBytes;
  size_t numCountersRead;

  if(vtcuptiCtx == NULL){
    VT_CHECK_THREAD;
    vtcuptiCtx = vt_cuptievt_getCurrentContext(VT_MY_THREAD);
    if(vtcuptiCtx == NULL) return;
  }

  vtcuptiGrp = vtcuptiCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    /* read events only, if the event group is enabled */
    if(vtcuptiGrp->enabled){

      bufferSizeBytes = vtcuptiGrp->evtNum * sizeof(uint64_t);
      arraySizeBytes = vtcuptiGrp->evtNum * sizeof(CUpti_EventID);

      /* read events */
      cuptiErr = cuptiEventGroupReadAllEvents(vtcuptiGrp->evtGrp,
                                              CUPTI_EVENT_READ_FLAG_NONE,
                                              &bufferSizeBytes, vtcuptiCtx->counterData,
                                              &arraySizeBytes, vtcuptiCtx->cuptiEvtIDs,
                                              &numCountersRead);
      VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupReadAllEvents");
      
      if(vtcuptiGrp->evtNum != numCountersRead){
        vt_error_msg("[CUPTI EVENTS] %d counter reads, %d metrics specified in "
                   "VT_CUPTI_METRICS!", numCountersRead, vtcuptiGrp->evtNum);
      }

      /* For all events of the event group: map added event IDs to just read event
       * IDs, as the order may not be the same. For small numbers of counter reads
       * this simple mapping should be fast enough.
       */
      {
        size_t j;

        for(j = 0; j < numCountersRead; j++){
          size_t i;
          for(i = 0; i < vtcuptiGrp->evtNum; i++){
            if(vtcuptiCtx->cuptiEvtIDs[j] == *(vtcuptiGrp->cuptiEvtIDs+i)){
              /* write the counter value as VampirTrace counter */
              vt_count(strmid, time, *(vtcuptiGrp->vtCIDs+i), vtcuptiCtx->counterData[i]);
            }
          }
        }
      }

    }

    vtcuptiGrp = vtcuptiGrp->next;
  }
  
}

/*
 * Reset the VampirTrace counter values (to zero) for active CUPTI counters.
 *
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 * @param strmid the stream id for the counter values
 * @param time the VampirTrace timestamps
 */
void vt_cuptievt_resetCounter(vt_cuptievt_ctx_t *vtcuptiCtx, uint32_t strmid,
                              uint64_t *time)
{
  size_t i;
  vt_cuptievt_grp_t *vtcuptiGrp = NULL;

  if(vtcuptiCtx == NULL){
    VT_CHECK_THREAD;
    vtcuptiCtx = vt_cuptievt_getCurrentContext(VT_MY_THREAD);
    if(vtcuptiCtx == NULL) return;
  }

  vtcuptiGrp = vtcuptiCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    for(i = 0; i < vtcuptiGrp->evtNum; i++){
      vt_count(strmid, time, *(vtcuptiGrp->vtCIDs+i), 0);
    }

    /* reset counter values of this group */
    VT_CUPTI_CALL(cuptiEventGroupResetAllEvents(vtcuptiGrp->evtGrp),
                      "cuptiEventGroupResetAllEvents");
    
    vtcuptiGrp = vtcuptiGrp->next;
  }
}

/*
 * Finalizes CUPTI device.
 * 
 * @param ptid VampirTrace process/thread id
 * @param cleanExit 1 to cleanup CUPTI event group, otherwise 0
 */
void vt_cuptievt_finalize_device(uint32_t ptid, uint8_t cleanExit){
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cuptievt_ctx_t *vtcuptiCtx = NULL;

  vt_cntl_msg(2, "[CUPTI EVENTS] Finalize device ... ");

  {
    CUcontext cuCtx;
    
#if (defined(CUDA_VERSION) && (CUDA_VERSION < 4000))
    CHECK_CU_ERROR(cuCtxPopCurrent(&cuCtx), "cuCtxPopCurrent");
    CHECK_CU_ERROR(cuCtxPushCurrent(cuCtx), "cuCtxPushCurrent");
#else
    CHECK_CU_ERROR(cuCtxGetCurrent(&cuCtx), "cuCtxGetCurrent");
#endif

    vtcuptiCtx = vt_cupti_takeCtxFromList(&cuCtx);
    if(vtcuptiCtx == NULL) return;
  }

  if(cleanExit && vt_gpu_debug != 0){
    /*uint64_t time = vt_pform_wtime();

    vt_cupti_resetCounter(vtcuptiCtx, 0, &time);*/

    /* stop CUPTI counter capturing */
    vt_cuptievt_stop(vtcuptiCtx);

    /* destroy all CUPTI event groups, which have been created */
    {
      vt_cuptievt_grp_t *vtcuptiGrp = vtcuptiCtx->vtGrpList;

      while(vtcuptiGrp != NULL){
        cuptiErr = cuptiEventGroupRemoveAllEvents(vtcuptiGrp->evtGrp);
        VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupRemoveAllEvents");

        cuptiErr = cuptiEventGroupDestroy(vtcuptiGrp->evtGrp);
        VT_CUPTI_CALL(cuptiErr, "cuptiEventGroupDestroy");

        vtcuptiGrp = vtcuptiGrp->next;
      }
    }
  }

  /* free VampirTrace CUPTI context */
  vt_cuptievt_freeCtx(vtcuptiCtx);
}

/* ------------------------------------------------------------------------- */
/* -------------- END: Implementation of public functions ------------------ */

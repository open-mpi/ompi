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
#include "vt_defs.h"        /* VampirTrace constants */
#include "vt_env.h"         /* get environment variables */
#include "vt_pform.h"       /* VampirTrace time measurement */
#include "vt_trc.h"         /* VampirTrace events */
#include "vt_cudacupti.h"
#include "vt_gpu.h"

#include <string.h>

#define CHECK_CUPTI_ERROR(err, cuptifunc)                \
  if(err != CUPTI_SUCCESS){                              \
    const char *errstr;                                  \
    cuptiGetResultString(err, &errstr);                  \
    vt_error_msg("[CUPTI] %s:%d:%s:'%s'",                \
                 __FILE__, __LINE__, cuptifunc, errstr); \
  }

#define PRINT_CUPTI_ERROR(err, cuptifunc){               \
    const char *errstr;                                  \
    cuptiGetResultString(err, &errstr);                  \
    vt_warning("[CUPTI] %s:%d:%s:'%s'",                  \
                 __FILE__, __LINE__, cuptifunc, errstr); \
  }

/* Mutex for locking the CUPTI environment */
#if (defined(VT_MT) || defined(VT_HYB))
static VTThrdMutex* VTThrdMutexCupti = NULL;
# define CUPTI_LOCK() VTThrd_lock(&VTThrdMutexCupti)
# define CUPTI_UNLOCK() VTThrd_unlock(&VTThrdMutexCupti)
#else /* VT_MT || VT_HYB */
# define CUPTI_LOCK()
# define CUPTI_UNLOCK()
#endif /* VT_MT || VT_HYB */

static uint32_t rid_cupti_init;
static uint8_t vt_cupti_initialized = 0;
static uint8_t vt_cupti_finalized = 0;

/* VampirTrace counter group ID */
static uint32_t cgid_cupti;

static vt_cupti_dev_t *vt_cupti_capList = NULL;
static vt_cupti_ctx_t *vtcuptiCtxlist = NULL;

/***** --- Declaration of internally used functions --- *****/

/*
 * Enables the recording of CUPTI counters. Either thread if or pointer to the
 * host thread structure has to be given.
 */
static void vt_cupti_start(vt_cupti_ctx_t *vtcuptiCtx);

/*
 * Disables recording of CUPTI counters.
 */
static void vt_cupti_stop(vt_cupti_ctx_t *vtcuptiCtx);

/*
 * Get to current VampirTrace CUPTI context or create a new one, if CUDA context
 * is not registered yet.
 *
 * @param ptid the VampirTrace thread id of current running thread
 *
 * @return the corresponding VampirTrace host thread structure.
 */
static vt_cupti_ctx_t* vt_cupti_getCtx(CUcontext cuCtx, uint32_t ptid);

/*
 * Free CUPTI event group and internally allocated memory for active host thread
 *
 * @param ptid VampirTrace thread id of current host thread
 */
static void vt_cupti_finish(vt_cupti_ctx_t *vtcuptiCtx);

static vt_cupti_grp_t* vt_cupti_createEvtGrp(vt_cupti_ctx_t *vtcuptiCtx);

static vt_cupti_ctx_t* vt_cupti_initCtx(uint32_t ptid, CUcontext cuCtx);
static void vt_cupti_freeCtx(vt_cupti_ctx_t *vtcuptiCtx);

static vt_cupti_dev_t* vt_cupti_setupMetricList(void);
static void vt_cupti_fillMetricList(vt_cupti_dev_t *capList);
static vt_cupti_dev_t* vt_cupti_checkMetricList(vt_cupti_dev_t *capList,
                                                int major, int minor);
static void vt_cupti_showAllCounters(CUdevice cuDev);
static vt_cupti_ctx_t* vt_cupti_takeCtxFromList(CUcontext cuCtx);
static void enumEvents(CUdevice cuDev, CUpti_EventDomainID domainId);
/* ------ */

/* ----------------------- internally used functions ----------------------- */

static vt_cupti_grp_t* vt_cupti_createEvtGrp(vt_cupti_ctx_t *vtcuptiCtx)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cupti_grp_t *vtcuptiGrp = NULL;

  vtcuptiGrp = (vt_cupti_grp_t*)malloc(sizeof(vt_cupti_grp_t));
  vtcuptiGrp->evtNum = 0;
  vtcuptiGrp->enabled = 0;
  vtcuptiGrp->next = NULL;

  /* create initial CUPTI counter group */
  cuptiErr = cuptiEventGroupCreate(vtcuptiCtx->cuCtx, &(vtcuptiGrp->evtGrp), 0);
  CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupCreate");

  vtcuptiGrp->cuptiEvtIDs = (CUpti_EventID *)malloc(
                            vtcuptiCtx->vtDevCap->evtNum*sizeof(CUpti_EventID));
  vtcuptiGrp->vtCIDs = (uint32_t *)malloc(
                       vtcuptiCtx->vtDevCap->evtNum*sizeof(uint32_t));

  return vtcuptiGrp;
}

static void vt_cupti_addEvtGrpsToCtx(vt_cupti_ctx_t *vtcuptiCtx)
{
    CUptiResult cuptiErr = CUPTI_SUCCESS;
    vt_cupti_grp_t *vtcuptiGrp = vt_cupti_createEvtGrp(vtcuptiCtx);
    vt_cupti_evt_t *vtcuptiEvt = vtcuptiCtx->vtDevCap->vtcuptiEvtList;

    /* try to add all events for current context/device */
    while(vtcuptiEvt != NULL && vtcuptiGrp->evtNum < vtcuptiCtx->vtDevCap->evtNum){
      cuptiErr = cuptiEventGroupAddEvent(vtcuptiGrp->evtGrp,
                                         vtcuptiEvt->cuptiEvtID);

      /* everything is fine */
      if(cuptiErr == CUPTI_SUCCESS){
        *(vtcuptiGrp->cuptiEvtIDs) = vtcuptiEvt->cuptiEvtID;
        *(vtcuptiGrp->vtCIDs) = vtcuptiEvt->vtCID;
        vtcuptiGrp->evtNum++;
      }else{
        /* we can at least try to put the event in another group */

        /* too many events in this group or
           event is in different domain or device limitation*/
        if(cuptiErr == CUPTI_ERROR_MAX_LIMIT_REACHED ||
           cuptiErr == CUPTI_ERROR_NOT_COMPATIBLE){

          vt_cntl_msg(2, "[CUPTI] Create another event group for event %d",
                         vtcuptiEvt->cuptiEvtID);

          /* prepend last group to list, if it is not empty */
          if(vtcuptiGrp->evtNum > 0){
            vtcuptiGrp->next = vtcuptiCtx->vtGrpList;
            vtcuptiCtx->vtGrpList = vtcuptiGrp;
          }

          /* create new VampirTrace CUPTI event group */
          vtcuptiGrp = vt_cupti_createEvtGrp(vtcuptiCtx);

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
static vt_cupti_ctx_t* vt_cupti_initCtx(uint32_t ptid, CUcontext cuCtx)
{
  vt_cupti_ctx_t *vtcuptiCtx = NULL;
  uint64_t time;

  vt_cntl_msg(2, "[CUPTI] Initializing VampirTrace CUPTI context (ptid=%d)",
              ptid);
  
  time = vt_pform_wtime();
  vt_enter(ptid, &time, rid_cupti_init);

  /* do not trace CUDA functions invoked here */
  VT_SUSPEND_CUDA_TRACING(ptid);

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
    vt_cupti_dev_t *cuptiDev;

    CHECK_CU_ERROR(cuCtxGetDevice(&cuDev), "cuCtxGetDevice");

    cuErr = cuDeviceComputeCapability(&dev_major, &dev_minor, cuDev);
    CHECK_CU_ERROR(cuErr, "cuDeviceComputeCapability");

    /* check if device capability already listed */
    CUPTI_LOCK();
      cuptiDev = vt_cupti_capList;
    CUPTI_UNLOCK();
    
    cuptiDev = vt_cupti_checkMetricList(cuptiDev, dev_major, dev_minor);
    if(cuptiDev){
      vtcuptiCtx = (vt_cupti_ctx_t*)malloc(sizeof(vt_cupti_ctx_t));
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
      VT_RESUME_CUDA_TRACING(ptid);
      return NULL;
    }
  }

  VT_RESUME_CUDA_TRACING(ptid);

  /* create and add the VampirTrace CUPTI groups to the context */
  vt_cupti_addEvtGrpsToCtx(vtcuptiCtx);

  /* allocate memory for CUPTI counter reads */
  {
    size_t allocSize = vtcuptiCtx->vtGrpList->evtNum;
    
    vtcuptiCtx->counterData = (uint64_t *)malloc(allocSize*sizeof(uint64_t));
    vtcuptiCtx->cuptiEvtIDs = (CUpti_EventID *)malloc(allocSize*sizeof(CUpti_EventID));
  }

  /* add VampirTrace CUPTI context entry to list (as first element) */
  CUPTI_LOCK();
    vtcuptiCtx->next = vtcuptiCtxlist;
    vtcuptiCtxlist = vtcuptiCtx;
  CUPTI_UNLOCK();

  time = vt_pform_wtime();
  vt_exit(ptid, &time);

  return vtcuptiCtx;
}

static void vt_cupti_freeCtx(vt_cupti_ctx_t *vtcuptiCtx)
{
  vt_cupti_grp_t *vtcuptiGrp = vtcuptiCtx->vtGrpList;

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
static vt_cupti_ctx_t* vt_cupti_getCtx(CUcontext cuCtx, uint32_t ptid)
{
  vt_cupti_ctx_t *vtcuptiCtx = NULL;

  /* check, if there has been at least one VampirTrace CUPTI context created */
  if(vtcuptiCtxlist == NULL) vt_cupti_init();

  /* check, if the current VampirTrace thread is enabled for GPU counters */
  if((vt_gpu_prop[ptid] & VTGPU_NO_PC) == VTGPU_NO_PC)
    return NULL;

  /* check if CUDA context is listed (linear search) */
  CUPTI_LOCK();
  vtcuptiCtx = vtcuptiCtxlist;
  while(vtcuptiCtx != NULL){
    if(vtcuptiCtx->cuCtx == cuCtx){
      CUPTI_UNLOCK();
      /*vt_cntl_msg(1, "[CUPTI] host thread %d (MPI rank %d)", ptid, vt_my_trace);*/
      return vtcuptiCtx;
    }
    vtcuptiCtx = vtcuptiCtx->next;
  }
  CUPTI_UNLOCK();

  vt_cntl_msg(2, "[CUPTI] Context for VT tid %d unknown! Creating ... ", ptid);

  vtcuptiCtx = vt_cupti_initCtx(ptid, cuCtx);
  if(vtcuptiCtx != NULL){
    vt_cupti_start(vtcuptiCtx);
  }else{
    /* no performance counters for this thread available */
    vt_gpu_prop[ptid] |= VTGPU_NO_PC;
    vt_cntl_msg(2, "[CUPTI] Could not initialize!");
  }

  return vtcuptiCtx;
}

/*
 * Parse the environment variable for CUPTI metrics (including CUDA device
 * capabilities) and fill the capability metric list.
 *
 * @param capList points to the first element of the capability metric list
 */
static void vt_cupti_fillMetricList(vt_cupti_dev_t *capList)
{
  char *metricString = vt_env_cupti_metrics();
  char *metric_sep = vt_env_metrics_sep();
  char *metric, *metric_cap;

  metric = strtok(metricString, metric_sep);

  while (metric != NULL){
    CUptiResult cuptiErr = CUPTI_SUCCESS;
    vt_cupti_dev_t *cuptiDev = NULL;
    vt_cupti_evt_t *vtcuptiEvt = NULL;
    int metr_major = 0;
    int metr_minor = 0;

    /* try to get CUDA device capability parsed from metric */
    metr_major = atoi(metric);
    metric_cap = strchr(metric+1, '.');
    if(metric_cap){
      metr_minor = atoi(metric_cap+1);
      metric_cap = strchr(metric_cap+1, '_');
    }

    /* check wether device capability is given or not */
    if(metric_cap){
      metric = metric_cap + 1;

      vt_cntl_msg(2, "Metric '%s', %d.%d", metric, metr_major, metr_minor);

      cuptiDev = vt_cupti_checkMetricList(capList, metr_major, metr_minor);
      if(cuptiDev == NULL){
        metric = strtok(NULL, metric_sep);
        continue;
      }
      
      vtcuptiEvt = (vt_cupti_evt_t*)malloc(sizeof(vt_cupti_evt_t));
      cuptiErr = cuptiEventGetIdFromName(cuptiDev->cuDev, metric,
                                         &vtcuptiEvt->cuptiEvtID);
      if(cuptiErr != CUPTI_SUCCESS){
        vt_warning("Skipping invalid event name: %s", metric);
        vt_cupti_showAllCounters(cuptiDev->cuDev);
        metric = strtok(NULL, metric_sep);
        continue;
      }

      /* create VampirTrace counter ID */
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
      vtcuptiEvt->vtCID = vt_def_counter(VT_MASTER_THREAD, metric,
                 VT_CNTR_ABS | VT_CNTR_LAST | VT_CNTR_UNSIGNED, cgid_cupti, "");
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif

      cuptiDev->evtNum++;
      vtcuptiEvt->next = cuptiDev->vtcuptiEvtList;
      cuptiDev->vtcuptiEvtList = vtcuptiEvt;
    }else{ /* try to add metric to all devices */
      uint32_t cid_metric;

      /* create VampirTrace counter ID */
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
      cid_metric = vt_def_counter(VT_MASTER_THREAD,
           metric, VT_CNTR_ABS | VT_CNTR_LAST | VT_CNTR_UNSIGNED, cgid_cupti, "");
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif

      cuptiDev = capList;
      while(cuptiDev != NULL){
        vtcuptiEvt = (vt_cupti_evt_t*)malloc(sizeof(vt_cupti_evt_t));
        cuptiErr = cuptiEventGetIdFromName(cuptiDev->cuDev, metric,
                                           &vtcuptiEvt->cuptiEvtID);

        if(cuptiErr != CUPTI_SUCCESS){
          vt_cntl_msg(2, "[CUPTI] Skipping event '%s' for device %d",
                         metric, cuptiDev->cuDev);
        }else{
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
 * Check wether the CUDA device capability is already listed.
 *
 * @param capList IN: list containing the CUDA device capabilities
 * @param major the major CUDA device capability
 * @param minor the minor CUDA device capability
 *
 * @return pointer to the list entry (NULL if not found)
 */
static vt_cupti_dev_t* vt_cupti_checkMetricList(vt_cupti_dev_t *capList,
                                                int major, int minor)
{
  vt_cupti_dev_t *cuptiDev;

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
 * 
 */
static vt_cupti_dev_t* vt_cupti_setupMetricList(void)
{
  CUresult err;
  int deviceCount, id;
  vt_cupti_dev_t *capList = NULL;

  /* CUDA initialization */
	err = cuInit( 0 );
	if ( err != CUDA_SUCCESS ) {
		printf( "Initialization of CUDA library failed.\n" );
		exit( EXIT_FAILURE );
	}

  /* How many gpgpu devices do we have? */
	err = cuDeviceGetCount( &deviceCount );
	CHECK_CU_ERROR(err, "cuDeviceGetCount");
	if(deviceCount == 0){
		printf("[CUPTI]There is no device supporting CUDA.\n");
		exit(EXIT_FAILURE);
	}

  /* create list with available compute capabilities */
  for(id = 0; id < deviceCount; id++){
    CUdevice cuDev;
    vt_cupti_dev_t *cuptiDev;
    int dev_major, dev_minor;

    err = cuDeviceGet(&cuDev, id);
		CHECK_CU_ERROR(err, "cuDeviceGet");

    err = cuDeviceComputeCapability(&dev_major, &dev_minor, cuDev);
    CHECK_CU_ERROR(err, "cuDeviceComputeCapability");

    /* check if device capability already listed */
    cuptiDev = vt_cupti_checkMetricList(capList, dev_major, dev_minor);

    if(cuptiDev == NULL){
      /* allocate memory for device list entry */
      cuptiDev = (vt_cupti_dev_t *)malloc(sizeof(vt_cupti_dev_t));
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
    vt_cupti_dev_t *curr = capList;
    vt_cupti_dev_t *last = capList;

    while(curr != NULL){
      vt_cupti_dev_t *freeDev = curr;
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
 * Enumerate/Print the available CUPTI events for a given CUDA device and
 * domain.
 *
 * @param cuDev the CUDA device
 * @param domainId the CUPTI event domain
 */
static void enumEvents(CUdevice cuDev, CUpti_EventDomainID domainId)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  /* size_t DESC_SHORT = 512; */
  CUpti_EventID *eventId = NULL;
  uint32_t maxEvents = 0;
  uint32_t i = 0;
  size_t size = 0;

  /* query num of events available in the domain */
  cuptiErr = cuptiEventDomainGetNumEvents(cuDev,
                                          (CUpti_EventDomainID)domainId,
                                          &maxEvents);
  if(cuptiErr == CUPTI_ERROR_INVALID_EVENT_DOMAIN_ID){
    vt_error_msg("Domain Id %d is not supported by device", domainId);
  }else{
    CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventDomainGetNumEvents");
  }

  size = sizeof(CUpti_EventID) * maxEvents;
  eventId = (CUpti_EventID*)malloc(size);
  if(eventId == NULL) vt_error_msg("Failed to allocate memory to event ID");
  memset(eventId, 0, size);

  cuptiErr = cuptiEventDomainEnumEvents(cuDev,
                                        (CUpti_EventDomainID)domainId,
                                        &size,
                                        eventId);
  CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventDomainEnumEvents");

  /* query event info */
  {
    size_t NAME_SHORT = 32;
    char *eventname = (char*)malloc(NAME_SHORT*sizeof(char)); /* event name */
    /*char *shortdesc = malloc(DESC_SHORT*sizeof(char));  short desc of the event */

    for(i = 0; i < maxEvents; i++){
      NAME_SHORT = 32;
      cuptiErr = cuptiEventGetAttribute(cuDev,
                                        eventId[i],
                                        CUPTI_EVENT_ATTR_NAME,
                                        &NAME_SHORT,
                                        eventname);
      CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGetAttribute");

      /*cuptiErr = cuptiEventGetAttribute(cuDev,
                                         eventId[i],
                                         CUPTI_EVENT_ATTR_SHORT_DESCRIPTION,
                                         &DESC_SHORT,
                                         (uint8_t*)shortdesc);
      CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGetAttribute");*/

      vt_cntl_msg(1, "Id:Name   = %d: %s", eventId[i], eventname);
      /*vt_cntl_msg(1, "Shortdesc = %s\n", shortdesc);*/
    }

    free(eventname);
  }

  free(eventId);
}

/*
 * Print all available counters for a given CUDA device to stdout.
 *
 * @param cuDev the CUDA device
 */
static void vt_cupti_showAllCounters(CUdevice cuDev)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  CUpti_EventDomainID *domainId = NULL;
  uint32_t maxDomains = 0;
  uint32_t i;
  size_t size = 0;

  cuptiErr = cuptiDeviceGetNumEventDomains(cuDev, &maxDomains);
  CHECK_CUPTI_ERROR(cuptiErr, "cuptiDeviceGetNumEventDomains");

  if(maxDomains == 0){
    vt_cntl_msg(1, "[CUPTI] No domain is exposed by dev = %d\n", cuDev);
    return;
  }

  size = sizeof(CUpti_EventDomainID) * maxDomains;
  domainId = (CUpti_EventDomainID*)malloc(size);
  if(domainId == NULL){
    vt_cntl_msg(1, "[CUPTI] Failed to allocate memory to domain ID");
    return;
  }
  memset(domainId, 0, size);

  cuptiErr = cuptiDeviceEnumEventDomains(cuDev, &size, domainId);
  CHECK_CUPTI_ERROR(cuptiErr, "cuptiDeviceEnumEventDomains");

  /* enum domains */
  for(i = 0; i < maxDomains; i++) enumEvents(cuDev, domainId[i]);

  free(domainId);
}


static void vt_cupti_start(vt_cupti_ctx_t *vtcuptiCtx)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cupti_grp_t *vtcuptiGrp = NULL;
  vt_cupti_grp_t *lastGrp = NULL;

  if(vtcuptiCtx == NULL) return;

  /* start all groups */
  vtcuptiGrp = vtcuptiCtx->vtGrpList;
  lastGrp = vtcuptiCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    cuptiErr = cuptiEventGroupEnable(vtcuptiGrp->evtGrp);
    
    /* if the event group could not be enabled, remove it */
    if(cuptiErr != CUPTI_SUCCESS){
      size_t i;
      vt_cupti_grp_t *freeGrp = vtcuptiGrp;
      size_t valueSize = 32;
      char name[32];

      vtcuptiGrp = vtcuptiGrp->next;

      /* give user information about the group, which cannot be enabled */
      for(i = 0; i < freeGrp->evtNum; i++){
        cuptiEventGetAttribute(vtcuptiCtx->vtDevCap->cuDev,
                               *(freeGrp->cuptiEvtIDs)+i,
                               CUPTI_EVENT_ATTR_NAME,
                               &valueSize, (char*)name);
        vt_warning("[CUPTI] Event '%s' (%d) cannot be enabled",
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
static void vt_cupti_stop(vt_cupti_ctx_t *vtcuptiCtx)
{
  vt_cupti_grp_t *vtcuptiGrp = NULL;
  /*vt_cntl_msg(1, "[CUPTI] vt_cupti_stop() ... ");*/

  if(vtcuptiCtx == NULL || vt_gpu_debug) return;

  /* stop counter reading for all groups */
  vtcuptiGrp = vtcuptiCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    if(vtcuptiGrp->enabled){
      CUptiResult cuptiErr = CUPTI_SUCCESS;

      cuptiErr = cuptiEventGroupDisable(vtcuptiGrp->evtGrp);
      CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupDisable");

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
static void vt_cupti_finish(vt_cupti_ctx_t *vtcuptiCtx)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;

  if(vtcuptiCtx == NULL || vt_gpu_debug) return;

  /*uint64_t time = vt_pform_wtime();
  vt_cupti_resetCounter(vtcuptiCtx, 0, &time);*/

  /* stop CUPTI counter capturing */
  vt_cupti_stop(vtcuptiCtx);

  /* destroy all CUPTI event groups, which have been created */
  {
    vt_cupti_grp_t *vtcuptiGrp = vtcuptiCtx->vtGrpList;
    
    while(vtcuptiGrp != NULL){
      cuptiErr = cuptiEventGroupRemoveAllEvents(vtcuptiGrp->evtGrp);
      CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupRemoveAllEvents");

      cuptiErr = cuptiEventGroupDestroy(vtcuptiGrp->evtGrp);
      CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupDestroy");

      vtcuptiGrp = vtcuptiGrp->next;
    }
  }
}

/*
 * Searches the requested host thread by its CUDA context and returns
 * the corresponding VampirTrace CUPTI context structure.
 *
 * @param cuCtx pointer to the CUDA context
 *
 * @return VampirTrace CUPTI context structure
 */
static vt_cupti_ctx_t* vt_cupti_takeCtxFromList(CUcontext cuCtx)
{
  vt_cupti_ctx_t *currCtx = NULL;
  vt_cupti_ctx_t *lastCtx = NULL;

  CUPTI_LOCK();
  currCtx = vtcuptiCtxlist;
  lastCtx = vtcuptiCtxlist;
  while(currCtx != NULL){
    if(currCtx->cuCtx == cuCtx){
      /* if first element in list */
      if(currCtx == vtcuptiCtxlist){
        vtcuptiCtxlist = vtcuptiCtxlist->next;
      }else{
        lastCtx->next = currCtx->next;
      }
      CUPTI_UNLOCK();
      return currCtx;
    }
    lastCtx = currCtx;
    currCtx = currCtx->next;
  }
  CUPTI_UNLOCK();

  vt_cntl_msg(2, "[CUPTI] Context structure not found!");
  return NULL;
}


/* ------------------ Implementation of public functions ------------------ */

/*
 * Initialize Mutex, VampirTrace ids and registers the finalize function.
 * This may be done implicitly by vt_cupti_count().
 */
void vt_cupti_init()
{
  if(!vt_cupti_initialized){
#if (defined(VT_MT) || defined(VT_HYB))
    VTThrd_createMutex(&VTThrdMutexCupti);
#endif
    CUPTI_LOCK();
    if(!vt_cupti_initialized){
      vt_cntl_msg(2, "[CUPTI] Initializing ... ");

      /* create VampirTrace counter group ID only once */
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
#endif
      rid_cupti_init = vt_def_region(VT_MASTER_THREAD, "vtcuptiHostThreadInit",
                      VT_NO_ID, VT_NO_LNO, VT_NO_LNO, "VT_CUPTI", VT_FUNCTION);

      cgid_cupti = vt_def_counter_group(VT_MASTER_THREAD, "CUPTI");
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_UNLOCK_IDS();
#endif

      vt_cupti_capList = vt_cupti_setupMetricList();

      /* register the finalize function of the CUDA wrapper to be called before
       * the program exits and CUDA has done its implicit clean-up */
      atexit(vt_cupti_finalize);

      vt_cupti_initialized = 1;
      CUPTI_UNLOCK();
    }
  }
}

/*
 * Finalizes the VampirTrace CUPTI implementation.
 */
void vt_cupti_finalize()
{
  if(!vt_cupti_finalized){

    CUPTI_LOCK();
    if(!vt_cupti_finalized){

      vt_cntl_msg(2, "[CUPTI] Finalizing ...");

      /* free VampirTrace CUPTI context structures (should already be freed) */
      while(vtcuptiCtxlist != NULL){
        vt_cupti_ctx_t *tmp =  vtcuptiCtxlist;

        vt_cupti_finish(vtcuptiCtxlist);

        vtcuptiCtxlist = vtcuptiCtxlist->next;

        free(tmp);
        tmp = NULL;
      }

      /* free capability metric list */
      while(vt_cupti_capList != NULL){
        vt_cupti_dev_t *tmp = vt_cupti_capList;
        vt_cupti_capList = vt_cupti_capList->next;
        
        /* free VampirTrace CUPTI events */
        while(tmp->vtcuptiEvtList != NULL){
          vt_cupti_evt_t *tmpEvt = tmp->vtcuptiEvtList;
          tmp->vtcuptiEvtList = tmp->vtcuptiEvtList->next;
          free(tmpEvt);
          tmpEvt = NULL;
        }

        free(tmp);
        tmp = NULL;
      }

      vt_cupti_finalized = 1;
      CUPTI_UNLOCK();

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
vt_cupti_ctx_t* vt_cupti_getCurrentContext(uint32_t ptid)
{
  CUcontext cuCtx = NULL;
  
  if(!vt_cupti_initialized) vt_cupti_init();

  VT_SUSPEND_CUDA_TRACING(ptid);

# if (defined(CUDA_VERSION) && (CUDA_VERSION < 4000))
  CHECK_CU_ERROR(cuCtxPopCurrent(&cuCtx), "cuCtxPopCurrent");
  CHECK_CU_ERROR(cuCtxPushCurrent(cuCtx), "cuCtxPushCurrent");
# else
  CHECK_CU_ERROR(cuCtxGetCurrent(&cuCtx), "cuCtxGetCurrent");
# endif

  VT_RESUME_CUDA_TRACING(ptid);
  
  if(cuCtx == NULL) {
    vt_cntl_msg(2, "[CUPTI] No context is bound to the calling CPU thread", cuCtx);
    return NULL;
  }
  
  return vt_cupti_getCtx(cuCtx, ptid);
}

/*
 * Request the CUTPI counter values and write it to the given VampirTrace
 * stream with the given timestamps.
 *
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 * @param strmid the stream id for the counter values
 * @param time the VampirTrace timestamps
 */
void vt_cupti_writeCounter(vt_cupti_ctx_t *vtcuptiCtx, uint32_t strmid,
                           uint64_t *time)
{
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cupti_grp_t *vtcuptiGrp = NULL;

  size_t bufferSizeBytes;
  size_t arraySizeBytes;
  size_t numCountersRead;

  if(vtcuptiCtx == NULL){
    VT_CHECK_THREAD;
    vtcuptiCtx = vt_cupti_getCurrentContext(VT_MY_THREAD);
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
      CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupReadAllEvents");

      if(vtcuptiGrp->evtNum != numCountersRead){
        vt_error_msg("[CUPTI] %d counter reads, %d metrics specified in "
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
void vt_cupti_resetCounter(vt_cupti_ctx_t *vtcuptiCtx, uint32_t strmid,
                           uint64_t *time)
{
  size_t i;
  vt_cupti_grp_t *vtcuptiGrp = NULL;

  if(vtcuptiCtx == NULL){
    VT_CHECK_THREAD;
    vtcuptiCtx = vt_cupti_getCurrentContext(VT_MY_THREAD);
    if(vtcuptiCtx == NULL) return;
  }

  vtcuptiGrp = vtcuptiCtx->vtGrpList;
  while(vtcuptiGrp != NULL){
    for(i = 0; i < vtcuptiGrp->evtNum; i++){
      vt_count(strmid, time, *(vtcuptiGrp->vtCIDs+i), 0);
    }

    /* reset counter values of this group */
    CHECK_CUPTI_ERROR(cuptiEventGroupResetAllEvents(vtcuptiGrp->evtGrp),
                      "cuptiEventGroupResetAllEvents");
    
    vtcuptiGrp = vtcuptiGrp->next;
  }
}

/*
 * Finalizes CUPTI device.
 * 
 * @param cleanExit 1 to cleanup CUPTI event group, otherwise 0
 */
void vt_cupti_finalize_device(uint32_t ptid, uint8_t cleanExit){
  CUptiResult cuptiErr = CUPTI_SUCCESS;
  vt_cupti_ctx_t *vtcuptiCtx = NULL;

  vt_cntl_msg(2, "[CUPTI] Finalize device ... ");

  {
    CUcontext cuCtx = NULL;

    VT_SUSPEND_CUDA_TRACING(ptid);
    
#if (defined(CUDA_VERSION) && (CUDA_VERSION < 4000))
    CHECK_CU_ERROR(cuCtxPopCurrent(&cuCtx), "cuCtxPopCurrent");
    CHECK_CU_ERROR(cuCtxPushCurrent(cuCtx), "cuCtxPushCurrent");
#else
    CHECK_CU_ERROR(cuCtxGetCurrent(&cuCtx), "cuCtxGetCurrent");
#endif
    
    VT_RESUME_CUDA_TRACING(ptid);

    vtcuptiCtx = vt_cupti_takeCtxFromList(cuCtx);
    if(vtcuptiCtx == NULL) return;
  }

  if(cleanExit && vt_gpu_debug != 0){
    /*uint64_t time = vt_pform_wtime();

    vt_cupti_resetCounter(vtcuptiCtx, 0, &time);*/

    /* stop CUPTI counter capturing */
    vt_cupti_stop(vtcuptiCtx);

    /* destroy all CUPTI event groups, which have been created */
    {
      vt_cupti_grp_t *vtcuptiGrp = vtcuptiCtx->vtGrpList;

      while(vtcuptiGrp != NULL){
        cuptiErr = cuptiEventGroupRemoveAllEvents(vtcuptiGrp->evtGrp);
        CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupRemoveAllEvents");

        cuptiErr = cuptiEventGroupDestroy(vtcuptiGrp->evtGrp);
        CHECK_CUPTI_ERROR(cuptiErr, "cuptiEventGroupDestroy");

        vtcuptiGrp = vtcuptiGrp->next;
      }
    }
  }

  /* free VampirTrace CUPTI context */
  vt_cupti_freeCtx(vtcuptiCtx);
}

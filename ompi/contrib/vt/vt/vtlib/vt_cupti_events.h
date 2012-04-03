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

#ifndef VT_CUPTI_EVENTS_H
#define	VT_CUPTI_EVENTS_H

#if (defined(VT_CUPTI_EVENTS))

#include "vt_cupti.h"
#include "vt_inttypes.h"    /* VampirTrace integer types */

/* 
 * VampirTrace CUPTI event (single linked list element) 
 */
typedef struct vtcuptievtevt_st
{
  CUpti_EventID cuptiEvtID;             /**< CUPTI event ID */
  uint32_t vtCID;                       /**< VampirTrace counter ID */
  /*CUpti_EventDomainID cuptiDomainID;    *< CUPTI domain ID */
  struct vtcuptievtevt_st *next;
}vt_cuptievt_evt_t;

/* 
 * Structure that stores events to be trace for specific device capability 
 * (single linked list element)
 */
typedef struct vtcuptievtdev_st
{
  int dev_major;    /**< Major CUDA device capability */
  int dev_minor;    /**< Minor CUDA device capability */
  CUdevice cuDev;   /**< CUDA device */
  vt_cuptievt_evt_t *vtcuptiEvtList; /**< list of events to be traced for this device*/
  size_t evtNum;    /**< Number of tracable CUPTI events */
  struct vtcuptievtdev_st *next;
}vt_cuptievt_dev_t;

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
}vt_cuptievt_grp_t;

/* 
 * The VampirTrace CUPTI context has the CUDA context as key and contains
 * further information about its device and counters.
 */
typedef struct vtcuptievtctx_st
{
  CUcontext cuCtx;            /**< CUDA context (primary key) */
  vt_cuptievt_dev_t *vtDevCap;   /**< pointer to device capability (events, ...) */
  vt_cuptievt_grp_t *vtGrpList;  /**< list of VT CUPTI event groups */
  uint64_t *counterData;      /**< preallocated buffer for counter data */
  CUpti_EventID *cuptiEvtIDs; /**< preallocated buffer for CUPTI event IDs*/
  struct vtcuptievtctx_st *next;
}vt_cuptievt_ctx_t;

/*
 * Initialize Mutex, VampirTrace IDs and registers the finalize function.
 * This may be done implicitly by vt_cuptievt_count().
 */
void vt_cupti_events_init(void);

/*
 * Finalizes the VampirTrace CUPTI implementation.
 */
void vt_cupti_events_finalize(void);

/*
 * Finalizes CUPTI device.
 * 
 * @param ptid the VampirTrace process/thread id
 * @param cleanExit 1 to cleanup CUPTI event group, otherwise 0
 */
void vt_cuptievt_finalize_device(uint32_t ptid, uint8_t cleanExit);


/*
 * Returns the VampirTrace CUPTI context for the CUDA context associated with
 * the calling host thread.
 *
 * @param ptid the VampirTrace thread id of the calling host thread
 */
vt_cuptievt_ctx_t* vt_cuptievt_getCurrentContext(uint32_t ptid);

/*
 * Request the CUTPI counter values and write it to the given VampirTrace
 * stream with the given timestamps.
 *
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 * @param strmid the stream id for the counter values
 * @param time the VampirTrace timestamps
 */
void vt_cuptievt_writeCounter(vt_cuptievt_ctx_t *vtcuptiCtx, uint32_t strmid, 
                              uint64_t *time);

/*
 * Reset the VampirTrace counter values (to zero) for active CUPTI counters.
 *
 * @param vtcuptiCtx pointer to the VampirTrace CUPTI context
 * @param strmid the stream id for the counter values
 * @param time the VampirTrace timestamps
 */
void vt_cuptievt_resetCounter(vt_cuptievt_ctx_t *vtcuptiCtx, uint32_t strmid, 
                              uint64_t *time);

#endif /* VT_CUPTI_EVENTS */

#endif	/* VT_CUPTI_EVENTS_H */


/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2013-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Ian Bradley Morgan and Anthony Skjellum. All
 *                         rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 */
#include "pnbc_osc_internal.h"
#include "ompi/op/op.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/win/win.h"
#include "ompi/mca/osc/osc.h"

/* only used in this file */
static inline int PNBC_OSC_Start_round(PNBC_OSC_Handle *handle);

/* #define PNBC_OSC_TIMING */

#ifdef PNBC_OSC_TIMING
static double Iput_time=0,Iget_time=0,Isend_time=0, Irecv_time=0, Wait_time=0, Test_time=0;
void PNBC_OSC_Reset_times() {
  Iwfree_time=Iput_time=Iget_time=Isend_time=Irecv_time=Wait_time=Test_time=0;
}

void PNBC_OSC_Print_times(double div) {
  printf("*** PNBC_OSC_TIMES: Isend: %lf, Irecv: %lf, Wait: %lf, Test: %lf\n",
         Isend_time*1e6/div, Irecv_time*1e6/div,
         Wait_time*1e6/div, Test_time*1e6/div);
}
#endif


/* finishes a request
 *
 * to be called *only* from the progress thread !!! */
static inline void PNBC_OSC_Free (PNBC_OSC_Handle* handle) {

  if (NULL != handle->schedule) {
    /* release schedule */
    OBJ_RELEASE (handle->schedule);
    handle->schedule = NULL;
  }

  if (NULL != handle->tmpbuf) {
    free((void*)handle->tmpbuf);
    handle->tmpbuf = NULL;
  }
}

/* progresses a request
 *
 * to be called *only* from the progress thread !!! */
int PNBC_OSC_Progress(PNBC_OSC_Handle *handle) {
  int res, ret=PNBC_OSC_CONTINUE;
  bool round_is_complete;
  unsigned long size = 0;
  char *delim;

  /* bozo case */
  if (handle->nbc_complete) {
    return PNBC_OSC_OK;
  }

  round_is_complete = true;

  /* figure out whether the current round is request-based or flag-based */
  switch (handle->schedule->rounds[handle->current_round]->round_type) {

/******/
  case PNBC_OSC_ROUND_FLAG_BASED: {
/******/
    PNBC_OSC_Round_flag_based *round = (PNBC_OSC_Round_flag_based*)(handle->schedule->rounds[handle->current_round]);

    for (int f=0;f<round->number_of_flags;++f) {
      if (round->flags[f] != 0) round_is_complete = false;
    }

    break;
  }

/******/
  case PNBC_OSC_ROUND_REQUEST_BASED: {
/******/
    PNBC_OSC_Round_request_based *round = (PNBC_OSC_Round_request_based*)(handle->schedule->rounds[handle->current_round]);

    PNBC_OSC_DEBUG(50, "PNBC_OSC_Progress: testing for %i requests\n", round->number_of_requests);
#ifdef PNBC_OSC_TIMING
    Test_time -= MPI_Wtime();
#endif
    /* don't call ompi_request_test_all as it causes a recursive call into opal_progress */
    for(int r=0;r<round->number_of_requests;++r) {
      ompi_request_t *subreq = round->requests[r];

      if (REQUEST_COMPLETE(subreq)) {

        if(OPAL_UNLIKELY( OMPI_SUCCESS != subreq->req_status.MPI_ERROR )) {
          PNBC_OSC_Error("MPI Error in PNBC_OSC subrequest %p : %d",
                         subreq, subreq->req_status.MPI_ERROR);
          /* copy the error code from the sub-request and let the round continue/finish */
          handle->super.req_status.MPI_ERROR = subreq->req_status.MPI_ERROR;
        }
        if (subreq->req_persistent == false) {
          ompi_request_free(&subreq); // only for nbc subreq; avoid for pnbc subreq
        }

      } else {
        round_is_complete = false;
      }
    }
#ifdef PNBC_OSC_TIMING
    Test_time += MPI_Wtime();
#endif

    /* previous round had an error */
    if (OPAL_UNLIKELY(OMPI_SUCCESS != handle->super.req_status.MPI_ERROR)) {
      res = handle->super.req_status.MPI_ERROR;
      PNBC_OSC_Error("PNBC_OSC_Progress: an error %d was found during schedule %p at row-offset %li - aborting the schedule\n", res, handle->schedule, handle->schedule->row_offset);
      handle->nbc_complete = true;
      if (!handle->super.req_persistent) {
        PNBC_OSC_Free(handle);
      }
      return res;
    }

    break;
  }

/******/
  default:
/******/
    PNBC_OSC_Error ("PNBC_OSC Error unknown round type (%i) in round %d for schedule %p in request %p",
                    handle->schedule->rounds[handle->current_round]->round_type,
                    handle->current_round, handle->schedule, handle);

    break;
  }

  /* a round has just finished */
  if (round_is_complete) {

//DJH// the rest of this function needs lots of work

//DJH// keep req_array per-round and do not free it until the top-level request is freed, re-use space for next start
    /* reset handle for next round */
    if (NULL != handle->req_array) {
      /* free request array */
      free (handle->req_array);
      handle->req_array = NULL;
    }
    handle->req_count = 0;

    /* adjust delim to start of current round */
    PNBC_OSC_DEBUG(5, "PNBC_OSC_Progress: going in schedule %p to row-offset: %li\n",
                   handle->schedule, handle->schedule->row_offset);
    delim = handle->schedule->data + handle->schedule->row_offset;
    PNBC_OSC_DEBUG(10, "delim: %p\n", delim);
    PNBC_OSC_Get_round_size(delim, &size);
    PNBC_OSC_DEBUG(10, "size: %li\n", size);
    /* adjust delim to end of current round -> delimiter */
    delim = delim + size;

    if (*delim == 0) {
      /* this was the last round - we're done */
      PNBC_OSC_DEBUG(5, "PNBC_OSC_Progress last round finished - we're done\n");

      handle->nbc_complete = true;
      if (!handle->super.req_persistent) {
        PNBC_OSC_Free(handle);
      }

      return PNBC_OSC_OK;
    }

    PNBC_OSC_DEBUG(5, "PNBC_OSC_Progress round finished - goto next round\n");
    /* move delim to start of next round */
    /* initializing handle for new virgin round */
    handle->schedule->row_offset = (intptr_t) (delim + 1) - (intptr_t) handle->schedule->data;
    /* kick it off */
    res = PNBC_OSC_Start_round(handle);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      PNBC_OSC_Error ("Error in PNBC_OSC_Start_round() (%i)", res);
      return res;
    }
  }

  return ret;
}


static inline int PNBC_OSC_Start_round(PNBC_OSC_Handle *handle) {
  int res;

  int num; /* number of operations in this round */
  char *ptr;
  void *buf1, *buf2;
  PNBC_OSC_Fn_type type;
  PNBC_OSC_Args_put       putargs;
  PNBC_OSC_Args_get       getargs;
  PNBC_OSC_Args_op         opargs;
  PNBC_OSC_Args_copy     copyargs;
  PNBC_OSC_Args_unpack unpackargs;

  /* get round-schedule address */
  ptr = handle->schedule->data + handle->schedule->row_offset;

  PNBC_OSC_GET_BYTES(ptr,num);
  PNBC_OSC_DEBUG(10, "start_round round at offset %d : posting %i operations\n",
                 handle->schedule->row_offset, num);

  handle->req_count = 0;

  for (int i = 0 ; i < num ; ++i) {
    long offset = (intptr_t)(ptr - handle->schedule->data);

    // peek at the first part of the next args struct, which will be its type field
    memcpy(&type, ptr, sizeof(type));

    switch(type) {

/***************/
    case PUT:
/***************/
      PNBC_OSC_DEBUG(5,"  PUT (offset %li) ", offset);
      PNBC_OSC_GET_BYTES(ptr,putargs);
      PNBC_OSC_DEBUG(5,"*buf: %p, origin count: %i, origin type: %p, target: %i, target count: %i, target type: %p, target displ: %lu)\n",
                     putargs.buf, putargs.origin_count, putargs.origin_datatype, putargs.target,
                                  putargs.target_count, putargs.target_datatype, putargs.target_displ);

#ifdef PNBC_OSC_TIMING
      Iput_time -= MPI_Wtime();
#endif

      res = handle->win->w_osc_module->osc_rput(putargs.buf,
                                                putargs.origin_count, putargs.origin_datatype,
                                                putargs.target, putargs.target_displ,
                                                putargs.target_count, putargs.target_datatype,
                                                handle->win, &(handle->req_array[handle->req_count]));
      handle->req_count++;

      if (OMPI_SUCCESS != res) {
        PNBC_OSC_Error ("Error in osc_rput(%p, %i, %p, %i, %i, %p, %lu, %lu) (%i)",
                        putargs.buf,
                        putargs.origin_count, putargs.origin_datatype, putargs.target,
                        putargs.target_count, putargs.target_datatype, putargs.target_displ,
                        (unsigned long)handle->comm, res);
        return res;
      }

#ifdef PNBC_OSC_TIMING
      Iput_time += MPI_Wtime();
#endif

      break;

/***************/
    case GET:
/***************/
      PNBC_OSC_DEBUG(5,"  GET (offset %li) ", offset);
      PNBC_OSC_GET_BYTES(ptr,getargs);
      PNBC_OSC_DEBUG(5,"*buf: %p, origin count: %i, origin type: %p, target: %i, target count: %i, target type: %p, target displ: %lu)\n",
                     getargs.buf, getargs.origin_count, getargs.origin_datatype, getargs.target,
                                  getargs.target_count, getargs.target_datatype, getargs.target_displ);

#ifdef PNBC_OSC_TIMING
      Iget_time -= MPI_Wtime();
#endif

      res = handle->win->w_osc_module->osc_rget(getargs.buf,
                                                getargs.origin_count, getargs.origin_datatype,
                                                getargs.target, getargs.target_displ,
                                                getargs.target_count, getargs.target_datatype,
                                                handle->win, &(handle->req_array[handle->req_count]));
      handle->req_count++;

      if (OMPI_SUCCESS != res) {
        PNBC_OSC_Error ("Error in osc_rget(%p, %i, %p, %i, %lu, %i, %p, %lu) (%i)",
                        getargs.buf,
                        getargs.origin_count, getargs.origin_datatype, getargs.target,
                        getargs.target_count, getargs.target_datatype, getargs.target_displ,
                        (unsigned long)handle->comm, res);
        return res;
      }

#ifdef PNBC_OSC_TIMING
      Iget_time += MPI_Wtime();
#endif

      break;

/***************/
//    case TRY_GET:
/***************/
//      PNBC_OSC_DEBUG(10,"  TRY_GET (offset %li) ", offset);
//      PNBC_OSC_GET_BYTES(ptr,trygetargs);
//      PNBC_OSC_DEBUG(10,"*buf: %p, origin count: %i, origin type: %p, target: %i, target disp: %i, "
//                     "target count: %i, target type: %p, notification: %i)\n",
//                     trygetargs.buf, trygetargs.origin_count, trygetargs.origin_datatype,
//                     trygetargs.target, trygetargs.target_disp, trygetargs.target_count,
//                     trygetargs.target_datatype, trygetargs.notify);
//
//      /* get an additional request */
//      handle->req_count++;
//      /* get buffer */
//      if(trygetargs.tmpbuf) {
//        buf1=(char*)handle->tmpbuf+(long)trygetargs.buf;
//      } else {
//        buf1=(void *)trygetargs.buf;
//      }
//
//#ifdef PNBC_OSC_TIMING
//      Iget_time -= MPI_Wtime();
//#endif
//
//      /* [state is unlocked] -> we try to lock */
//      if( UNLOCKED == trygetargs.lock_status ){
//        /* notification */
//        if(trygetargs.notify == true ){
///* DJH 2020_09_25 temporarily removed to enable compilation
//          res = handle->winflag->w_osc_module->osc_try_lock(trygetargs.lock_type, trygetargs.target,
//                                                            trygetargs.assert, handle->winflag);
//*/
//          if(OMPI_SUCCESS == res){
//            trygetargs.lock_status = LOCKED;
//            res = handle->winflag->w_osc_module->osc_get(buf1, trygetargs.origin_count,
//                                                         trygetargs.origin_datatype,
//                                                         trygetargs.target ,trygetargs.target_disp,
//                                                         trygetargs.target_count,
//                                                         trygetargs.target_datatype, handle->winflag);
//
//          }
//
//          /* getting data */
//        } else {
//
///* DJH 2020_09_25 temporarily removed to enable compilation
//          res = handle->win->w_osc_module->osc_try_lock(trygetargs.lock_type, trygetargs.target,
//                                                        trygetargs.assert, handle->win);
//*/
//          if(OMPI_SUCCESS == res){
//            trygetargs.lock_status = LOCKED;
//            res = handle->win->w_osc_module->osc_get(buf1, trygetargs.origin_count,
//                                                     trygetargs.origin_datatype,
//                                                     trygetargs.target ,trygetargs.target_disp,
//                                                     trygetargs.target_count,
//                                                     trygetargs.target_datatype, handle->win);
//
//          }
//        }
//
//        /* something went wrong */
//        if (OMPI_SUCCESS != res){
//          /* return error code */
//          PNBC_OSC_Error ("Error in MPI_try_get(%lu, %i, %p, %i, %i, %p, %lu) (%i)",
//                          (unsigned long)buf1,
//                          trygetargs.origin_count, trygetargs.origin_datatype, trygetargs.target,
//                          trygetargs.target_count, trygetargs.target_datatype,
//                          (unsigned long)handle->comm, res);
//
//          /* return error */
//          return res;
//        }
//      }
//
//      /* [state is locked] -> we try to unlock */
//      if( LOCKED == trygetargs.lock_status ){
//        /* notification */
///* DJH 2020_09_25 temporarily removed to enable compilation
//        if(trygetargs.notify == true ){
//          res = handle->winflag->w_osc_module->osc_try_unlock(trygetargs.target, handle->winflag);
//        }else{
//          res = handle->win->w_osc_module->osc_try_unlock(trygetargs.target, handle->win);
//        }
//*/
//        if (OMPI_SUCCESS != res){
//
//          /* return error code */
//          PNBC_OSC_Error ("Error in MPI_try_unlock(%i) (%i)",
//                          trygetargs.target, res);
//          return res;
//        }
//      }
//
//      break;

/***************/
    case OP:
/***************/
      PNBC_OSC_DEBUG(5, "  OP2  (offset %li) ", offset);
      PNBC_OSC_GET_BYTES(ptr,opargs);
      PNBC_OSC_DEBUG(5, "*buf1: %p, buf2: %p, count: %i, type: %p)\n", opargs.buf1, opargs.buf2,
                opargs.count, opargs.datatype);
      /* get buffers */
      if(opargs.tmpbuf1) {
        buf1=(char*)handle->tmpbuf+(long)opargs.buf1;
      } else {
        buf1=(void *)opargs.buf1;
      }
      if(opargs.tmpbuf2) {
        buf2=(char*)handle->tmpbuf+(long)opargs.buf2;
      } else {
        buf2=opargs.buf2;
      }
      ompi_op_reduce(opargs.op, buf1, buf2, opargs.count, opargs.datatype);
      break;
    case COPY:
      PNBC_OSC_DEBUG(5, "  COPY   (offset %li) ", offset);
      PNBC_OSC_GET_BYTES(ptr,copyargs);
      PNBC_OSC_DEBUG(5, "*src: %lu, srccount: %i, srctype: %p, *tgt: %lu, tgtcount: %i, tgttype: %p)\n",
                (unsigned long) copyargs.src, copyargs.srccount, copyargs.srctype,
                (unsigned long) copyargs.tgt, copyargs.tgtcount, copyargs.tgttype);
      /* get buffers */
      if(copyargs.tmpsrc) {
        buf1=(char*)handle->tmpbuf+(long)copyargs.src;
      } else {
        buf1=copyargs.src;
      }
      if(copyargs.tmptgt) {
        buf2=(char*)handle->tmpbuf+(long)copyargs.tgt;
      } else {
        buf2=copyargs.tgt;
      }
      res = PNBC_OSC_Copy (buf1, copyargs.srccount, copyargs.srctype, buf2, copyargs.tgtcount, copyargs.tgttype,
                           handle->comm);
      if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
        return res;
      }
      break;
      
/***************/
    case UNPACK:
/***************/
      PNBC_OSC_DEBUG(5, "  UNPACK   (offset %li) ", offset);
      PNBC_OSC_GET_BYTES(ptr,unpackargs);
      PNBC_OSC_DEBUG(5, "*src: %lu, srccount: %i, srctype: %p, *tgt: %lu\n", (unsigned long) unpackargs.inbuf,
                     unpackargs.count, unpackargs.datatype, (unsigned long) unpackargs.outbuf);
      /* get buffers */
      if(unpackargs.tmpinbuf) {
        buf1=(char*)handle->tmpbuf+(long)unpackargs.inbuf;
      } else {
        buf1=unpackargs.inbuf;
      }
      if(unpackargs.tmpoutbuf) {
        buf2=(char*)handle->tmpbuf+(long)unpackargs.outbuf;
      } else {
        buf2=unpackargs.outbuf;
      }
      res = PNBC_OSC_Unpack (buf1, unpackargs.count, unpackargs.datatype, buf2, handle->comm);
      if (OMPI_SUCCESS != res) {
        PNBC_OSC_Error ("NBC_Unpack() failed (code: %i)", res);
        return res;
      }
      break;

/***************/
    case WIN_FREE:
/***************/
      PNBC_OSC_DEBUG(5,"  WIN_FREE (offset %li) ", offset);

#ifdef PNBC_OSC_TIMING
      Iwfree_time -= MPI_Wtime();
#endif

      res = handle->win->w_osc_module->osc_free(handle->win);
      if (OMPI_SUCCESS != res) {
        PNBC_OSC_Error ("Error in win_free");
        return res;
      }

#ifdef PNBC_OSC_TIMING
      Iwfree_time += MPI_Wtime();
#endif

      break;

/***************/
    default:
/***************/
      PNBC_OSC_Error ("PNBC_OSC_Start_round: bad type %li at offset %li", (long)type, offset);
      return OMPI_ERROR;
    }
  }

  /* check if we can make progress - not in the first round, this allows us to leave the
   * initialization faster and to reach more overlap
   *
   * threaded case: calling progress in the first round can lead to a
   * deadlock if PNBC_OSC_Free is called in this round :-( */
  if (handle->schedule->row_offset) {
    res = PNBC_OSC_Progress(handle);
    if ((PNBC_OSC_OK != res) && (PNBC_OSC_CONTINUE != res)) {
      return OMPI_ERROR;
    }
  }

  return OMPI_SUCCESS;
}

void PNBC_OSC_Return_handle(ompi_coll_libpnbc_osc_request_t *request) {
  PNBC_OSC_Free (request);
  OMPI_COLL_LIBPNBC_OSC_REQUEST_RETURN(request);
}

int PNBC_OSC_Start(PNBC_OSC_Handle *handle) {
  int res;

  /* bozo case */
  if ((ompi_request_t *)handle == &ompi_request_empty) {
    return OMPI_SUCCESS;
  }

  /* kick off first round */
  handle->super.req_state = OMPI_REQUEST_ACTIVE;
  handle->super.req_status.MPI_ERROR = OMPI_SUCCESS;
  res = PNBC_OSC_Start_round(handle);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  OPAL_THREAD_LOCK(&mca_coll_libpnbc_osc_component.lock);
  opal_list_append(&mca_coll_libpnbc_osc_component.active_requests, &(handle->super.super.super));
  OPAL_THREAD_UNLOCK(&mca_coll_libpnbc_osc_component.lock);

  return OMPI_SUCCESS;
}


/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 */
#include "pnbc_internal.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/op/op.h"
#include "ompi/mca/pml/pml.h"

/* only used in this file */
static inline int PNBC_Start_round(PNBC_Handle *handle);

/* #define PNBC_TIMING */

#ifdef PNBC_TIMING
static double Isend_time=0, Irecv_time=0, Wait_time=0, Test_time=0;
void PNBC_Reset_times() {
  Isend_time=Irecv_time=Wait_time=Test_time=0;
}
void PNBC_Print_times(double div) {
  printf("*** PNBC_TIMES: Isend: %lf, Irecv: %lf, Wait: %lf, Test: %lf\n", Isend_time*1e6/div, Irecv_time*1e6/div, Wait_time*1e6/div, Test_time*1e6/div);
}
#endif

static void pnbc_schedule_constructor (PNBC_Schedule *schedule) {
  /* initial total size of the schedule */
  schedule->size = sizeof (int);
  schedule->current_round_offset = 0;
  schedule->data = calloc (1, schedule->size);
}

static void pnbc_schedule_destructor (PNBC_Schedule *schedule) {
  PNBC_DEBUG(5, "** pnbc_schedule_destructor FREEING schedule data! **\n");
  free (schedule->data);
  schedule->data = NULL;
}

OBJ_CLASS_INSTANCE(PNBC_Schedule, opal_object_t, pnbc_schedule_constructor,
                   pnbc_schedule_destructor);

static int pnbc_schedule_grow (PNBC_Schedule *schedule, int additional) {
  void *tmp;
  int size;

  /* get current size of schedule */
  size = pnbc_schedule_get_size (schedule);

  tmp = realloc (schedule->data, size + additional);
  if (NULL == tmp) {
    PNBC_Error ("Could not increase the size of NBC schedule");
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  schedule->data = tmp;
  return OMPI_SUCCESS;
}

static int pnbc_schedule_round_append (PNBC_Schedule *schedule, void *data, int data_size, bool barrier) {
  int ret, size = pnbc_schedule_get_size (schedule);

  if (barrier) {
    ret = pnbc_schedule_grow (schedule, data_size + 1 + sizeof (int));
  } else {
    ret = pnbc_schedule_grow (schedule, data_size);
  }
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  /* append to the round-schedule */
  if (data_size) {
    memcpy (schedule->data + size, data, data_size);

    /* increase number of elements in round-schedule */
    pnbc_schedule_inc_round (schedule);

    /* increase size of schedule */
    pnbc_schedule_inc_size (schedule, data_size);
  }

  if (barrier) {
    /* add the barrier */
    schedule->data[size + data_size] = 1;
    /* set next round counter to 0 */
    memset (schedule->data + size + data_size + 1, 0, sizeof (int));

    PNBC_DEBUG(10, "ended round at byte %i\n", size + data_size + 1);

    schedule->current_round_offset = size + data_size + 1;

    /* increase size of schedule */
    pnbc_schedule_inc_size (schedule, sizeof (int) + 1);
  }

  return OMPI_SUCCESS;
}

/* this function puts a send into the schedule */
static int PNBC_Sched_send_internal (const void* buf, char tmpbuf, int count, MPI_Datatype datatype, int dest, bool local, PNBC_Schedule *schedule, bool barrier) {
  PNBC_Args_send send_args;
  int ret;

  /* store the passed arguments */
  send_args.type = SEND;
  send_args.buf = buf;
  send_args.tmpbuf = tmpbuf;
  send_args.count = count;
  send_args.datatype = datatype;
  send_args.dest = dest;
  send_args.local = local;

  /* append to the round-schedule */
  ret = pnbc_schedule_round_append (schedule, &send_args, sizeof (send_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_DEBUG(10, "added send - ends at byte %i\n", pnbc_schedule_get_size (schedule));

  return OMPI_SUCCESS;
}

int PNBC_Sched_send (const void* buf, char tmpbuf, int count, MPI_Datatype datatype, int dest, PNBC_Schedule *schedule, bool barrier) {
  return PNBC_Sched_send_internal (buf, tmpbuf, count, datatype, dest, false, schedule, barrier);
}

int PNBC_Sched_local_send (const void* buf, char tmpbuf, int count, MPI_Datatype datatype, int dest, PNBC_Schedule *schedule, bool barrier) {
  return PNBC_Sched_send_internal (buf, tmpbuf, count, datatype, dest, true, schedule, barrier);
}

/* this function puts a receive into the schedule */
static int PNBC_Sched_recv_internal (void* buf, char tmpbuf, int count, MPI_Datatype datatype, int source, bool local, PNBC_Schedule *schedule, bool barrier) {
  PNBC_Args_recv recv_args;
  int ret;

  /* store the passed arguments */
  recv_args.type = RECV;
  recv_args.buf = buf;
  recv_args.tmpbuf = tmpbuf;
  recv_args.count = count;
  recv_args.datatype = datatype;
  recv_args.source = source;
  recv_args.local = local;

  /* append to the round-schedule */
  ret = pnbc_schedule_round_append (schedule, &recv_args, sizeof (recv_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_DEBUG(10, "added receive - ends at byte %d\n", pnbc_schedule_get_size (schedule));

  return OMPI_SUCCESS;
}

int PNBC_Sched_recv (void* buf, char tmpbuf, int count, MPI_Datatype datatype, int source, PNBC_Schedule *schedule, bool barrier) {
  return PNBC_Sched_recv_internal(buf, tmpbuf, count, datatype, source, false, schedule, barrier);
}

int PNBC_Sched_local_recv (void* buf, char tmpbuf, int count, MPI_Datatype datatype, int source, PNBC_Schedule *schedule, bool barrier) {
  return PNBC_Sched_recv_internal(buf, tmpbuf, count, datatype, source, true, schedule, barrier);
}

/* this function puts an operation into the schedule */
int PNBC_Sched_op (const void* buf1, char tmpbuf1, void* buf2, char tmpbuf2, int count, MPI_Datatype datatype,
                  MPI_Op op, PNBC_Schedule *schedule, bool barrier) {
  PNBC_Args_op op_args;
  int ret;

  /* store the passed arguments */
  op_args.type = OP;
  op_args.buf1 = buf1;
  op_args.buf2 = buf2;
  op_args.tmpbuf1 = tmpbuf1;
  op_args.tmpbuf2 = tmpbuf2;
  op_args.count = count;
  op_args.op = op;
  op_args.datatype = datatype;

  /* append to the round-schedule */
  ret = pnbc_schedule_round_append (schedule, &op_args, sizeof (op_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_DEBUG(10, "added op2 - ends at byte %i\n", pnbc_schedule_get_size (schedule));

  return OMPI_SUCCESS;
}

/* this function puts a copy into the schedule */
int PNBC_Sched_copy (void *src, char tmpsrc, int srccount, MPI_Datatype srctype, void *tgt, char tmptgt, int tgtcount,
                    MPI_Datatype tgttype, PNBC_Schedule *schedule, bool barrier) {
  PNBC_Args_copy copy_args;
  int ret;

  /* store the passed arguments */
  copy_args.type = COPY;
  copy_args.src = src;
  copy_args.tmpsrc = tmpsrc;
  copy_args.srccount = srccount;
  copy_args.srctype = srctype;
  copy_args.tgt = tgt;
  copy_args.tmptgt = tmptgt;
  copy_args.tgtcount = tgtcount;
  copy_args.tgttype = tgttype;

  /* append to the round-schedule */
  ret = pnbc_schedule_round_append (schedule, &copy_args, sizeof (copy_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_DEBUG(10, "added copy - ends at byte %i\n", pnbc_schedule_get_size (schedule));

  return OMPI_SUCCESS;
}

/* this function puts a unpack into the schedule */
int PNBC_Sched_unpack (void *inbuf, char tmpinbuf, int count, MPI_Datatype datatype, void *outbuf, char tmpoutbuf,
                      PNBC_Schedule *schedule, bool barrier) {
  PNBC_Args_unpack unpack_args;
  int ret;

  /* store the passed arguments */
  unpack_args.type = UNPACK;
  unpack_args.inbuf = inbuf;
  unpack_args.tmpinbuf = tmpinbuf;
  unpack_args.count = count;
  unpack_args.datatype = datatype;
  unpack_args.outbuf = outbuf;
  unpack_args.tmpoutbuf = tmpoutbuf;

  /* append to the round-schedule */
  ret = pnbc_schedule_round_append (schedule, &unpack_args, sizeof (unpack_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_DEBUG(10, "added unpack - ends at byte %i\n", pnbc_schedule_get_size (schedule));

  return OMPI_SUCCESS;
}

/* this function ends a round of a schedule */
int PNBC_Sched_barrier (PNBC_Schedule *schedule) {
  return pnbc_schedule_round_append (schedule, NULL, 0, true);
}

/* this function ends a schedule */
int PNBC_Sched_commit(PNBC_Schedule *schedule) {
  int size = pnbc_schedule_get_size (schedule);
  char *ptr;
  int ret;

  ret = pnbc_schedule_grow (schedule, 1);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  /* add the barrier char (0) because this is the last round */
  ptr = schedule->data + size;
  *((char *) ptr) = 0;

  /* increase size of schedule */
  pnbc_schedule_inc_size (schedule, 1);

  PNBC_DEBUG(10, "closed schedule %p at byte %i\n", schedule, (int)(size + 1));

  return OMPI_SUCCESS;
}

/* finishes a request
 *
 * to be called *only* from the progress thread !!! */
static inline void PNBC_Free (PNBC_Handle* handle) {

  if (NULL != handle->schedule) {
    /* release schedule */
    OBJ_RELEASE (handle->schedule);
    handle->schedule = NULL;
  }

  /* if the pnbc_I<collective> attached some data */
  /* problems with schedule cache here, see comment (TODO) in
   * pnbc_internal.h */
  if (NULL != handle->tmpbuf) {
    free((void*)handle->tmpbuf);
    handle->tmpbuf = NULL;
  }
}

/* progresses a request
 *
 * to be called *only* from the progress thread !!! */
int PNBC_Progress(PNBC_Handle *handle) {
  int flag, res, ret=PNBC_CONTINUE;
  unsigned long size = 0;
  char *delim;
  int i;
  ompi_status_public_t status;

  /* the handle is done if there is no schedule attached */
  if (NULL == handle->schedule) {
    return PNBC_OK;
  }

  if ((handle->req_count > 0) && (handle->req_array != NULL)) {
    PNBC_DEBUG(50, "PNBC_Progress: testing for %i requests\n", handle->req_count);
#ifdef PNBC_TIMING
    Test_time -= MPI_Wtime();
#endif
    res = ompi_request_test_all(handle->req_count, handle->req_array, &flag, MPI_STATUSES_IGNORE);
    if(res != OMPI_SUCCESS) {
      // Attempt to cancel outstanding requests
      for(i = 0; i < handle->req_count; ++i ) {
        // If the request is complete, then try to report the error code
        if( handle->req_array[i]->req_complete ) {
          if( OMPI_SUCCESS != handle->req_array[i]->req_status.MPI_ERROR ) {
            PNBC_Error ("MPI Error in MPI_Testall() (req %d = %d)", i, handle->req_array[i]->req_status.MPI_ERROR);
          }
        }
        else {
          ompi_request_cancel(handle->req_array[i]);
          // If the PML actually canceled the request, then wait on it
          if( handle->req_array[i]->req_status._cancelled) {
            ompi_request_wait(&handle->req_array[i], &status);
          }
          // Warn the user that we had to leave a PML message outstanding so
          // bad things could happen if they continue using nonblocking collectives
          else {
            PNBC_Error ("MPI Error: Not able to cancel the internal request %d. "
                       "Be aware that continuing to use nonblocking collectives on this communicator may result in undefined behavior.", i);
          }
        }
      }

      return OMPI_ERROR;
    }
#ifdef PNBC_TIMING
    Test_time += MPI_Wtime();
#endif
  } else {
    flag = 1; /* we had no open requests -> proceed to next round */
  }

  /* a round is finished */
  if (flag) {
    /* adjust delim to start of current round */
    PNBC_DEBUG(5, "PNBC_Progress: going in schedule %p to row-offset: %li\n", handle->schedule, handle->row_offset);
    delim = handle->schedule->data + handle->row_offset;
    PNBC_DEBUG(10, "delim: %p\n", delim);
    pnbc_get_round_size(delim, &size);
    PNBC_DEBUG(10, "size: %li\n", size);
    /* adjust delim to end of current round -> delimiter */
    delim = delim + size;

    if (NULL != handle->req_array) {
      /* free request array */
      free (handle->req_array);
      handle->req_array = NULL;
    }

    handle->req_count = 0;

    if (*delim == 0) {
      /* this was the last round - we're done */
      PNBC_DEBUG(5, "PNBC_Progress last round finished - we're done\n");

      if(!REQUEST_COMPLETE(&handle->super)) {
    	  ompi_request_complete(&handle->super, true);
    	  PNBC_DEBUG(5, "Request Marked COMPLETED\n");
      }

      return PNBC_OK;

    }

    PNBC_DEBUG(5, "PNBC_Progress round finished - goto next round\n");
    /* move delim to start of next round */
    /* initializing handle for new virgin round */
    handle->row_offset = (intptr_t) (delim + 1) - (intptr_t) handle->schedule->data;
    /* kick it off */
    res = PNBC_Start_round(handle);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
      PNBC_Error ("Error in PNBC_Start_round() (%i)", res);
      return res;
    }
  }

  PNBC_DEBUG(5, "Leaving PNBC_Progress\n");

  return ret;
}

static inline int PNBC_Start_round(PNBC_Handle *handle) {
  int num; /* number of operations */
  int res;
  char* ptr;
  MPI_Request *tmp;
  PNBC_Fn_type type;
  PNBC_Args_send     sendargs;
  PNBC_Args_recv     recvargs;
  PNBC_Args_op         opargs;
  PNBC_Args_copy     copyargs;
  PNBC_Args_unpack unpackargs;
  void *buf1,  *buf2;

  PNBC_DEBUG(10, "\n\n*** getting round schedule address... ***\n\n");

  /* get round-schedule address */
  ptr = handle->schedule->data + handle->row_offset;

  PNBC_DEBUG(10, "setting array index to %u + %u = %u\n", handle->schedule->data, handle->row_offset, ptr);

  PNBC_GET_BYTES(ptr,num);

  PNBC_DEBUG(10, "start_round round at offset %d : posting %i operations\n", handle->row_offset, num);

  for (int i = 0 ; i < num ; ++i) {
    int offset = (intptr_t)(ptr - handle->schedule->data);

    memcpy (&type, ptr, sizeof (type));
    switch(type) {
      case SEND:
        PNBC_DEBUG(5,"  SEND (offset %li) ", offset);
        PNBC_GET_BYTES(ptr,sendargs);
        PNBC_DEBUG(5,"*buf: %p, count: %i, type: %p, dest: %i, tag: %i)\n", sendargs.buf,
                  sendargs.count, sendargs.datatype, sendargs.dest, handle->tag);
        /* get an additional request */
        handle->req_count++;
        /* get buffer */
        if(sendargs.tmpbuf) {
          buf1=(char*)handle->tmpbuf+(long)sendargs.buf;
        } else {
          buf1=(void *)sendargs.buf;
        }
#ifdef PNBC_TIMING
        Isend_time -= MPI_Wtime();
#endif
        tmp = (MPI_Request *) realloc ((void *) handle->req_array, handle->req_count * sizeof (MPI_Request));
        if (NULL == tmp) {
          return OMPI_ERR_OUT_OF_RESOURCE;
        }

        handle->req_array = tmp;

        res = MCA_PML_CALL(isend(buf1, sendargs.count, sendargs.datatype, sendargs.dest, handle->tag,
                                 MCA_PML_BASE_SEND_STANDARD, sendargs.local?handle->comm->c_local_comm:handle->comm,
                                 handle->req_array+handle->req_count - 1));
        if (OMPI_SUCCESS != res) {
          PNBC_Error ("Error in MPI_Isend(%lu, %i, %p, %i, %i, %lu) (%i)", (unsigned long)buf1, sendargs.count,
                     sendargs.datatype, sendargs.dest, handle->tag, (unsigned long)handle->comm, res);
          return res;
        }
#ifdef PNBC_TIMING
        Isend_time += MPI_Wtime();
#endif
        break;
      case RECV:
        PNBC_DEBUG(5, "  RECV (offset %li) ", offset);
        PNBC_GET_BYTES(ptr,recvargs);
        PNBC_DEBUG(5, "*buf: %p, count: %i, type: %p, source: %i, tag: %i)\n", recvargs.buf, recvargs.count,
                  recvargs.datatype, recvargs.source, handle->tag);
        /* get an additional request - TODO: req_count NOT thread safe */
        handle->req_count++;
        /* get buffer */
        if(recvargs.tmpbuf) {
          buf1=(char*)handle->tmpbuf+(long)recvargs.buf;
        } else {
          buf1=recvargs.buf;
        }
#ifdef PNBC_TIMING
        Irecv_time -= MPI_Wtime();
#endif
        tmp = (MPI_Request *) realloc ((void *) handle->req_array, handle->req_count * sizeof (MPI_Request));
        if (NULL == tmp) {
          return OMPI_ERR_OUT_OF_RESOURCE;
        }

        handle->req_array = tmp;

        res = MCA_PML_CALL(irecv(buf1, recvargs.count, recvargs.datatype, recvargs.source, handle->tag, recvargs.local?handle->comm->c_local_comm:handle->comm,
                                 handle->req_array+handle->req_count-1));
        if (OMPI_SUCCESS != res) {
          PNBC_Error("Error in MPI_Irecv(%lu, %i, %p, %i, %i, %lu) (%i)", (unsigned long)buf1, recvargs.count,
                    recvargs.datatype, recvargs.source, handle->tag, (unsigned long)handle->comm, res);
          return res;
        }
#ifdef PNBC_TIMING
        Irecv_time += MPI_Wtime();
#endif
        break;
      case OP:
        PNBC_DEBUG(5, "  OP2  (offset %li) ", offset);
        PNBC_GET_BYTES(ptr,opargs);
        PNBC_DEBUG(5, "*buf1: %p, buf2: %p, count: %i, type: %p)\n", opargs.buf1, opargs.buf2,
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
        PNBC_DEBUG(5, "  COPY   (offset %li) ", offset);
        PNBC_GET_BYTES(ptr,copyargs);
        PNBC_DEBUG(5, "*src: %lu, srccount: %i, srctype: %p, *tgt: %lu, tgtcount: %i, tgttype: %p)\n",
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
        res = PNBC_Copy (buf1, copyargs.srccount, copyargs.srctype, buf2, copyargs.tgtcount, copyargs.tgttype,
                        handle->comm);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
          return res;
        }
        break;
      case UNPACK:
        PNBC_DEBUG(5, "  UNPACK   (offset %li) ", offset);
        PNBC_GET_BYTES(ptr,unpackargs);
        PNBC_DEBUG(5, "*src: %lu, srccount: %i, srctype: %p, *tgt: %lu\n", (unsigned long) unpackargs.inbuf,
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
        res = PNBC_Unpack (buf1, unpackargs.count, unpackargs.datatype, buf2, handle->comm);
        if (OMPI_SUCCESS != res) {
          PNBC_Error ("PNBC_Unpack() failed (code: %i)", res);
          return res;
        }

        break;
      default:
        PNBC_Error ("PNBC_Start_round: bad type %li at offset %li", (long)type, offset);
        return OMPI_ERROR;
    }
  }

  /* check if we can make progress - not in the first round, this allows us to leave the
   * initialization faster and to reach more overlap
   *
   * threaded case: calling progress in the first round can lead to a
   * deadlock if PNBC_Free is called in this round :-( */
  //if (handle->row_offset) {
    res = PNBC_Progress(handle);
    PNBC_DEBUG(5, "Returned from PNBC_Progress\n");
    if ((PNBC_OK != res) && (PNBC_CONTINUE != res)) {
      return OMPI_ERROR;
    }
  //}

  PNBC_DEBUG(5, "Leaving PNBC_Start_round\n");

  return OMPI_SUCCESS;
}

int PNBC_Init_handle(struct ompi_communicator_t *comm, ompi_coll_libpnbc_request_t **request, ompi_coll_libpnbc_module_t *comminfo) {

  PNBC_DEBUG(5, "PNBC_Init_handle\n");

  int tmp_tag;
  bool need_register = false;
  ompi_coll_libpnbc_request_t *handle;

  OMPI_COLL_LIBPNBC_REQUEST_ALLOC(comm, handle);
  if (NULL == handle) return OMPI_ERR_OUT_OF_RESOURCE;
  *request = handle;

  handle->tmpbuf = NULL;
  handle->req_count = 0;
  handle->req_array = NULL;
  handle->comm = comm;
  handle->schedule = NULL;
  handle->row_offset = 0;

  /******************** Do the tag and shadow comm administration ...  ***************/

  OPAL_THREAD_LOCK(&comminfo->mutex);
  tmp_tag = comminfo->tag--;
  if (tmp_tag == MCA_COLL_BASE_TAG_NONBLOCKING_END) {
      tmp_tag = comminfo->tag = MCA_COLL_BASE_TAG_NONBLOCKING_BASE;
      PNBC_DEBUG(2,"resetting tags ...\n");
  }

  if (true != comminfo->comm_registered) {
      comminfo->comm_registered = true;
      need_register = true;
  }
  OPAL_THREAD_UNLOCK(&comminfo->mutex);

  handle->tag = tmp_tag;

  /* register progress */
  if (need_register) {
      int32_t tmp =
          OPAL_THREAD_ADD32(&mca_coll_libpnbc_component.active_comms, 1);
      if (tmp == 1) {
          opal_progress_register(ompi_coll_libpnbc_progress);
      }
  }

  handle->comm=comm;
  /*printf("got comminfo: %lu tag: %i\n", comminfo, comminfo->tag);*/

  /******************** end of tag and shadow comm administration ...  ***************/
  handle->comminfo = comminfo;

  PNBC_DEBUG(3, "got tag %i\n", handle->tag);

  return OMPI_SUCCESS;
}

void PNBC_Return_handle(ompi_coll_libpnbc_request_t *request) {
  PNBC_Free (request);
  OMPI_COLL_LIBPNBC_REQUEST_RETURN(request);
}

int  PNBC_Init_comm(MPI_Comm comm, PNBC_Comminfo *comminfo) {
  comminfo->tag= MCA_COLL_BASE_TAG_NONBLOCKING_BASE;

#ifdef PNBC_CACHE_SCHEDULE
  /* initialize the PNBC_ALLTOALL SchedCache tree */
  comminfo->PNBC_Dict[PNBC_ALLTOALL] = hb_tree_new((dict_cmp_func)PNBC_Alltoall_args_compare, PNBC_SchedCache_args_delete_key_dummy, PNBC_SchedCache_args_delete);
  if(comminfo->PNBC_Dict[PNBC_ALLTOALL] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  PNBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->PNBC_Dict[PNBC_ALLTOALL]);
  comminfo->PNBC_Dict_size[PNBC_ALLTOALL] = 0;
  /* initialize the PNBC_ALLGATHER SchedCache tree */
  comminfo->PNBC_Dict[PNBC_ALLGATHER] = hb_tree_new((dict_cmp_func)PNBC_Allgather_args_compare, PNBC_SchedCache_args_delete_key_dummy, PNBC_SchedCache_args_delete);
  if(comminfo->PNBC_Dict[PNBC_ALLGATHER] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  PNBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->PNBC_Dict[PNBC_ALLGATHER]);
  comminfo->PNBC_Dict_size[PNBC_ALLGATHER] = 0;
  /* initialize the PNBC_ALLREDUCE SchedCache tree */
  comminfo->PNBC_Dict[PNBC_ALLREDUCE] = hb_tree_new((dict_cmp_func)PNBC_Allreduce_args_compare, PNBC_SchedCache_args_delete_key_dummy, PNBC_SchedCache_args_delete);
  if(comminfo->PNBC_Dict[PNBC_ALLREDUCE] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  PNBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->PNBC_Dict[PNBC_ALLREDUCE]);
  comminfo->PNBC_Dict_size[PNBC_ALLREDUCE] = 0;
  /* initialize the PNBC_BARRIER SchedCache tree - is not needed -
   * schedule is hung off directly */
  comminfo->PNBC_Dict_size[PNBC_BARRIER] = 0;
  /* initialize the PNBC_BCAST SchedCache tree */
  comminfo->PNBC_Dict[PNBC_BCAST] = hb_tree_new((dict_cmp_func)PNBC_Bcast_args_compare, PNBC_SchedCache_args_delete_key_dummy, PNBC_SchedCache_args_delete);
  if(comminfo->PNBC_Dict[PNBC_BCAST] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  PNBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->PNBC_Dict[PNBC_BCAST]);
  comminfo->PNBC_Dict_size[PNBC_BCAST] = 0;
  /* initialize the PNBC_GATHER SchedCache tree */
  comminfo->PNBC_Dict[PNBC_GATHER] = hb_tree_new((dict_cmp_func)PNBC_Gather_args_compare, PNBC_SchedCache_args_delete_key_dummy, PNBC_SchedCache_args_delete);
  if(comminfo->PNBC_Dict[PNBC_GATHER] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  PNBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->PNBC_Dict[PNBC_GATHER]);
  comminfo->PNBC_Dict_size[PNBC_GATHER] = 0;
  /* initialize the PNBC_REDUCE SchedCache tree */
  comminfo->PNBC_Dict[PNBC_REDUCE] = hb_tree_new((dict_cmp_func)PNBC_Reduce_args_compare, PNBC_SchedCache_args_delete_key_dummy, PNBC_SchedCache_args_delete);
  if(comminfo->PNBC_Dict[PNBC_REDUCE] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  PNBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->PNBC_Dict[PNBC_REDUCE]);
  comminfo->PNBC_Dict_size[PNBC_REDUCE] = 0;
  /* initialize the PNBC_SCAN SchedCache tree */
  comminfo->PNBC_Dict[PNBC_SCAN] = hb_tree_new((dict_cmp_func)PNBC_Scan_args_compare, PNBC_SchedCache_args_delete_key_dummy, PNBC_SchedCache_args_delete);
  if(comminfo->PNBC_Dict[PNBC_SCAN] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  PNBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->PNBC_Dict[PNBC_SCAN]);
  comminfo->PNBC_Dict_size[PNBC_SCAN] = 0;
  /* initialize the PNBC_SCATTER SchedCache tree */
  comminfo->PNBC_Dict[PNBC_SCATTER] = hb_tree_new((dict_cmp_func)PNBC_Scatter_args_compare, PNBC_SchedCache_args_delete_key_dummy, PNBC_SchedCache_args_delete);
  if(comminfo->PNBC_Dict[PNBC_SCATTER] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  PNBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->PNBC_Dict[PNBC_SCATTER]);
  comminfo->PNBC_Dict_size[PNBC_SCATTER] = 0;
#endif

  return OMPI_SUCCESS;
}

int PNBC_Start_internal(PNBC_Handle *handle, PNBC_Schedule *schedule) {

  int res;

  handle->schedule = schedule;
  handle->super.req_complete = REQUEST_PENDING;
  handle->super.req_state = OMPI_REQUEST_ACTIVE;  // DAN: added

  /* kick off first round */
  res = PNBC_Start_round(handle);
  if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
    return res;
  }

  OPAL_THREAD_LOCK(&mca_coll_libpnbc_component.lock);
  opal_list_append(&mca_coll_libpnbc_component.active_requests, &(handle->super.super.super));
  OPAL_THREAD_UNLOCK(&mca_coll_libpnbc_component.lock);

  return OMPI_SUCCESS;

}

#ifdef PNBC_CACHE_SCHEDULE
void PNBC_SchedCache_args_delete_key_dummy(void *k) {
    /* do nothing because the key and the data element are identical :-)
     * both (the single one :) is freed in PNBC_<COLLOP>_args_delete() */
}

void PNBC_SchedCache_args_delete(void *entry) {
  struct PNBC_dummyarg *tmp = (struct PNBC_dummyarg*)entry;
  OBJ_RELEASE(tmp->schedule);
  free(entry);
}
#endif

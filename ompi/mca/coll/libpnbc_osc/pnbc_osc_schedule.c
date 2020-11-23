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
#include "pnbc_osc_schedule.h"
#include "pnbc_osc_internal.h"

static void PNBC_OSC_Schedule_constructor (PNBC_OSC_Schedule *schedule) {
  /* initial total size of the schedule */
  schedule->size = sizeof (int);
  schedule->current_round_offset = 0;
  schedule->data = calloc (1, schedule->size);
}

static void PNBC_OSC_Schedule_destructor (PNBC_OSC_Schedule *schedule) {
  free (schedule->data);
  schedule->data = NULL;
}

OBJ_CLASS_INSTANCE(PNBC_OSC_Schedule, opal_object_t,
                   PNBC_OSC_Schedule_constructor,
                   PNBC_OSC_Schedule_destructor);

static int PNBC_OSC_Schedule_grow (PNBC_OSC_Schedule *schedule, int additional) {
  void *tmp;
  int size;

  /* get current size of schedule */
  size = PNBC_OSC_Schedule_get_size (schedule);

  tmp = realloc (schedule->data, size + additional);
  if (NULL == tmp) {
    PNBC_OSC_Error ("Could not increase the size of PNBC_OSC schedule");
    return OMPI_ERR_OUT_OF_RESOURCE;
  }

  schedule->data = tmp;
  return OMPI_SUCCESS;
}

static int PNBC_OSC_Schedule_round_append (PNBC_OSC_Schedule *schedule, void *data, int data_size, bool barrier) {
  int ret, size = PNBC_OSC_Schedule_get_size (schedule);

  if (barrier) {
    ret = PNBC_OSC_Schedule_grow (schedule, data_size + 1 + sizeof (int));
  } else {
    ret = PNBC_OSC_Schedule_grow (schedule, data_size);
  }
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  /* append to the round-schedule */
  if (data_size) {
    memcpy (schedule->data + size, data, data_size);

    /* increase number of elements in round-schedule */
    PNBC_OSC_Schedule_inc_round (schedule);

    /* increase size of schedule */
    PNBC_OSC_Schedule_inc_size (schedule, data_size);
  }

  if (barrier) {
    /* add the barrier */
    schedule->data[size + data_size] = 1;
    /* set next round counter to 0 */
    memset (schedule->data + size + data_size + 1, 0, sizeof (int));

    PNBC_OSC_DEBUG(10, "ended round at byte %i\n", size + data_size + 1);

    schedule->current_round_offset = size + data_size + 1;

    /* increase size of schedule */
    PNBC_OSC_Schedule_inc_size (schedule, sizeof (int) + 1);
  }

  return OMPI_SUCCESS;
}

/* this function appends an rput into the schedule */
int PNBC_OSC_Sched_rput(const void* buf, int target,
                        int origin_count, MPI_Datatype origin_datatype,
                        int target_count, MPI_Datatype target_datatype,
                        MPI_Aint target_displ,
                        PNBC_OSC_Schedule *schedule, bool barrier) {
  int ret;
  PNBC_OSC_Args_put put_args;

  /* store the passed arguments */
  put_args.type = PUT;
  put_args.buf = buf;
  put_args.origin_count = origin_count;
  put_args.origin_datatype = origin_datatype;
  put_args.target = target;
  put_args.target_count = target_count;
  put_args.target_datatype = target_datatype;
  put_args.target_displ = target_displ;

  /* append to the round-schedule */
  ret = PNBC_OSC_Schedule_round_append (schedule, &put_args, sizeof (put_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_OSC_DEBUG(10, "added rput - ends at byte %i\n", PNBC_OSC_Schedule_get_size (schedule));

  return OMPI_SUCCESS;
}


/* this function appends an rget into the schedule */
int PNBC_OSC_Sched_rget(void* buf, int target,
                       int origin_count, MPI_Datatype origin_datatype,
                       int target_count, MPI_Datatype target_datatype,
                       MPI_Aint target_displ,
                       PNBC_OSC_Schedule *schedule, bool barrier) {
  PNBC_OSC_Args_get get_args;
  int ret;

  /* store the passed arguments */
  get_args.type = GET;
  get_args.buf = buf;
  get_args.origin_count = origin_count;
  get_args.origin_datatype = origin_datatype;
  get_args.target = target;
  get_args.target_count = target_count;
  get_args.target_datatype = target_datatype;
  get_args.target_displ = target_displ;

  /* append to the round-schedule */
  ret = PNBC_OSC_Schedule_round_append (schedule, &get_args, sizeof (get_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_OSC_DEBUG(10, "added rget - ends at byte %i\n", PNBC_OSC_Schedule_get_size (schedule));

  return OMPI_SUCCESS;
}


/* this function appends an operation into the schedule */
int PNBC_OSC_Sched_op (const void* buf1, char tmpbuf1, void* buf2, char tmpbuf2, int count,
                       MPI_Datatype datatype, MPI_Op op, PNBC_OSC_Schedule *schedule,
                       bool barrier) {
  PNBC_OSC_Args_op op_args;
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
  ret = PNBC_OSC_Schedule_round_append (schedule, &op_args, sizeof (op_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_OSC_DEBUG(10, "added op2 - ends at byte %i\n", PNBC_OSC_Schedule_get_size (schedule));

  return OMPI_SUCCESS;
}

/* this function puts a copy into the schedule */
int PNBC_OSC_Sched_copy (void *src, char tmpsrc, int srccount, MPI_Datatype srctype, void *tgt, char tmptgt, int tgtcount,
                         MPI_Datatype tgttype, PNBC_OSC_Schedule *schedule, bool barrier) {
  PNBC_OSC_Args_copy copy_args;
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
  ret = PNBC_OSC_Schedule_round_append (schedule, &copy_args, sizeof (copy_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_OSC_DEBUG(10, "added copy - ends at byte %i\n", PNBC_OSC_Schedule_get_size (schedule));

  return OMPI_SUCCESS;
}

/* this function puts a unpack into the schedule */
int PNBC_OSC_Sched_unpack (void *inbuf, char tmpinbuf, int count, MPI_Datatype datatype, void *outbuf,
                           char tmpoutbuf, PNBC_OSC_Schedule *schedule, bool barrier) {

  PNBC_OSC_Args_unpack unpack_args;
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
  ret = PNBC_OSC_Schedule_round_append (schedule, &unpack_args, sizeof (unpack_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_OSC_DEBUG(10, "added unpack - ends at byte %i\n", PNBC_OSC_Schedule_get_size (schedule));

  return OMPI_SUCCESS;
}

/* this function adds win_ifree into the schedule */
int PNBC_OSC_Sched_win_free ( PNBC_OSC_Schedule *schedule, bool barrier) {
  int ret;
  PNBC_OSC_Args_win_free wfree_args;
  wfree_args.type = WIN_FREE;

  /* append to the round-schedule */
  ret = PNBC_OSC_Schedule_round_append (schedule, &wfree_args, sizeof(wfree_args), barrier);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  PNBC_OSC_DEBUG(10, "added win_free - ends at byte %i\n", PNBC_OSC_Schedule_get_size (schedule));

  return OMPI_SUCCESS;
}

/* this function ends a round of a schedule */
int PNBC_OSC_Sched_barrier (PNBC_OSC_Schedule *schedule) {
  return PNBC_OSC_Schedule_round_append (schedule, NULL, 0, true);
}

/* this function ends a schedule */
int PNBC_OSC_Sched_commit(PNBC_OSC_Schedule *schedule) {
  int size = PNBC_OSC_Schedule_get_size (schedule);
  char *ptr;
  int ret;

  ret = PNBC_OSC_Schedule_grow (schedule, 1);
  if (OMPI_SUCCESS != ret) {
    return ret;
  }

  /* add the barrier char (0) because this is the last round */
  ptr = schedule->data + size;
  *((char *) ptr) = 0;

  /* increase size of schedule */
  PNBC_OSC_Schedule_inc_size (schedule, 1);

  PNBC_OSC_DEBUG(10, "closed schedule %p at byte %i\n", schedule, (int)(size + 1));

  return OMPI_SUCCESS;
}

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


int PNBC_OSC_Schedule_request(PNBC_OSC_Schedule *schedule, ompi_communicator_t *comm,
                              ompi_coll_libpnbc_osc_module_t *module, bool persistent,
                              ompi_request_t **request, void *tmpbuf) {
  int ret;
  bool need_register = false;
  ompi_coll_libpnbc_osc_request_t *handle;

  /* no operation (e.g. one process barrier)? */
  if (((int *)schedule->data)[0] == 0 && schedule->data[sizeof(int)] == 0) {
    ret = nbc_get_noop_request(persistent, request);
    if (OMPI_SUCCESS != ret) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    OBJ_RELEASE(schedule);
    free(tmpbuf);

    return OMPI_SUCCESS;
  }

  OMPI_COLL_LIBPNBC_OSC_REQUEST_ALLOC(comm, persistent, handle);
  if (NULL == handle) return OMPI_ERR_OUT_OF_RESOURCE;

  handle->tmpbuf = NULL;
  handle->req_count = 0;
  handle->req_array = NULL;
  handle->comm = comm;
  handle->schedule = NULL;
  handle->nbc_complete = persistent ? true : false;

  /******************** Do the shadow comm administration ...  ***************/

  OPAL_THREAD_LOCK(&module->mutex);
  if (true != module->comm_registered) {
    module->comm_registered = true;
    need_register = true;
  }
  OPAL_THREAD_UNLOCK(&module->mutex);

  /* register progress */
  if (need_register) {
    int32_t tmp =
      OPAL_THREAD_ADD_FETCH32(&mca_coll_libpnbc_osc_component.active_comms, 1);
    if (tmp == 1) {
      opal_progress_register(ompi_coll_libpnbc_osc_progress);
    }
  }

  /******************** end of shadow comm administration ...  ***************/
  handle->comminfo = module;

  handle->tmpbuf = tmpbuf;
  handle->schedule = schedule;
  handle->schedule->row_offset = 0;
  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

int PNBC_OSC_Schedule_request_win(PNBC_OSC_Schedule *schedule, ompi_communicator_t *comm,
                                  ompi_win_t *win, ompi_win_t *winflag, int req_count,
                                  ompi_coll_libpnbc_osc_module_t *module,
                                  bool persistent, ompi_request_t **request, void *tmpbuf) {
  int ret;
  bool need_register = false;
  ompi_coll_libpnbc_osc_request_t *handle;

  /* no operation (e.g. one process barrier)? */
  if (((int *)schedule->data)[0] == 0 && schedule->data[sizeof(int)] == 0) {
    ret = nbc_get_noop_request(persistent, request);
    if (OMPI_SUCCESS != ret) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    OBJ_RELEASE(schedule);
    free(tmpbuf);

    return OMPI_SUCCESS;
  }

  OMPI_COLL_LIBPNBC_OSC_REQUEST_ALLOC(comm, persistent, handle);
  if (NULL == handle) return OMPI_ERR_OUT_OF_RESOURCE;

  handle->req_count = 0;
  handle->nbc_complete = persistent ? true : false;
  handle->schedule = schedule;
  handle->schedule->row_offset = 0;
  handle->comm = comm;
  handle->win = win;
  handle->winflag = winflag;
  handle->comminfo = module;
  handle->tmpbuf = tmpbuf;
  if (req_count > 0)
    handle->req_array = malloc(req_count*sizeof(MPI_Request));
  else
    handle->req_array = NULL;

  /******************** Do the shadow comm administration ...  ***************/

  OPAL_THREAD_LOCK(&module->mutex);
  if (true != module->comm_registered) {
    module->comm_registered = true;
    need_register = true;
  }
  OPAL_THREAD_UNLOCK(&module->mutex);

  /* register progress */
  if (need_register) {
    int32_t tmp =
      OPAL_THREAD_ADD_FETCH32(&mca_coll_libpnbc_osc_component.active_comms, 1);
    if (tmp == 1) {
      opal_progress_register(ompi_coll_libpnbc_osc_progress);
    }
  }

  /******************** end of shadow comm administration ...  ***************/

  *request = (ompi_request_t *) handle;

  return OMPI_SUCCESS;
}

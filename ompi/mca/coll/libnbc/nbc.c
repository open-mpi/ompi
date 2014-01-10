/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All 
 *                         rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 *
 */
#include "nbc_internal.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/op/op.h"
#include "ompi/mca/pml/pml.h"

/* only used in this file */
static inline int NBC_Start_round(NBC_Handle *handle);

/* #define NBC_TIMING */

#ifdef NBC_TIMING
static double Isend_time=0, Irecv_time=0, Wait_time=0, Test_time=0;
void NBC_Reset_times() {
  Isend_time=Irecv_time=Wait_time=Test_time=0;
}
void NBC_Print_times(double div) {
  printf("*** NBC_TIMES: Isend: %lf, Irecv: %lf, Wait: %lf, Test: %lf\n", Isend_time*1e6/div, Irecv_time*1e6/div, Wait_time*1e6/div, Test_time*1e6/div);
}
#endif

/* allocates a new schedule array */
int NBC_Sched_create(NBC_Schedule* schedule) {
  int *ptr;

  *schedule=malloc(2*sizeof(int));
  if(*schedule == NULL) { return NBC_OOR; }

  /* initialize the schedule */
  ptr = (int*) *schedule;
  ptr[0] = 2 * sizeof(int);  /* initial total size of the schedule */
  ptr[1] = 0;                /* initial round-schedule has num=(int)0 and no actions */
  /* The schedule's final end=(char)0 delimiter won't be added until NBC_Sched_commit(). */

  return NBC_OK;
}

/* this function puts a send into the schedule */
int NBC_Sched_send(void* buf, char tmpbuf, int count, MPI_Datatype datatype, int dest, NBC_Schedule *schedule) {
  int size;
  char* ptr;
  NBC_Fn_type type = SEND;
  NBC_Args_send send_args;
  
  /* get size of actual schedule */
  NBC_GET_SIZE(*schedule, size);
  /*printf("schedule is %i bytes\n", size);*/
  *schedule = (NBC_Schedule)realloc(*schedule, size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_send));
  if(*schedule == NULL) { printf("Error in realloc()\n"); return NBC_OOR; }
  
  /* store the passed arguments */
  send_args.buf=buf;
  send_args.tmpbuf=tmpbuf;
  send_args.count=count;
  send_args.datatype=datatype;
  send_args.dest=dest;

  /* append to the round-schedule */
  ptr = (char*)*schedule + size;
  NBC_PUT_BYTES(ptr,type);
  NBC_PUT_BYTES(ptr,send_args);

  /* increase number of elements in round-schedule */
  NBC_INC_NUM_ROUND(*schedule);
  NBC_DEBUG(10, "adding send - ends at byte %i\n", (int)(size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_send)));

  /* increase size of schedule */
  NBC_INC_SIZE(*schedule, sizeof(NBC_Fn_type)+sizeof(NBC_Args_send));

  return NBC_OK;
}

/* this function puts a receive into the schedule */
int NBC_Sched_recv(void* buf, char tmpbuf, int count, MPI_Datatype datatype, int source, NBC_Schedule *schedule) {
  int size;
  char* ptr;
  NBC_Fn_type type = RECV;
  NBC_Args_recv recv_args;
  
  /* get size of actual schedule */
  NBC_GET_SIZE(*schedule, size);
  /*printf("schedule is %i bytes\n", size);*/
  *schedule = (NBC_Schedule)realloc(*schedule, size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_recv));
  if(*schedule == NULL) { printf("Error in realloc()\n"); return NBC_OOR; }
  
  /* store the passed arguments */
  recv_args.buf=buf;
  recv_args.tmpbuf=tmpbuf;
  recv_args.count=count;
  recv_args.datatype=datatype;
  recv_args.source=source;

  /* append to the round-schedule */
  ptr = (char*)*schedule + size;
  NBC_PUT_BYTES(ptr,type);
  NBC_PUT_BYTES(ptr,recv_args);

  /* increase number of elements in round-schedule */
  NBC_INC_NUM_ROUND(*schedule);
  NBC_DEBUG(10, "adding receive - ends at byte %i\n", (int)(size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_recv)));

  /* increase size of schedule */
  NBC_INC_SIZE(*schedule, sizeof(NBC_Fn_type)+sizeof(NBC_Args_recv));

  return NBC_OK;
}

/* this function puts an operation into the schedule */
int NBC_Sched_op(void *buf3, char tmpbuf3, void* buf1, char tmpbuf1, void* buf2, char tmpbuf2, int count, MPI_Datatype datatype, MPI_Op op, NBC_Schedule *schedule) {
  int size;
  char* ptr;
  NBC_Fn_type type = OP;
  NBC_Args_op op_args;
  
  /* get size of actual schedule */
  NBC_GET_SIZE(*schedule, size);
  /*printf("schedule is %i bytes\n", size);*/
  *schedule = (NBC_Schedule)realloc(*schedule, size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_op));
  if(*schedule == NULL) { printf("Error in realloc()\n"); return NBC_OOR; }
  
  /* store the passed arguments */
  op_args.buf1=buf1;
  op_args.buf2=buf2;
  op_args.buf3=buf3;
  op_args.tmpbuf1=tmpbuf1;
  op_args.tmpbuf2=tmpbuf2;
  op_args.tmpbuf3=tmpbuf3;
  op_args.count=count;
  op_args.op=op;
  op_args.datatype=datatype;

  /* append to the round-schedule */
  ptr = (char*)*schedule + size;
  NBC_PUT_BYTES(ptr,type);
  NBC_PUT_BYTES(ptr,op_args);

  /* increase number of elements in round-schedule */
  NBC_INC_NUM_ROUND(*schedule);
  NBC_DEBUG(10, "adding op - ends at byte %i\n", (int)(size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_op)));

  /* increase size of schedule */
  NBC_INC_SIZE(*schedule, sizeof(NBC_Fn_type)+sizeof(NBC_Args_op));
  
  return NBC_OK;
}

/* this function puts a copy into the schedule */
int NBC_Sched_copy(void *src, char tmpsrc, int srccount, MPI_Datatype srctype, void *tgt, char tmptgt, int tgtcount, MPI_Datatype tgttype, NBC_Schedule *schedule) {
  int size;
  char* ptr;
  NBC_Fn_type type = COPY;
  NBC_Args_copy copy_args;
  
  /* get size of actual schedule */
  NBC_GET_SIZE(*schedule, size);
  /*printf("schedule is %i bytes\n", size);*/
  *schedule = (NBC_Schedule)realloc(*schedule, size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_copy));
  if(*schedule == NULL) { printf("Error in realloc()\n"); return NBC_OOR; }
  
  /* store the passed arguments */
  copy_args.src=src;
  copy_args.tmpsrc=tmpsrc;
  copy_args.srccount=srccount;
  copy_args.srctype=srctype;
  copy_args.tgt=tgt;
  copy_args.tmptgt=tmptgt;
  copy_args.tgtcount=tgtcount;
  copy_args.tgttype=tgttype;

  /* append to the round-schedule */
  ptr = (char*)*schedule + size;
  NBC_PUT_BYTES(ptr,type);
  NBC_PUT_BYTES(ptr,copy_args);

  /* increase number of elements in round-schedule */
  NBC_INC_NUM_ROUND(*schedule);
  NBC_DEBUG(10, "adding copy - ends at byte %i\n", (int)(size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_copy)));

  /* increase size of schedule */
  NBC_INC_SIZE(*schedule, sizeof(NBC_Fn_type)+sizeof(NBC_Args_copy));

  return NBC_OK;
}

/* this function puts a unpack into the schedule */
int NBC_Sched_unpack(void *inbuf, char tmpinbuf, int count, MPI_Datatype datatype, void *outbuf, char tmpoutbuf, NBC_Schedule *schedule) {
  int size;
  char* ptr;
  NBC_Fn_type type = UNPACK;
  NBC_Args_unpack unpack_args;
  
  /* get size of actual schedule */
  NBC_GET_SIZE(*schedule, size);
  /*printf("schedule is %i bytes\n", size);*/
  *schedule = (NBC_Schedule)realloc(*schedule, size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_unpack));
  if(*schedule == NULL) { printf("Error in realloc()\n"); return NBC_OOR; }
  
  /* store the passed arguments */
  unpack_args.inbuf=inbuf;
  unpack_args.tmpinbuf=tmpinbuf;
  unpack_args.count=count;
  unpack_args.datatype=datatype;
  unpack_args.outbuf=outbuf;
  unpack_args.tmpoutbuf=tmpoutbuf;

  /* append to the round-schedule */
  ptr = (char*)*schedule + size;
  NBC_PUT_BYTES(ptr,type);
  NBC_PUT_BYTES(ptr,unpack_args);

  /* increase number of elements in round-schedule */
  NBC_INC_NUM_ROUND(*schedule);
  NBC_DEBUG(10, "adding unpack - ends at byte %i\n", (int)(size+sizeof(NBC_Fn_type)+sizeof(NBC_Args_unpack)));

  /* increase size of schedule */
  NBC_INC_SIZE(*schedule, sizeof(NBC_Fn_type)+sizeof(NBC_Args_unpack));

  return NBC_OK;
}

/* this function ends a round of a schedule */
int NBC_Sched_barrier(NBC_Schedule *schedule) {
  int size, num = 0;
  char *ptr;
  char delimiter = 1;
  
  /* get size of actual schedule */
  NBC_GET_SIZE(*schedule, size);
  /*printf("round terminated at %i bytes\n", size);*/
  *schedule = (NBC_Schedule)realloc(*schedule, size+sizeof(char)+sizeof(int));
  if(*schedule == NULL) { printf("Error in realloc()\n"); return NBC_OOR; }
  
  ptr = (char*)*schedule + size;
  NBC_PUT_BYTES(ptr,delimiter);  /* round-schedule delimiter */
  NBC_PUT_BYTES(ptr,num);        /* initialize num=0 for next round-schedule */
  
  NBC_DEBUG(10, "ending round at byte %i\n", (int)(size+sizeof(char)+sizeof(int)));
  
  /* increase size of schedule */
  NBC_INC_SIZE(*schedule, sizeof(char)+sizeof(int));

  return NBC_OK;
}

/* this function ends a schedule */
int NBC_Sched_commit(NBC_Schedule *schedule) {
  int size;
 
  /* get size of actual schedule */
  NBC_GET_SIZE(*schedule, size);
  /*printf("schedule terminated at %i bytes\n", size);*/
  *schedule = (NBC_Schedule)realloc(*schedule, size+sizeof(char));
  if(*schedule == NULL) { printf("Error in realloc()\n"); return NBC_OOR; }
 
  /* add the barrier char (0) because this is the last round */
  *(char*)((char*)*schedule+size)=0;
  NBC_DEBUG(10, "closing schedule %p at byte %i\n", *schedule, (int)(size+sizeof(char)));

  /* increase size of schedule */
  NBC_INC_SIZE(*schedule, sizeof(char));
 
  return NBC_OK;
}

/* finishes a request
 *
 * to be called *only* from the progress thread !!! */
static inline int NBC_Free(NBC_Handle* handle) {

#ifdef NBC_CACHE_SCHEDULE
  /* do not free schedule because it is in the cache */
  handle->schedule = NULL;
#else
  if(handle->schedule != NULL) {
    /* free schedule */
    free((void*)*(handle->schedule));
    free((void*)handle->schedule);
    handle->schedule = NULL;
  }
#endif

  /* if the nbc_I<collective> attached some data */
  /* problems with schedule cache here, see comment (TODO) in
   * nbc_internal.h */
  if(NULL != handle->tmpbuf) {
    free((void*)handle->tmpbuf);
    handle->tmpbuf = NULL;
  }

  return NBC_OK;
}

/* progresses a request
 *
 * to be called *only* from the progress thread !!! */
int NBC_Progress(NBC_Handle *handle) {
  int flag, res, ret=NBC_CONTINUE;
  long size;
  char *delim;

  /* the handle is done if there is no schedule attached */
  if(handle->schedule != NULL) {

    if((handle->req_count > 0) && (handle->req_array != NULL)) {
      NBC_DEBUG(50, "NBC_Progress: testing for %i requests\n", handle->req_count);
#ifdef NBC_TIMING
      Test_time -= MPI_Wtime();
#endif
      res = ompi_request_test_all(handle->req_count, handle->req_array, &flag, MPI_STATUSES_IGNORE);
      if(res != OMPI_SUCCESS) { printf("MPI Error in MPI_Testall() (%i)\n", res); ret=res; goto error; }
#ifdef NBC_TIMING
      Test_time += MPI_Wtime();
#endif
    } else {
      flag = 1; /* we had no open requests -> proceed to next round */
    }

    /* a round is finished */
    if(flag) {
      /* adjust delim to start of current round */
      NBC_DEBUG(5, "NBC_Progress: going in schedule %p to row-offset: %li\n", *handle->schedule, handle->row_offset);
      delim = (char*)*handle->schedule + handle->row_offset;
      NBC_DEBUG(10, "delim: %p\n", delim);
      NBC_GET_ROUND_SIZE(delim, size);
      NBC_DEBUG(10, "size: %li\n", size);
      /* adjust delim to end of current round -> delimiter */
      delim = delim + size;

      if(handle->req_array != NULL) {
        /* free request array */
        free((void*)handle->req_array);
        handle->req_array = NULL;
      }
      handle->req_count = 0;

      if(*delim == 0) {
        /* this was the last round - we're done */
        NBC_DEBUG(5, "NBC_Progress last round finished - we're done\n");
        
        res = NBC_Free(handle);
        if((NBC_OK != res)) { printf("Error in NBC_Free() (%i)\n", res); ret=res; goto error; }

        return NBC_OK;
      } else {
        NBC_DEBUG(5, "NBC_Progress round finished - goto next round\n");
        /* move delim to start of next round */
        delim = delim+1;
        /* initializing handle for new virgin round */
        handle->row_offset = (long)delim - (long)*handle->schedule;
        /* kick it off */
        res = NBC_Start_round(handle);
        if(NBC_OK != res) { printf("Error in NBC_Start_round() (%i)\n", res); ret=res; goto error; }
      }
    }
  } else {
    ret= NBC_OK;
  }

error:
  return ret;
}

static inline int NBC_Start_round(NBC_Handle *handle) {
  int num; /* number of operations */
  int i, res, ret=NBC_OK;
  char* ptr;
  NBC_Fn_type type;
  NBC_Args_send     sendargs; 
  NBC_Args_recv     recvargs; 
  NBC_Args_op         opargs; 
  NBC_Args_copy     copyargs; 
  NBC_Args_unpack unpackargs; 
  NBC_Schedule myschedule;
  void *buf1, *buf2, *buf3;

  /* get round-schedule address */
  myschedule = (NBC_Schedule*)((char*)*handle->schedule + handle->row_offset);
  ptr = (char*) myschedule;

  NBC_GET_BYTES(ptr,num);
  NBC_DEBUG(10, "start_round round at address %p : posting %i operations\n", myschedule, num);

  for (i=0; i<num; i++) {
    NBC_GET_BYTES(ptr,type);
    switch(type) {
      case SEND:
        NBC_DEBUG(5,"  SEND (offset %li) ", (long)ptr-(long)myschedule);
        NBC_GET_BYTES(ptr,sendargs);
        NBC_DEBUG(5,"*buf: %p, count: %i, type: %lu, dest: %i, tag: %i)\n", sendargs.buf, sendargs.count, (unsigned long)sendargs.datatype, sendargs.dest, handle->tag);
        /* get an additional request */
        handle->req_count++;
        /* get buffer */
        if(sendargs.tmpbuf) {
          buf1=(char*)handle->tmpbuf+(long)sendargs.buf;
        } else {
          buf1=sendargs.buf;
        }
#ifdef NBC_TIMING
        Isend_time -= MPI_Wtime();
#endif
        handle->req_array = (MPI_Request*)realloc((void*)handle->req_array, (handle->req_count)*sizeof(MPI_Request));
        NBC_CHECK_NULL(handle->req_array);
        res = MCA_PML_CALL(isend(buf1, sendargs.count, sendargs.datatype, sendargs.dest, handle->tag, MCA_PML_BASE_SEND_STANDARD, handle->comm, handle->req_array+handle->req_count-1));
        if(OMPI_SUCCESS != res) { printf("Error in MPI_Isend(%lu, %i, %lu, %i, %i, %lu) (%i)\n", (unsigned long)buf1, sendargs.count, (unsigned long)sendargs.datatype, sendargs.dest, handle->tag, (unsigned long)handle->comm, res); ret=res; goto error; }
#ifdef NBC_TIMING
        Isend_time += MPI_Wtime();
#endif
        break;
      case RECV:
        NBC_DEBUG(5, "  RECV (offset %li) ", (long)ptr-(long)myschedule);
        NBC_GET_BYTES(ptr,recvargs);
        NBC_DEBUG(5, "*buf: %p, count: %i, type: %lu, source: %i, tag: %i)\n", recvargs.buf, recvargs.count, (unsigned long)recvargs.datatype, recvargs.source, handle->tag);
        /* get an additional request - TODO: req_count NOT thread safe */
        handle->req_count++;
        /* get buffer */
        if(recvargs.tmpbuf) {
          buf1=(char*)handle->tmpbuf+(long)recvargs.buf;
        } else {
          buf1=recvargs.buf;
        }
#ifdef NBC_TIMING
        Irecv_time -= MPI_Wtime();
#endif
        handle->req_array = (MPI_Request*)realloc((void*)handle->req_array, (handle->req_count)*sizeof(MPI_Request));
        NBC_CHECK_NULL(handle->req_array);
        res = MCA_PML_CALL(irecv(buf1, recvargs.count, recvargs.datatype, recvargs.source, handle->tag, handle->comm, handle->req_array+handle->req_count-1)); 
        if(OMPI_SUCCESS != res) { printf("Error in MPI_Irecv(%lu, %i, %lu, %i, %i, %lu) (%i)\n", (unsigned long)buf1, recvargs.count, (unsigned long)recvargs.datatype, recvargs.source, handle->tag, (unsigned long)handle->comm, res); ret=res; goto error; }
#ifdef NBC_TIMING
        Irecv_time += MPI_Wtime();
#endif
        break;
      case OP:
        NBC_DEBUG(5, "  OP   (offset %li) ", (long)ptr-(long)myschedule);
        NBC_GET_BYTES(ptr,opargs);
        NBC_DEBUG(5, "*buf1: %p, buf2: %p, count: %i, type: %lu)\n", opargs.buf1, opargs.buf2, opargs.count, (unsigned long)opargs.datatype);
        /* get buffers */
        if(opargs.tmpbuf1) {
          buf1=(char*)handle->tmpbuf+(long)opargs.buf1;
        } else {
          buf1=opargs.buf1;
        }
        if(opargs.tmpbuf2) {
          buf2=(char*)handle->tmpbuf+(long)opargs.buf2;
        } else {
          buf2=opargs.buf2;
        }
        if(opargs.tmpbuf3) {
          buf3=(char*)handle->tmpbuf+(long)opargs.buf3;
        } else {
          buf3=opargs.buf3;
        }
        ompi_3buff_op_reduce(opargs.op, buf1, buf2, buf3, opargs.count, opargs.datatype);
        break;
      case COPY:
        NBC_DEBUG(5, "  COPY   (offset %li) ", (long)ptr-(long)myschedule);
        NBC_GET_BYTES(ptr,copyargs);
        NBC_DEBUG(5, "*src: %lu, srccount: %i, srctype: %lu, *tgt: %lu, tgtcount: %i, tgttype: %lu)\n", (unsigned long)copyargs.src, copyargs.srccount, (unsigned long)copyargs.srctype, (unsigned long)copyargs.tgt, copyargs.tgtcount, (unsigned long)copyargs.tgttype);
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
        res = NBC_Copy(buf1, copyargs.srccount, copyargs.srctype, buf2, copyargs.tgtcount, copyargs.tgttype, handle->comm);
        if(res != NBC_OK) { printf("NBC_Copy() failed (code: %i)\n", res); ret=res; goto error; }
        break;
      case UNPACK:
        NBC_DEBUG(5, "  UNPACK   (offset %li) ", (long)ptr-(long)myschedule);
        NBC_GET_BYTES(ptr,unpackargs);
        NBC_DEBUG(5, "*src: %lu, srccount: %i, srctype: %lu, *tgt: %lu\n", (unsigned long)unpackargs.inbuf, unpackargs.count, (unsigned long)unpackargs.datatype, (unsigned long)unpackargs.outbuf);
        /* get buffers */
        if(unpackargs.tmpinbuf) {
          buf1=(char*)handle->tmpbuf+(long)unpackargs.inbuf;
        } else {
          buf1=unpackargs.outbuf;
        }
        if(unpackargs.tmpoutbuf) {
          buf2=(char*)handle->tmpbuf+(long)unpackargs.outbuf;
        } else {
          buf2=unpackargs.outbuf;
        }
        res = NBC_Unpack(buf1, unpackargs.count, unpackargs.datatype, buf2, handle->comm);
        if(res != NBC_OK) { printf("NBC_Unpack() failed (code: %i)\n", res); ret=res; goto error; }
        break;
      default:
        printf("NBC_Start_round: bad type %li at offset %li\n", (long)type, (long)ptr-(long)myschedule);
        ret=NBC_BAD_SCHED;
        goto error;
    }
  }

  /* check if we can make progress - not in the first round, this allows us to leave the
   * initialization faster and to reach more overlap 
   *
   * threaded case: calling progress in the first round can lead to a
   * deadlock if NBC_Free is called in this round :-( */
  if(handle->row_offset != sizeof(int)) {
    res = NBC_Progress(handle);
    if((NBC_OK != res) && (NBC_CONTINUE != res)) { printf("Error in NBC_Progress() (%i)\n", res); ret=res; goto error; }
  }

error:
  return ret;
}

int NBC_Init_handle(struct ompi_communicator_t *comm, ompi_coll_libnbc_request_t **request, ompi_coll_libnbc_module_t *comminfo)
{
  int tmp_tag;
  bool need_register = false;
  ompi_coll_libnbc_request_t *handle;

  OMPI_COLL_LIBNBC_REQUEST_ALLOC(comm, handle);
  if (NULL == handle) return OMPI_ERR_OUT_OF_RESOURCE;
  *request = handle;

  handle->tmpbuf = NULL;
  handle->req_count = 0;
  handle->req_array = NULL;
  handle->comm = comm;
  handle->schedule = NULL;
  /* first int is the schedule size */
  handle->row_offset = sizeof(int);

  /******************** Do the tag and shadow comm administration ...  ***************/

  OPAL_THREAD_LOCK(&comminfo->mutex);
  tmp_tag = comminfo->tag--;
  if (tmp_tag == MCA_COLL_BASE_TAG_NONBLOCKING_END) {
      tmp_tag = comminfo->tag = MCA_COLL_BASE_TAG_NONBLOCKING_BASE;
      NBC_DEBUG(2,"resetting tags ...\n"); 
  }

  if (true != comminfo->comm_registered) {
      comminfo->comm_registered = true;
      need_register = true;
  }
  OPAL_THREAD_UNLOCK(&comminfo->mutex);

  handle->tag=comminfo->tag;

  /* register progress */
  if (need_register) {
      int32_t tmp = 
          OPAL_THREAD_ADD32(&mca_coll_libnbc_component.active_comms, 1);
      if (tmp == 1) {
          opal_progress_register(ompi_coll_libnbc_progress);
      }
  }

  handle->comm=comm;
  /*printf("got comminfo: %lu tag: %i\n", comminfo, comminfo->tag);*/
  
  /******************** end of tag and shadow comm administration ...  ***************/
  handle->comminfo = comminfo;
  
  NBC_DEBUG(3, "got tag %i\n", handle->tag);

  return NBC_OK;
}

int  NBC_Init_comm(MPI_Comm comm, NBC_Comminfo *comminfo) {
  comminfo->tag= MCA_COLL_BASE_TAG_NONBLOCKING_BASE;

#ifdef NBC_CACHE_SCHEDULE
  /* initialize the NBC_ALLTOALL SchedCache tree */
  comminfo->NBC_Dict[NBC_ALLTOALL] = hb_tree_new((dict_cmp_func)NBC_Alltoall_args_compare, NBC_SchedCache_args_delete_key_dummy, NBC_SchedCache_args_delete);
  if(comminfo->NBC_Dict[NBC_ALLTOALL] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  NBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->NBC_Dict[NBC_ALLTOALL]);
  comminfo->NBC_Dict_size[NBC_ALLTOALL] = 0;
  /* initialize the NBC_ALLGATHER SchedCache tree */
  comminfo->NBC_Dict[NBC_ALLGATHER] = hb_tree_new((dict_cmp_func)NBC_Allgather_args_compare, NBC_SchedCache_args_delete_key_dummy, NBC_SchedCache_args_delete);
  if(comminfo->NBC_Dict[NBC_ALLGATHER] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  NBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->NBC_Dict[NBC_ALLGATHER]);
  comminfo->NBC_Dict_size[NBC_ALLGATHER] = 0;
  /* initialize the NBC_ALLREDUCE SchedCache tree */
  comminfo->NBC_Dict[NBC_ALLREDUCE] = hb_tree_new((dict_cmp_func)NBC_Allreduce_args_compare, NBC_SchedCache_args_delete_key_dummy, NBC_SchedCache_args_delete);
  if(comminfo->NBC_Dict[NBC_ALLREDUCE] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  NBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->NBC_Dict[NBC_ALLREDUCE]);
  comminfo->NBC_Dict_size[NBC_ALLREDUCE] = 0;
  /* initialize the NBC_BARRIER SchedCache tree - is not needed -
   * schedule is hung off directly */
  comminfo->NBC_Dict_size[NBC_BARRIER] = 0;
  /* initialize the NBC_BCAST SchedCache tree */
  comminfo->NBC_Dict[NBC_BCAST] = hb_tree_new((dict_cmp_func)NBC_Bcast_args_compare, NBC_SchedCache_args_delete_key_dummy, NBC_SchedCache_args_delete);
  if(comminfo->NBC_Dict[NBC_BCAST] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  NBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->NBC_Dict[NBC_BCAST]);
  comminfo->NBC_Dict_size[NBC_BCAST] = 0;
  /* initialize the NBC_GATHER SchedCache tree */
  comminfo->NBC_Dict[NBC_GATHER] = hb_tree_new((dict_cmp_func)NBC_Gather_args_compare, NBC_SchedCache_args_delete_key_dummy, NBC_SchedCache_args_delete);
  if(comminfo->NBC_Dict[NBC_GATHER] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  NBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->NBC_Dict[NBC_GATHER]);
  comminfo->NBC_Dict_size[NBC_GATHER] = 0;
  /* initialize the NBC_REDUCE SchedCache tree */
  comminfo->NBC_Dict[NBC_REDUCE] = hb_tree_new((dict_cmp_func)NBC_Reduce_args_compare, NBC_SchedCache_args_delete_key_dummy, NBC_SchedCache_args_delete);
  if(comminfo->NBC_Dict[NBC_REDUCE] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  NBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->NBC_Dict[NBC_REDUCE]);
  comminfo->NBC_Dict_size[NBC_REDUCE] = 0;
  /* initialize the NBC_SCAN SchedCache tree */
  comminfo->NBC_Dict[NBC_SCAN] = hb_tree_new((dict_cmp_func)NBC_Scan_args_compare, NBC_SchedCache_args_delete_key_dummy, NBC_SchedCache_args_delete);
  if(comminfo->NBC_Dict[NBC_SCAN] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  NBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->NBC_Dict[NBC_SCAN]);
  comminfo->NBC_Dict_size[NBC_SCAN] = 0;
  /* initialize the NBC_SCATTER SchedCache tree */
  comminfo->NBC_Dict[NBC_SCATTER] = hb_tree_new((dict_cmp_func)NBC_Scatter_args_compare, NBC_SchedCache_args_delete_key_dummy, NBC_SchedCache_args_delete);
  if(comminfo->NBC_Dict[NBC_SCATTER] == NULL) { printf("Error in hb_tree_new()\n"); return OMPI_ERROR;; }
  NBC_DEBUG(1, "added tree at address %lu\n", (unsigned long)comminfo->NBC_Dict[NBC_SCATTER]);
  comminfo->NBC_Dict_size[NBC_SCATTER] = 0;
#endif

  return OMPI_SUCCESS;
}

int NBC_Start(NBC_Handle *handle, NBC_Schedule *schedule) {
  int res;

  handle->schedule = schedule;

  /* kick off first round */
  res = NBC_Start_round(handle);
  if((NBC_OK != res)) { printf("Error in NBC_Start_round() (%i)\n", res); return res; }

  opal_list_append(&mca_coll_libnbc_component.active_requests, &(handle->super.super.super));

  return NBC_OK;
}

#ifdef NBC_CACHE_SCHEDULE
void NBC_SchedCache_args_delete_key_dummy(void *k) {
    /* do nothing because the key and the data element are identical :-) 
     * both (the single one :) is freed in NBC_<COLLOP>_args_delete() */
}

void NBC_SchedCache_args_delete(void *entry) {
  struct NBC_dummyarg *tmp;
  
  tmp = (struct NBC_dummyarg*)entry;
  /* free taglistentry */
  free((void*)*(tmp->schedule));
  /* the schedule pointer itself is also malloc'd */
  free((void*)tmp->schedule);
  free((void*)tmp);
}
#endif

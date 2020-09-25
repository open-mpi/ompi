/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006 The Trustees of Indiana University and Indiana
 *                    University Research and Technology
 *                    Corporation.  All rights reserved.
 * Copyright (c) 2006 The Technical University of Chemnitz. All
 *                    rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 */
#ifndef __PNBC_OSC_INTERNAL_H__
#define __PNBC_OSC_INTERNAL_H__
#include "ompi_config.h"

/* correct fortran bindings */
#define PNBC_OSC_F77_FUNC_ F77_FUNC_

#include "mpi.h"

#include "coll_libpnbc_osc.h"
#if OPAL_CUDA_SUPPORT
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_cuda.h"
#endif /* OPAL_CUDA_SUPPORT */
#include "ompi/include/ompi/constants.h"
#include "ompi/request/request.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <math.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

  /* log(2) */
#define LOG2 0.69314718055994530941

  /* true/false */
#define true 1
#define false 0

  /* all collectives */
#define PNBC_OSC_ALLGATHER 0
#define PNBC_OSC_ALLGATHERV 1
#define PNBC_OSC_ALLREDUCE 2
#define PNBC_OSC_ALLTOALL 3
#define PNBC_OSC_ALLTOALLV 4
#define PNBC_OSC_ALLTOALLW 5
#define PNBC_OSC_BARRIER 6
#define PNBC_OSC_BCAST 7
#define PNBC_OSC_EXSCAN 8
#define PNBC_OSC_GATHER 9
#define PNBC_OSC_GATHERV 10
#define PNBC_OSC_REDUCE 11
#define PNBC_OSC_REDUCESCAT 12
#define PNBC_OSC_SCAN 13
#define PNBC_OSC_SCATTER 14
#define PNBC_OSC_SCATTERV 15
  /* set the number of collectives in nbc.h !!!! */

  /* several typedefs for PNBC_OSC */
  /* the function type enum */
  typedef enum {
                SEND,
                RECV,
                OP,
                COPY,
                UNPACK,
                PUT,
                GET,
                TRY_GET,
                WIN_FREE,
  } PNBC_OSC_Fn_type;

  typedef enum {
                LOCKED,
                UNLOCKED
  } PNBC_OSC_Lock_status;

  /* the put argument struct */
  typedef struct {
    PNBC_OSC_Fn_type type;
    int origin_count;
    int target_count;
    const void *buf;
    MPI_Datatype origin_datatype;
    MPI_Datatype target_datatype;
    int target;
    char tmpbuf;
    bool local;
  } PNBC_OSC_Args_put;

  /* the win_free argument struct */
  typedef struct {
    PNBC_OSC_Fn_type type;
  } PNBC_OSC_Args_win_free;

  /* the get argument struct */
  typedef struct {
    PNBC_OSC_Fn_type type;
    int origin_count;
    int target_count;
    const void *buf;
    MPI_Datatype origin_datatype;
    MPI_Datatype target_datatype;
    MPI_Aint target_disp;
    int target;
    char tmpbuf;
    bool local;
    int lock_type;
    PNBC_OSC_Lock_status lock_status;
  } PNBC_OSC_Args_get;

  typedef struct {
    PNBC_OSC_Fn_type type;
    int origin_count;
    int target_count;
    const void *buf;
    MPI_Datatype origin_datatype;
    MPI_Datatype target_datatype;
    MPI_Aint target_disp;
    int target;
    char tmpbuf;
    bool local;
    int lock_type;
    int assert;
    PNBC_OSC_Lock_status lock_status;
    bool notify;
  } PNBC_OSC_Args_try_get;

  /* the send argument struct */
  typedef struct {
    PNBC_OSC_Fn_type type;
    int count;
    const void *buf;
    MPI_Datatype datatype;
    int dest;
    char tmpbuf;
    bool local;
  } PNBC_OSC_Args_send;

  /* the receive argument struct */
  typedef struct {
    PNBC_OSC_Fn_type type;
    int count;
    void *buf;
    MPI_Datatype datatype;
    char tmpbuf;
    int source;
    bool local;
  } PNBC_OSC_Args_recv;

  /* the operation argument struct */
  typedef struct {
    PNBC_OSC_Fn_type type;
    char tmpbuf1;
    char tmpbuf2;
    const void *buf1;
    void *buf2;
    MPI_Op op;
    MPI_Datatype datatype;
    int count;
  } PNBC_OSC_Args_op;

  /* the copy argument struct */
  typedef struct {
    PNBC_OSC_Fn_type type;
    int srccount;
    void *src;
    void *tgt;
    MPI_Datatype srctype;
    MPI_Datatype tgttype;
    int tgtcount;
    char tmpsrc;
    char tmptgt;
  } PNBC_OSC_Args_copy;

  /* unpack operation arguments */
  typedef struct {
    PNBC_OSC_Fn_type type;
    int count;
    void *inbuf;
    void *outbuf;
    MPI_Datatype datatype;
    char tmpinbuf;
    char tmpoutbuf;
  } PNBC_OSC_Args_unpack;

  /* internal function prototypes */
  /* Put */
  int PNBC_OSC_Sched_put (const void* buf, char tmpbuf, int origin_count,
                          MPI_Datatype origin_datatype,
                          int target, int target_count,  MPI_Datatype target_datatype,
                          PNBC_OSC_Schedule *schedule, bool barrier);
  int PNBC_OSC_Sched_local_put (const void* buf, char tmpbuf, int origin_count,
                                MPI_Datatype origin_datatype, int target,
                                PNBC_OSC_Schedule *schedule, bool barrier);

  /* Get */
  int PNBC_OSC_Sched_get (const void* buf, char tmpbuf, int origin_count,
                          MPI_Datatype origin_datatype, int target,
                          MPI_Aint target_disp, int target_count,
                          MPI_Datatype target_datatype,
                          PNBC_OSC_Schedule *schedule, int lock_type, bool barrier);
  int PNBC_OSC_Sched_local_get (const void* buf, char tmpbuf, int origin_count,
                                MPI_Datatype origin_datatype, int target,
                                PNBC_OSC_Schedule *schedule, bool barrier);
  /* try_get */
  int PNBC_OSC_Sched_try_get (const void* buf, char tmpbuf, int origin_count,
                              MPI_Datatype origin_datatype, int target,
                              MPI_Aint target_disp, int target_count,
                              MPI_Datatype target_datatype,
                              PNBC_OSC_Schedule *schedule, int lock_type,
                              int assert, bool notify, bool barrier);
  int PNBC_OSC_Sched_send (const void* buf, char tmpbuf, int count,
                           MPI_Datatype datatype, int dest,
                           PNBC_OSC_Schedule *schedule, bool barrier);
  int PNBC_OSC_Sched_local_send (const void* buf, char tmpbuf, int count,
                                 MPI_Datatype datatype, int dest,
                                 PNBC_OSC_Schedule *schedule, bool barrier);
  int PNBC_OSC_Sched_recv (void* buf, char tmpbuf, int count, MPI_Datatype datatype,
                           int source, PNBC_OSC_Schedule *schedule, bool barrier);
  int PNBC_OSC_Sched_local_recv (void* buf, char tmpbuf, int count, MPI_Datatype datatype
                                 , int source, PNBC_OSC_Schedule *schedule, bool barrier);
  int PNBC_OSC_Sched_op (const void* buf1, char tmpbuf1, void* buf2, char tmpbuf2, int count,
                         MPI_Datatype datatype, MPI_Op op, PNBC_OSC_Schedule *schedule,
                         bool barrier);
  int PNBC_OSC_Sched_copy (void *src, char tmpsrc, int srccount, MPI_Datatype srctype,
                           void *tgt, char tmptgt, int tgtcount,
                           MPI_Datatype tgttype, PNBC_OSC_Schedule *schedule, bool barrier);
  int PNBC_OSC_Sched_unpack (void *inbuf, char tmpinbuf, int count, MPI_Datatype datatype,
                             void *outbuf, char tmpoutbuf,
                             PNBC_OSC_Schedule *schedule, bool barrier);
  
  int PNBC_OSC_Sched_win_free(PNBC_OSC_Schedule *schedule, bool barrier);
  int PNBC_OSC_Sched_barrier (PNBC_OSC_Schedule *schedule);
  int PNBC_OSC_Sched_commit (PNBC_OSC_Schedule *schedule);


  int PNBC_OSC_Start(PNBC_OSC_Handle *handle);
  int PNBC_OSC_Schedule_request(PNBC_OSC_Schedule *schedule, ompi_communicator_t *comm,
                                ompi_coll_libpnbc_osc_module_t *module, bool persistent,
                                ompi_request_t **request, void *tmpbuf);
  int PNBC_OSC_Schedule_request_win(PNBC_OSC_Schedule *schedule, ompi_communicator_t *comm,
                                    ompi_win_t *win,ompi_win_t *winflag,
                                    ompi_coll_libpnbc_osc_module_t *module,
                                    bool persistent, ompi_request_t **request, void *tmpbuf);
  
  void PNBC_OSC_Return_handle(ompi_coll_libpnbc_osc_request_t *request);
  
  static inline int PNBC_OSC_Type_intrinsic(MPI_Datatype type);
  int PNBC_OSC_Create_fortran_handle(int *fhandle, PNBC_OSC_Handle **handle);
  
  /* some macros */
  
  static inline void PNBC_OSC_Error (char *format, ...) {
    va_list args;

    va_start (args, format);
    vfprintf (stderr, format, args);
    fprintf (stderr, "\n");
    va_end (args);
  }

  /* a schedule has the following format:
   * [schedule] ::= [size][round-schedule][delimiter][round-schedule][delimiter]...[end]
   * [size] ::= size of the schedule (int)
   * [round-schedule] ::= [num][type][type-args][type][type-args]...
   * [num] ::= number of elements in round (int)
   * [type] ::= function type (PNBC_OSC_Fn_type)
   * [type-args] ::= type specific arguments (PNBC_OSC_Args_send, PNBC_OSC_Args_recv or, PNBC_OSC_Args_op)
   * [delimiter] ::= 1 (char) - indicates that a round follows
   * [end] ::= 0 (char) - indicates that this is the last round
   */

  /*
   * The addresses of components of a round-schedule may be poorly aligned.
   * E.g., single-char delimiters can push addresses to odd-byte boundaries.
   * Or even ints can push 8-byte pointers to 4-byte boundaries.
   * So, for greater portability, we access components of a round-schedule with memcpy.
   */
#define PNBC_OSC_GET_BYTES(ptr,x) {memcpy(&x,ptr,sizeof(x)); ptr += sizeof(x);}
#define PNBC_OSC_PUT_BYTES(ptr,x) {memcpy(ptr,&x,sizeof(x)); ptr += sizeof(x);}

  /* PNBC_OSC_GET_ROUND_SIZE returns the size in bytes of a round of a PNBC_OSC_Schedule
   * schedule. A round has the format:
   * [num]{[type][type-args]}
   * e.g. [(int)2][(PNBC_OSC_Fn_type)SEND][(PNBC_OSC_Args_send)SEND-ARGS][(PNBC_OSC_Fn_type)RECV][(PNBC_OSC_Args_recv)RECV-ARGS] */
  static inline void nbc_get_round_size (char *p, unsigned long *size) {
    PNBC_OSC_Fn_type type;
    unsigned long offset = 0;
    int num;

    PNBC_OSC_GET_BYTES(p,num);
    /*PNBC_OSC_DEBUG(10, "GET_ROUND_SIZE got %i elements\n", num); */
    for (int i = 0 ; i < num ; ++i) {
      memcpy (&type, p + offset, sizeof (type));
      switch(type) {
      case WIN_FREE:
        /*printf("found a iFREE at offset %li\n", (long)p-(long)schedule); */
        offset += sizeof(PNBC_OSC_Args_win_free);
        break;
      case PUT:
        /*printf("found a PUT at offset %li\n", (long)p-(long)schedule); */
        offset += sizeof(PNBC_OSC_Args_put);
        break;
      case GET:
        /*printf("found a GET at offset %li\n", (long)p-(long)schedule); */
        offset += sizeof(PNBC_OSC_Args_get);
        break;
      case TRY_GET:
        /*printf("found a TRY_GET at offset %li\n", (long)p-(long)schedule); */
        offset += sizeof(PNBC_OSC_Args_try_get);
        break;
      case SEND:
        /*printf("found a SEND at offset %li\n", (long)p-(long)schedule); */
        offset += sizeof(PNBC_OSC_Args_send);
        break;
      case RECV:
        /*printf("found a RECV at offset %li\n", (long)p-(long)schedule); */
        offset += sizeof(PNBC_OSC_Args_recv);
        break;
      case OP:
        /*printf("found a OP at offset %li\n", (long)p-(long)schedule); */
        offset += sizeof(PNBC_OSC_Args_op);            \
        break;
      case COPY:
        /*printf("found a COPY at offset %li\n", (long)p-(long)schedule); */
        offset += sizeof(PNBC_OSC_Args_copy);
        break;
      case UNPACK:
        /*printf("found a UNPACK at offset %li\n", (long)p-(long)schedule); */
        offset += sizeof(PNBC_OSC_Args_unpack);
        break;
      default:
        PNBC_OSC_Error("PNBC_OSC_GET_ROUND_SIZE: bad type %i at offset %li", type, offset);
        return;
      }
    }

    *size = offset + sizeof (int);
  }


  /* returns the size of a schedule in bytes */
  static inline int nbc_schedule_get_size (PNBC_OSC_Schedule *schedule) {
    return schedule->size;
  }

  /* increase the size of a schedule by size bytes */
  static inline void nbc_schedule_inc_size (PNBC_OSC_Schedule *schedule, int size) {
    schedule->size += size;
  }

  /* increments the number of operations in the last round */
  static inline void nbc_schedule_inc_round (PNBC_OSC_Schedule *schedule) {
    int last_round_num;
    char *lastround;

    lastround = schedule->data + schedule->current_round_offset;

    /* increment the count in the last round of the schedule (memcpy is used
     * to protect against unaligned access) */
    memcpy (&last_round_num, lastround, sizeof (last_round_num));
    ++last_round_num;
    memcpy (lastround, &last_round_num, sizeof (last_round_num));
  }

  /* returns a no-operation request (e.g. for one process barrier) */
  static inline int nbc_get_noop_request(bool persistent, ompi_request_t **request) {
    if (persistent) {
      return ompi_request_persistent_noop_create(request);
    } else {
      *request = &ompi_request_empty;
      return OMPI_SUCCESS;
    }
  }

  /* PNBC_OSC_PRINT_ROUND prints a round in a schedule. A round has the format:
   * [num]{[type][type-args]} types: [int]{[enum][args-type]}
   * e.g. [2][SEND][SEND-ARGS][RECV][RECV-ARGS] */
#define PNBC_OSC_PRINT_ROUND(schedule)                                       \
  {                                                                     \
    int myrank, i, num;                                                 \
    char *p = (char*) schedule;                                         \
    PNBC_OSC_Fn_type type;                                                   \
    PNBC_OSC_Args_send     sendargs;                                         \
    PNBC_OSC_Args_recv     recvargs;                                         \
    PNBC_OSC_Args_op         opargs;                                         \
    PNBC_OSC_Args_copy     copyargs;                                         \
    PNBC_OSC_Args_unpack unpackargs;                                         \
                                                                        \
    PNBC_OSC_GET_BYTES(p,num);                                               \
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);                             \
    printf("[%i] has %i actions: \n", myrank, num);                     \
    for (i=0; i<num; i++) {                                             \
      PNBC_OSC_GET_BYTES(p,type);                                            \
      switch(type) {                                                    \
      case SEND:                                                        \
        printf("[%i]  SEND (offset %li) ", myrank, (long)p-(long)schedule); \
        PNBC_OSC_GET_BYTES(p,sendargs);                                      \
        printf("*buf: %lu, count: %i, type: %lu, dest: %i)\n", (unsigned long)sendargs.buf, sendargs.count, (unsigned long)sendargs.datatype, sendargs.dest); \
        break;                                                          \
      case RECV:                                                        \
        printf("[%i]  RECV (offset %li) ", myrank, (long)p-(long)schedule); \
        PNBC_OSC_GET_BYTES(p,recvargs);                                      \
        printf("*buf: %lu, count: %i, type: %lu, source: %i)\n", (unsigned long)recvargs.buf, recvargs.count, (unsigned long)recvargs.datatype, recvargs.source); \
        break;                                                          \
      case OP:                                                          \
        printf("[%i]  OP   (offset %li) ", myrank, (long)p-(long)schedule); \
        PNBC_OSC_GET_BYTES(p,opargs);                                        \
        printf("*buf1: %lu, buf2: %lu, count: %i, type: %lu)\n", (unsigned long)opargs.buf1, (unsigned long)opargs.buf2, opargs.count, (unsigned long)opargs.datatype); \
        break;                                                          \
      case COPY:                                                        \
        printf("[%i]  COPY   (offset %li) ", myrank, (long)p-(long)schedule); \
        PNBC_OSC_GET_BYTES(p,copyargs);                                      \
        printf("*src: %lu, srccount: %i, srctype: %lu, *tgt: %lu, tgtcount: %i, tgttype: %lu)\n", (unsigned long)copyargs.src, copyargs.srccount, (unsigned long)copyargs.srctype, (unsigned long)copyargs.tgt, copyargs.tgtcount, (unsigned long)copyargs.tgttype); \
        break;                                                          \
      case UNPACK:                                                      \
        printf("[%i]  UNPACK   (offset %li) ", myrank, (long)p-(long)schedule); \
        PNBC_OSC_GET_BYTES(p,unpackargs);                                    \
        printf("*src: %lu, srccount: %i, srctype: %lu, *tgt: %lu\n",(unsigned long)unpackargs.inbuf, unpackargs.count, (unsigned long)unpackargs.datatype, (unsigned long)unpackargs.outbuf); \
        break;                                                          \
      default:                                                          \
        printf("[%i] PNBC_OSC_PRINT_ROUND: bad type %i at offset %li\n", myrank, type, (long)p-sizeof(type)-(long)schedule); \
        return PNBC_OSC_BAD_SCHED;                                           \
      }                                                                 \
    }                                                                   \
    printf("\n");                                                       \
  }

#define PNBC_OSC_PRINT_SCHED(schedule)                                       \
  {                                                                     \
    int size, myrank;                                                   \
    long round_size;                                                    \
    char *ptr;                                                          \
                                                                        \
    PNBC_OSC_GET_SIZE(schedule, size);                                       \
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);                             \
    printf("[%i] printing schedule of size %i\n", myrank, size);        \
                                                                        \
    /* ptr begins at first round (first int is overall size) */         \
    ptr = (char*)schedule+sizeof(int);                                  \
    while ((long)ptr-(long)schedule < size) {                           \
      PNBC_OSC_GET_ROUND_SIZE(ptr, round_size);                              \
      printf("[%i] Round at byte %li (size %li) ", myrank, (long)ptr-(long)schedule, round_size); \
      PNBC_OSC_PRINT_ROUND(ptr);                                             \
      ptr += round_size;                                                \
      ptr += sizeof(char); /* barrier delimiter */                      \
    }                                                                   \
  }

  /*
    #define PNBC_OSC_DEBUG(level, ...) {}
  */

  static inline void PNBC_OSC_DEBUG(int level, const char *fmt, ...)
  {
#if PNBC_OSC_DLEVEL > 0
    va_list ap;
    int rank;

    if(PNBC_OSC_DLEVEL >= level) {
      MPI_Comm_rank(MPI_COMM_WORLD, &rank);

      printf("[LibPNBC_OSC - %i] ", rank);
      va_start(ap, fmt);
      vprintf(fmt, ap);
      va_end (ap);
    }
#endif
  }

  /* returns true (1) or false (0) if type is intrinsic or not */
  static inline int PNBC_OSC_Type_intrinsic(MPI_Datatype type) {

    if( ( type == MPI_INT ) ||
        ( type == MPI_LONG ) ||
        ( type == MPI_SHORT ) ||
        ( type == MPI_UNSIGNED ) ||
        ( type == MPI_UNSIGNED_SHORT ) ||
        ( type == MPI_UNSIGNED_LONG ) ||
        ( type == MPI_FLOAT ) ||
        ( type == MPI_DOUBLE ) ||
        ( type == MPI_LONG_DOUBLE ) ||
        ( type == MPI_BYTE ) ||
        ( type == MPI_FLOAT_INT) ||
        ( type == MPI_DOUBLE_INT) ||
        ( type == MPI_LONG_INT) ||
        ( type == MPI_2INT) ||
        ( type == MPI_SHORT_INT) ||
        ( type == MPI_LONG_DOUBLE_INT))
      return 1;
    else
      return 0;
  }

  /* let's give a try to inline functions */
  static inline int PNBC_OSC_Copy(const void *src, int srccount, MPI_Datatype srctype,
                                  void *tgt, int tgtcount, MPI_Datatype tgttype,
                                  MPI_Comm comm) {
    int res;

    res = ompi_datatype_sndrcv(src, srccount, srctype, tgt, tgtcount, tgttype);
    if (OMPI_SUCCESS != res) {
      PNBC_OSC_Error ("MPI Error in ompi_datatype_sndrcv() (%i)", res);
      return res;
    }

    return OMPI_SUCCESS;
  }

  static inline int PNBC_OSC_Unpack(void *src, int srccount, MPI_Datatype srctype,
                                    void *tgt, MPI_Comm comm) {
    MPI_Aint size, pos;
    int res;
    ptrdiff_t ext, lb;

    res = ompi_datatype_pack_external_size("external32", srccount, srctype, &size);
    if (OMPI_SUCCESS != res) {
      PNBC_OSC_Error ("MPI Error in ompi_datatype_pack_external_size() (%i)", res);
      return res;
    }
#if OPAL_CUDA_SUPPORT
    if(PNBC_OSC_Type_intrinsic(srctype) && !(opal_cuda_check_bufs((char *)tgt, (char *)src))) {
#else
      if(PNBC_OSC_Type_intrinsic(srctype)) {
#endif /* OPAL_CUDA_SUPPORT */
        /* if we have the same types and they are contiguous (intrinsic
         * types are contiguous), we can just use a single memcpy */
        res = ompi_datatype_get_extent (srctype, &lb, &ext);
        if (OMPI_SUCCESS != res) {
          PNBC_OSC_Error ("MPI Error in MPI_Type_extent() (%i)", res);
          return res;
        }

        memcpy(tgt, src, srccount * ext);

      } else {
        /* we have to unpack */
        pos = 0;
        res = ompi_datatype_unpack_external("external32", src, size, &pos, tgt,
                                            srccount, srctype);
        if (MPI_SUCCESS != res) {
          PNBC_OSC_Error ("MPI Error in ompi_datatype_unpack_external() (%i)", res);
          return res;
        }
      }

      return OMPI_SUCCESS;
    }

#define PNBC_OSC_IN_PLACE(sendbuf, recvbuf, inplace) \
    {                                           \
      inplace = 0;                              \
      if(recvbuf == sendbuf) {                  \
        inplace = 1;                            \
      } else                                    \
        if(sendbuf == MPI_IN_PLACE) {           \
          sendbuf = recvbuf;                    \
          inplace = 1;                          \
        } else                                  \
          if(recvbuf == MPI_IN_PLACE) {         \
            recvbuf = (void *)sendbuf;          \
            inplace = 1;                        \
          }                                     \
    }

    int PNBC_OSC_Comm_neighbors_count (ompi_communicator_t *comm, int *indegree,
                                       int *outdegree);
    int PNBC_OSC_Comm_neighbors (ompi_communicator_t *comm, int **sources,
                                 int *source_count, int **destinations,
                                 int *dest_count);

#ifdef __cplusplus
  }
#endif

#endif

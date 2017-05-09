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
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 */
#ifndef __NBC_INTERNAL_H__
#define __NBC_INTERNAL_H__
#include "ompi_config.h"

/* correct fortran bindings */
#define PNBC_F77_FUNC_ F77_FUNC_

#include "mpi.h"

#include "coll_libpnbc.h"
#if OPAL_CUDA_SUPPORT
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_cuda.h"
#endif /* OPAL_CUDA_SUPPORT */
#include "ompi/include/ompi/constants.h"
#include "ompi/request/request.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <math.h>
#include <string.h>
#include "libdict/dict.h"

#ifdef __cplusplus
extern "C" {
#endif

/* log(2) */
#define LOG2 0.69314718055994530941

/* true/false */
#define true 1
#define false 0

/* all collectives */
#define PNBC_ALLGATHER 0
#define PNBC_ALLGATHERV 1
#define PNBC_ALLREDUCE 2
#define PNBC_ALLTOALL 3
#define PNBC_ALLTOALLV 4
#define PNBC_ALLTOALLW 5
#define PNBC_BARRIER 6
#define PNBC_BCAST 7
#define PNBC_EXSCAN 8
#define PNBC_GATHER 9
#define PNBC_GATHERV 10
#define PNBC_REDUCE 11
#define PNBC_REDUCESCAT 12
#define PNBC_SCAN 13
#define PNBC_SCATTER 14
#define PNBC_SCATTERV 15
/* set the number of collectives in nbc.h !!!! */

/* several typedefs for NBC */

/* the function type enum */
typedef enum {
  SEND,
  RECV,
  OP,
  COPY,
  UNPACK
} PNBC_Fn_type;

/* the send argument struct */
typedef struct {
  PNBC_Fn_type type;
  int count;
  const void *buf;
  MPI_Datatype datatype;
  int dest;
  char tmpbuf;
  bool local;
} PNBC_Args_send;

/* the receive argument struct */
typedef struct {
  PNBC_Fn_type type;
  int count;
  void *buf;
  MPI_Datatype datatype;
  char tmpbuf;
  int source;
  bool local;
} PNBC_Args_recv;

/* the operation argument struct */
typedef struct {
  PNBC_Fn_type type;
  char tmpbuf1;
  char tmpbuf2;
  const void *buf1;
  void *buf2;
  MPI_Op op;
  MPI_Datatype datatype;
  int count;
} PNBC_Args_op;

/* the copy argument struct */
typedef struct {
  PNBC_Fn_type type;
  int srccount;
  void *src;
  void *tgt;
  MPI_Datatype srctype;
  MPI_Datatype tgttype;
  int tgtcount;
  char tmpsrc;
  char tmptgt;
} PNBC_Args_copy;

/* unpack operation arguments */
typedef struct {
  PNBC_Fn_type type;
  int count;
  void *inbuf;
  void *outbuf;
  MPI_Datatype datatype;
  char tmpinbuf;
  char tmpoutbuf;
} PNBC_Args_unpack;

/* internal function prototypes */
int PNBC_Sched_send (const void* buf, char tmpbuf, int count, MPI_Datatype datatype, int dest, PNBC_Schedule *schedule, bool barrier);
int PNBC_Sched_local_send (const void* buf, char tmpbuf, int count, MPI_Datatype datatype, int dest,PNBC_Schedule *schedule, bool barrier);
int PNBC_Sched_recv (void* buf, char tmpbuf, int count, MPI_Datatype datatype, int source, PNBC_Schedule *schedule, bool barrier);
int PNBC_Sched_local_recv (void* buf, char tmpbuf, int count, MPI_Datatype datatype, int source, PNBC_Schedule *schedule, bool barrier);
int PNBC_Sched_op (const void* buf1, char tmpbuf1, void* buf2, char tmpbuf2, int count, MPI_Datatype datatype,
                  MPI_Op op, PNBC_Schedule *schedule, bool barrier);
int PNBC_Sched_copy (void *src, char tmpsrc, int srccount, MPI_Datatype srctype, void *tgt, char tmptgt, int tgtcount,
                    MPI_Datatype tgttype, PNBC_Schedule *schedule, bool barrier);
int PNBC_Sched_unpack (void *inbuf, char tmpinbuf, int count, MPI_Datatype datatype, void *outbuf, char tmpoutbuf,
                      PNBC_Schedule *schedule, bool barrier);

int PNBC_Sched_barrier (PNBC_Schedule *schedule);
int PNBC_Sched_commit (PNBC_Schedule *schedule);

#ifdef PNBC_CACHE_SCHEDULE
/* this is a dummy structure which is used to get the schedule out of
 * the collop specific structure. The schedule pointer HAS to be at the
 * first position and should NOT BE REORDERED by the compiler (C
 * guarantees that */
struct PNBC_dummyarg {
  PNBC_Schedule *schedule;
};

typedef struct {
  PNBC_Schedule *schedule;
  void *sendbuf;
  int sendcount;
  MPI_Datatype sendtype;
  void* recvbuf;
  int recvcount;
  MPI_Datatype recvtype;
} PNBC_Alltoall_args;
int PNBC_Alltoall_args_compare(PNBC_Alltoall_args *a, PNBC_Alltoall_args *b, void *param);

typedef struct {
  PNBC_Schedule *schedule;
  void *sendbuf;
  int sendcount;
  MPI_Datatype sendtype;
  void* recvbuf;
  int recvcount;
  MPI_Datatype recvtype;
} PNBC_Allgather_args;
int PNBC_Allgather_args_compare(PNBC_Allgather_args *a, PNBC_Allgather_args *b, void *param);

typedef struct {
  PNBC_Schedule *schedule;
  void *sendbuf;
  void* recvbuf;
  int count;
  MPI_Datatype datatype;
  MPI_Op op;
} PNBC_Allreduce_args;
int PNBC_Allreduce_args_compare(PNBC_Allreduce_args *a, PNBC_Allreduce_args *b, void *param);

typedef struct {
  PNBC_Schedule *schedule;
  void *buffer;
  int count;
  MPI_Datatype datatype;
  int root;
} PNBC_Bcast_args;
int PNBC_Bcast_args_compare(PNBC_Bcast_args *a, PNBC_Bcast_args *b, void *param);

typedef struct {
  PNBC_Schedule *schedule;
  void *sendbuf;
  int sendcount;
  MPI_Datatype sendtype;
  void* recvbuf;
  int recvcount;
  MPI_Datatype recvtype;
  int root;
} PNBC_Gather_args;
int PNBC_Gather_args_compare(PNBC_Gather_args *a, PNBC_Gather_args *b, void *param);

typedef struct {
  PNBC_Schedule *schedule;
  void *sendbuf;
  void* recvbuf;
  int count;
  MPI_Datatype datatype;
  MPI_Op op;
  int root;
} PNBC_Reduce_args;
int PNBC_Reduce_args_compare(PNBC_Reduce_args *a, PNBC_Reduce_args *b, void *param);

typedef struct {
  PNBC_Schedule *schedule;
  void *sendbuf;
  void* recvbuf;
  int count;
  MPI_Datatype datatype;
  MPI_Op op;
} PNBC_Scan_args;
int PNBC_Scan_args_compare(PNBC_Scan_args *a, PNBC_Scan_args *b, void *param);

typedef struct {
  PNBC_Schedule *schedule;
  void *sendbuf;
  int sendcount;
  MPI_Datatype sendtype;
  void* recvbuf;
  int recvcount;
  MPI_Datatype recvtype;
  int root;
} PNBC_Scatter_args;
int PNBC_Scatter_args_compare(PNBC_Scatter_args *a, PNBC_Scatter_args *b, void *param);

/* Schedule cache structures/functions */
void PNBC_SchedCache_args_delete(void *entry);
void PNBC_SchedCache_args_delete_key_dummy(void *k);

#endif


int PNBC_Start_internal(PNBC_Handle *handle, PNBC_Schedule *schedule);
int PNBC_Init_handle(struct ompi_communicator_t *comm, ompi_coll_libpnbc_request_t **request, ompi_coll_libpnbc_module_t *module);
void PNBC_Return_handle(ompi_coll_libpnbc_request_t *request);
static inline int PNBC_Type_intrinsic(MPI_Datatype type);
int PNBC_Create_fortran_handle(int *fhandle, PNBC_Handle **handle);

/* some macros */

static inline void PNBC_Error (char *format, ...) {
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
 * [type] ::= function type (PNBC_Fn_type)
 * [type-args] ::= type specific arguments (PNBC_Args_send, PNBC_Args_recv or, PNBC_Args_op)
 * [delimiter] ::= 1 (char) - indicates that a round follows
 * [end] ::= 0 (char) - indicates that this is the last round
 */

/*
 * The addresses of components of a round-schedule may be poorly aligned.
 * E.g., single-char delimiters can push addresses to odd-byte boundaries.
 * Or even ints can push 8-byte pointers to 4-byte boundaries.
 * So, for greater portability, we access components of a round-schedule with memcpy.
 */
#define PNBC_GET_BYTES(ptr,x) {memcpy(&x,ptr,sizeof(x)); ptr += sizeof(x);}
#define PNBC_PUT_BYTES(ptr,x) {memcpy(ptr,&x,sizeof(x)); ptr += sizeof(x);}

/* PNBC_GET_ROUND_SIZE returns the size in bytes of a round of a PNBC_Schedule
 * schedule. A round has the format:
 * [num]{[type][type-args]}
 * e.g. [(int)2][(PNBC_Fn_type)SEND][(PNBC_Args_send)SEND-ARGS][(PNBC_Fn_type)RECV][(PNBC_Args_recv)RECV-ARGS] */
static inline void pnbc_get_round_size (char *p, unsigned long *size) {
  PNBC_Fn_type type;
  unsigned long offset = 0;
  int num;

  PNBC_GET_BYTES(p,num);
  printf("GET_ROUND_SIZE got %i elements\n", num);
  for (int i = 0 ; i < num ; ++i) {
    memcpy (&type, p + offset, sizeof (type));
    switch(type) {
    case SEND:
      //printf("found a SEND at offset %li\n", (long)p-(long)schedule);
      offset += sizeof(PNBC_Args_send);
      break;
    case RECV:
      //printf("found a RECV at offset %li\n", (long)p-(long)schedule);
      offset += sizeof(PNBC_Args_recv);
      break;
    case OP:
     // printf("found a OP at offset %li\n", (long)p-(long)schedule);
      offset += sizeof(PNBC_Args_op);            \
      break;
    case COPY:
      //printf("found a COPY at offset %li\n", (long)p-(long)schedule);
      offset += sizeof(PNBC_Args_copy);
      break;
    case UNPACK:
      //printf("found a UNPACK at offset %li\n", (long)p-(long)schedule);
      offset += sizeof(PNBC_Args_unpack);
      break;
    default:
      PNBC_Error("PNBC_GET_ROUND_SIZE: bad type %i at offset %li", type, offset);
      return;
    }
  }

  *size = offset + sizeof (int);
}


/* returns the size of a schedule in bytes */
static inline int pnbc_schedule_get_size (PNBC_Schedule *schedule) {
  return schedule->size;
}

/* increase the size of a schedule by size bytes */
static inline void pnbc_schedule_inc_size (PNBC_Schedule *schedule, int size) {
  schedule->size += size;
}

/* increments the number of operations in the last round */
static inline void pnbc_schedule_inc_round (PNBC_Schedule *schedule) {
  int last_round_num;
  char *lastround;

  lastround = schedule->data + schedule->current_round_offset;

  /* increment the count in the last round of the schedule (memcpy is used
   * to protect against unaligned access) */
  memcpy (&last_round_num, lastround, sizeof (last_round_num));
  ++last_round_num;
  memcpy (lastround, &last_round_num, sizeof (last_round_num));
}

/* PNBC_PRINT_ROUND prints a round in a schedule. A round has the format:
 * [num]{[type][type-args]} types: [int]{[enum][args-type]}
 * e.g. [2][SEND][SEND-ARGS][RECV][RECV-ARGS] */
#define PNBC_PRINT_ROUND(schedule) \
 {  \
   int myrank, i, num; \
   char *p = (char*) schedule; \
   PNBC_Fn_type type; \
   PNBC_Args_send     sendargs; \
   PNBC_Args_recv     recvargs; \
   PNBC_Args_op         opargs; \
   PNBC_Args_copy     copyargs; \
   PNBC_Args_unpack unpackargs; \
     \
   PNBC_GET_BYTES(p,num); \
   MPI_Comm_rank(MPI_COMM_WORLD, &myrank); \
   printf("[%i] has %i actions: \n", myrank, num); \
   for (i=0; i<num; i++) { \
     PNBC_GET_BYTES(p,type); \
     switch(type) { \
       case SEND: \
         printf("[%i]  SEND (offset %li) ", myrank, (long)p-(long)schedule); \
         PNBC_GET_BYTES(p,sendargs); \
         printf("*buf: %lu, count: %i, type: %lu, dest: %i)\n", (unsigned long)sendargs.buf, sendargs.count, (unsigned long)sendargs.datatype, sendargs.dest); \
         break; \
       case RECV: \
         printf("[%i]  RECV (offset %li) ", myrank, (long)p-(long)schedule); \
         PNBC_GET_BYTES(p,recvargs); \
         printf("*buf: %lu, count: %i, type: %lu, source: %i)\n", (unsigned long)recvargs.buf, recvargs.count, (unsigned long)recvargs.datatype, recvargs.source); \
         break; \
       case OP: \
         printf("[%i]  OP   (offset %li) ", myrank, (long)p-(long)schedule); \
         PNBC_GET_BYTES(p,opargs); \
         printf("*buf1: %lu, buf2: %lu, count: %i, type: %lu)\n", (unsigned long)opargs.buf1, (unsigned long)opargs.buf2, opargs.count, (unsigned long)opargs.datatype); \
         break; \
       case COPY: \
         printf("[%i]  COPY   (offset %li) ", myrank, (long)p-(long)schedule); \
         PNBC_GET_BYTES(p,copyargs); \
         printf("*src: %lu, srccount: %i, srctype: %lu, *tgt: %lu, tgtcount: %i, tgttype: %lu)\n", (unsigned long)copyargs.src, copyargs.srccount, (unsigned long)copyargs.srctype, (unsigned long)copyargs.tgt, copyargs.tgtcount, (unsigned long)copyargs.tgttype); \
         break; \
       case UNPACK: \
         printf("[%i]  UNPACK   (offset %li) ", myrank, (long)p-(long)schedule); \
         PNBC_GET_BYTES(p,unpackargs); \
         printf("*src: %lu, srccount: %i, srctype: %lu, *tgt: %lu\n",(unsigned long)unpackargs.inbuf, unpackargs.count, (unsigned long)unpackargs.datatype, (unsigned long)unpackargs.outbuf); \
         break; \
       default: \
         printf("[%i] PNBC_PRINT_ROUND: bad type %i at offset %li\n", myrank, type, (long)p-sizeof(type)-(long)schedule); \
         return PNBC_BAD_SCHED; \
     } \
   } \
   printf("\n"); \
 }

#define PNBC_PRINT_SCHED(schedule) \
{ \
  int size, myrank; \
  long round_size; \
  char *ptr; \
 \
  PNBC_GET_SIZE(schedule, size); \
  MPI_Comm_rank(MPI_COMM_WORLD, &myrank); \
  printf("[%i] printing schedule of size %i\n", myrank, size); \
 \
  /* ptr begins at first round (first int is overall size) */ \
  ptr = (char*)schedule+sizeof(int); \
  while ((long)ptr-(long)schedule < size) { \
    PNBC_GET_ROUND_SIZE(ptr, round_size); \
    printf("[%i] Round at byte %li (size %li) ", myrank, (long)ptr-(long)schedule, round_size); \
    PNBC_PRINT_ROUND(ptr); \
    ptr += round_size; \
    ptr += sizeof(char); /* barrier delimiter */ \
  } \
}

/*
#define PNBC_DEBUG(level, ...) {}
*/

static inline void PNBC_DEBUG(int level, const char *fmt, ...)
{
#if PNBC_DLEVEL > 0
  va_list ap;
  int rank;

  if(PNBC_DLEVEL >= level) {
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    printf("[LibNBC - %i] ", rank);
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end (ap);
  }
#endif
}

/* returns true (1) or false (0) if type is intrinsic or not */
static inline int PNBC_Type_intrinsic(MPI_Datatype type) {

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
static inline int PNBC_Copy(const void *src, int srccount, MPI_Datatype srctype, void *tgt, int tgtcount, MPI_Datatype tgttype, MPI_Comm comm) {
  int size, pos, res;
  void *packbuf;

#if OPAL_CUDA_SUPPORT
  if((srctype == tgttype) && PNBC_Type_intrinsic(srctype) && !(opal_cuda_check_bufs((char *)tgt, (char *)src))) {
#else
  if((srctype == tgttype) && PNBC_Type_intrinsic(srctype)) {
#endif /* OPAL_CUDA_SUPPORT */
    /* if we have the same types and they are contiguous (intrinsic
     * types are contiguous), we can just use a single memcpy */
    ptrdiff_t gap, span;
    span = opal_datatype_span(&srctype->super, srccount, &gap);

    memcpy(tgt, src, span);
  } else {
    /* we have to pack and unpack */
    res = PMPI_Pack_size(srccount, srctype, comm, &size);
    if (MPI_SUCCESS != res) {
      PNBC_Error ("MPI Error in PMPI_Pack_size() (%i:%i)", res, size);
      return res;
    }

    if (0 == size) {
        return OMPI_SUCCESS;
    }
    packbuf = malloc(size);
    if (NULL == packbuf) {
      PNBC_Error("Error in malloc()");
      return res;
    }

    pos=0;
    res = PMPI_Pack(src, srccount, srctype, packbuf, size, &pos, comm);

    if (MPI_SUCCESS != res) {
      PNBC_Error ("MPI Error in PMPI_Pack() (%i)", res);
      free (packbuf);
      return res;
    }

    pos=0;
    res = PMPI_Unpack(packbuf, size, &pos, tgt, tgtcount, tgttype, comm);
    free(packbuf);
    if (MPI_SUCCESS != res) {
      PNBC_Error ("MPI Error in PMPI_Unpack() (%i)", res);
      return res;
    }
  }

  return OMPI_SUCCESS;
}

static inline int PNBC_Unpack(void *src, int srccount, MPI_Datatype srctype, void *tgt, MPI_Comm comm) {
  int size, pos, res;
  ptrdiff_t ext, lb;

#if OPAL_CUDA_SUPPORT
  if(PNBC_Type_intrinsic(srctype) && !(opal_cuda_check_bufs((char *)tgt, (char *)src))) {
#else
  if(PNBC_Type_intrinsic(srctype)) {
#endif /* OPAL_CUDA_SUPPORT */
    /* if we have the same types and they are contiguous (intrinsic
     * types are contiguous), we can just use a single memcpy */
    res = ompi_datatype_get_extent (srctype, &lb, &ext);
    if (OMPI_SUCCESS != res) {
      PNBC_Error ("MPI Error in MPI_Type_extent() (%i)", res);
      return res;
    }

    memcpy(tgt, src, srccount * ext);

  } else {
    /* we have to unpack */
    res = PMPI_Pack_size(srccount, srctype, comm, &size);
    if (MPI_SUCCESS != res) {
      PNBC_Error ("MPI Error in PMPI_Pack_size() (%i)", res);
      return res;
    }
    pos = 0;
    res = PMPI_Unpack(src, size, &pos, tgt, srccount, srctype, comm);
    if (MPI_SUCCESS != res) {
      PNBC_Error ("MPI Error in PMPI_Unpack() (%i)", res);
      return res;
    }
  }

  return OMPI_SUCCESS;
}

/* deletes elements from dict until low watermark is reached */
static inline void PNBC_SchedCache_dictwipe(hb_tree *dict, int *size) {
  hb_itor *itor;

  itor = hb_itor_new(dict);
  for (; hb_itor_valid(itor) && (*size>PNBC_SCHED_DICT_LOWER); hb_itor_next(itor)) {
    hb_tree_remove(dict, hb_itor_key(itor), 0);
    *size = *size-1;
  }
  hb_itor_destroy(itor);
}

#define PNBC_IN_PLACE(sendbuf, recvbuf, inplace) \
{ \
  inplace = 0; \
  if(recvbuf == sendbuf) { \
    inplace = 1; \
  } else \
  if(sendbuf == MPI_IN_PLACE) { \
    sendbuf = recvbuf; \
    inplace = 1; \
  } else \
  if(recvbuf == MPI_IN_PLACE) { \
    recvbuf = (void *)sendbuf; \
    inplace = 1; \
  } \
}

int PNBC_Comm_neighbors_count (ompi_communicator_t *comm, int *indegree, int *outdegree);
int PNBC_Comm_neighbors (ompi_communicator_t *comm, int **sources, int *source_count, int **destinations, int *dest_count);

#ifdef __cplusplus
}
#endif

#endif



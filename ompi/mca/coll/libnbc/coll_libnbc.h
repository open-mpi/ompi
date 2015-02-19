/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_LIBNBC_EXPORT_H
#define MCA_COLL_LIBNBC_EXPORT_H

#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "opal/sys/atomic.h"

BEGIN_C_DECLS

/*********************** LibNBC tuning parameters ************************/

/* the debug level */
#define NBC_DLEVEL 0

/* enable schedule caching - undef NBC_CACHE_SCHEDULE to deactivate it */
/* TODO: this whole schedule cache stuff does not work with the tmbuf
 * :-( - first, the tmpbuf must not be freed if a schedule using it is
 * still in the cache and second, the tmpbuf used by the schedule must
 * be attached to the handle that uses this schedule !!!! 
 * I.E., THIS IS EXPERIMENTAL AND MIGHT NOT WORK */
/* It also leaks memory because the schedule is never cleaned up when
   the communicator is destroyed, so don't use it for now */
#ifdef NBC_CACHE_SCHEDULE
#undef NBC_CACHE_SCHEDULE
#endif
#define NBC_SCHED_DICT_UPPER 1024 /* max. number of dict entries */
#define NBC_SCHED_DICT_LOWER 512  /* nuber of dict entries after wipe, if SCHED_DICT_UPPER is reached */

/********************* end of LibNBC tuning parameters ************************/

/* Function return codes  */
#define NBC_OK 0 /* everything went fine */
#define NBC_SUCCESS 0 /* everything went fine (MPI compliant :) */
#define NBC_OOR 1 /* out of resources */
#define NBC_BAD_SCHED 2 /* bad schedule */
#define NBC_CONTINUE 3 /* progress not done */
#define NBC_DATATYPE_NOT_SUPPORTED 4 /* datatype not supported or not valid */
#define NBC_OP_NOT_SUPPORTED 5 /* operation not supported or not valid */
#define NBC_NOT_IMPLEMENTED 6
#define NBC_INVALID_PARAM 7 /* invalid parameters */
#define NBC_INVALID_TOPOLOGY_COMM 8 /* invalid topology attached to communicator */

/* number of implemented collective functions */
#define NBC_NUM_COLL 17

struct ompi_coll_libnbc_component_t {
    mca_coll_base_component_2_0_0_t super;
    opal_free_list_t requests;
    opal_list_t active_requests;
    int32_t active_comms;
    opal_atomic_lock_t progress_lock;
};
typedef struct ompi_coll_libnbc_component_t ompi_coll_libnbc_component_t;

/* Globally exported variables */
OMPI_MODULE_DECLSPEC extern ompi_coll_libnbc_component_t mca_coll_libnbc_component;

struct ompi_coll_libnbc_module_t {
    mca_coll_base_module_t super;
    opal_mutex_t mutex;
    bool comm_registered;
    int tag;
#ifdef NBC_CACHE_SCHEDULE
  void *NBC_Dict[NBC_NUM_COLL]; /* this should point to a struct
                                      hb_tree, but since this is a
                                      public header-file, this would be
                                      an include mess :-(. So let's void
                                      it ...*/
  int NBC_Dict_size[NBC_NUM_COLL];
#endif
};
typedef struct ompi_coll_libnbc_module_t ompi_coll_libnbc_module_t;
OBJ_CLASS_DECLARATION(ompi_coll_libnbc_module_t);

typedef ompi_coll_libnbc_module_t NBC_Comminfo;

/* a schedule is basically a pointer to some memory location where the
 * schedule array resides */ 
typedef void* NBC_Schedule;

struct ompi_coll_libnbc_request_t {
    ompi_request_t super;
    MPI_Comm comm;
    long row_offset;
    int tag;
    volatile int req_count;
    ompi_request_t **req_array;
    NBC_Comminfo *comminfo;
    volatile NBC_Schedule *schedule;
    void *tmpbuf; /* temporary buffer e.g. used for Reduce */
    /* TODO: we should make a handle pointer to a state later (that the user
     * can move request handles) */
};
typedef struct ompi_coll_libnbc_request_t ompi_coll_libnbc_request_t;
OBJ_CLASS_DECLARATION(ompi_coll_libnbc_request_t);

typedef ompi_coll_libnbc_request_t NBC_Handle;


#define OMPI_COLL_LIBNBC_REQUEST_ALLOC(comm, req)                       \
    do {                                                                \
        opal_free_list_item_t *item;                                    \
        item = opal_free_list_wait (&mca_coll_libnbc_component.requests); \
        req = (ompi_coll_libnbc_request_t*) item;                       \
        OMPI_REQUEST_INIT(&req->super, false);                          \
        req->super.req_mpi_object.comm = comm;                          \
        req->super.req_complete = false;                                \
        req->super.req_state = OMPI_REQUEST_ACTIVE;                     \
    } while (0)

#define OMPI_COLL_LIBNBC_REQUEST_RETURN(req)                            \
    do {                                                                \
        OMPI_REQUEST_FINI(&request->super);                             \
        opal_free_list_return (&mca_coll_libnbc_component.requests,     \
                               (opal_free_list_item_t*) req);           \
    } while (0)

int ompi_coll_libnbc_progress(void);

int NBC_Init_comm(MPI_Comm comm, ompi_coll_libnbc_module_t *module);
int NBC_Progress(NBC_Handle *handle);


int ompi_coll_libnbc_iallgather(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iallgatherv(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *displs, 
                                 MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iallreduce(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op, 
                                struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ialltoall(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                               MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ialltoallv(void* sendbuf, int *sendcounts, int *sdispls,
                                MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *rdispls,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ialltoallw(void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t **sdtypes, 
                                void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t **rdtypes, 
                                struct ompi_communicator_t *comm, ompi_request_t **request, 
                                struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ibarrier(struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ibcast(void *buffer, int count, MPI_Datatype datatype, int root,
                            struct ompi_communicator_t *comm, ompi_request_t ** request,
                            struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iexscan(void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, 
                             struct ompi_op_t *op, struct ompi_communicator_t *comm, ompi_request_t **request,
                             struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_igather(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                             MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_igatherv(void* sendbuf, int sendcount, MPI_Datatype sendtype, 
                              void* recvbuf, int *recvcounts, int *displs, MPI_Datatype recvtype, 
                              int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ireduce(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, 
                             MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ireduce_scatter(void* sendbuf, void* recvbuf, int *recvcounts, MPI_Datatype datatype, 
                                     MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ireduce_scatter_block(void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                           struct ompi_op_t *op, struct ompi_communicator_t *comm, 
                                           ompi_request_t **request,
                                           struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iscan(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op, 
                           struct ompi_communicator_t *comm, ompi_request_t ** request,
                           struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iscatter(void* sendbuf, int sendcount, MPI_Datatype sendtype, 
                              void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, 
                              struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iscatterv(void* sendbuf, int *sendcounts, int *displs, MPI_Datatype sendtype, 
                               void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, 
                               struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_1_0_t *module);


int ompi_coll_libnbc_iallgather_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iallgatherv_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *displs, 
                                 MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iallreduce_inter(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op, 
                                struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ialltoall_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                               MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ialltoallv_inter(void* sendbuf, int *sendcounts, int *sdispls,
                                MPI_Datatype sendtype, void* recvbuf, int *recvcounts, int *rdispls,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ialltoallw_inter(void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t **sdtypes, 
                                      void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t **rdtypes, 
                                      struct ompi_communicator_t *comm, ompi_request_t **request, 
                                      struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ibarrier_inter(struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ibcast_inter(void *buffer, int count, MPI_Datatype datatype, int root,
                            struct ompi_communicator_t *comm, ompi_request_t ** request,
                            struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_igather_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount, 
                             MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_igatherv_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, 
                              void* recvbuf, int *recvcounts, int *displs, MPI_Datatype recvtype, 
                              int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ireduce_inter(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, 
                             MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ireduce_scatter_inter(void* sendbuf, void* recvbuf, int *recvcounts, MPI_Datatype datatype, 
                                     MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ireduce_scatter_block_inter(void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                                 struct ompi_op_t *op, struct ompi_communicator_t *comm, 
                                                 ompi_request_t **request,
                                                 struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iscatter_inter(void* sendbuf, int sendcount, MPI_Datatype sendtype, 
                              void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, 
                              struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_iscatterv_inter(void* sendbuf, int *sendcounts, int *displs, MPI_Datatype sendtype, 
                               void* recvbuf, int recvcount, MPI_Datatype recvtype, int root, 
                               struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_1_0_t *module);


int ompi_coll_libnbc_ineighbor_allgather(void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                         int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm,
                                         ompi_request_t ** request, struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ineighbor_allgatherv(void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                          int *rcounts, int *displs, MPI_Datatype rtype,
                                          struct ompi_communicator_t *comm, ompi_request_t ** request,
                                          struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ineighbor_alltoall(void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                        int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm,
                                        ompi_request_t ** request, struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ineighbor_alltoallv(void *sbuf, int *scounts, int *sdispls, MPI_Datatype stype,
                                         void *rbuf, int *rcounts, int *rdispls, MPI_Datatype rtype,
                                         struct ompi_communicator_t *comm, ompi_request_t ** request,
                                         struct mca_coll_base_module_2_1_0_t *module);
int ompi_coll_libnbc_ineighbor_alltoallw(void *sbuf, int *scounts, MPI_Aint *sdisps, MPI_Datatype *stypes,
                                         void *rbuf, int *rcounts, MPI_Aint *rdisps, MPI_Datatype *rtypes,
                                         struct ompi_communicator_t *comm, ompi_request_t ** request,
                                         struct mca_coll_base_module_2_1_0_t *module);


END_C_DECLS

#endif /* MCA_COLL_LIBNBC_EXPORT_H */

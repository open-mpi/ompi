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
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_LIBPNBC_OSC_EXPORT_H
#define MCA_COLL_LIBPNBC_OSC_EXPORT_H

#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "opal/sys/atomic.h"

BEGIN_C_DECLS

/*********************** LibPNBC_OSC tuning parameters ************************/

/* the debug level */
#define PNBC_OSC_DLEVEL 10

/* enable schedule caching - undef PNBC_OSC_CACHE_SCHEDULE to deactivate it */
/* TODO: this whole schedule cache stuff does not work with the tmbuf
 * :-( - first, the tmpbuf must not be freed if a schedule using it is
 * still in the cache and second, the tmpbuf used by the schedule must
 * be attached to the handle that uses this schedule !!!!
 * I.E., THIS IS EXPERIMENTAL AND MIGHT NOT WORK */
/* It also leaks memory because the schedule is never cleaned up when
   the communicator is destroyed, so don't use it for now */
#ifdef PNBC_OSC_CACHE_SCHEDULE
#undef PNBC_OSC_CACHE_SCHEDULE
#endif
#define PNBC_OSC_SCHED_DICT_UPPER 1024 /* max. number of dict entries */
#define PNBC_OSC_SCHED_DICT_LOWER 512  /* nuber of dict entries after wipe, if SCHED_DICT_UPPER is reached */

/********************* end of LibPNBC_OSC tuning parameters ************************/

/* Function return codes  */
#define PNBC_OSC_OK 0 /* everything went fine */
#define PNBC_OSC_SUCCESS 0 /* everything went fine (MPI compliant :) */
#define PNBC_OSC_OOR 1 /* out of resources */
#define PNBC_OSC_BAD_SCHED 2 /* bad schedule */
#define PNBC_OSC_CONTINUE 3 /* progress not done */
#define PNBC_OSC_DATATYPE_NOT_SUPPORTED 4 /* datatype not supported or not valid */
#define PNBC_OSC_OP_NOT_SUPPORTED 5 /* operation not supported or not valid */
#define PNBC_OSC_NOT_IMPLEMENTED 6
#define PNBC_OSC_INVALID_PARAM 7 /* invalid parameters */
#define PNBC_OSC_INVALID_TOPOLOGY_COMM 8 /* invalid topology attached to communicator */

/* number of implemented collective functions */
//#define PNBC_OSC_NUM_COLL 17
#define PNBC_OSC_NUM_COLL 1

struct ompi_coll_libpnbc_osc_component_t {
    mca_coll_base_component_2_0_0_t super;
    opal_free_list_t requests;
    opal_list_t active_requests;
    opal_atomic_int32_t active_comms;
    opal_mutex_t lock;                /* protect access to the active_requests list */
};
typedef struct ompi_coll_libpnbc_osc_component_t ompi_coll_libpnbc_osc_component_t;

/* Globally exported variables */
OMPI_MODULE_DECLSPEC extern ompi_coll_libpnbc_osc_component_t mca_coll_libpnbc_osc_component;

struct ompi_coll_libpnbc_osc_module_t {
    mca_coll_base_module_t super;
    opal_mutex_t mutex;
    bool comm_registered;
    int tag;
};
typedef struct ompi_coll_libpnbc_osc_module_t ompi_coll_libpnbc_osc_module_t;
OBJ_CLASS_DECLARATION(ompi_coll_libpnbc_osc_module_t);

typedef ompi_coll_libpnbc_osc_module_t PNBC_OSC_Comminfo;

struct PNBC_OSC_Schedule {
    opal_object_t super;
    volatile int size;
    volatile int current_round_offset;
    char *data;
};

typedef struct PNBC_OSC_Schedule PNBC_OSC_Schedule;

OBJ_CLASS_DECLARATION(PNBC_OSC_Schedule);

struct ompi_coll_libpnbc_osc_request_t {
    ompi_request_t super;
    MPI_Comm comm;
    long row_offset;
    bool nbc_complete; /* status in libpnbc_osc level */
    int tag;
    volatile int req_count;
    ompi_request_t **req_array;
    PNBC_OSC_Comminfo *comminfo;
    PNBC_OSC_Schedule *schedule;
    MPI_Win win;
    MPI_Win winflag;
    void *tmpbuf; /* temporary buffer e.g. used for Reduce */
    /* TODO: we should make a handle pointer to a state later (that the user
     * can move request handles) */
};
typedef struct ompi_coll_libpnbc_osc_request_t ompi_coll_libpnbc_osc_request_t;
OBJ_CLASS_DECLARATION(ompi_coll_libpnbc_osc_request_t);

typedef ompi_coll_libpnbc_osc_request_t PNBC_OSC_Handle;


#define OMPI_COLL_LIBPNBC_OSC_REQUEST_ALLOC(comm, persistent, req)           \
    do {                                                                \
        opal_free_list_item_t *item;                                    \
        item = opal_free_list_wait (&mca_coll_libpnbc_osc_component.requests); \
        req = (ompi_coll_libpnbc_osc_request_t*) item;                       \
        OMPI_REQUEST_INIT(&req->super, persistent);                     \
        req->super.req_mpi_object.comm = comm;                          \
    } while (0)

#define OMPI_COLL_LIBPNBC_OSC_REQUEST_RETURN(req)                            \
    do {                                                                \
        OMPI_REQUEST_FINI(&(req)->super);                               \
        opal_free_list_return (&mca_coll_libpnbc_osc_component.requests,     \
                               (opal_free_list_item_t*) (req));         \
    } while (0)

int ompi_coll_libpnbc_osc_progress(void);

int PNBC_OSC_Init_comm(MPI_Comm comm, ompi_coll_libpnbc_osc_module_t *module);
int PNBC_OSC_Progress(PNBC_OSC_Handle *handle);

/*
int ompi_coll_libpnbc_osc_iallgather(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iallgatherv(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *displs,
                                 MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iallreduce(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ialltoall(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                               MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_3_0_t *module);
*/
int ompi_coll_libpnbc_osc_alltoallv_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                        MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                        MPI_Datatype recvtype, struct ompi_communicator_t *comm, struct ompi_info_t *,
                        ompi_request_t ** request, struct mca_coll_base_module_2_3_0_t *module);
/*
int ompi_coll_libpnbc_osc_ialltoallw(const void *sbuf, const int *scounts, const int *sdisps,
                                     struct ompi_datatype_t * const *sdtypes,
                                           void *rbuf, const int *rcounts, const int *rdisps,
                                     struct ompi_datatype_t * const *rdtypes,
                                     struct ompi_communicator_t *comm, struct ompi_info_t *,
                                     ompi_request_t **request, struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ibarrier(struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ibcast(void *buffer, int count, MPI_Datatype datatype, int root,
                            struct ompi_communicator_t *comm, ompi_request_t ** request,
                            struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iexscan(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                             struct ompi_op_t *op, struct ompi_communicator_t *comm, ompi_request_t **request,
                             struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_igather(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                             MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_igatherv(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                              int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ireduce(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                             MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ireduce_scatter(const void* sendbuf, void* recvbuf, const int *recvcounts, MPI_Datatype datatype,
                                     MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ireduce_scatter_block(const void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                           struct ompi_op_t *op, struct ompi_communicator_t *comm,
                                           ompi_request_t **request,
                                           struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iscan(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                           struct ompi_communicator_t *comm, ompi_request_t ** request,
                           struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iscatter(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                              struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iscatterv(const void* sendbuf, const int *sendcounts, const int *displs, MPI_Datatype sendtype,
                               void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                               struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_3_0_t *module);


int ompi_coll_libpnbc_osc_iallgather_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iallgatherv_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *displs,
                                 MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iallreduce_inter(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ialltoall_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                               MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ialltoallv_inter(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ialltoallw_inter(const void *sbuf, const int *scounts, const int *sdisps, struct ompi_datatype_t * const *sdtypes,
                                      void *rbuf, const int *rcounts, const int *rdisps, struct ompi_datatype_t * const *rdtypes,
                                      struct ompi_communicator_t *comm, ompi_request_t **request,
                                      struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ibarrier_inter(struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ibcast_inter(void *buffer, int count, MPI_Datatype datatype, int root,
                            struct ompi_communicator_t *comm, ompi_request_t ** request,
                            struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_igather_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                             MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_igatherv_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                              int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ireduce_inter(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                             MPI_Op op, int root, struct ompi_communicator_t *comm, ompi_request_t ** request,
                             struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ireduce_scatter_inter(const void* sendbuf, void* recvbuf, const int *recvcounts, MPI_Datatype datatype,
                                     MPI_Op op, struct ompi_communicator_t *comm, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ireduce_scatter_block_inter(const void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                                 struct ompi_op_t *op, struct ompi_communicator_t *comm,
                                                 ompi_request_t **request,
                                                 struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iscatter_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                              struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_iscatterv_inter(const void* sendbuf, const int *sendcounts, const int *displs, MPI_Datatype sendtype,
                               void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                               struct ompi_communicator_t *comm, ompi_request_t ** request,
                               struct mca_coll_base_module_2_3_0_t *module);


int ompi_coll_libpnbc_osc_ineighbor_allgather(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                         int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm,
                                         ompi_request_t ** request, struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ineighbor_allgatherv(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                          const int *rcounts, const int *displs, MPI_Datatype rtype,
                                          struct ompi_communicator_t *comm, ompi_request_t ** request,
                                          struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ineighbor_alltoall(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                        int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm,
                                        ompi_request_t ** request, struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ineighbor_alltoallv(const void *sbuf, const int *scounts, const int *sdispls, MPI_Datatype stype,
                                         void *rbuf, const int *rcounts, const int *rdispls, MPI_Datatype rtype,
                                         struct ompi_communicator_t *comm, ompi_request_t ** request,
                                         struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_ineighbor_alltoallw(const void *sbuf, const int *scounts, const MPI_Aint *sdisps, struct ompi_datatype_t * const *stypes,
                                         void *rbuf, const int *rcounts, const MPI_Aint *rdisps, struct ompi_datatype_t * const *rtypes,
                                         struct ompi_communicator_t *comm, ompi_request_t ** request,
                                         struct mca_coll_base_module_2_3_0_t *module);

int ompi_coll_libpnbc_osc_allgather_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                    MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_allgatherv_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *displs,
                                     MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_allreduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                    struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_alltoall_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                   MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                   struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_alltoallv_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                    MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                    MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                    struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_alltoallw_init(const void *sbuf, const int *scounts, const int *sdisps, struct ompi_datatype_t * const *sdtypes,
                                    void *rbuf, const int *rcounts, const int *rdisps, struct ompi_datatype_t * const *rdtypes,
                                    struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t **request,
                                    struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_barrier_init(struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                  struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_bcast_init(void *buffer, int count, MPI_Datatype datatype, int root,
                                struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_exscan_init(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                                 struct ompi_op_t *op, struct ompi_communicator_t *comm, MPI_Info info,  ompi_request_t **request,
                                 struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_gather_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                 MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_gatherv_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                                  void* recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                                  int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                  struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_reduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                                 MPI_Op op, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_reduce_scatter_init(const void* sendbuf, void* recvbuf, const int *recvcounts, MPI_Datatype datatype,
                                         MPI_Op op, struct ompi_communicator_t *comm, MPI_Info info,  ompi_request_t ** request,
                                         struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_reduce_scatter_block_init(const void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                               struct ompi_op_t *op, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t **request,
                                               struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_scan_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                               struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                               struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_scatter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                                  void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                                  struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                  struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_scatterv_init(const void* sendbuf, const int *sendcounts, const int *displs, MPI_Datatype sendtype,
                                   void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                                   struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                   struct mca_coll_base_module_2_3_0_t *module);

int ompi_coll_libpnbc_osc_allgather_inter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                          MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_allgatherv_inter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *displs,
                                           MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                           struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_allreduce_inter_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                          struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_alltoall_inter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                         MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                         struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_alltoallv_inter_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                          MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                          MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_alltoallw_inter_init(const void *sbuf, const int *scounts, const int *sdisps, struct ompi_datatype_t * const *sdtypes,
                                          void *rbuf, const int *rcounts, const int *rdisps, struct ompi_datatype_t * const *rdtypes,
                                          struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t **request,
                                          struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_barrier_inter_init(struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                        struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_bcast_inter_init(void *buffer, int count, MPI_Datatype datatype, int root,
                                      struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                      struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_gather_inter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                       MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                       struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_gatherv_inter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                                        void* recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                                        int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                        struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_reduce_inter_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                                       MPI_Op op, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                       struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_reduce_scatter_inter_init(const void* sendbuf, void* recvbuf, const int *recvcounts, MPI_Datatype datatype,
                                               MPI_Op op, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                               struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_reduce_scatter_block_inter_init(const void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                                     struct ompi_op_t *op, struct ompi_communicator_t *comm,
                                                     MPI_Info info, ompi_request_t **request,
                                                     struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_scatter_inter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                                        void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                                        struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                        struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_scatterv_inter_init(const void* sendbuf, const int *sendcounts, const int *displs, MPI_Datatype sendtype,
                                         void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                                         struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                         struct mca_coll_base_module_2_3_0_t *module);

int ompi_coll_libpnbc_osc_neighbor_allgather_init(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                             int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm,
                                             MPI_Info info, ompi_request_t ** request, struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_neighbor_allgatherv_init(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                              const int *rcounts, const int *displs, MPI_Datatype rtype,
                                              struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                              struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_neighbor_alltoall_init(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                            int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm, MPI_Info info,
                                            ompi_request_t ** request, struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_neighbor_alltoallv_init(const void *sbuf, const int *scounts, const int *sdispls, MPI_Datatype stype,
                                             void *rbuf, const int *rcounts, const int *rdispls, MPI_Datatype rtype,
                                             struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                             struct mca_coll_base_module_2_3_0_t *module);
int ompi_coll_libpnbc_osc_neighbor_alltoallw_init(const void *sbuf, const int *scounts, const MPI_Aint *sdisps, struct ompi_datatype_t * const *stypes,
                                             void *rbuf, const int *rcounts, const MPI_Aint *rdisps, struct ompi_datatype_t * const *rtypes,
                                             struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                             struct mca_coll_base_module_2_3_0_t *module);
*/

END_C_DECLS

#endif /* MCA_COLL_LIBPNBC_OSC_EXPORT_H */

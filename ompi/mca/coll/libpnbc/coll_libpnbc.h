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
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
 * Copyright (c) 2017 Ian Bradley Morgan and Anthony Skjellum
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright 
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright 
 *    notice, this list of conditions and the following disclaimer in the 
 *    documentation and/or other materials provided with the distribution.
 * 
 * 3. Neither the name of the copyright holder nor the names of its 
 *    contributors may be used to endorse or promote products derived from 
 *    this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_LIBPNBC_EXPORT_H
#define MCA_COLL_LIBPNBC_EXPORT_H

#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "opal/sys/atomic.h"

BEGIN_C_DECLS

/*********************** LibNBC tuning parameters ************************/

/* the debug level */
#define PNBC_DLEVEL 0

/********************* end of LibNBC tuning parameters ************************/

/* Function return codes  */
#define PNBC_OK 0 /* everything went fine */
#define PNBC_SUCCESS 0 /* everything went fine (MPI compliant :) */
#define PNBC_OOR 1 /* out of resources */
#define PNBC_BAD_SCHED 2 /* bad schedule */
#define PNBC_CONTINUE 3 /* progress not done */
#define PNBC_DATATYPE_NOT_SUPPORTED 4 /* datatype not supported or not valid */
#define PNBC_OP_NOT_SUPPORTED 5 /* operation not supported or not valid */
#define PNBC_NOT_IMPLEMENTED 6
#define PNBC_INVALID_PARAM 7 /* invalid parameters */
#define PNBC_INVALID_TOPOLOGY_COMM 8 /* invalid topology attached to communicator */

extern bool libpnbc_ibcast_skip_dt_decision;

struct ompi_coll_libpnbc_component_t {
    mca_coll_base_component_2_0_0_t super;
    opal_free_list_t requests;
    opal_list_t active_requests;
    int32_t active_comms;
    opal_atomic_lock_t progress_lock; /* protect from recursive calls */
    opal_mutex_t lock;                /* protect access to the active_requests list */
};
typedef struct ompi_coll_libpnbc_component_t ompi_coll_libpnbc_component_t;

/* Globally exported variables */
OMPI_MODULE_DECLSPEC extern ompi_coll_libpnbc_component_t mca_coll_libpnbc_component;

struct ompi_coll_libpnbc_module_t {
    mca_coll_base_module_t super;
    opal_mutex_t mutex;
    bool comm_registered;
    int tag;
};
typedef struct ompi_coll_libpnbc_module_t ompi_coll_libpnbc_module_t;
OBJ_CLASS_DECLARATION(ompi_coll_libpnbc_module_t);

typedef ompi_coll_libpnbc_module_t PNBC_Comminfo;

struct PNBC_Schedule {
    opal_object_t super;
    volatile int size;
    volatile int current_round_offset;
    char *data;
};

typedef struct PNBC_Schedule PNBC_Schedule;

OBJ_CLASS_DECLARATION(PNBC_Schedule);

struct ompi_coll_libpnbc_request_t {
    ompi_request_t super;
    MPI_Comm comm;
    long row_offset;
    int tag;
    volatile int req_count;
    ompi_request_t **req_array;
    PNBC_Comminfo *comminfo;
    PNBC_Schedule *schedule;
    void *tmpbuf; /* temporary buffer e.g. used for Reduce */
};
typedef struct ompi_coll_libpnbc_request_t ompi_coll_libpnbc_request_t;
OBJ_CLASS_DECLARATION(ompi_coll_libpnbc_request_t);

typedef ompi_coll_libpnbc_request_t PNBC_Handle;


#define OMPI_COLL_LIBPNBC_REQUEST_ALLOC(comm, req)                         \
    do {                                                                   \
        opal_free_list_item_t *item;                                       \
        item = opal_free_list_wait (&mca_coll_libpnbc_component.requests); \
        req = (ompi_coll_libpnbc_request_t*) item;                         \
        OMPI_REQUEST_INIT(&req->super, false);                             \
        req->super.req_mpi_object.comm = comm;                             \
        req->super.req_complete = false;                                   \
        req->super.req_state = OMPI_REQUEST_ACTIVE;                        \
    } while (0)

#define OMPI_COLL_LIBPNBC_REQUEST_RETURN(req)                        \
    do {                                                             \
        OMPI_REQUEST_FINI(&(req)->super);                            \
        opal_free_list_return (&mca_coll_libpnbc_component.requests, \
                               (opal_free_list_item_t*) (req));      \
    } while (0)

int ompi_coll_libpnbc_progress(void);

int PNBC_Init_comm(MPI_Comm comm, ompi_coll_libpnbc_module_t *module);
int PNBC_Progress(PNBC_Handle *handle);


int ompi_coll_libpnbc_iallgather_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iallgatherv_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *displs,
                                 MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iallreduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ialltoall_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                               MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                               struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ialltoallv_init(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ialltoallw_init(const void *sbuf, const int *scounts, const int *sdisps, struct ompi_datatype_t * const *sdtypes,
                                void *rbuf, const int *rcounts, const int *rdisps, struct ompi_datatype_t * const *rdtypes,
                                struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t **request,
                                struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ibarrier_init(struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                              struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ibcast_init(void *buffer, int count, MPI_Datatype datatype, int root,
                            struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                            struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iexscan_init(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                             struct ompi_op_t *op, struct ompi_communicator_t *comm, MPI_Info info,  ompi_request_t **request,
                             struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_igather_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                             MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                             struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_igatherv_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                              int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                              struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ireduce_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                             MPI_Op op, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                             struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ireduce_scatter_init(const void* sendbuf, void* recvbuf, const int *recvcounts, MPI_Datatype datatype,
                                     MPI_Op op, struct ompi_communicator_t *comm, MPI_Info info,  ompi_request_t ** request,
                                     struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ireduce_scatter_block_init(const void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                           struct ompi_op_t *op, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t **request,
                                           struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iscan_init(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                           struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                           struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iscatter_init(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                              struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                              struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iscatterv_init(const void* sendbuf, const int *sendcounts, const int *displs, MPI_Datatype sendtype,
                               void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                               struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                               struct mca_coll_base_module_2_2_0_t *module);


int ompi_coll_libpnbc_iallgather_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iallgatherv_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *displs,
                                 MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                 struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iallreduce_inter(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
                                struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ialltoall_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                               MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                               struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ialltoallv_inter(const void* sendbuf, const int *sendcounts, const int *sdispls,
                                MPI_Datatype sendtype, void* recvbuf, const int *recvcounts, const int *rdispls,
                                MPI_Datatype recvtype, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ialltoallw_inter(const void *sbuf, const int *scounts, const int *sdisps, struct ompi_datatype_t * const *sdtypes,
                                      void *rbuf, const int *rcounts, const int *rdisps, struct ompi_datatype_t * const *rdtypes,
                                      struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t **request,
                                      struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ibarrier_inter(struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                              struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ibcast_inter(void *buffer, int count, MPI_Datatype datatype, int root,
                            struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                            struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_igather_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype, void* recvbuf, int recvcount,
                             MPI_Datatype recvtype, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                             struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_igatherv_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                              int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                              struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ireduce_inter(const void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype,
                             MPI_Op op, int root, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                             struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ireduce_scatter_inter(const void* sendbuf, void* recvbuf, const int *recvcounts, MPI_Datatype datatype,
                                     MPI_Op op, struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                     struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ireduce_scatter_block_inter(const void *sbuf, void *rbuf, int rcount, struct ompi_datatype_t *dtype,
                                                 struct ompi_op_t *op, struct ompi_communicator_t *comm,
												 MPI_Info info, ompi_request_t **request,
                                                 struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iscatter_inter(const void* sendbuf, int sendcount, MPI_Datatype sendtype,
                              void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                              struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                              struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_iscatterv_inter(const void* sendbuf, const int *sendcounts, const int *displs, MPI_Datatype sendtype,
                               void* recvbuf, int recvcount, MPI_Datatype recvtype, int root,
                               struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                               struct mca_coll_base_module_2_2_0_t *module);


int ompi_coll_libpnbc_ineighbor_allgather_init(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                         int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm,
										 MPI_Info info, ompi_request_t ** request, struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ineighbor_allgatherv_init(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                          const int *rcounts, const int *displs, MPI_Datatype rtype,
                                          struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                          struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ineighbor_alltoall_init(const void *sbuf, int scount, MPI_Datatype stype, void *rbuf,
                                        int rcount, MPI_Datatype rtype, struct ompi_communicator_t *comm, MPI_Info info,
                                        ompi_request_t ** request, struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ineighbor_alltoallv_init(const void *sbuf, const int *scounts, const int *sdispls, MPI_Datatype stype,
                                         void *rbuf, const int *rcounts, const int *rdispls, MPI_Datatype rtype,
                                         struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                         struct mca_coll_base_module_2_2_0_t *module);
int ompi_coll_libpnbc_ineighbor_alltoallw_init(const void *sbuf, const int *scounts, const MPI_Aint *sdisps, struct ompi_datatype_t * const *stypes,
                                         void *rbuf, const int *rcounts, const MPI_Aint *rdisps, struct ompi_datatype_t * const *rtypes,
                                         struct ompi_communicator_t *comm, MPI_Info info, ompi_request_t ** request,
                                         struct mca_coll_base_module_2_2_0_t *module);

int ompi_coll_libpnbc_start(ompi_request_t ** request);


END_C_DECLS

#endif /* MCA_COLL_LIBPNBC_EXPORT_H */

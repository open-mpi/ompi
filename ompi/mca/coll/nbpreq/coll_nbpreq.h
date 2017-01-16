/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * This component exists for a fallback of the persistent collective
 * communication request. All `*_init` interfaces of the COLL framework
 * is implemented.
 *
 * This component does not have collective communication algorithms.
 * Instead, when `MPI_START` or `MPI_STARTALL` is called, it calls the
 * corresponding nonblocking collective communication function and the
 * communication is delegated. The performance optimization of the
 * persistent collective communication request such as specific algorithms
 * and communication scheduling will be implemented in other components
 * which have a higher priority.
 *
 * "nbpreq" is an abbreviation of "nonblocking persistent request".
 */

#ifndef MCA_COLL_NBPREQ_EXPORT_H
#define MCA_COLL_NBPREQ_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/communicator/communicator.h"

BEGIN_C_DECLS

/**
 * Arguments passed to the `MPI_*_INIT` routine.
 */
union mca_coll_nbpreq_func_args_t {
    struct {
        const void *sendbuf;
        int sendcount;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        int recvcount;
        struct ompi_datatype_t *recvtype;
        struct ompi_communicator_t *comm;
    } allgather;
    struct {
        const void *sendbuf;
        int sendcount;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        const int *recvcounts;
        const int *displs;
        struct ompi_datatype_t *recvtype;
        struct ompi_communicator_t *comm;
    } allgatherv;
    struct {
        const void *sendbuf;
        void *recvbuf;
        int count;
        struct ompi_datatype_t *datatype;
        struct ompi_op_t *op;
        struct ompi_communicator_t *comm;
    } allreduce;
    struct {
        const void *sendbuf;
        int sendcount;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        int recvcount;
        struct ompi_datatype_t *recvtype;
        struct ompi_communicator_t *comm;
    } alltoall;
    struct {
        const void *sendbuf;
        const int *sendcounts;
        const int *sdispls;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        const int *recvcounts;
        const int *rdispls;
        struct ompi_datatype_t *recvtype;
        struct ompi_communicator_t *comm;
    } alltoallv;
    struct {
        const void *sendbuf;
        const int *sendcounts;
        const int *sdispls;
        struct ompi_datatype_t * const *sendtypes;
        void *recvbuf;
        const int *recvcounts;
        const int *rdispls;
        struct ompi_datatype_t * const *recvtypes;
        struct ompi_communicator_t *comm;
    } alltoallw;
    struct {
        struct ompi_communicator_t *comm;
    } barrier;
    struct {
        void *buffer;
        int count;
        struct ompi_datatype_t *datatype;
        int root;
        struct ompi_communicator_t *comm;
    } bcast;
    struct {
        const void *sendbuf;
        void *recvbuf;
        int count;
        struct ompi_datatype_t *datatype;
        struct ompi_op_t *op;
        struct ompi_communicator_t *comm;
    } exscan;
    struct {
        const void *sendbuf;
        int sendcount;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        int recvcount;
        struct ompi_datatype_t *recvtype;
        int root;
        struct ompi_communicator_t *comm;
    } gather;
    struct {
        const void *sendbuf;
        int sendcount;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        const int *recvcounts;
        const int *displs;
        struct ompi_datatype_t *recvtype;
        int root;
        struct ompi_communicator_t *comm;
    } gatherv;
    struct {
        const void *sendbuf;
        void *recvbuf;
        int count;
        struct ompi_datatype_t *datatype;
        struct ompi_op_t *op;
        int root;
        struct ompi_communicator_t *comm;
    } reduce;
    struct {
        const void *sendbuf;
        void *recvbuf;
        const int *recvcounts;
        struct ompi_datatype_t *datatype;
        struct ompi_op_t *op;
        struct ompi_communicator_t *comm;
    } reducescatter;
    struct {
        const void *sendbuf;
        void *recvbuf;
        int recvcount;
        struct ompi_datatype_t *datatype;
        struct ompi_op_t *op;
        struct ompi_communicator_t *comm;
    } reducescatterblock;
    struct {
        const void *sendbuf;
        void *recvbuf;
        int count;
        struct ompi_datatype_t *datatype;
        struct ompi_op_t *op;
        struct ompi_communicator_t *comm;
    } scan;
    struct {
        const void *sendbuf;
        int sendcount;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        int recvcount;
        struct ompi_datatype_t *recvtype;
        int root;
        struct ompi_communicator_t *comm;
    } scatter;
    struct {
        const void *sendbuf;
        const int *sendcounts;
        const int *displs;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        int recvcount;
        struct ompi_datatype_t *recvtype;
        int root;
        struct ompi_communicator_t *comm;
    } scatterv;
    struct {
        const void *sendbuf;
        int sendcount;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        int recvcount;
        struct ompi_datatype_t *recvtype;
        struct ompi_communicator_t *comm;
    } neighbor_allgather;
    struct {
        const void *sendbuf;
        int sendcount;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        const int *recvcounts;
        const int *displs;
        struct ompi_datatype_t *recvtype;
        struct ompi_communicator_t *comm;
    } neighbor_allgatherv;
    struct {
        const void *sendbuf;
        int sendcount;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        int recvcount;
        struct ompi_datatype_t *recvtype;
        struct ompi_communicator_t *comm;
    } neighbor_alltoall;
    struct {
        const void *sendbuf;
        const int *sendcounts;
        const int *sdispls;
        struct ompi_datatype_t *sendtype;
        void *recvbuf;
        const int *recvcounts;
        const int *rdispls;
        struct ompi_datatype_t *recvtype;
        struct ompi_communicator_t *comm;
    } neighbor_alltoallv;
    struct {
        const void *sendbuf;
        const int *sendcounts;
        const MPI_Aint *sdispls;
        struct ompi_datatype_t * const *sendtypes;
        void *recvbuf;
        const int *recvcounts;
        const MPI_Aint *rdispls;
        struct ompi_datatype_t * const *recvtypes;
        struct ompi_communicator_t *comm;
    } neighbor_alltoallw;
};
typedef union mca_coll_nbpreq_func_args_t mca_coll_nbpreq_func_args_t;

/**
 * Persistent request created in the `MPI_*_INIT` routine.
 */
struct mca_coll_nbpreq_request_t {
    /** Base request. */
    ompi_request_t super;

    //// Members set in the `MPI_*_INIT` routine

    /** Type of collective communication (BARRIER, BCAST, ...). */
    COLLTYPE_T coll_type;
    /** Communicator associated with this request. */
    ompi_communicator_t *comm;
    /** Arguments passed to the `MPI_*_INIT` routine. */
    mca_coll_nbpreq_func_args_t func_args;

    //// Members set in the `MPI_START` or `MPI_STARTALL` routine

    /** Nonblocking collective communication request associated
     *  with this persistent request. */
    ompi_request_t *nb_request;
    /** Callback function originally set as `nb_request->req_complete_cb`.
     *  The value may be `NULL`. */
    ompi_request_complete_fn_t nb_req_complete_cb;
    /** Callback data originally set as `nb_request->req_complete_cb_data`.
     *  The value may be `NULL`. */
    void *nb_req_complete_cb_data;
};
typedef struct mca_coll_nbpreq_request_t mca_coll_nbpreq_request_t;
OBJ_CLASS_DECLARATION(mca_coll_nbpreq_request_t);

struct mca_coll_nbpreq_component_t {
    /** Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /** MCA parameter: Priority of this component */
    int priority;

    /** Free list of `mca_coll_nbpreq_request_t`. */
    opal_free_list_t free_requests;
};
typedef struct mca_coll_nbpreq_component_t mca_coll_nbpreq_component_t;

OMPI_MODULE_DECLSPEC extern mca_coll_nbpreq_component_t mca_coll_nbpreq_component;

struct mca_coll_nbpreq_module_t {
    mca_coll_base_module_t super;
};
typedef struct mca_coll_nbpreq_module_t mca_coll_nbpreq_module_t;
OBJ_CLASS_DECLARATION(mca_coll_nbpreq_module_t);

int mca_coll_nbpreq_init_query(bool enable_progress_threads,
                               bool enable_mpi_threads);

mca_coll_base_module_t *
mca_coll_nbpreq_comm_query(struct ompi_communicator_t *comm,
                           int *priority);

int mca_coll_nbpreq_module_enable(mca_coll_base_module_t *module,
                                  struct ompi_communicator_t *comm);

int mca_coll_nbpreq_allgather_init(ALLGATHER_INIT_ARGS);
int mca_coll_nbpreq_allgatherv_init(ALLGATHERV_INIT_ARGS);
int mca_coll_nbpreq_allreduce_init(ALLREDUCE_INIT_ARGS);
int mca_coll_nbpreq_alltoall_init(ALLTOALL_INIT_ARGS);
int mca_coll_nbpreq_alltoallv_init(ALLTOALLV_INIT_ARGS);
int mca_coll_nbpreq_alltoallw_init(ALLTOALLW_INIT_ARGS);
int mca_coll_nbpreq_barrier_init(BARRIER_INIT_ARGS);
int mca_coll_nbpreq_bcast_init(BCAST_INIT_ARGS);
int mca_coll_nbpreq_exscan_init(EXSCAN_INIT_ARGS);
int mca_coll_nbpreq_gather_init(GATHER_INIT_ARGS);
int mca_coll_nbpreq_gatherv_init(GATHERV_INIT_ARGS);
int mca_coll_nbpreq_reduce_init(REDUCE_INIT_ARGS);
int mca_coll_nbpreq_reduce_scatter_init(REDUCESCATTER_INIT_ARGS);
int mca_coll_nbpreq_reduce_scatter_block_init(REDUCESCATTERBLOCK_INIT_ARGS);
int mca_coll_nbpreq_scan_init(SCAN_INIT_ARGS);
int mca_coll_nbpreq_scatter_init(SCATTER_INIT_ARGS);
int mca_coll_nbpreq_scatterv_init(SCATTERV_INIT_ARGS);
int mca_coll_nbpreq_neighbor_allgather_init(NEIGHBOR_ALLGATHER_INIT_ARGS);
int mca_coll_nbpreq_neighbor_allgatherv_init(NEIGHBOR_ALLGATHERV_INIT_ARGS);
int mca_coll_nbpreq_neighbor_alltoall_init(NEIGHBOR_ALLTOALL_INIT_ARGS);
int mca_coll_nbpreq_neighbor_alltoallv_init(NEIGHBOR_ALLTOALLV_INIT_ARGS);
int mca_coll_nbpreq_neighbor_alltoallw_init(NEIGHBOR_ALLTOALLW_INIT_ARGS);

int mca_coll_nbpreq_ft_event(int status);

END_C_DECLS

#endif /* MCA_COLL_NBPREQ_EXPORT_H */

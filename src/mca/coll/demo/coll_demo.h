/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_DEMO_EXPORT_H
#define MCA_COLL_DEMO_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/coll/coll.h"
#include "request/request.h"
#include "mca/pml/pml.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /* Globally exported variables */

    extern const mca_coll_base_component_1_0_0_t mca_coll_demo_component;
    extern int mca_coll_demo_priority_param;
    extern int mca_coll_demo_verbose_param;
    extern int mca_coll_demo_verbose;


    /* Component functions */

    int mca_coll_demo_init_query(bool *allow_demo_user_threads,
                                 bool *have_hidden_threads);
    const struct mca_coll_base_module_1_0_0_t *
    mca_coll_demo_comm_query(struct ompi_communicator_t *comm, int *priority,
                             struct mca_coll_base_comm_t **data);
    
    /* Module functions */

    const struct mca_coll_base_module_1_0_0_t *
        mca_coll_demo_module_init(struct ompi_communicator_t *comm);
    int mca_coll_demo_module_finalize(struct ompi_communicator_t *comm);

    int mca_coll_demo_allgather_intra(void *sbuf, int scount, 
                                      struct ompi_datatype_t *sdtype, 
                                      void *rbuf, int rcount, 
                                      struct ompi_datatype_t *rdtype, 
                                      struct ompi_communicator_t *comm);
    int mca_coll_demo_allgather_inter(void *sbuf, int scount, 
                                      struct ompi_datatype_t *sdtype, 
                                      void *rbuf, int rcount, 
                                      struct ompi_datatype_t *rdtype, 
                                      struct ompi_communicator_t *comm);
    
    int mca_coll_demo_allgatherv_intra(void *sbuf, int scount, 
                                       struct ompi_datatype_t *sdtype, 
                                       void * rbuf, int *rcounts, int *disps, 
                                       struct ompi_datatype_t *rdtype, 
                                       struct ompi_communicator_t *comm);
    int mca_coll_demo_allgatherv_inter(void *sbuf, int scount, 
                                       struct ompi_datatype_t *sdtype, 
                                       void * rbuf, int *rcounts, int *disps, 
                                       struct ompi_datatype_t *rdtype, 
                                       struct ompi_communicator_t *comm);
    
    int mca_coll_demo_allreduce_intra(void *sbuf, void *rbuf, int count, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, 
                                      struct ompi_communicator_t *comm);
    int mca_coll_demo_allreduce_inter(void *sbuf, void *rbuf, int count, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, 
                                      struct ompi_communicator_t *comm);
    
    int mca_coll_demo_alltoall_intra(void *sbuf, int scount, 
                                     struct ompi_datatype_t *sdtype, 
                                     void* rbuf, int rcount, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);
    int mca_coll_demo_alltoall_inter(void *sbuf, int scount, 
                                     struct ompi_datatype_t *sdtype, 
                                     void* rbuf, int rcount, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);
    
    int mca_coll_demo_alltoallv_intra(void *sbuf, int *scounts, int *sdisps, 
                                      struct ompi_datatype_t *sdtype, 
                                      void *rbuf, int *rcounts, int *rdisps, 
                                      struct ompi_datatype_t *rdtype, 
                                      struct ompi_communicator_t *comm);
    int mca_coll_demo_alltoallv_inter(void *sbuf, int *scounts, int *sdisps, 
                                      struct ompi_datatype_t *sdtype, 
                                      void *rbuf, int *rcounts, int *rdisps, 
                                      struct ompi_datatype_t *rdtype, 
                                      struct ompi_communicator_t *comm);
    
    int mca_coll_demo_alltoallw_intra(void *sbuf, int *scounts, int *sdisps, 
                                      struct ompi_datatype_t **sdtypes, 
                                      void *rbuf, int *rcounts, int *rdisps, 
                                      struct ompi_datatype_t **rdtypes, 
                                      struct ompi_communicator_t *comm);
    int mca_coll_demo_alltoallw_inter(void *sbuf, int *scounts, int *sdisps, 
                                      struct ompi_datatype_t **sdtypes, 
                                      void *rbuf, int *rcounts, int *rdisps, 
                                      struct ompi_datatype_t **rdtypes, 
                                      struct ompi_communicator_t *comm);
    
    int mca_coll_demo_barrier_intra(struct ompi_communicator_t *comm);
    int mca_coll_demo_barrier_inter(struct ompi_communicator_t *comm);
    
    int mca_coll_demo_bcast_intra(void *buff, int count, 
                                  struct ompi_datatype_t *datatype,
                                  int root, 
                                  struct ompi_communicator_t *comm);
    int mca_coll_demo_bcast_inter(void *buff, int count, 
                                  struct ompi_datatype_t *datatype, 
                                  int root, 
                                  struct ompi_communicator_t *comm);
    
    int mca_coll_demo_exscan_intra(void *sbuf, void *rbuf, int count, 
                                   struct ompi_datatype_t *dtype, 
                                   struct ompi_op_t *op, 
                                   struct ompi_communicator_t *comm);
    int mca_coll_demo_exscan_inter(void *sbuf, void *rbuf, int count, 
                                   struct ompi_datatype_t *dtype, 
                                   struct ompi_op_t *op, 
                                   struct ompi_communicator_t *comm);
    
    int mca_coll_demo_gather_intra(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int rcount, struct ompi_datatype_t *rdtype, 
                                   int root, struct ompi_communicator_t *comm);
    int mca_coll_demo_gather_inter(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int rcount, struct ompi_datatype_t *rdtype, 
                                   int root, struct ompi_communicator_t *comm);
    
    int mca_coll_demo_gatherv_intra(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, void *rbuf, 
                                    int *rcounts, int *disps, 
                                    struct ompi_datatype_t *rdtype, int root, 
                                    struct ompi_communicator_t *comm);
    int mca_coll_demo_gatherv_inter(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, void *rbuf, 
                                    int *rcounts, int *disps, 
                                    struct ompi_datatype_t *rdtype, int root, 
                                    struct ompi_communicator_t *comm);
    
    int mca_coll_demo_reduce_intra(void *sbuf, void* rbuf, int count, 
                                   struct ompi_datatype_t *dtype, 
                                   struct ompi_op_t *op, 
                                   int root,
                                   struct ompi_communicator_t *comm);
    int mca_coll_demo_reduce_inter(void *sbuf, void* rbuf, int count, 
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op, 
                                   int root,
                                   struct ompi_communicator_t *comm);
    
    int mca_coll_demo_reduce_scatter_intra(void *sbuf, void *rbuf, 
                                           int *rcounts, 
                                           struct ompi_datatype_t *dtype, 
                                           struct ompi_op_t *op, 
                                           struct ompi_communicator_t *comm);
    int mca_coll_demo_reduce_scatter_inter(void *sbuf, void *rbuf, 
                                           int *rcounts, 
                                           struct ompi_datatype_t *dtype, 
                                           struct ompi_op_t *op, 
                                           struct ompi_communicator_t *comm);
    
    int mca_coll_demo_scan_intra(void *sbuf, void *rbuf, int count, 
                                 struct ompi_datatype_t *dtype, 
                                 struct ompi_op_t *op, 
                                 struct ompi_communicator_t *comm);
    int mca_coll_demo_scan_inter(void *sbuf, void *rbuf, int count, 
                                 struct ompi_datatype_t *dtype, 
                                 struct ompi_op_t *op, 
                                 struct ompi_communicator_t *comm);
    
    int mca_coll_demo_scatter_intra(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, void *rbuf, 
                                    int rcount, struct ompi_datatype_t *rdtype, 
                                    int root, struct ompi_communicator_t *comm);
    int mca_coll_demo_scatter_inter(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, void *rbuf, 
                                    int rcount, struct ompi_datatype_t *rdtype, 
                                    int root, struct ompi_communicator_t *comm);
    
    int mca_coll_demo_scatterv_intra(void *sbuf, int *scounts, int *disps, 
                                     struct ompi_datatype_t *sdtype, 
                                     void* rbuf, int rcount, 
                                     struct ompi_datatype_t *rdtype, int root, 
                                     struct ompi_communicator_t *comm);
    int mca_coll_demo_scatterv_inter(void *sbuf, int *scounts, int *disps, 
                                     struct ompi_datatype_t *sdtype, 
                                     void* rbuf, int rcount, 
                                     struct ompi_datatype_t *rdtype, int root, 
                                     struct ompi_communicator_t *comm);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_COLL_DEMO_EXPORT_H */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_TUNED_EXPORT_H
#define MCA_COLL_TUNED_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/coll/coll.h"
#include "request/request.h"
#include "mca/pml/pml.h"

/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "coll_tuned_topo.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Globally exported variable
 */

OMPI_COMP_EXPORT extern const mca_coll_base_component_1_0_0_t mca_coll_tuned_component;

OMPI_COMP_EXPORT extern int mca_coll_tuned_stream;
OMPI_COMP_EXPORT extern int mca_coll_tuned_priority;
OMPI_COMP_EXPORT extern int mca_coll_tuned_preallocate_memory_comm_size_limit;
OMPI_COMP_EXPORT extern int mca_coll_tuned_use_dynamic_rules;
OMPI_COMP_EXPORT extern int mca_coll_tuned_init_tree_fanout;
OMPI_COMP_EXPORT extern int mca_coll_tuned_init_chain_fanout;

/* forced algorithm choices */
OMPI_COMP_EXPORT extern int mca_coll_tuned_allreduce_forced_choice;
OMPI_COMP_EXPORT extern int mca_coll_tuned_allreduce_forced_segsize;
OMPI_COMP_EXPORT extern int mca_coll_tuned_allreduce_forced_tree_fanout;
OMPI_COMP_EXPORT extern int mca_coll_tuned_allreduce_forced_chain_fanout;

OMPI_COMP_EXPORT extern int mca_coll_tuned_alltoall_forced_choice;
OMPI_COMP_EXPORT extern int mca_coll_tuned_alltoall_forced_segsize;
OMPI_COMP_EXPORT extern int mca_coll_tuned_alltoall_forced_tree_fanout;
OMPI_COMP_EXPORT extern int mca_coll_tuned_alltoall_forced_chain_fanout;

OMPI_COMP_EXPORT extern int mca_coll_tuned_barrier_forced_choice;

OMPI_COMP_EXPORT extern int mca_coll_tuned_bcast_forced_choice;
OMPI_COMP_EXPORT extern int mca_coll_tuned_bcast_forced_segsize;
OMPI_COMP_EXPORT extern int mca_coll_tuned_bcast_forced_tree_fanout;
OMPI_COMP_EXPORT extern int mca_coll_tuned_bcast_forced_chain_fanout;

OMPI_COMP_EXPORT extern int mca_coll_tuned_reduce_forced_choice;
OMPI_COMP_EXPORT extern int mca_coll_tuned_reduce_forced_segsize;
OMPI_COMP_EXPORT extern int mca_coll_tuned_reduce_forced_tree_fanout;
OMPI_COMP_EXPORT extern int mca_coll_tuned_reduce_forced_chain_fanout;

/*
 * coll API functions
 */


  /* API functions */

  int mca_coll_tuned_init_query(bool enable_progress_threads,
                                bool enable_mpi_threads);
  const struct mca_coll_base_module_1_0_0_t *
    mca_coll_tuned_comm_query(struct ompi_communicator_t *comm, int *priority,
                              struct mca_coll_base_comm_t **data);

  const struct mca_coll_base_module_1_0_0_t *
    mca_coll_tuned_module_init(struct ompi_communicator_t *comm);
  int mca_coll_tuned_module_finalize(struct ompi_communicator_t *comm);

  /* API functions of decision functions and any implementations */

  /*
   * Note this gets long as we have to have a prototype for each 
   * MPI collective 4 times.. 2 for the comm type and 2 for each decision
   * type. 
   * we might cut down the decision prototypes by conditional compiling
   */

  int mca_coll_tuned_allgather_intra_dec_fixed(void *sbuf, int scount, 
                                     struct ompi_datatype_t *sdtype, 
                                     void *rbuf, int rcount, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_allgather_intra_dec_dynamic(void *sbuf, int scount, 
                                     struct ompi_datatype_t *sdtype, 
                                     void *rbuf, int rcount, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_allgather_inter_dec_fixed(void *sbuf, int scount, 
                                     struct ompi_datatype_t *sdtype, 
                                     void *rbuf, int rcount, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_allgather_inter_dec_dynamic(void *sbuf, int scount, 
                                     struct ompi_datatype_t *sdtype, 
                                     void *rbuf, int rcount, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);

  int mca_coll_tuned_allgatherv_intra_dec_fixed(void *sbuf, int scount, 
                                      struct ompi_datatype_t *sdtype, 
                                      void * rbuf, int *rcounts, int *disps, 
                                      struct ompi_datatype_t *rdtype, 
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_allgatherv_intra_dec_dynamic(void *sbuf, int scount, 
                                      struct ompi_datatype_t *sdtype, 
                                      void * rbuf, int *rcounts, int *disps, 
                                      struct ompi_datatype_t *rdtype, 
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_allgatherv_inter_dec_fixed(void *sbuf, int scount, 
                                      struct ompi_datatype_t *sdtype, 
                                      void * rbuf, int *rcounts, int *disps, 
                                      struct ompi_datatype_t *rdtype, 
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_allgatherv_inter_dec_dynamic(void *sbuf, int scount, 
                                      struct ompi_datatype_t *sdtype, 
                                      void * rbuf, int *rcounts, int *disps, 
                                      struct ompi_datatype_t *rdtype, 
                                      struct ompi_communicator_t *comm);

  int mca_coll_tuned_allreduce_intra_dec_fixed(void *sbuf, void *rbuf, 
                                     int count, 
                                     struct ompi_datatype_t *dtype, 
                                     struct ompi_op_t *op, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_allreduce_intra_dec_dynamic(void *sbuf, void *rbuf, 
                                     int count, 
                                     struct ompi_datatype_t *dtype, 
                                     struct ompi_op_t *op, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_allreduce_intra_do_forced(void *sbuf, void *rbuf, 
                                     int count, 
                                     struct ompi_datatype_t *dtype, 
                                     struct ompi_op_t *op, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_allreduce_intra_check_forced(void);
  int mca_coll_tuned_allreduce_intra_query(void);

  int mca_coll_tuned_allreduce_intra_nonoverlapping(void *sbuf, void *rbuf, 
                                     int count, 
                                     struct ompi_datatype_t *dtype, 
                                     struct ompi_op_t *op, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_allreduce_intra_basic_linear(void *sbuf, void *rbuf, 
                                     int count, 
                                     struct ompi_datatype_t *dtype, 
                                     struct ompi_op_t *op, 
                                     struct ompi_communicator_t *comm);



  int mca_coll_tuned_allreduce_inter_dec_fixed(void *sbuf, void *rbuf, 
                                     int count, 
                                     struct ompi_datatype_t *dtype, 
                                     struct ompi_op_t *op, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_allreduce_inter_dec_dynamic(void *sbuf, void *rbuf, 
                                     int count, 
                                     struct ompi_datatype_t *dtype, 
                                     struct ompi_op_t *op, 
                                     struct ompi_communicator_t *comm);

  int mca_coll_tuned_alltoall_intra_dec_fixed(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoall_intra_dec_dynamic(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoall_intra_do_forced(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoall_intra_check_forced(void);
  int mca_coll_tuned_alltoall_intra_query (void);

  int mca_coll_tuned_alltoall_intra_pairwise(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoall_intra_bruck(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoall_intra_basic_linear(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoall_intra_two_procs(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);


  int mca_coll_tuned_alltoall_inter_dec_fixed(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoall_inter_dec_dynamic(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void* rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);

  int mca_coll_tuned_alltoallv_intra_dec_fixed(void *sbuf, int *scounts, 
                                     int *sdisps, 
                                     struct ompi_datatype_t *sdtype, 
                                     void *rbuf, int *rcounts, int *rdisps, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoallv_intra_dec_dynamic(void *sbuf, int *scounts, 
                                     int *sdisps, 
                                     struct ompi_datatype_t *sdtype, 
                                     void *rbuf, int *rcounts, int *rdisps, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoallv_inter_dec_fixed(void *sbuf, int *scounts, 
                                     int *sdisps, 
                                     struct ompi_datatype_t *sdtype, 
                                     void *rbuf, int *rcounts, int *rdisps, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoallv_inter_dec_dynamic(void *sbuf, int *scounts, 
                                     int *sdisps, 
                                     struct ompi_datatype_t *sdtype, 
                                     void *rbuf, int *rcounts, int *rdisps, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);

  int mca_coll_tuned_alltoallw_intra_dec_fixed(void *sbuf, int *scounts, 
                                     int *sdisps, 
                                     struct ompi_datatype_t **sdtypes, 
                                     void *rbuf, int *rcounts, int *rdisps, 
                                     struct ompi_datatype_t **rdtypes, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoallw_intra_dec_dynamic(void *sbuf, int *scounts, 
                                     int *sdisps, 
                                     struct ompi_datatype_t **sdtypes, 
                                     void *rbuf, int *rcounts, int *rdisps, 
                                     struct ompi_datatype_t **rdtypes, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoallw_inter_dec_fixed(void *sbuf, int *scounts, 
                                     int *sdisps, 
                                     struct ompi_datatype_t **sdtypes, 
                                     void *rbuf, int *rcounts, int *rdisps, 
                                     struct ompi_datatype_t **rdtypes, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_alltoallw_inter_dec_dynamic(void *sbuf, int *scounts, 
                                     int *sdisps, 
                                     struct ompi_datatype_t **sdtypes, 
                                     void *rbuf, int *rcounts, int *rdisps, 
                                     struct ompi_datatype_t **rdtypes, 
                                     struct ompi_communicator_t *comm);

  int mca_coll_tuned_barrier_intra_dec_fixed(struct ompi_communicator_t *comm);
  int mca_coll_tuned_barrier_intra_dec_dynamic(
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_barrier_intra_do_forced(struct ompi_communicator_t *comm);
  int mca_coll_tuned_barrier_intra_check_forced(void);
  int mca_coll_tuned_barrier_intra_query (void);

  int mca_coll_tuned_barrier_inter_dec_fixed(struct ompi_communicator_t *comm);
  int mca_coll_tuned_barrier_inter_dec_dynamic(
                                     struct ompi_communicator_t *comm);

  int mca_coll_tuned_barrier_intra_doublering(struct ompi_communicator_t *comm);
  int mca_coll_tuned_barrier_intra_recursivedoubling(struct ompi_communicator_t *comm);
  int mca_coll_tuned_barrier_intra_bruck(struct ompi_communicator_t *comm);
  int mca_coll_tuned_barrier_intra_two_procs(struct ompi_communicator_t *comm);
  int mca_coll_tuned_barrier_intra_linear(struct ompi_communicator_t *comm);


  int mca_coll_tuned_bcast_intra_dec_fixed(void *buff, int count, 
                                     struct ompi_datatype_t *datatype,
                                     int root, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_bcast_intra_dec_dynamic(void *buff, int count, 
                                     struct ompi_datatype_t *datatype,
                                     int root, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_bcast_intra_do_forced(void *buff, int count, 
                                     struct ompi_datatype_t *datatype,
                                     int root, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_bcast_intra_check_forced(void);
  int mca_coll_tuned_bcast_intra_query (void);

  int mca_coll_tuned_bcast_intra_basic_linear(void *buff, int count, 
                                     struct ompi_datatype_t *datatype,
                                     int root, 
                                     struct ompi_communicator_t *comm);

  int mca_coll_tuned_bcast_intra_chain(void *buff, int count, 
                                     struct ompi_datatype_t *datatype,
                                     int root, 
                                     struct ompi_communicator_t *comm,
                                     uint32_t segsize, int32_t chains);

  int mca_coll_tuned_bcast_intra_pipeline(void *buff, int count, 
                                     struct ompi_datatype_t *datatype,
                                     int root, 
                                     struct ompi_communicator_t *comm,
                                     uint32_t segsize);

  int mca_coll_tuned_bcast_intra_bmtree(void *buff, int count, 
                                     struct ompi_datatype_t *datatype,
                                     int root, 
                                     struct ompi_communicator_t *comm,
                                     uint32_t segsize, int32_t chains);

  int mca_coll_tuned_bcast_intra_bintree(void *buff, int count, 
                                     struct ompi_datatype_t *datatype,
                                     int root, 
                                     struct ompi_communicator_t *comm,
                                     uint32_t segsize);
  int mca_coll_tuned_bcast_intra_split_bintree(void *buff, int count, 
                                     struct ompi_datatype_t *datatype,
                                     int root, 
                                     struct ompi_communicator_t *comm,
                                     uint32_t segsize);





  int mca_coll_tuned_bcast_inter_dec_fixed(void *buff, int count, 
                                     struct ompi_datatype_t *datatype, 
                                     int root, 
                                     struct ompi_communicator_t *comm);
  int mca_coll_tuned_bcast_inter_dec_dynamic(void *buff, int count, 
                                     struct ompi_datatype_t *datatype, 
                                     int root, 
                                     struct ompi_communicator_t *comm);

  int mca_coll_tuned_exscan_intra_dec_fixed(void *sbuf, void *rbuf, int count, 
                                  struct ompi_datatype_t *dtype, 
                                  struct ompi_op_t *op, 
                                  struct ompi_communicator_t *comm);
  int mca_coll_tuned_exscan_intra_dec_dynamic(void *sbuf, void *rbuf, 
                                  int count, 
                                  struct ompi_datatype_t *dtype, 
                                  struct ompi_op_t *op, 
                                  struct ompi_communicator_t *comm);
  int mca_coll_tuned_exscan_inter_dec_fixed(void *sbuf, void *rbuf, int count, 
                                  struct ompi_datatype_t *dtype, 
                                  struct ompi_op_t *op, 
                                  struct ompi_communicator_t *comm);
  int mca_coll_tuned_exscan_inter_dec_dynamic(void *sbuf, void *rbuf, 
                                  int count, 
                                  struct ompi_datatype_t *dtype, 
                                  struct ompi_op_t *op, 
                                  struct ompi_communicator_t *comm);

  int mca_coll_tuned_gather_intra_dec_fixed(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, void *rbuf, 
                                  int rcount, struct ompi_datatype_t *rdtype, 
                                  int root, struct ompi_communicator_t *comm);
  int mca_coll_tuned_gather_intra_dec_dynamic(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, void *rbuf, 
                                  int rcount, struct ompi_datatype_t *rdtype, 
                                  int root, struct ompi_communicator_t *comm);
  int mca_coll_tuned_gather_inter_dec_fixed(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, void *rbuf, 
                                  int rcount, struct ompi_datatype_t *rdtype, 
                                  int root, struct ompi_communicator_t *comm);
  int mca_coll_tuned_gather_inter_dec_dynamic(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, void *rbuf, 
                                  int rcount, struct ompi_datatype_t *rdtype, 
                                  int root, struct ompi_communicator_t *comm);

  int mca_coll_tuned_gatherv_intra_dec_fixed(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int *rcounts, int *disps, 
                                   struct ompi_datatype_t *rdtype, int root, 
                                   struct ompi_communicator_t *comm);
  int mca_coll_tuned_gatherv_intra_dec_dynamic(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int *rcounts, int *disps, 
                                   struct ompi_datatype_t *rdtype, int root, 
                                   struct ompi_communicator_t *comm);
  int mca_coll_tuned_gatherv_inter_dec_fixed(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int *rcounts, int *disps, 
                                   struct ompi_datatype_t *rdtype, int root, 
                                   struct ompi_communicator_t *comm);
  int mca_coll_tuned_gatherv_inter_dec_dynamic(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int *rcounts, int *disps, 
                                   struct ompi_datatype_t *rdtype, int root, 
                                   struct ompi_communicator_t *comm);

  int mca_coll_tuned_reduce_intra_dec_fixed(void *sbuf, void* rbuf, int count, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, int root,
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_reduce_intra_dec_dynamic(void *sbuf, void* rbuf, int count, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, int root,
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_reduce_intra_do_forced(void *sbuf, void* rbuf, int count, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, int root,
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_reduce_intra_check_forced(void);
  int mca_coll_tuned_reduce_intra_query (void);

  int mca_coll_tuned_reduce_intra_basic_linear(void *sbuf, void* rbuf, int count, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, int root,
                                      struct ompi_communicator_t *comm); 
  int mca_coll_tuned_reduce_intra_chain(void *sbuf, void* rbuf, int count, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, int root,
                                      struct ompi_communicator_t *comm, 
                                      uint32_t segsize, int fanout);
  int mca_coll_tuned_reduce_intra_pipeline(void *sbuf, void* rbuf, int count, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, int root,
                                      struct ompi_communicator_t *comm, 
                                      uint32_t segsize);

  int mca_coll_tuned_reduce_inter_dec_fixed(void *sbuf, void* rbuf, int count, 
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op, int root,
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_reduce_inter_dec_dynamic(void *sbuf, void* rbuf, int count, 
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op, int root,
                                      struct ompi_communicator_t *comm);

  int mca_coll_tuned_reduce_scatter_intra_dec_fixed(void *sbuf, void *rbuf, 
                                      int *rcounts, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, 
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_reduce_scatter_intra_dec_dynamic(void *sbuf, void *rbuf, 
                                      int *rcounts, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, 
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_reduce_scatter_inter_dec_fixed(void *sbuf, void *rbuf, 
                                      int *rcounts, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, 
                                      struct ompi_communicator_t *comm);
  int mca_coll_tuned_reduce_scatter_inter_dec_dynamic(void *sbuf, void *rbuf, 
                                      int *rcounts, 
                                      struct ompi_datatype_t *dtype, 
                                      struct ompi_op_t *op, 
                                      struct ompi_communicator_t *comm);
  
  int mca_coll_tuned_scan_intra_dec_fixed(void *sbuf, void *rbuf, int count, 
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op, 
                                struct ompi_communicator_t *comm);
  int mca_coll_tuned_scan_intra_dec_dynamic(void *sbuf, void *rbuf, int count, 
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op, 
                                struct ompi_communicator_t *comm);
  int mca_coll_tuned_scan_inter_dec_fixed(void *sbuf, void *rbuf, int count, 
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op, 
                                struct ompi_communicator_t *comm);
  int mca_coll_tuned_scan_inter_dec_dynamic(void *sbuf, void *rbuf, int count, 
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op, 
                                struct ompi_communicator_t *comm);

  int mca_coll_tuned_scatter_intra_dec_fixed(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int rcount, struct ompi_datatype_t *rdtype, 
                                   int root, struct ompi_communicator_t *comm);
  int mca_coll_tuned_scatter_intra_dec_dynamic(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int rcount, struct ompi_datatype_t *rdtype, 
                                   int root, struct ompi_communicator_t *comm);
  int mca_coll_tuned_scatter_inter_dec_fixed(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int rcount, struct ompi_datatype_t *rdtype, 
                                   int root, struct ompi_communicator_t *comm);
  int mca_coll_tuned_scatter_inter_dec_dynamic(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, void *rbuf, 
                                   int rcount, struct ompi_datatype_t *rdtype, 
                                   int root, struct ompi_communicator_t *comm);

  int mca_coll_tuned_scatterv_intra_dec_fixed(void *sbuf, int *scounts, 
                                   int *disps, 
                                   struct ompi_datatype_t *sdtype, 
                                   void* rbuf, int rcount, 
                                   struct ompi_datatype_t *rdtype, int root, 
                                   struct ompi_communicator_t *comm);
  int mca_coll_tuned_scatterv_intra_dec_dynamic(void *sbuf, int *scounts, 
                                   int *disps, 
                                   struct ompi_datatype_t *sdtype, 
                                   void* rbuf, int rcount, 
                                   struct ompi_datatype_t *rdtype, int root, 
                                   struct ompi_communicator_t *comm);
  int mca_coll_tuned_scatterv_inter_dec_fixed(void *sbuf, int *scounts, 
                                   int *disps, 
                                   struct ompi_datatype_t *sdtype, 
                                   void* rbuf, int rcount, 
                                   struct ompi_datatype_t *rdtype, int root, 
                                   struct ompi_communicator_t *comm);
  int mca_coll_tuned_scatterv_inter_dec_dynamic(void *sbuf, int *scounts, 
                                   int *disps, 
                                   struct ompi_datatype_t *sdtype, 
                                   void* rbuf, int rcount, 
                                   struct ompi_datatype_t *rdtype, int root, 
                                   struct ompi_communicator_t *comm);

/* Utility functions */

static inline void mca_coll_tuned_free_reqs(ompi_request_t **reqs, int count)
{
  int i;
  for (i = 0; i < count; ++i)
     ompi_request_free(&reqs[i]);
}

/* decision table declaraion */
/* currently a place holder */
typedef struct rule_s {
} rule_t;

/*
 * Data structure for hanging data off the communicator 
 */
struct mca_coll_base_comm_t {
  /* standard data for requests and PML usage */

  /* Precreate space for requests 
   * Note this does not effect basic, 
   * but if in wrong context can confuse a debugger
   * this is controlled by an MCA param
   */

  ompi_request_t **mcct_reqs;
  int mcct_num_reqs;

  /* 
   * tuned topo information caching per communicator 
   *
   * for each communicator we cache the topo information so we can 
   * reuse without regenerating if we change the root, [or fanout]
   * then regenerate and recache this information 
   *
   */

   /* general tree with n fan out */
   ompi_coll_tree_t *cached_ntree;
   int cached_ntree_root; 
   int cached_ntree_fanout; 

   /* binary tree */
   ompi_coll_tree_t *cached_bintree;
   int cached_bintree_root; 

   /* binomial tree */
   ompi_coll_tree_t *cached_bmtree;
   int cached_bmtree_root;

   /* chained tree (fanout followed by pipelines) */
   ompi_coll_chain_t *cached_chain;
   int cached_chain_root;
   int cached_chain_fanout; 

   /* pipeline */
   ompi_coll_chain_t *cached_pipeline;
   int cached_pipeline_root;

  /* extra data required by the decision functions */
  rule_t* decision_table;
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_COLL_TUNED_EXPORT_H */

/*
 * $HEADER$
 */

#ifndef MCA_COLL_H
#define MCA_COLL_H

#include "lam_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/mpi/base/base.h"


/*
 * Coll module function typedefs
 */

typedef int (*mca_coll_base_init_query_fn_t)(int *thread_min, 
                                             int *thread_max);
typedef const struct mca_coll_1_0_0_t *
  (*mca_coll_base_comm_query_1_0_0_fn_t)(MPI_Comm comm, int *priority);


/*
 * Coll interface function typedefs
 */

typedef int 
  (*mca_coll_base_init_1_0_0_fn_t)
  (MPI_Comm comm, const struct mca_coll_1_0_0_t **new_coll);
typedef int (*mca_coll_base_finalize_fn_t)(MPI_Comm comm);

typedef int (*mca_coll_base_checkpoint_fn_t)(MPI_Comm comm);
typedef int (*mca_coll_base_continue_fn_t)(MPI_Comm comm);
typedef int (*mca_coll_base_restart_fn_t)(MPI_Comm comm);
typedef int (*mca_coll_base_interrupt_fn_t)(void);

typedef int (*mca_coll_base_allgather_fn_t)(void *sbuf, int scount, 
                                            MPI_Datatype sdtype, void *rbuf, 
                                            int rcount, MPI_Datatype rdtype, 
                                            MPI_Comm comm);
typedef int (*mca_coll_base_allgatherv_fn_t)(void *sbuf, int scount, 
                                             MPI_Datatype sdtype, void * rbuf, 
                                             int *rcounts, int *disps, 
                                             MPI_Datatype rdtype, 
                                             MPI_Comm comm);
typedef int (*mca_coll_base_allreduce_fn_t)(void *sbuf, void *rbuf, int count, 
                                            MPI_Datatype dtype, MPI_Op op, 
                                            MPI_Comm comm);
typedef int (*mca_coll_base_alltoall_fn_t)(void *sbuf, int scount, 
                                           MPI_Datatype sdtype, void* rbuf, 
                                           int rcount, MPI_Datatype rdtype, 
                                           MPI_Comm comm);
typedef int (*mca_coll_base_alltoallv_fn_t)(void *sbuf, int *scounts, 
                                            int *sdisps, MPI_Datatype sdtype, 
                                            void *rbuf, int *rcounts, 
                                            int *rdisps, MPI_Datatype rdtype, 
                                            MPI_Comm comm);
typedef int (*mca_coll_base_alltoallw_fn_t)(void *sbuf, int *scounts, 
                                            int *sdisps, 
                                            MPI_Datatype *sdtypes, 
                                            void *rbuf, int *rcounts, 
                                            int *rdisps, 
                                            MPI_Datatype *rdtypes, 
                                            MPI_Comm comm);
typedef int (*mca_coll_base_barrier_fn_t)(MPI_Comm comm);
typedef int (*mca_coll_base_bcast_fn_t)(void *buff, int count, 
                                        MPI_Datatype datatype, int root, 
                                        MPI_Comm comm);
typedef int (*mca_coll_base_exscan_fn_t)(void *sbuf, void *rbuf, int count, 
                                         MPI_Datatype dtype, MPI_Op op, 
                                         MPI_Comm comm);
typedef int (*mca_coll_base_gather_fn_t)(void *sbuf, int scount, 
                                         MPI_Datatype sdtype, void *rbuf, 
                                         int rcount, MPI_Datatype rdtype, 
                                         int root, MPI_Comm comm);
typedef int (*mca_coll_base_gatherv_fn_t)(void *sbuf, int scount, 
                                          MPI_Datatype sdtype, void *rbuf, 
                                          int *rcounts, int *disps, 
                                          MPI_Datatype rdtype, int root, 
                                          MPI_Comm comm);
typedef int (*mca_coll_base_reduce_fn_t)(void *sbuf, void* rbuf, int count, 
                                         MPI_Datatype dtype, MPI_Op op, 
                                         int root, MPI_Comm comm);
typedef int (*mca_coll_base_reduce_scatter_fn_t)(void *sbuf, void *rbuf, 
                                                 int *rcounts, 
                                                 MPI_Datatype dtype, 
                                                 MPI_Op op, MPI_Comm comm);
typedef int (*mca_coll_base_scan_fn_t)(void *sbuf, void *rbuf, int count, 
                                       MPI_Datatype dtype, MPI_Op op, 
                                       MPI_Comm comm);
typedef int (*mca_coll_base_scatter_fn_t)(void *sbuf, int scount, 
                                          MPI_Datatype sdtype, void *rbuf, 
                                          int rcount, MPI_Datatype rdtype, 
                                          int root, MPI_Comm comm);
typedef int (*mca_coll_base_scatterv_fn_t)(void *sbuf, int *scounts, 
                                           int *disps, MPI_Datatype sdtype, 
                                           void* rbuf, int rcount, 
                                           MPI_Datatype rdtype, int root, 
                                           MPI_Comm comm);


/*
 * Structure for coll v1.0.0 modules
 * Chained to MCA v1.0.0
 */
struct mca_coll_base_module_1_0_0_t {
  mca_base_module_t collm_version;
  mca_base_module_data_1_0_0_t collm_data;

  /* Initialization / querying functions */

  mca_coll_base_init_query_fn_t collm_init_query;
  mca_coll_base_comm_query_1_0_0_fn_t collm_comm_query;
};
typedef struct mca_coll_base_module_1_0_0_t mca_coll_base_module_1_0_0_t;


/*
 * This struct is hung on the communicator by the winning coll module
 * after the negotiation.  It has pointers for all the collective
 * functions, as well as a "finalize" function for when the
 * communicator is freed.
 */

struct mca_coll_1_0_0_t {

  /* Per-communicator initialization and finalization functions */

  mca_coll_base_init_1_0_0_fn_t coll_init;
  mca_coll_base_finalize_fn_t coll_finalize;

  /* Checkpoint / restart functions */

  mca_coll_base_checkpoint_fn_t coll_checkpoint;
  mca_coll_base_continue_fn_t coll_continue;
  mca_coll_base_restart_fn_t coll_restart;
  mca_coll_base_interrupt_fn_t coll_interrupt;

  /* Memory allocation / freeing */

  mca_mpi_alloc_mem_fn_t coll_alloc_mem;
  mca_mpi_free_mem_fn_t coll_free_mem;

  /* Collective function pointers */

  mca_coll_base_allgather_fn_t coll_allgather_intra;
  mca_coll_base_allgather_fn_t coll_allgather_inter;

  mca_coll_base_allgatherv_fn_t coll_allgatherv_intra;
  mca_coll_base_allgatherv_fn_t coll_allgatherv_inter;

  mca_coll_base_allreduce_fn_t coll_allreduce_intra;
  mca_coll_base_allreduce_fn_t coll_allreduce_inter;

  mca_coll_base_alltoall_fn_t coll_alltoall_intra;
  mca_coll_base_alltoall_fn_t coll_alltoall_inter;

  mca_coll_base_alltoallv_fn_t coll_alltoallv_intra;
  mca_coll_base_alltoallv_fn_t coll_alltoallv_inter;

  mca_coll_base_alltoallw_fn_t coll_alltoallw_intra;
  mca_coll_base_alltoallw_fn_t coll_alltoallw_inter;

  mca_coll_base_barrier_fn_t coll_barrier_intra;
  mca_coll_base_barrier_fn_t coll_barrier_inter;

  bool coll_bcast_optimization;
  mca_coll_base_bcast_fn_t coll_bcast_intra;
  mca_coll_base_bcast_fn_t coll_bcast_inter;

  mca_coll_base_exscan_fn_t coll_exscan_intra;
  mca_coll_base_exscan_fn_t coll_exscan_inter;

  mca_coll_base_gather_fn_t coll_gather_intra;
  mca_coll_base_gather_fn_t coll_gather_inter;

  mca_coll_base_gatherv_fn_t coll_gatherv_intra;
  mca_coll_base_gatherv_fn_t coll_gatherv_inter;
  
  bool coll_reduce_optimization;
  mca_coll_base_reduce_fn_t coll_reduce_intra;
  mca_coll_base_reduce_fn_t coll_reduce_inter;

  mca_coll_base_reduce_scatter_fn_t coll_reduce_scatter_intra;
  mca_coll_base_reduce_scatter_fn_t coll_reduce_scatter_inter;

  mca_coll_base_scan_fn_t coll_scan_intra;
  mca_coll_base_scan_fn_t coll_scan_inter;

  mca_coll_base_scatter_fn_t coll_scatter_intra;
  mca_coll_base_scatter_fn_t coll_scatter_inter;

  mca_coll_base_scatterv_fn_t coll_scatterv_intra;
  mca_coll_base_scatterv_fn_t coll_scatterv_inter;
};
typedef struct mca_coll_1_0_0_t mca_coll_1_0_0_t;
typedef mca_coll_1_0_0_t mca_coll_t;


/*
 * Macro for use in modules that are of type coll v1.0.0
 */
#define MCA_COLL_BASE_VERSION_1_0_0 \
  /* coll v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* coll v1.0 */ \
  "coll", 1, 0, 0

/*
 * This function is technically part of the basic module, but since it
 * ships with LAM, and other modules may use the basic module for
 * query/init functionality, prototype this function here.
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  const mca_coll_1_0_0_t *
    mca_coll_basic_comm_query(MPI_Comm comm, int *priority);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_COLL_H */

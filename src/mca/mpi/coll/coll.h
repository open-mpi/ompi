/*
 * $HEADER$
 */

#ifndef MCA_COLL_H
#define MCA_COLL_H

#include "lam_config.h"

#include "mpi.h"
#include "lam/lfc/list.h"
#include "lam/util/cmd_line.h"
#include "mca/mca.h"


/*
 * Types for each API function
 */

typedef int (*mca_coll_thread_query_fn_t)(int *thread_min, int *thread_max);
typedef const struct mca_coll_1_0_0 *
  (*mca_coll_query_1_0_0_fn_t)(MPI_Comm comm, int *priority);

typedef int 
  (*mca_coll_init_1_0_0_fn_t)
  (MPI_Comm comm, const struct mca_coll_1_0_0 **new_coll);
typedef int (*mca_coll_finalize_fn_t)(MPI_Comm comm);

typedef int (*mca_coll_checkpoint_fn_t)(MPI_Comm comm);
typedef int (*mca_coll_continue_fn_t)(MPI_Comm comm);
typedef int (*mca_coll_restart_fn_t)(MPI_Comm comm);
typedef int (*mca_coll_interrupt_fn_t)(void);

typedef int (*mca_coll_allgather_fn_t)(void *sbuf, int scount, 
                                       MPI_Datatype sdtype, void *rbuf, 
                                       int rcount, MPI_Datatype rdtype, 
                                       MPI_Comm comm);
typedef int (*mca_coll_allgatherv_fn_t)(void *sbuf, int scount, 
                                        MPI_Datatype sdtype, void * rbuf, 
                                        int *rcounts, int *disps, 
                                        MPI_Datatype rdtype, 
                                        MPI_Comm comm);
typedef int (*mca_coll_allreduce_fn_t)(void *sbuf, void *rbuf, int count, 
                                       MPI_Datatype dtype, MPI_Op op, 
                                       MPI_Comm comm);
typedef int (*mca_coll_alltoall_fn_t)(void *sbuf, int scount, 
                                      MPI_Datatype sdtype, void* rbuf, 
                                      int rcount, MPI_Datatype rdtype, 
                                      MPI_Comm comm);
typedef int (*mca_coll_alltoallv_fn_t)(void *sbuf, int *scounts, 
                                       int *sdisps, MPI_Datatype sdtype, 
                                       void *rbuf, int *rcounts, 
                                       int *rdisps, MPI_Datatype rdtype, 
                                       MPI_Comm comm);
typedef int (*mca_coll_alltoallw_fn_t)(void *sbuf, int *scounts, 
                                       int *sdisps, MPI_Datatype *sdtypes, 
                                       void *rbuf, int *rcounts, 
                                       int *rdisps, MPI_Datatype *rdtypes, 
                                       MPI_Comm comm);
typedef int (*mca_coll_barrier_fn_t)(MPI_Comm comm);
typedef int (*mca_coll_bcast_fn_t)(void *buff, int count, 
                                   MPI_Datatype datatype, int root, 
                                   MPI_Comm comm);
typedef int (*mca_coll_exscan_fn_t)(void *sbuf, void *rbuf, int count, 
                                    MPI_Datatype dtype, MPI_Op op, 
                                    MPI_Comm comm);
typedef int (*mca_coll_gather_fn_t)(void *sbuf, int scount, 
                                    MPI_Datatype sdtype, void *rbuf, 
                                    int rcount, MPI_Datatype rdtype, 
                                    int root, MPI_Comm comm);
typedef int (*mca_coll_gatherv_fn_t)(void *sbuf, int scount, 
                                     MPI_Datatype sdtype, void *rbuf, 
                                     int *rcounts, int *disps, 
                                     MPI_Datatype rdtype, int root, 
                                     MPI_Comm comm);
typedef int (*mca_coll_reduce_fn_t)(void *sbuf, void* rbuf, int count, 
                                    MPI_Datatype dtype, MPI_Op op, 
                                    int root, MPI_Comm comm);
typedef int (*mca_coll_reduce_scatter_fn_t)(void *sbuf, void *rbuf, 
                                            int *rcounts, 
                                            MPI_Datatype dtype, 
                                            MPI_Op op, MPI_Comm comm);
typedef int (*mca_coll_scan_fn_t)(void *sbuf, void *rbuf, int count, 
                                  MPI_Datatype dtype, MPI_Op op, 
                                  MPI_Comm comm);
typedef int (*mca_coll_scatter_fn_t)(void *sbuf, int scount, 
                                     MPI_Datatype sdtype, void *rbuf, 
                                     int rcount, MPI_Datatype rdtype, 
                                     int root, MPI_Comm comm);
typedef int (*mca_coll_scatterv_fn_t)(void *sbuf, int *scounts, 
                                      int *disps, MPI_Datatype sdtype, 
                                      void* rbuf, int rcount, 
                                      MPI_Datatype rdtype, int root, 
                                      MPI_Comm comm);


/*
 * Ver 1.0.0
 */

struct mca_coll_module_1_0_0_t {
  mca_module_1_0_0_t super;

  /* Initialization / querying functions */

  mca_coll_thread_query_fn_t collm_thread_query;
  mca_coll_query_1_0_0_fn_t collm_query;
};
typedef struct mca_coll_module_1_0_0_t mca_coll_module_1_0_0_t;


/*
 * This struct is hung on the communicator by the winning coll module
 * after the negotiation.  It has pointers for all the collective
 * functions, as well as a "finalize" function for when the
 * communicator is freed.
 */

struct mca_coll_1_0_0_t {

  /* Per-communicator initialization and finalization functions */

  mca_coll_init_1_0_0_fn_t coll_init;
  mca_coll_finalize_fn_t coll_finalize;

  /* Checkpoint / restart functions */

  mca_coll_checkpoint_fn_t coll_checkpoint;
  mca_coll_continue_fn_t coll_continue;
  mca_coll_restart_fn_t coll_restart;
  mca_coll_interrupt_fn_t coll_interrupt;

  /* Memory allocation / freeing */

  mca_alloc_mem_fn_t coll_alloc_mem;
  mca_free_mem_fn_t coll_free_mem;

  /* Collective function pointers */

  mca_coll_allgather_fn_t coll_allgather_intra;
  mca_coll_allgather_fn_t coll_allgather_inter;

  mca_coll_allgatherv_fn_t coll_allgatherv_intra;
  mca_coll_allgatherv_fn_t coll_allgatherv_inter;

  mca_coll_allreduce_fn_t coll_allreduce_intra;
  mca_coll_allreduce_fn_t coll_allreduce_inter;

  mca_coll_alltoall_fn_t coll_alltoall_intra;
  mca_coll_alltoall_fn_t coll_alltoall_inter;

  mca_coll_alltoallv_fn_t coll_alltoallv_intra;
  mca_coll_alltoallv_fn_t coll_alltoallv_inter;

  mca_coll_alltoallw_fn_t coll_alltoallw_intra;
  mca_coll_alltoallw_fn_t coll_alltoallw_inter;

  mca_coll_barrier_fn_t coll_barrier_intra;
  mca_coll_barrier_fn_t coll_barrier_inter;

  bool coll_bcast_optimization;
  mca_coll_bcast_fn_t coll_bcast_intra;
  mca_coll_bcast_fn_t coll_bcast_inter;

  mca_coll_exscan_fn_t coll_exscan_intra;
  mca_coll_exscan_fn_t coll_exscan_inter;

  mca_coll_gather_fn_t coll_gather_intra;
  mca_coll_gather_fn_t coll_gather_inter;

  mca_coll_gatherv_fn_t coll_gatherv_intra;
  mca_coll_gatherv_fn_t coll_gatherv_inter;
  
  bool coll_reduce_optimization;
  mca_coll_reduce_fn_t coll_reduce_intra;
  mca_coll_reduce_fn_t coll_reduce_inter;

  mca_coll_reduce_scatter_fn_t coll_reduce_scatter_intra;
  mca_coll_reduce_scatter_fn_t coll_reduce_scatter_inter;

  mca_coll_scan_fn_t coll_scan_intra;
  mca_coll_scan_fn_t coll_scan_inter;

  mca_coll_scatter_fn_t coll_scatter_intra;
  mca_coll_scatter_fn_t coll_scatter_inter;

  mca_coll_scatterv_fn_t coll_scatterv_intra;
  mca_coll_scatterv_fn_t coll_scatterv_inter;
};
struct mca_coll_1_0_0_t mca_coll_1_0_0_t;


/*
 * Global functions for MCA overall collective open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_coll_base_close(void);
  int mca_coll_base_init_comm(MPI_Comm comm);
  int mca_coll_base_get_param(MPI_Comm comm, int keyval);
  int mca_coll_base_open(lam_cmd_line_t *cmd);
  int mca_coll_base_query(void);

  /*
   * This is technically part of the basic module, but since it ships
   * with LAM, and other modules may use the basic module for
   * query/init functionality, prototype this function here.
   */

  const mca_coll_module_1_0_0_t *
    mca_coll_basic_query(MPI_Comm comm, int *priority);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Public variables
 */

extern int mca_coll_verbose;
extern int mca_coll_did;
extern int mca_coll_base_crossover;
extern int mca_coll_base_associative;
extern int mca_coll_base_reduce_crossover;
extern lam_list_t *mca_coll_base_opened;
extern lam_list_t *mca_coll_base_available;


/*
 * Global instance of array of pointers to mca_coll_t.  Will
 * effectively be filled in by configure.
 */

extern const mca_module_t **mca_coll_modules;


#endif /* MCA_COLL_H */

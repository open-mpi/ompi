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
typedef const struct mca_coll_actions_1_0_0 *
  (*mca_coll_query_1_0_0_fn_t)(MPI_Comm comm, int *priority);

typedef int 
  (*mca_coll_init_1_0_0_fn_t)
  (MPI_Comm comm, const struct mca_coll_actions_1_0_0 **new_actions);
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

typedef struct mca_coll_1_0_0 {
  mca_t mc_meta_info;

  /* Initialization / querying functions */

  mca_coll_thread_query_fn_t mc_thread_query;
  mca_coll_query_1_0_0_fn_t mc_query;

  /* Flags */

  int mc_has_checkpoint;
} mca_coll_1_0_0_t;


/*
 * This struct is hung on the communicator (struct _comm) by the
 * winning coll module after the negotiation.  It has pointers for all
 * the collective functions, as well as a "finalize" function for when
 * the communicator is freed.
 */

typedef struct mca_coll_actions_1_0_0 {

  /* Per-communicator initialization and finalization functions */

  mca_coll_init_1_0_0_fn_t mca_init;
  mca_coll_finalize_fn_t mca_finalize;

  /* Checkpoint / restart functions */

  mca_coll_checkpoint_fn_t mca_checkpoint;
  mca_coll_continue_fn_t mca_continue;
  mca_coll_restart_fn_t mca_restart;
  mca_coll_interrupt_fn_t mca_interrupt;

  /* Collective function pointers */

  mca_coll_allgather_fn_t mca_allgather_intra;
  mca_coll_allgather_fn_t mca_allgather_inter;

  mca_coll_allgatherv_fn_t mca_allgatherv_intra;
  mca_coll_allgatherv_fn_t mca_allgatherv_inter;

  mca_coll_allreduce_fn_t mca_allreduce_intra;
  mca_coll_allreduce_fn_t mca_allreduce_inter;

  mca_coll_alltoall_fn_t mca_alltoall_intra;
  mca_coll_alltoall_fn_t mca_alltoall_inter;

  mca_coll_alltoallv_fn_t mca_alltoallv_intra;
  mca_coll_alltoallv_fn_t mca_alltoallv_inter;

  mca_coll_alltoallw_fn_t mca_alltoallw_intra;
  mca_coll_alltoallw_fn_t mca_alltoallw_inter;

  mca_coll_barrier_fn_t mca_barrier_intra;
  mca_coll_barrier_fn_t mca_barrier_inter;

  int mca_bcast_optimization;
  mca_coll_bcast_fn_t mca_bcast_intra;
  mca_coll_bcast_fn_t mca_bcast_inter;

  mca_coll_exscan_fn_t mca_exscan_intra;
  mca_coll_exscan_fn_t mca_exscan_inter;

  mca_coll_gather_fn_t mca_gather_intra;
  mca_coll_gather_fn_t mca_gather_inter;

  mca_coll_gatherv_fn_t mca_gatherv_intra;
  mca_coll_gatherv_fn_t mca_gatherv_inter;
  
  int mca_reduce_optimization;
  mca_coll_reduce_fn_t mca_reduce_intra;
  mca_coll_reduce_fn_t mca_reduce_inter;

  mca_coll_reduce_scatter_fn_t mca_reduce_scatter_intra;
  mca_coll_reduce_scatter_fn_t mca_reduce_scatter_inter;

  mca_coll_scan_fn_t mca_scan_intra;
  mca_coll_scan_fn_t mca_scan_inter;

  mca_coll_scatter_fn_t mca_scatter_intra;
  mca_coll_scatter_fn_t mca_scatter_inter;

  mca_coll_scatterv_fn_t mca_scatterv_intra;
  mca_coll_scatterv_fn_t mca_scatterv_inter;
} mca_coll_actions_1_0_0_t;


/*
 * Global functions for SSI overall collective open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_coll_base_close(void);
  int mca_coll_base_init_comm(MPI_Comm comm);
  int mca_coll_base_get_param(MPI_Comm comm, int keyval);
  int mca_coll_base_open(lam_cmt_line_t *cmd);
  int mca_coll_base_query(void);

  int mca_coll_base_empty_checkpoint(MPI_Comm comm);
  int mca_coll_base_empty_continue(MPI_Comm comm);
  int mca_coll_base_empty_restart(MPI_Comm comm);
  int mca_coll_base_empty_interrupt(void);

  /*
   * This is technically part of the basic module, but since it ships
   * with LAM, and other modules may use the basic module for
   * query/init functionality, prototype this function here.
   */

  const mca_coll_actions_1_0_0_t *
    mca_coll_lam_basic_query(MPI_Comm comm, int *priority);
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

extern const mca_t **mca_coll_modules;


#endif /* MCA_COLL_H */

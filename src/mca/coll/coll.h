/*
 * $HEADER$
 */

#ifndef MCA_COLL_H
#define MCA_COLL_H

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/base/base.h"


/*
 * Coll module function typedefs
 */

typedef int (*mca_coll_base_init_query_fn_t)(bool *allow_multi_user_threads,
                                             bool *have_hidden_threads);
typedef const struct mca_coll_1_0_0_t *
  (*mca_coll_base_comm_query_1_0_0_fn_t)(struct ompi_communicator_t *comm, 
                                         int *priority);
typedef int (*mca_coll_base_comm_unquery_fn_t)
  (struct ompi_communicator_t *comm);

typedef const struct mca_coll_1_0_0_t *
  (*mca_coll_base_module_init_1_0_0_fn_t)(struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_finalize_fn_t)
  (struct ompi_communicator_t *comm);


/*
 * Coll interface function typedefs
 */

typedef int (*mca_coll_base_checkpoint_fn_t)(struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_continue_fn_t)(struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_restart_fn_t)(struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_interrupt_fn_t)(void);

typedef int (*mca_coll_base_allgather_fn_t)(void *sbuf, int scount, 
                                            struct ompi_datatype_t *sdtype, 
                                            void *rbuf, int rcount, 
                                            struct ompi_datatype_t *rdtype, 
                                            struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_allgatherv_fn_t)(void *sbuf, int scount, 
                                             ompi_datatype_t *sdtype, 
                                             void * rbuf, int *rcounts, 
                                             int *disps, 
                                             struct ompi_datatype_t *rdtype, 
                                             struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_allreduce_fn_t)(void *sbuf, void *rbuf, int count, 
                                            struct ompi_datatype_t *dtype, 
                                            struct ompi_op_t *op, 
                                            struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_alltoall_fn_t)(void *sbuf, int scount, 
                                           struct ompi_datatype_t *sdtype, 
                                           void* rbuf, int rcount, 
                                           struct ompi_datatype_t *rdtype, 
                                           struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_alltoallv_fn_t)(void *sbuf, int *scounts, 
                                            int *sdisps, 
                                            struct ompi_datatype_t *sdtype, 
                                            void *rbuf, int *rcounts, 
                                            int *rdisps, 
                                            struct ompi_datatype_t *rdtype, 
                                            struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_alltoallw_fn_t)(void *sbuf, int *scounts, 
                                            int *sdisps, 
                                            struct ompi_datatype_t **sdtypes, 
                                            void *rbuf, int *rcounts, 
                                            int *rdisps, 
                                            struct ompi_datatype_t **rdtypes, 
                                            struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_barrier_fn_t)(struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_bcast_fn_t)(void *buff, int count, 
                                        struct ompi_datatype_t *datatype, 
                                        int root, 
                                        struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_exscan_fn_t)(void *sbuf, void *rbuf, int count, 
                                         struct ompi_datatype_t *dtype, 
                                         struct ompi_op_t *op,
                                         struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_gather_fn_t)(void *sbuf, int scount, 
                                         struct ompi_datatype_t *sdtype, 
                                         void *rbuf, int rcount, 
                                         struct ompi_datatype_t *rdtype, 
                                         int root, 
                                         struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_gatherv_fn_t)(void *sbuf, int scount, 
                                          struct ompi_datatype_t *sdtype, 
                                          void *rbuf, 
                                          int *rcounts, int *disps, 
                                          struct ompi_datatype_t *rdtype, 
                                          int root, 
                                          struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_reduce_fn_t)(void *sbuf, void* rbuf, int count, 
                                         struct ompi_datatype_t *dtype, 
                                         struct ompi_op_t *op, int root, 
                                         struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_reduce_scatter_fn_t)(void *sbuf, void *rbuf, 
                                                 int *rcounts, 
                                                 struct ompi_datatype_t *dtype,
                                                 struct ompi_op_t *op,
                                                 struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_scan_fn_t)(void *sbuf, void *rbuf, int count, 
                                       struct ompi_datatype_t *dtype, 
                                       MPI_Op op, 
                                       struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_scatter_fn_t)(void *sbuf, int scount, 
                                          struct ompi_datatype_t *sdtype, 
                                          void *rbuf, int rcount, 
                                          struct ompi_datatype_t *rdtype, 
                                          int root, 
                                          struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_scatterv_fn_t)(void *sbuf, int *scounts, 
                                           int *disps, 
                                           struct ompi_datatype_t *sdtype, 
                                           void* rbuf, int rcount, 
                                           struct ompi_datatype_t *rdtype,
                                           int root, 
                                           struct ompi_communicator_t *comm);


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
  mca_coll_base_comm_unquery_fn_t collm_comm_unquery;
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

  mca_coll_base_module_init_1_0_0_fn_t coll_module_init;
  mca_coll_base_module_finalize_fn_t coll_module_finalize;

  /* Checkpoint / restart functions */

  mca_coll_base_checkpoint_fn_t coll_checkpoint;
  mca_coll_base_continue_fn_t coll_continue;
  mca_coll_base_restart_fn_t coll_restart;
  mca_coll_base_interrupt_fn_t coll_interrupt;

  /* Collective function pointers */

  mca_coll_base_allgather_fn_t coll_allgather;
  mca_coll_base_allgatherv_fn_t coll_allgatherv;
  mca_coll_base_allreduce_fn_t coll_allreduce;
  mca_coll_base_alltoall_fn_t coll_alltoall;
  mca_coll_base_alltoallv_fn_t coll_alltoallv;
  mca_coll_base_alltoallw_fn_t coll_alltoallw;
  mca_coll_base_barrier_fn_t coll_barrier;
  bool coll_bcast_optimization;
  mca_coll_base_bcast_fn_t coll_bcast;
  mca_coll_base_exscan_fn_t coll_exscan;
  mca_coll_base_gather_fn_t coll_gather;
  mca_coll_base_gatherv_fn_t coll_gatherv;
  bool coll_reduce_optimization;
  mca_coll_base_reduce_fn_t coll_reduce;
  mca_coll_base_reduce_scatter_fn_t coll_reduce_scatter;
  mca_coll_base_scan_fn_t coll_scan;
  mca_coll_base_scatter_fn_t coll_scatter;
  mca_coll_base_scatterv_fn_t coll_scatterv;
};
typedef struct mca_coll_1_0_0_t mca_coll_1_0_0_t;


/*
 * Macro for use in modules that are of type coll v1.0.0
 */
#define MCA_COLL_BASE_VERSION_1_0_0 \
  /* coll v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* coll v1.0 */ \
  "coll", 1, 0, 0

#endif /* MCA_COLL_H */

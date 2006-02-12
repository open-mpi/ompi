/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#ifndef MCA_COLL_H
#define MCA_COLL_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"


/*
 * Forward declaration
 */

struct mca_coll_base_comm_t;


/*
 * Coll component function typedefs
 */

typedef int (*mca_coll_base_component_init_query_fn_t)
  (bool enable_progress_threads, bool enable_mpi_threads);
typedef const struct mca_coll_base_module_1_0_0_t *
  (*mca_coll_base_component_comm_query_1_0_0_fn_t)
  (struct ompi_communicator_t *comm, int *priority,
   struct mca_coll_base_comm_t **data);
typedef int (*mca_coll_base_component_comm_unquery_fn_t)
  (struct ompi_communicator_t *comm,
   struct mca_coll_base_comm_t *data);

/*
 * Coll module function typedefs
 */

typedef const struct mca_coll_base_module_1_0_0_t *
  (*mca_coll_base_module_init_1_0_0_fn_t)(struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_finalize_fn_t)
  (struct ompi_communicator_t *comm);


typedef int (*mca_coll_base_module_allgather_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void *rbuf, int rcount, struct ompi_datatype_t *rdtype, 
   struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_allgatherv_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void * rbuf, int *rcounts, int *disps,  struct ompi_datatype_t *rdtype, 
   struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_allreduce_fn_t)
  (void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, 
   struct ompi_op_t *op, struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_alltoall_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void* rbuf, int rcount, struct ompi_datatype_t *rdtype, 
   struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_alltoallv_fn_t)
  (void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t *sdtype, 
   void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t *rdtype, 
   struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_alltoallw_fn_t)
  (void *sbuf, int *scounts, int *sdisps, struct ompi_datatype_t **sdtypes, 
   void *rbuf, int *rcounts, int *rdisps, struct ompi_datatype_t **rdtypes, 
   struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_barrier_fn_t)
  (struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_bcast_fn_t)
  (void *buff, int count, struct ompi_datatype_t *datatype, int root,
   struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_exscan_fn_t)
  (void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, 
   struct ompi_op_t *op, struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_gather_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void *rbuf, int rcount, struct ompi_datatype_t *rdtype, 
   int root, struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_gatherv_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void *rbuf, int *rcounts, int *disps, struct ompi_datatype_t *rdtype, 
   int root, struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_reduce_fn_t)
  (void *sbuf, void* rbuf, int count, struct ompi_datatype_t *dtype, 
   struct ompi_op_t *op, int root, struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_reduce_scatter_fn_t)
  (void *sbuf, void *rbuf, int *rcounts, struct ompi_datatype_t *dtype,
   struct ompi_op_t *op, struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_scan_fn_t)
  (void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, 
   struct ompi_op_t *op, struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_scatter_fn_t)
  (void *sbuf, int scount, struct ompi_datatype_t *sdtype, 
   void *rbuf, int rcount, struct ompi_datatype_t *rdtype, 
   int root, struct ompi_communicator_t *comm);
typedef int (*mca_coll_base_module_scatterv_fn_t)
  (void *sbuf, int *scounts, int *disps, struct ompi_datatype_t *sdtype, 
   void* rbuf, int rcount, struct ompi_datatype_t *rdtype,
   int root, struct ompi_communicator_t *comm);


/*
 * Structure for coll v1.0.0 components
 * Chained to MCA v1.0.0
 */
struct mca_coll_base_component_1_0_0_t {
  mca_base_component_t collm_version;
  mca_base_component_data_1_0_0_t collm_data;

  /* Initialization / querying functions */

  mca_coll_base_component_init_query_fn_t collm_init_query;
  mca_coll_base_component_comm_query_1_0_0_fn_t collm_comm_query;
  mca_coll_base_component_comm_unquery_fn_t collm_comm_unquery;
};
typedef struct mca_coll_base_component_1_0_0_t mca_coll_base_component_1_0_0_t;


/*
 * This struct is hung on the communicator by the winning coll component
 * after the negotiation.  It has pointers for all the collective
 * functions, as well as a "finalize" function for when the
 * communicator is freed.
 */

struct mca_coll_base_module_1_0_0_t {

  /* Per-communicator initialization and finalization functions */

  mca_coll_base_module_init_1_0_0_fn_t coll_module_init;
  mca_coll_base_module_finalize_fn_t coll_module_finalize;

  /* Collective function pointers */

  mca_coll_base_module_allgather_fn_t coll_allgather;
  mca_coll_base_module_allgatherv_fn_t coll_allgatherv;
  mca_coll_base_module_allreduce_fn_t coll_allreduce;
  mca_coll_base_module_alltoall_fn_t coll_alltoall;
  mca_coll_base_module_alltoallv_fn_t coll_alltoallv;
  mca_coll_base_module_alltoallw_fn_t coll_alltoallw;
  mca_coll_base_module_barrier_fn_t coll_barrier;
  mca_coll_base_module_bcast_fn_t coll_bcast;
  mca_coll_base_module_exscan_fn_t coll_exscan;
  mca_coll_base_module_gather_fn_t coll_gather;
  mca_coll_base_module_gatherv_fn_t coll_gatherv;
  mca_coll_base_module_reduce_fn_t coll_reduce;
  mca_coll_base_module_reduce_scatter_fn_t coll_reduce_scatter;
  mca_coll_base_module_scan_fn_t coll_scan;
  mca_coll_base_module_scatter_fn_t coll_scatter;
  mca_coll_base_module_scatterv_fn_t coll_scatterv;
};
typedef struct mca_coll_base_module_1_0_0_t mca_coll_base_module_1_0_0_t;


/*
 * Macro for use in components that are of type coll v1.0.0
 */
#define MCA_COLL_BASE_VERSION_1_0_0 \
  /* coll v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* coll v1.0 */ \
  "coll", 1, 0, 0

#endif /* MCA_COLL_H */

/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_base_topo.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include <math.h>

typedef struct ompi_coll_adapt_algorithm_index_s {
    int algorithm_index;
    uintptr_t algorithm_fn_ptr;
} ompi_coll_adapt_algorithm_index_t;

/* Bcast */
int ompi_coll_adapt_ibcast_init(void);
int ompi_coll_adapt_ibcast_fini(void);
int ompi_coll_adapt_bcast(void *buff, int count, struct ompi_datatype_t *datatype, int root,
                         struct ompi_communicator_t *comm, mca_coll_base_module_t * module);
int ompi_coll_adapt_ibcast(void *buff, int count, struct ompi_datatype_t *datatype, int root,
                          struct ompi_communicator_t *comm, ompi_request_t ** request,
                          mca_coll_base_module_t * module);
int ompi_coll_adapt_ibcast_generic(void *buff, int count, struct ompi_datatype_t *datatype, int root,
                                  struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  mca_coll_base_module_t * module, ompi_coll_tree_t * tree,
                                  size_t seg_size, int ibcast_tag);
int ompi_coll_adapt_ibcast_binomial(void *buff, int count, struct ompi_datatype_t *datatype,
                                   int root, struct ompi_communicator_t *comm,
                                   ompi_request_t ** request, mca_coll_base_module_t * module,
                                   int ibcast_tag);
int ompi_coll_adapt_ibcast_in_order_binomial(void *buff, int count, struct ompi_datatype_t *datatype,
                                            int root, struct ompi_communicator_t *comm,
                                            ompi_request_t ** request,
                                            mca_coll_base_module_t * module, int ibcast_tag);
int ompi_coll_adapt_ibcast_binary(void *buff, int count, struct ompi_datatype_t *datatype, int root,
                                 struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 mca_coll_base_module_t * module, int ibcast_tag);
int ompi_coll_adapt_ibcast_pipeline(void *buff, int count, struct ompi_datatype_t *datatype,
                                   int root, struct ompi_communicator_t *comm,
                                   ompi_request_t ** request, mca_coll_base_module_t * module,
                                   int ibcast_tag);
int ompi_coll_adapt_ibcast_chain(void *buff, int count, struct ompi_datatype_t *datatype, int root,
                                struct ompi_communicator_t *comm, ompi_request_t ** request,
                                mca_coll_base_module_t * module, int ibcast_tag);
int ompi_coll_adapt_ibcast_linear(void *buff, int count, struct ompi_datatype_t *datatype, int root,
                                 struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 mca_coll_base_module_t * module, int ibcast_tag);
int ompi_coll_adapt_ibcast_tuned(void *buff, int count, struct ompi_datatype_t *datatype, int root,
				struct ompi_communicator_t *comm, ompi_request_t ** request,
				mca_coll_base_module_t *module, int ibcast_tag); 

/* Reduce */
int ompi_coll_adapt_ireduce_init(void);
int ompi_coll_adapt_ireduce_fini(void);
int ompi_coll_adapt_reduce(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                          struct ompi_op_t *op, int root, struct ompi_communicator_t *comm,
                          mca_coll_base_module_t * module);
int ompi_coll_adapt_ireduce(const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
                           struct ompi_op_t *op, int root, struct ompi_communicator_t *comm,
                           ompi_request_t ** request, mca_coll_base_module_t * module);
int ompi_coll_adapt_ireduce_generic(const void *sbuf, void *rbuf, int count,
                                   struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                   struct ompi_communicator_t *comm, ompi_request_t ** request,
                                   mca_coll_base_module_t * module, ompi_coll_tree_t * tree,
                                   size_t seg_size, int ireduce_tag);
int ompi_coll_adapt_ireduce_tuned(const void *sbuf, void *rbuf, int count,
				 struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
				 struct ompi_communicator_t *comm, ompi_request_t ** request,
				 mca_coll_base_module_t *module, int ireduce_tag);
int ompi_coll_adapt_ireduce_binomial(const void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                    struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    mca_coll_base_module_t * module, int ireduce_tag);
int ompi_coll_adapt_ireduce_in_order_binomial(const void *sbuf, void *rbuf, int count,
                                             struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                                             int root, struct ompi_communicator_t *comm,
                                             ompi_request_t ** request,
                                             mca_coll_base_module_t * module, int ireduce_tag);
int ompi_coll_adapt_ireduce_binary(const void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                  struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  mca_coll_base_module_t * module, int ireduce_tag);
int ompi_coll_adapt_ireduce_pipeline(const void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                    struct ompi_communicator_t *comm, ompi_request_t ** request,
                                    mca_coll_base_module_t * module, int ireduce_tag);
int ompi_coll_adapt_ireduce_chain(const void *sbuf, void *rbuf, int count,
                                 struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                 struct ompi_communicator_t *comm, ompi_request_t ** request,
                                 mca_coll_base_module_t * module, int ireduce_tag);
int ompi_coll_adapt_ireduce_linear(const void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype, struct ompi_op_t *op, int root,
                                  struct ompi_communicator_t *comm, ompi_request_t ** request,
                                  mca_coll_base_module_t * module, int ireduce_tag);

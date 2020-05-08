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
int ompi_coll_adapt_ibcast_register(void);
int ompi_coll_adapt_ibcast_fini(void);
int ompi_coll_adapt_bcast(BCAST_ARGS);
int ompi_coll_adapt_ibcast(IBCAST_ARGS);
int ompi_coll_adapt_ibcast_generic(IBCAST_ARGS,
                                   ompi_coll_tree_t * tree, size_t seg_size);
int ompi_coll_adapt_ibcast_binomial(IBCAST_ARGS);
int ompi_coll_adapt_ibcast_in_order_binomial(IBCAST_ARGS);
int ompi_coll_adapt_ibcast_binary(IBCAST_ARGS);
int ompi_coll_adapt_ibcast_pipeline(IBCAST_ARGS);
int ompi_coll_adapt_ibcast_chain(IBCAST_ARGS);
int ompi_coll_adapt_ibcast_linear(IBCAST_ARGS);
int ompi_coll_adapt_ibcast_tuned(IBCAST_ARGS);

/* Reduce */
int ompi_coll_adapt_ireduce_register(void);
int ompi_coll_adapt_ireduce_fini(void);
int ompi_coll_adapt_reduce(REDUCE_ARGS);
int ompi_coll_adapt_ireduce(IREDUCE_ARGS);
int ompi_coll_adapt_ireduce_generic(IREDUCE_ARGS,
                                    ompi_coll_tree_t * tree, size_t seg_size);
int ompi_coll_adapt_ireduce_tuned(IREDUCE_ARGS);
int ompi_coll_adapt_ireduce_binomial(IREDUCE_ARGS);
int ompi_coll_adapt_ireduce_in_order_binomial(IREDUCE_ARGS);
int ompi_coll_adapt_ireduce_binary(IREDUCE_ARGS);
int ompi_coll_adapt_ireduce_pipeline(IREDUCE_ARGS);
int ompi_coll_adapt_ireduce_chain(IREDUCE_ARGS);
int ompi_coll_adapt_ireduce_linear(IREDUCE_ARGS);

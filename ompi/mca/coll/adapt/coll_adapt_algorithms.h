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

typedef int (*ompi_mca_coll_adapt_ibcast_function_t)(IBCAST_ARGS);
typedef int (*ompi_mca_coll_adapt_ireduce_function_t)(IREDUCE_ARGS);

typedef struct ompi_coll_adapt_algorithm_index_s {
    int algorithm_index;
    union {
        ompi_mca_coll_adapt_ibcast_function_t  ibcast_fn_ptr;
        ompi_mca_coll_adapt_ireduce_function_t ireduce_fn_ptr;
    };
} ompi_coll_adapt_algorithm_index_t;

/* Bcast */
int ompi_coll_adapt_ibcast_register(void);
int ompi_coll_adapt_ibcast_fini(void);
int ompi_coll_adapt_bcast(BCAST_ARGS);
int ompi_coll_adapt_ibcast(IBCAST_ARGS);

/* Reduce */
int ompi_coll_adapt_ireduce_register(void);
int ompi_coll_adapt_ireduce_fini(void);
int ompi_coll_adapt_reduce(REDUCE_ARGS);
int ompi_coll_adapt_ireduce(IREDUCE_ARGS);

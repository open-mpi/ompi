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


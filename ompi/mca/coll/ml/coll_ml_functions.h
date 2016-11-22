/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file */

#ifndef MCA_COLL_ML_FUNCTIONS_H
#define MCA_COLL_ML_FUNCTIONS_H

#include "ompi_config.h"

BEGIN_C_DECLS

#define ML_MEMSYNC -100

enum {
    ML_BARRIER_DEFAULT
};
 /* small data algorithm */
/* broadcast functions */
enum {
    /* small data algorithm */
    ML_BCAST_SMALL_DATA_KNOWN,
    /* small data - dynamic decision making supported */
    ML_BCAST_SMALL_DATA_UNKNOWN,
    /* Sequential algorithm */
    ML_BCAST_SMALL_DATA_SEQUENTIAL,

    ML_BCAST_LARGE_DATA_KNOWN,

    ML_BCAST_LARGE_DATA_UNKNOWN,

    ML_BCAST_LARGE_DATA_SEQUENTIAL,

    /* marker - all routines about this are expected to be used in
     * selection logic that is based on size of the data */
    ML_N_DATASIZE_BINS,

    /* number of functions - also counts some markers, but ... */
    ML_NUM_BCAST_FUNCTIONS
};


/* scatter functions */
enum {
    /* small data algorithm */
    ML_SCATTER_SMALL_DATA_KNOWN,

    /* marker - all routines about this are expected to be used in
     * selection logic that is based on size of the data */
    ML_SCATTER_N_DATASIZE_BINS,

    /* small data - dynamic decision making supported */
    ML_SCATTER_SMALL_DATA_UNKNOWN,

    /* Sequential algorithm */
    ML_SCATTER_SMALL_DATA_SEQUENTIAL,

    /* number of functions - also counts some markers, but ... */
    ML_NUM_SCATTER_FUNCTIONS
};


/* Allreduce functions */
enum {
    /* small data algorithm */
    ML_SMALL_DATA_ALLREDUCE,

    /* Large data algorithm */
    ML_LARGE_DATA_ALLREDUCE,

    /* If some of bcols doesn't support
       all possibles types, use these extra algthms */
    /* small data algorithm */
    ML_SMALL_DATA_EXTRA_TOPO_ALLREDUCE,

    /* large data algorithm */
    ML_LARGE_DATA_EXTRA_TOPO_ALLREDUCE,

    /* number of functions */
    ML_NUM_ALLREDUCE_FUNCTIONS
};

/* Reduce functions */
enum {
    /* small data algorithm */
    ML_SMALL_DATA_REDUCE,

    /* Large data algorithm */
    ML_LARGE_DATA_REDUCE,

    /* number of functions */
    ML_NUM_REDUCE_FUNCTIONS
};
/* Alltoall functions */
enum {
    /* small data algorithm */
    ML_SMALL_DATA_ALLTOALL,
    /* large all to all */
    ML_LARGE_DATA_ALLTOALL,
    /* number of functions */
    ML_NUM_ALLTOALL_FUNCTIONS
};

/* Allgather functions */
enum {
    /* small data */
    ML_SMALL_DATA_ALLGATHER,
    /* large data */
    ML_LARGE_DATA_ALLGATHER,
    /* number of functions */
    ML_NUM_ALLGATHER_FUNCTIONS
};

/* gather functions */
enum {
    /* small data */
    ML_SMALL_DATA_GATHER,
    /* large data */
    ML_LARGE_DATA_GATHER,
    /* number of functions */
    ML_NUM_GATHER_FUNCTIONS
};

END_C_DECLS

#endif /* MCA_COLL_ML_FUNCTIONS_H */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef MCA_COLL_TUNED_FORCED_H_HAS_BEEN_INCLUDED
#define MCA_COLL_TUNED_FORCED_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/* this structure is for storing the indexes to the forced algorithm mca params... */
/* we get these at component query (so that registered values appear in ompi_infoi) */

struct coll_tuned_force_algorithm_mca_param_indices_t {
   int  algorithm_param_index;      /* which algorithm you want to force */
   int  segsize_param_index;        /* segsize to use (if supported), 0 = no segmentation */
   int  tree_fanout_param_index;    /* tree fanout/in to use */
   int  chain_fanout_param_index;   /* K-chain fanout/in to use */
};

typedef struct coll_tuned_force_algorithm_mca_param_indices_t coll_tuned_force_algorithm_mca_param_indices_t;


/* the following type is for storing actual value obtained from the MCA on each tuned module */
/* via their mca param indices lookup in the component */
/* this structure is stored once per collective type per communicator... */
struct coll_tuned_force_algorithm_params_t {
   int  algorithm;      /* which algorithm you want to force */
   int  segsize;        /* segsize to use (if supported), 0 = no segmentation */
   int  tree_fanout;    /* tree fanout/in to use */
   int  chain_fanout;   /* K-chain fanout/in to use */
};

typedef struct coll_tuned_force_algorithm_params_t coll_tuned_force_algorithm_params_t;


/* prototypes */

int ompi_coll_tuned_forced_getvalues (coll_tuned_force_algorithm_mca_param_indices_t mca_params,
                                        coll_tuned_force_algorithm_params_t *forced_values);

/* barrier has less options than any other collective so it gets its own special function */
int ompi_coll_tuned_forced_getvalues_barrier (coll_tuned_force_algorithm_mca_param_indices_t mca_params,
                                        coll_tuned_force_algorithm_params_t *forced_values);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_COLL_TUNED_FORCED_H_HAS_BEEN_INCLUDED */



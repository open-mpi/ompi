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

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "coll_tuned.h"

/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "coll_tuned_topo.h"

/* also need the dynamic rule structures */
#include "coll_tuned_forced.h"

#include "coll_tuned_util.h"

#include <stdlib.h>
#include <stdio.h>

/* We put all routines that handle the MCA user forced algorithm and parameter choices here */
/* recheck the setting of forced, called on module create (i.e. for each new comm) */
                                                                                                          
int ompi_coll_tuned_forced_getvalues (coll_tuned_force_algorithm_mca_param_indices_t mca_params, 
                                      coll_tuned_force_algorithm_params_t *forced_values)
{
    mca_base_param_lookup_int (mca_params.algorithm_param_index,    &(forced_values->algorithm));
    mca_base_param_lookup_int (mca_params.segsize_param_index,      &(forced_values->segsize));
    mca_base_param_lookup_int (mca_params.tree_fanout_param_index,  &(forced_values->tree_fanout));
    mca_base_param_lookup_int (mca_params.chain_fanout_param_index, &(forced_values->chain_fanout));

    return (MPI_SUCCESS);
}


/* special version of above just for barrier which only has one option available (at the moment...) */
int ompi_coll_tuned_forced_getvalues_barrier (coll_tuned_force_algorithm_mca_param_indices_t mca_params, 
                                              coll_tuned_force_algorithm_params_t *forced_values)
{
    mca_base_param_lookup_int (mca_params.algorithm_param_index,    &(forced_values->algorithm));

    return (MPI_SUCCESS);
}



/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/include/ompi/constants.h"
#include "ompi/mca/coll/ml/coll_ml.h"

int ml_coll_hier_nonblocking_barrier_setup(mca_coll_ml_module_t *ml_module,
        mca_coll_ml_topology_t *topo_info)
{
    /* local variables */
    int ret=OMPI_SUCCESS;
    /* int global_low_hierarchy_index=topo_info->global_lowest_hier_group_index; */
    int global_high_hierarchy_index=topo_info->global_highest_hier_group_index;
    int my_n_hierarchies=topo_info->n_levels;
    int hier_loop_range,i,cnt;
    mca_bcol_base_module_t *bcol_module;
    /* collective algorithms */
    coll_ml_collective_description_t **hierarchical_algorithms = topo_info->hierarchical_algorithms;

    /* RLG:  one non-blocking barrier collective algorithm - this is really a hack,
     * we need to figure out how to do this in a bit more extensible
     * manner.
     */
     hierarchical_algorithms[BCOL_IBARRIER]= (coll_ml_collective_description_t *)
         malloc(sizeof(coll_ml_collective_description_t));
     if( NULL == hierarchical_algorithms[BCOL_IBARRIER]) {
         ret=OMPI_ERROR;
         goto Error;
     }

    /* am I a member of the highest level subgroup */
    if( global_high_hierarchy_index ==
          topo_info->component_pairs[my_n_hierarchies-1].bcol_index )
    {
        hier_loop_range=my_n_hierarchies-1;
        /* allocate the function description */
        hierarchical_algorithms[BCOL_IBARRIER][0].n_functions=2*my_n_hierarchies-1;
        /* collective description */
        hierarchical_algorithms[BCOL_IBARRIER][0].alg_params.coll_fn.
            ibarrier_recursive_doubling.n_fanin_steps=my_n_hierarchies-1;
        hierarchical_algorithms[BCOL_IBARRIER][0].alg_params.coll_fn.
            ibarrier_recursive_doubling.n_fanout_steps=my_n_hierarchies-1;
        hierarchical_algorithms[BCOL_IBARRIER][0].alg_params.coll_fn.
            ibarrier_recursive_doubling.n_recursive_doubling_steps=1;
    } else {
        hier_loop_range=my_n_hierarchies;
        /* allocate the function description */
        hierarchical_algorithms[BCOL_IBARRIER][0].n_functions=2*my_n_hierarchies;
        /* collective description */
        hierarchical_algorithms[BCOL_IBARRIER][0].alg_params.coll_fn.
            ibarrier_recursive_doubling.n_fanin_steps=my_n_hierarchies;
        hierarchical_algorithms[BCOL_IBARRIER][0].alg_params.coll_fn.
            ibarrier_recursive_doubling.n_fanout_steps=my_n_hierarchies;
        hierarchical_algorithms[BCOL_IBARRIER][0].alg_params.coll_fn.
            ibarrier_recursive_doubling.n_recursive_doubling_steps=0;
    }

    /* number of temp buffers */
    hierarchical_algorithms[BCOL_IBARRIER]->n_buffers=0;

    /* allocate space for the functions */
    hierarchical_algorithms[BCOL_IBARRIER][0].functions=(mca_bcol_base_function_t *)
        malloc(sizeof(mca_bcol_base_function_t)*
            hierarchical_algorithms[BCOL_IBARRIER][0].n_functions);
    if( NULL == hierarchical_algorithms[BCOL_IBARRIER][0].functions) {
        ret=OMPI_ERROR;
        goto Error;
    }


    /*
     * The algorithm used here for an N level system
     *  - up to level N-2, inclusive : fan in
     *  - level N-1: barrier
     *  - level N-2, to level 0: fanout
     */

    cnt=0;
    /* fan-in phase */
    for(i=0 ; i < hier_loop_range ; i++ ) {
        bcol_module=topo_info->component_pairs[i].bcol_modules[0];
        hierarchical_algorithms[BCOL_IBARRIER][0].functions[cnt].fn_idx = BCOL_IFANIN;
/* RLG NOTE - is this a bug ?  We do not set the bcol module pointer in the functions array ... */
        cnt++;
    }


    /* barrier */
    if( hier_loop_range != my_n_hierarchies ) {
        bcol_module=topo_info->component_pairs[my_n_hierarchies-1].bcol_modules[0];
        hierarchical_algorithms[BCOL_IBARRIER][0].functions[cnt].fn_idx = BCOL_IBARRIER;
        hierarchical_algorithms[BCOL_IBARRIER][0].functions[cnt].bcol_module = bcol_module;
        cnt++;
    }

    /* fan-out */
    for( i=hier_loop_range-1 ; i >=0 ; i-- ) {
        bcol_module=topo_info->component_pairs[i].bcol_modules[0];
        hierarchical_algorithms[BCOL_IBARRIER][0].functions[cnt].fn_idx = BCOL_IFANOUT;
        cnt++;
    }

    /* set the number of functions pointers needed by this routine - this is
     * used to figure out how large to allocate the function argument
     * array.
     */
    if( ml_module->max_fn_calls < cnt ) {
        ml_module->max_fn_calls=cnt;
    }

    /* set algorithm parameters */
    hierarchical_algorithms[BCOL_IBARRIER][0].
        alg_params.coll_fn.ibarrier_recursive_doubling.n_fanin_steps=hier_loop_range;
    hierarchical_algorithms[BCOL_IBARRIER][0].
        alg_params.coll_fn.ibarrier_recursive_doubling.n_fanout_steps=hier_loop_range;
    if( hier_loop_range != my_n_hierarchies ) {
        hierarchical_algorithms[BCOL_IBARRIER][0].
            alg_params.coll_fn.ibarrier_recursive_doubling.n_recursive_doubling_steps=1;
    } else {
        hierarchical_algorithms[BCOL_IBARRIER][0].
            alg_params.coll_fn.ibarrier_recursive_doubling.n_recursive_doubling_steps=0;
    }


    /* done */
    return ret;

Error :

    if( hierarchical_algorithms[BCOL_IBARRIER][0].functions) {
       free(hierarchical_algorithms[BCOL_IBARRIER][0].functions);
       hierarchical_algorithms[BCOL_IBARRIER][0].functions=NULL;
    }

    if( hierarchical_algorithms[BCOL_IBARRIER]) {
       free(hierarchical_algorithms[BCOL_IBARRIER]);
       hierarchical_algorithms[BCOL_IBARRIER]=NULL;
    }

    return ret;
}

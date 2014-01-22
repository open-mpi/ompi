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

#include "ompi_config.h"

#include "ompi/constants.h"
#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/bcol/bcol.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/coll/ml/coll_ml.h"
#include "ompi/mca/coll/ml/coll_ml_hier_algorithms.h"

    coll_ml_collective_description_t *collective_op;
    bcol_fn_args_t fn_arguments;
    mca_coll_ml_descriptor_t *msg_desc;

    static int coll_ml_setup_ibarrier_instance_recursive_doubling(
            mca_coll_ml_descriptor_t *msg_desc,
            coll_ml_collective_description_t *collective_op)
{
    /* local variables */
    ret=OMPI_SUCCESS;
    int i_fn,cnt;

    /* initialize function arguments */

    /* mark all routines as not yet started - need this, so that
     * when we try to progress the barrier, we know where to pickup
     * when a function is called - MOVE this into the setup function.
     */
    for(i_fn=0 ; i_fn < collective_op->n_functions ; i_fn++ ) {
        msg_desc->fragment.fn_args[i_fn].function_status=FUNCTION_NOT_STARTED;
    }

    /* setup the fanin root */
    for(i_fn=0 ; i_fn < collective_op->
        alg_params.coll_fn.ibarrier_recursive_doubling.n_fanin_steps ;
        i_fn++ ) {
        mca_bcol_base_module_t *bcol_mod=
           msg_desc->local_comm_description->functions[i_fn].bcol_module;
        /* the lowest rank in the group will be the root */
        msg_desc->fragment.fn_args[i_fn].root=0;
    }

    /* setup the fanout root */
    cnt=alg_params.coll_fn.ibarrier_recursive_doubling.n_fanin_steps+
        alg_params.coll_fn.ibarrier_recursive_doubling.n_recursive_doubling_steps;
    for(i_fn=cnt ; i_fn < cnt + collective_op->
        alg_params.coll_fn.ibarrier_recursive_doubling.n_fanin_steps ;
        i_fn++ ) {
        mca_bcol_base_module_t *bcol_mod=
           msg_desc->local_comm_description->functions[i_fn].bcol_module;
        /* the lowest rank in the group will be the root */
        msg_desc->fragment.fn_args[i_fn].root=0;
    }

    /* successful completion */
    return ret;
}

/**
 * Hierarchical blocking barrier
 */
int mca_coll_ml_nb_barrier_intra( struct ompi_communicator_t *comm,
        ompi_request_t ** request, mca_coll_base_module_t *module)
{
    /* local variables */
    int ret=OMPI_SUCCESS;
    mca_coll_ml_module_t *ml_module;
    uint64_t sequence_number;
    int i_fn;
    coll_ml_collective_description_t *collective_op;
    bcol_fn_args_t fn_arguments;
    mca_coll_ml_descriptor_t *msg_desc;

    ml_module=(mca_coll_ml_module_t *) module;
/* debug */
fprintf(stderr," mca_coll_ml_nb_barrier_intra called \n");
fflush(stderr);
/* end debug */

    /* grab full message descriptor - RLG: this is really not optimal,
     * as we may be doing too much initialization if the collective
     * routine completes on the first call to progress which is called
     * within this routine.  Need to see if we can be more efficient
     * here.  The current issue is that the only way that I can think
     * to do this now is with two separate code paths, which I want to
     * avoid at this stage.
     */
    OMPI_FREE_LIST_GET(&(ml_module->message_descriptors),
       msg_desc,ret);
    if( OMPI_SUCCESS != ret) {
        goto Error;
    }

    /* get message sequence number */
    sequence_number=OPAL_THREAD_ADD64(
            &(ml_module->no_data_collective_sequence_num),1);
    fn_arguments.sequence_num=sequence_number;


    /* get pointer to schedule - only one algorithm at this stage */
    collective_op=&(ml_module->hierarchical_algorithms[BCOL_NB_BARRIER][0]);

    /* call setup function - RLG: right now this is totally extra,
     * but if we are going to have more than one algorithm,
     * this is a better way to do this. */
    coll_ml_setup_ibarrier_instance_recursive_doubling(
            msg_desc,collective_op);

    /* call the progress function to actually start the barrier */

    /* recycle resources - RLG: need to think about this one */

#if 0
    /* run barrier */
    /* need to add bcol context for the call */
    for( i_fn =0 ; i_fn < collective_op->n_functions ; i_fn++ ) {
        mca_bcol_base_module_t *bcol_module=
            collective_op->functions[i_fn].bcol_module;
        /* for barrier, all we need is the group information that is
         * captured in the bcol module
         */
        ret=collective_op->functions[i_fn].fn(&fn_arguments,
                NULL,NULL,bcol_module);
        if( OMPI_SUCCESS != ret) {
        } goto Error;
    }
#endif

    return OMPI_SUCCESS;

Error:
    return ret;
}

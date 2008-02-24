/*
 * Copyright (c) 2007-2008 UT-Battelle, LLC
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "coll_sm2.h"
#include "ompi/op/op.h"
#include "ompi/datatype/datatype.h"


/**
 * Shared memory blocking allreduce.
 */
int mca_coll_sm2_allreduce_intra_fanin_fanout(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                struct mca_coll_base_module_1_1_0_t *module)
{
    /* local variables */
    int rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag;
    volatile char *sm_buffer;
    mca_coll_sm2_module_t *sm_module;

    sm_module=(mca_coll_sm2_module_t *) module;

    /* get unique tag for this collective - assume only one collective
     *  per communicator at a given time, so no locking needed
     *  for atomic update of the tag */
    sm_module->collective_tag++;
    tag=sm_module->collective_tag;

    /* get size of data needed - same layout as user data, so that
     *   we can apply the reudction routines directly on these buffers
     */
    rc=ompi_ddt_type_size(dtype, &dt_extent);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }
    message_extent=dt_extent*count;

    /* lenght of control and data regions */
    ctl_size=sm_module->ctl_memory_per_proc_per_segment;
    len_data_buffer=sm_module->data_memory_per_proc_per_segment;

    /* number of data types copies that the scratch buffer can hold */
    n_dts_per_buffer=((int) len_data_buffer)/dt_extent;
    if ( 0 == n_dts_per_buffer ) {
        rc=OMPI_ERROR;
        goto Error;
    }

    /* compute number of stripes needed to process this collective */
    n_data_segments=(count+n_dts_per_buffer -1 ) / n_dts_per_buffer ;

    /* get a pointer to the shared-memory working buffer */
    /* NOTE: starting with a rather synchronous approach */
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
        sm_buffer=alloc_sm2_shared_buffer(sm_module);
        if( NULL == sm_buffer) {
            rc=OMPI_ERR_OUT_OF_RESOURCE;
            goto Error;
        }
        /*
         * Fan into root phase
         */

        /* copy segment into shared buffer - later on will optimize to
         *   eliminate extra copies.
         */

        /*
         * Wait on children, and apply op to their data
         */

        /*
         * Signal parent that data is ready
         */
    
    
        /*
         * Fan out from root phase
         */

        /*
         * wait on Parent to signal that data is ready
         */

        /*
         * Copy data to shared buffer
         */

        /*
         * Signal children that Data is ready for reading
         */

        /*
         * Copy data out to destination
         */

        /* "free" the shared-memory working buffer */
        rc=free_sm2_shared_buffer(sm_module);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }
    }

    /* return */
    return rc;

Error:
    return rc;
}

/**
 * Shared memory blocking allreduce.
 */
int mca_coll_sm2_allreduce_intra(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                struct mca_coll_base_module_1_1_0_t *module)
{
    /* local variables */
    int rc;

    if( 0 != (op->o_flags & OMPI_OP_FLAGS_COMMUTE)) {
        /* Commutative Operation */
        rc= mca_coll_sm2_allreduce_intra_fanin_fanout(sbuf, rbuf, count,
                dtype, op, comm, module);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }
    } else {
        /* Non-Commutative Operation */
        rc= mca_coll_sm2_allreduce_intra_fanin_fanout(sbuf, rbuf, count,
                dtype, op, comm, module);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }
    }


    return OMPI_SUCCESS;

Error:
    /* debug */
    fprintf(stderr," EEEError from allredcue : %d \n",rc);
    fflush(stderr);
    /* end debug */
    return rc;
}

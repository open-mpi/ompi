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
#include "ompi/communicator/communicator.h"


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
    int my_rank, child_rank, parent_rank, child, n_parents, n_children;
    int my_parent;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag;
    volatile char *sm_buffer;
    char *my_data_pointer, * volatile child_data_pointer;
    char * volatile parent_data_pointer, * volatile root_data_pointer;
    char *my_base_temp_pointer, * volatile child_base_temp_pointer;
    char * volatile parent_base_temp_pointer, * volatile root_base_temp_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t * volatile child_ctl_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t * volatile parent_ctl_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t * volatile root_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    tree_node_t *my_reduction_node;

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

    /* get my node for the reduction tree */
    my_rank=ompi_comm_rank(comm);
    my_reduction_node=&(sm_module->reduction_tree[my_rank]);
    n_children=my_reduction_node->n_children;
    n_parents=my_reduction_node->n_parents;
    my_parent=my_reduction_node->parent_rank;

    /* get a pointer to the shared-memory working buffer */
    /* NOTE: starting with a rather synchronous approach */
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
        sm_buffer=alloc_sm2_shared_buffer(sm_module);
        if( NULL == sm_buffer) {
            rc=OMPI_ERR_OUT_OF_RESOURCE;
            goto Error;
        }

        /* get base address to "my" memory segment */
        my_base_temp_pointer=(char *)
            ((char *)sm_buffer+sm_module->sm_buffer_mgmt_barrier_tree.my_rank*
             sm_module->segement_size_per_process);
        /* offset to data segment */
        my_data_pointer=my_base_temp_pointer+ctl_size;
        my_ctl_pointer=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            my_base_temp_pointer;

        /*
         * Fan into root phase
         */

        /* copy segment into shared buffer - later on will optimize to
         *   eliminate extra copies.
         */

        /*
         * Wait on children, and apply op to their data
         */
        for( child=0 ; child < n_children ; child++ ) {
            child_rank=my_reduction_node->children_ranks[child];

            /* get base address of child process */
            child_base_temp_pointer=(char *)
                ((char *)sm_buffer+child_rank*
                 sm_module->segement_size_per_process);
   
            child_data_pointer=child_base_temp_pointer+ctl_size;
            child_ctl_pointer=
                ( mca_coll_sm2_nb_request_process_shared_mem_t * volatile)
                child_base_temp_pointer;

            /* wait until child flag is set */
            while(! 
                    (child_ctl_pointer->flag == tag &
                     child_ctl_pointer->index== stripe_number) ) {
                /* Note: Actually need to make progress here */
                ;
            }

            /* apply collective operation */
            ompi_op_reduce(op,child_data_pointer,my_data_pointer,
                    count,dtype);
        }

        /* set memory barriet to make sure data is in main memory before
         *  the completion flgas are set.
         */
        MB();

        /*
         * Signal parent that data is ready
         */
        my_ctl_pointer->flag=tag;
        my_ctl_pointer->index=stripe_number;
    
    
        /*
         * Fan out from root phase - let the memory copies at each
         *   stage help reduce memory contention.
         */
        if( 0 < my_reduction_node->n_parents ) {
            /* I am the root - so copy  signal children, and then
             *   start reading
             */
            my_ctl_pointer->flag=-tag;

            /* copy data to user supplied buffer */

        } else {
            parent_data_pointer=(char *)
                ((char *)sm_buffer+my_parent*
                 sm_module->segement_size_per_process);

            parent_ctl_pointer=parent_data_pointer+ctl_size;
            child_ctl_pointer=
                ( mca_coll_sm2_nb_request_process_shared_mem_t * volatile)
                parent_data_pointer;
       
            /*
             * wait on Parent to signal that data is ready
             */
            while(! 
                    (parent_ctl_pointer->flag == -tag &
                     parent_ctl_pointer->index== stripe_number) ) {
                /* Note: Actually need to make progress here */
                ;
            }

            /* copy data to user supplied buffer */
            root_base_temp_pointer=(char *)sm_buffer;
            root_data_pointer=child_base_temp_pointer+ctl_size;

            /* signal children that they may read the result data */
            my_ctl_pointer->flag=-tag;

        }

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

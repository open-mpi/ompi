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
/* debug 
#include "opal/sys/timer.h"

extern uint64_t timers[7];
 end debug */



/**
 * Shared memory blocking allreduce.
 */
static
int mca_coll_sm2_reduce_intra_fanin(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op,
                                int root,
                                struct ompi_communicator_t *comm,
                                struct mca_coll_base_module_1_1_0_t *module)
{
    /* local variables */
    int rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    int my_rank, comm_size, child_rank, child, n_children;
    int count_processed,count_this_stripe;
    int process_shift,my_node_index;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag;
    volatile char * my_data_pointer;
    volatile char * child_data_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * child_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    tree_node_t *my_reduction_node;
    sm_work_buffer_t *sm_buffer_desc;

    sm_module=(mca_coll_sm2_module_t *) module;

    /* compute process shift */
    my_rank=ompi_comm_rank(comm);
    comm_size=ompi_comm_size(comm);
    process_shift=root;
    my_node_index=my_rank-root;
    /* wrap around */
    if(0 > my_node_index ) {
        my_node_index+=comm_size;
    }

    /* get size of data needed - same layout as user data, so that
     *   we can apply the reudction routines directly on these buffers
     */
    rc=ompi_ddt_type_extent(dtype, &dt_extent);
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
    my_reduction_node=&(sm_module->reduction_tree[my_node_index]);
    n_children=my_reduction_node->n_children;

    if( 1 == n_data_segments ) {
        /* single data segment */
        
            /* get unique tag for this stripe - assume only one collective
             *  per communicator at a given time, so no locking needed
             *  for atomic update of the tag */
            tag=sm_module->collective_tag;
            sm_module->collective_tag++;
    
            /* get a pointer to the shared-memory working buffer */
            sm_buffer_desc=alloc_sm2_shared_buffer(sm_module);
    
            /* get number of elements to process in this stripe */
            count_this_stripe=count;
    
            /* offset to data segment */
            my_ctl_pointer=sm_buffer_desc->proc_memory[my_rank].control_region;
            my_data_pointer=sm_buffer_desc->proc_memory[my_rank].data_segment;
    
            /***************************
             * Fan into root phase
             ***************************/
    
            if( ROOT_NODE == my_reduction_node->my_node_type ) {
                /* 
                 * copy local data from source buffer to result buffer
                 */
                rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                        (char *)rbuf, 
                        (char *)sbuf);
                if( 0 != rc ) {
                    return OMPI_ERROR;
                }
    
                /*
                 * Wait on children, and apply op to their data
                 */
                for( child=0 ; child < n_children ; child++ ) {
                    child_rank=my_reduction_node->children_ranks[child];
                    child_rank+=process_shift;
                    /* wrap around */
                    if( comm_size <= child_rank ){
                        child_rank-=comm_size;
                    }   
        
                    child_ctl_pointer=
                        sm_buffer_desc->proc_memory[child_rank].control_region;
                    child_data_pointer=
                        sm_buffer_desc->proc_memory[child_rank].data_segment;
        
                    /* wait until child flag is set */
                    while(child_ctl_pointer->flag != tag) {
                        opal_progress();
                    }
        
                    /* apply collective operation */
                    ompi_op_reduce(op,(void *)child_data_pointer,
                            (void *)rbuf, count_this_stripe,dtype);
    
                } /* end child loop */
        
            } else if( INTERIOR_NODE == my_reduction_node->my_node_type ) {

                /* copy segment into shared buffer - ompi_op_reduce
                 *   provids only 2 buffers, so can't add from two
                 *   into a third buffer.
                 */
                rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                        (char *)my_data_pointer, 
                        (char *)sbuf);
                if( 0 != rc ) {
                    return OMPI_ERROR;
                }
    
                /*
                 * Wait on children, and apply op to their data
                 */
                for( child=0 ; child < n_children ; child++ ) {
                    child_rank=my_reduction_node->children_ranks[child];
                    child_rank+=process_shift;
                    /* wrap around */
                    if( comm_size <= child_rank ){
                        child_rank-=comm_size;
                    }   
        
                    child_ctl_pointer=
                        sm_buffer_desc->proc_memory[child_rank].control_region;
                    child_data_pointer=
                        sm_buffer_desc->proc_memory[child_rank].data_segment;
        
                    /* wait until child flag is set */
                    while(child_ctl_pointer->flag != tag) {
                        opal_progress();
                    }
        
                    /* apply collective operation */
                    ompi_op_reduce(op,(void *)child_data_pointer,
                            (void *)my_data_pointer, count_this_stripe,dtype);
    
                } /* end child loop */
        
                /* set memory barriet to make sure data is in main memory before
                 *  the completion flgas are set.
                 */
                MB();
        
                /*
                 * Signal parent that data is ready
                 */
                my_ctl_pointer->flag=tag;
    
    
            } else {
                /* leaf node */
                /* copy segment into shared buffer - later on will optimize to
                 *   eliminate extra copies.
                 */
                rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                        (char *)my_data_pointer, 
                        (char *)sbuf);
                if( 0 != rc ) {
                    return OMPI_ERROR;
                }
        
                /* set memory barriet to make sure data is in main memory before
                 *  the completion flgas are set.
                 */
                MB();
        
                /*
                 * Signal parent that data is ready
                 */
                my_ctl_pointer->flag=tag;
            }
        
            /* "free" the shared-memory working buffer */
            rc=free_sm2_shared_buffer(sm_module);
            if( OMPI_SUCCESS != rc ) {
                goto Error;
            }
        
    } else {
        count_processed=0;
        for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
        
            /* get unique tag for this stripe - assume only one collective
             *  per communicator at a given time, so no locking needed
             *  for atomic update of the tag */
            tag=sm_module->collective_tag;
            sm_module->collective_tag++;
    
            /* get a pointer to the shared-memory working buffer */
            sm_buffer_desc=alloc_sm2_shared_buffer(sm_module);
    
            /* get number of elements to process in this stripe */
            count_this_stripe=n_dts_per_buffer;
            if( count_processed + count_this_stripe > count )
                count_this_stripe=count-count_processed;
    
            /* offset to data segment */
            my_ctl_pointer=sm_buffer_desc->proc_memory[my_rank].control_region;
            my_data_pointer=sm_buffer_desc->proc_memory[my_rank].data_segment;
    
            /***************************
             * Fan into root phase
             ***************************/
    
            if( LEAF_NODE != my_reduction_node->my_node_type ) {
                /* copy segment into shared buffer - ompi_op_reduce
                 *   provids only 2 buffers, so can't add from two
                 *   into a third buffer.
                 */
                rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                        (char *)my_data_pointer, 
                        (char *)((char *)sbuf+dt_extent*count_processed));
                if( 0 != rc ) {
                    return OMPI_ERROR;
                }
    
                /*
                 * Wait on children, and apply op to their data
                 */
                for( child=0 ; child < n_children ; child++ ) {
                    child_rank=my_reduction_node->children_ranks[child];
                    child_rank+=process_shift;
                    /* wrap around */
                    if( comm_size <= child_rank ){
                        child_rank-=comm_size;
                    }   
        
                    child_ctl_pointer=
                        sm_buffer_desc->proc_memory[child_rank].control_region;
                    child_data_pointer=
                        sm_buffer_desc->proc_memory[child_rank].data_segment;
        
                    /* wait until child flag is set */
                    while(child_ctl_pointer->flag != tag) {
                        opal_progress();
                    }
        
                    /* apply collective operation */
                    ompi_op_reduce(op,(void *)child_data_pointer,
                            (void *)my_data_pointer, count_this_stripe,dtype);
    
                } /* end child loop */
        
                /* set memory barriet to make sure data is in main memory before
                 *  the completion flgas are set.
                 */
                MB();
        
                /*
                 * Signal parent that data is ready
                 */
                my_ctl_pointer->flag=tag;
    
                /* copy data to destination */
                if( ROOT_NODE == my_reduction_node->my_node_type ) {
                    /* copy data to user supplied buffer */
                    rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                            (char *)rbuf+dt_extent*count_processed,
                            (char *)my_data_pointer);
                    if( 0 != rc ) {
                        return OMPI_ERROR;
                    }
                }
    
            } else {
                /* leaf node */
                /* copy segment into shared buffer - later on will optimize to
                 *   eliminate extra copies.
                 */
                rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                        (char *)my_data_pointer, 
                        (char *)((char *)sbuf+dt_extent*count_processed));
                if( 0 != rc ) {
                    return OMPI_ERROR;
                }
        
                /* set memory barriet to make sure data is in main memory before
                 *  the completion flgas are set.
                 */
                MB();
        
                /*
                 * Signal parent that data is ready
                 */
                my_ctl_pointer->flag=tag;
            }
        
            /* "free" the shared-memory working buffer */
            rc=free_sm2_shared_buffer(sm_module);
            if( OMPI_SUCCESS != rc ) {
                goto Error;
            }
        
            /* update the count of elements processed */
            count_processed+=count_this_stripe;
        }
    }

    /* return */
    return rc;

Error:
    return rc;
}

/**
 * Shared memory blocking reduce.
 */
int mca_coll_sm2_reduce_intra(void *sbuf, void *rbuf, int count,
        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
        int root, struct ompi_communicator_t *comm,
        struct mca_coll_base_module_1_1_0_t *module)
{
    /* local variables */
    int rc;

        rc= mca_coll_sm2_reduce_intra_fanin(sbuf, rbuf, count,
                dtype, op, root, comm, module);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }

    return OMPI_SUCCESS;

Error:
    return rc;
}

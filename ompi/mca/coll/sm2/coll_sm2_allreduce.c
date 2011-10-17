/*
 * Copyright (c) 2007-2008 UT-Battelle, LLC
 * Copyright (c) 2011      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
/* debug 
#include "opal/sys/timer.h"

extern uint64_t timers[7];
 end debug */



/**
 * Shared memory blocking allreduce.
 */
int mca_coll_sm2_allreduce_intra_fanin_fanout(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    /* local variables */
    int rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    int my_rank, child_rank, child, n_parents, n_children;
    int my_fanin_parent,count_processed,count_this_stripe;
    int my_fanout_parent;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag;
    volatile char * my_data_pointer;
    volatile char * child_data_pointer;
    volatile char * parent_data_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * child_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * parent_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    tree_node_t *my_reduction_node, *my_fanout_read_tree;
    sm_work_buffer_t *sm_buffer_desc;

    sm_module=(mca_coll_sm2_module_t *) module;


    /* get size of data needed - same layout as user data, so that
     *   we can apply the reudction routines directly on these buffers
     */
    rc=ompi_datatype_type_extent(dtype, &dt_extent);
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
    my_fanout_read_tree=&(sm_module->fanout_read_tree[my_rank]);
    n_children=my_reduction_node->n_children;
    n_parents=my_reduction_node->n_parents;
    my_fanin_parent=my_reduction_node->parent_rank;
    my_fanout_parent=my_fanout_read_tree->parent_rank;
    count_processed=0;

    /* get a pointer to the shared-memory working buffer */
    /* NOTE: starting with a rather synchronous approach */
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
    
        /* get unique tag for this stripe - assume only one collective
         *  per communicator at a given time, so no locking needed
         *  for atomic update of the tag */
        tag=sm_module->collective_tag;
        sm_module->collective_tag++;

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
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
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
    
                child_ctl_pointer=
                    sm_buffer_desc->proc_memory[child_rank].control_region;
                child_data_pointer=
                    sm_buffer_desc->proc_memory[child_rank].data_segment;
    
                /* wait until child flag is set */
                while(! 
                        ( (child_ctl_pointer->flag == tag) &
                         (child_ctl_pointer->index== stripe_number) ) ) {
                    opal_progress();
                }
    
                /* apply collective operation */
                ompi_op_reduce(op,(void *)child_data_pointer,
                        (void *)my_data_pointer, count_this_stripe,dtype);
                /* test
                {
                    int ii,n_ints;
                    int *my_int=(int *)my_data_pointer;
                    int *child_int=(int *)child_data_pointer;
                    n_ints=count_this_stripe/4;
                    for(ii=0 ; ii < n_ints ; ii++ ) {
                        my_int[ii]+=child_data_pointer[ii];
                    }
                }
                end test */

                /* end test */
            } /* end child loop */
    
            /* set memory barriet to make sure data is in main memory before
             *  the completion flgas are set.
             */
            MB();
    
            /*
             * Signal parent that data is ready
             */
            my_ctl_pointer->flag=tag;
            my_ctl_pointer->index=stripe_number;
        } else {
            /* leaf node */
            /* copy segment into shared buffer - later on will optimize to
             *   eliminate extra copies.
             */
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
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
            my_ctl_pointer->index=stripe_number;
        }
    
        /***************************
         * Fan out from root
         ***************************/
        /*
         * Fan out from root - let the memory copies at each
         *   stage help reduce memory contention.
         */
        if( ROOT_NODE == my_fanout_read_tree->my_node_type ) {
            /* I am the root - so copy  signal children, and then
             *   start reading
             */
            MB();
            my_ctl_pointer->flag=-tag;

            /* copy data to user supplied buffer */
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)((char *)rbuf+dt_extent*count_processed),
                    (char *)my_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
    
        } else if( LEAF_NODE == my_fanout_read_tree->my_node_type ) {
    
            parent_data_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].data_segment;
            parent_ctl_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].control_region;

            /*
             * wait on Parent to signal that data is ready
             */
            while(! 
                    ( (parent_ctl_pointer->flag == -tag) &
                     (parent_ctl_pointer->index== stripe_number) ) ) {
                /* Note: Actually need to make progress here */
                opal_progress();
            }

            /* copy data to user supplied buffer */
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)rbuf+dt_extent*count_processed,
                    (char *)parent_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
    
        } else {
            /* interior nodes */
   
            parent_data_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].data_segment;
            parent_ctl_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].control_region;

            /*
             * wait on Parent to signal that data is ready
             */
            while(! 
                    ( (parent_ctl_pointer->flag == -tag) &
                     (parent_ctl_pointer->index== stripe_number) ) ) {
                /* Note: Actually need to make progress here */
                opal_progress();
            }

            /* copy the data to my shared buffer, for access by children */
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)my_data_pointer,(char *)parent_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
        
            /* set memory barriet to make sure data is in main memory before
             *  the completion flgas are set.
             */
            MB();

            /* signal children that they may read the result data */
            my_ctl_pointer->flag=-tag;

            /* copy data to user supplied buffer */
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)rbuf+dt_extent*count_processed,
                    (char *)my_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
        }

        /* "free" the shared-memory working buffer */
        rc=free_sm2_shared_buffer(sm_module);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }
    
        /* update the count of elements processed */
        count_processed+=count_this_stripe;
    }

    /* return */
    return rc;

Error:
    return rc;
}

/*
 *  fanin/fanout progress function.
 */

static
int progress_fanin_fanout( void *sbuf, void *rbuf,
        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
        mca_coll_sm2_module_allreduce_pipeline_t *reduction_desc,
        int n_poll_loops, int *completed)
{
    /* local variables */
        
    int my_rank,cnt;
    int rc=OMPI_SUCCESS;
    int my_fanout_parent;
    int child_rank,n_children,child;
    int count_processed,count_this_stripe;
    ptrdiff_t dt_extent;
    long long tag;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * parent_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * child_ctl_pointer;
    volatile char * my_data_pointer;
    volatile char * parent_data_pointer;
    volatile char * child_data_pointer;
    sm_work_buffer_t *sm_buffer_desc;
    tree_node_t *my_reduction_node;
    tree_node_t *my_fanout_read_tree;

    tag=reduction_desc->tag;
    sm_buffer_desc=reduction_desc->shared_buffer;
    my_rank=reduction_desc->my_rank;
    my_reduction_node=reduction_desc->my_reduction_node;
    my_fanout_read_tree=reduction_desc->my_fanout_read_tree;
    /* initialize flag indicating that segment is still active in the
     *   reduction
     */
    *completed=0;

    my_ctl_pointer=sm_buffer_desc->proc_memory[my_rank].control_region;
    my_data_pointer=sm_buffer_desc->proc_memory[my_rank].data_segment;

    /* figure out where to proceed */
    if( FANOUT == reduction_desc->status) {
        goto REDUCTION_FANOUT;
    }
    /*
     * fan in
     */
    switch (my_reduction_node->my_node_type) {
        case LEAF_NODE:
            /* leaf node */
            /* copy segment into shared buffer - later on will optimize to
             *   eliminate extra copies.
             */
            count_processed=reduction_desc->count_processed;
            count_this_stripe=reduction_desc->count_this_stripe;
            /* error conditions already checed */
            ompi_datatype_type_extent(dtype, &dt_extent);
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
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

            break;

        default:
            /* ROOT_NODE and INTERIOR_NODE */
            /* copy segment into shared buffer - ompi_op_reduce
             *   provids only 2 buffers, so can't add from two
             *   into a third buffer.
             */
            count_this_stripe=reduction_desc->count_this_stripe;
            if( STARTED == reduction_desc->status) {
                /* copy-in only the first time through */
                count_processed=reduction_desc->count_processed;
                /* error conditions already checed */
                ompi_datatype_type_extent(dtype, &dt_extent);
                rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                        (char *)my_data_pointer, 
                        (char *)((char *)sbuf+dt_extent*count_processed));
                if( 0 != rc ) {
                    return OMPI_ERROR;
                }
            }

            /*
             * Wait on children, and apply op to their data
             */
            n_children=my_reduction_node->n_children;
            for( child=reduction_desc->n_child_loops_completed ; 
                    child < n_children ; child++ ) {
                child_rank=my_reduction_node->children_ranks[child];
    
                child_ctl_pointer=
                    sm_buffer_desc->proc_memory[child_rank].control_region;
                child_data_pointer=
                    sm_buffer_desc->proc_memory[child_rank].data_segment;
    
                /* wait until child flag is set */
                cnt=0;
                while( child_ctl_pointer->flag != tag ) {
                    opal_progress();
                    cnt++;
                    if( n_poll_loops == cnt ) {
                        /* break out */
                        reduction_desc->status=FANIN;
                        reduction_desc->n_child_loops_completed=child;
                        goto RETURN;
                    }
                }
    
                /* apply collective operation */
                count_this_stripe=reduction_desc->count_this_stripe;
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

    }

REDUCTION_FANOUT:
    my_fanout_parent=my_fanout_read_tree->parent_rank;
    switch (my_reduction_node->my_node_type) {
        case LEAF_NODE:

            parent_data_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].data_segment;
            parent_ctl_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].control_region;

            /*
             * wait on Parent to signal that data is ready
             */
            cnt=0;
            while(parent_ctl_pointer->flag != -tag) {
                opal_progress();
                cnt++;
                if( n_poll_loops == cnt ) {
                    /* break out */
                    reduction_desc->status=FANOUT;
                    goto RETURN;
                }
            }

            /* copy data to user supplied buffer */
            count_processed=reduction_desc->count_processed;
            count_this_stripe=reduction_desc->count_this_stripe;
            /* error conditions already checed */
            ompi_datatype_type_extent(dtype, &dt_extent);
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)rbuf+dt_extent*count_processed,
                    (char *)parent_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }

            break;

        case INTERIOR_NODE:

            /* interior nodes */
   
            parent_data_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].data_segment;
            parent_ctl_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].control_region;

            /*
             * wait on Parent to signal that data is ready
             */
            cnt=0;
            while(parent_ctl_pointer->flag != -tag)  {
                opal_progress();
                cnt++;
                if( n_poll_loops == cnt ) {
                    /* break out */
                    reduction_desc->status=FANOUT;
                    goto RETURN;
                }
            }

            /* copy the data to my shared buffer, for access by children */
            count_this_stripe=reduction_desc->count_this_stripe;
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)my_data_pointer,(char *)parent_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
        
            /* set memory barriet to make sure data is in main memory before
             *  the completion flgas are set.
             */
            MB();

            /* signal children that they may read the result data */
            my_ctl_pointer->flag=-tag;

            /* copy data to user supplied buffer */
            count_processed=reduction_desc->count_processed;
            count_this_stripe=reduction_desc->count_this_stripe;
            /* error conditions already checed */
            ompi_datatype_type_extent(dtype, &dt_extent);
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)rbuf+dt_extent*count_processed,
                    (char *)my_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }

            break;

        case ROOT_NODE:

            /* I am the root - so copy  signal children, and then
             *   start reading
             */
            MB();
            my_ctl_pointer->flag=-tag;

            /* copy data to user supplied buffer */
            count_processed=reduction_desc->count_processed;
            count_this_stripe=reduction_desc->count_this_stripe;
            /* error conditions already checed */
            ompi_datatype_type_extent(dtype, &dt_extent);
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)((char *)rbuf+dt_extent*count_processed),
                    (char *)my_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
    }

    /* completed processing the data in this stripe */
    *completed=1;
    
    /* mark the descriptor as available */
    reduction_desc->status=BUFFER_AVAILABLE;

    /* return */
RETURN:
    return OMPI_SUCCESS;

}


/**
 * Shared memory blocking allreduce - pipeline algorithm.
 */
#define depth_pipeline 2

static
int mca_coll_sm2_allreduce_intra_fanin_fanout_pipeline
    (void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype, 
     struct ompi_op_t *op, struct ompi_communicator_t *comm,
     mca_coll_base_module_t *module)
{

    /* local variables */
    int i,buffer_index,stripe_number,my_rank,n_completed,completed;
    int count_processed,count_this_stripe;
    mca_coll_sm2_module_allreduce_pipeline_t working_buffers[depth_pipeline];
    int rc=OMPI_SUCCESS;
    long long tag;
    tree_node_t *my_reduction_node, *my_fanout_read_tree;
    mca_coll_sm2_module_t *sm_module;
    int n_dts_per_buffer,n_data_segments;
    size_t len_data_buffer;
    ptrdiff_t dt_extent;

    sm_module=(mca_coll_sm2_module_t *) module;

    /* get size of data needed - same layout as user data, so that
     *   we can apply the reudction routines directly on these buffers
     */
    rc=ompi_datatype_type_extent(dtype, &dt_extent);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

    /* lenght of control and data regions */
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
    my_fanout_read_tree=&(sm_module->fanout_read_tree[my_rank]);
    count_processed=0;

    /* get the working data segments */
    /* NOTE: need to check at communicator creation that we have enough
     *   temporary buffes for this
     */

    for(i=0 ; i < depth_pipeline ; i++ ) {
        /*
        working_buffers[i].shared_buffer=alloc_sm2_shared_buffer(sm_module);
        */
        working_buffers[i].status=BUFFER_AVAILABLE;
        working_buffers[i].my_rank=my_rank;
        working_buffers[i].my_reduction_node=my_reduction_node;
        working_buffers[i].my_fanout_read_tree=my_fanout_read_tree;
    }

    n_completed=0;
    buffer_index=-1;
    /* loop over data segments */
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {

        /* 
         * allocate working buffer 
         */

        /* get working_buffers index - this needs to be deterministic,
         *   as each process is getting this pointer on it's own, so all
         *   need to point to the same data structure
         */
        buffer_index++;
        /* wrap around */
        if( buffer_index == depth_pipeline){
            buffer_index=0;
        }

        /* wait for buffer to become available */
        while ( working_buffers[buffer_index].status != BUFFER_AVAILABLE ) {
            /* loop over working buffers, and progress the reduction */
            for( i=0 ; i < depth_pipeline ; i++ ) {
                if( working_buffers[i].status != BUFFER_AVAILABLE ){
                    rc=progress_fanin_fanout( sbuf, rbuf, dtype, op,
                            &(working_buffers[i]),
                            sm_module->n_poll_loops, &completed);
                    if( OMPI_SUCCESS != rc ) {
                        goto Error;
                    }
                    /* update the number of completed segments */
                    if( completed ) {
                        n_completed+=completed;
                        /* release of resources may be our of order, but allocation
                         * is ordered, and only after the pipleline tracker
                         * (working_buffers[]) indicates that it is complete, so
                         * resources will not be re-used too early
                         */
                        rc=free_sm2_shared_buffer(sm_module);
                    }
                }
            }

            /* overall ompi progress */
            opal_progress();
        }

        /* initialize working buffer for this stripe */
        working_buffers[buffer_index].shared_buffer=
            alloc_sm2_shared_buffer(sm_module);
        working_buffers[buffer_index].status=STARTED;
        working_buffers[buffer_index].n_child_loops_completed=0;
        count_processed=stripe_number*n_dts_per_buffer;
        count_this_stripe=n_dts_per_buffer;
        if( count_processed + count_this_stripe > count )
            count_this_stripe=count-count_processed;
        working_buffers[buffer_index].count_this_stripe=count_this_stripe;
        working_buffers[buffer_index].count_processed=count_processed;
        tag=sm_module->collective_tag;
        sm_module->collective_tag++;
        working_buffers[buffer_index].tag=tag;

        /* progress this stripe */
        rc=progress_fanin_fanout( sbuf, rbuf, dtype, op,
                &(working_buffers[buffer_index]),
                sm_module->n_poll_loops, &completed);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }
        /* update the number of completed segments */
        if( completed ) {
            n_completed+=completed;
            /* release of resources may be our of order, but allocation
             * is ordered, and only after the pipleline tracker
             * (working_buffers[]) indicates that it is complete, so
             * resources will not be re-used too early
             */
            rc=free_sm2_shared_buffer(sm_module);
        }

    }

    /* progress remaining data stripes */
    while( n_completed <  n_data_segments ) {
        for( i=0 ; i < depth_pipeline ; i++ ) {
            if( working_buffers[i].status != BUFFER_AVAILABLE ){
                rc=progress_fanin_fanout( sbuf, rbuf, dtype, op,
                        &(working_buffers[i]),
                        sm_module->n_poll_loops, &completed);
                if( OMPI_SUCCESS != rc ) {
                    goto Error;
                }
                /* update the number of completed segments */
                if( completed ) {
                    n_completed+=completed;
                    /* release of resources may be our of order, but allocation
                     * is ordered, and only after the pipleline tracker
                     * (working_buffers[]) indicates that it is complete, so
                     * resources will not be re-used too early
                     */
                    rc=free_sm2_shared_buffer(sm_module);
                }
            }
        }
    }

    /* free work buffers */
    /*
    for(i=0 ; i < depth_pipeline ; i++ ) {
        rc=free_sm2_shared_buffer(sm_module);
    }
    */

    /* return */
    return rc;

Error:
    /* free work buffers */
    /*
    for(i=0 ; i < depth_pipeline ; i++ ) {
        rc=free_sm2_shared_buffer(sm_module);
    }
    */
    return rc;
}
#undef depth_pipeline


/**
 * Shared memory blocking allreduce.
 */
static
int mca_coll_sm2_allreduce_intra_recursive_doubling(void *sbuf, void *rbuf, 
        int count, struct ompi_datatype_t *dtype, 
        struct ompi_op_t *op, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{
    /* local variables */
    int rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    int pair_rank,exchange,extra_rank;
    int index_read,index_write;
    pair_exchange_node_t *my_exchange_node;
    int my_rank,count_processed,count_this_stripe;
    size_t message_extent,ctl_size,len_data_buffer;
    ptrdiff_t dt_extent;
    long long tag, base_tag;
    sm_work_buffer_t *sm_buffer_desc;
    volatile char * my_tmp_data_buffer[2];
    volatile char * my_write_pointer;
    volatile char * my_extra_write_pointer;
    volatile char * my_read_pointer;
    volatile char * extra_rank_write_data_pointer;
    volatile char * extra_rank_read_data_pointer;
    volatile char * partner_read_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * 
        partner_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * 
        extra_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    /* debug 
    opal_timer_t t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
     end debug */

    sm_module=(mca_coll_sm2_module_t *) module;

    /* get size of data needed - same layout as user data, so that
     *   we can apply the reudction routines directly on these buffers
     */
    rc=ompi_datatype_type_extent(dtype, &dt_extent);
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

    /* need a read and a write buffer for a pair-wise exchange of data */
    n_dts_per_buffer/=2;
    len_data_buffer=n_dts_per_buffer*dt_extent;

    /* compute number of stripes needed to process this collective */
    n_data_segments=(count+n_dts_per_buffer -1 ) / n_dts_per_buffer ;

    /* get my node for the reduction tree */
    my_exchange_node=&(sm_module->recursive_doubling_tree);
    my_rank=ompi_comm_rank(comm);

    count_processed=0;

    /* debug 
    t0=opal_timer_base_get_cycles();
     end debug */
    sm_buffer_desc=alloc_sm2_shared_buffer(sm_module);
    /* debug 
    t1=opal_timer_base_get_cycles();
            timers[0]+=(t1-t0);
             end debug */

    /* get a pointer to the shared-memory working buffer */
    /* NOTE: starting with a rather synchronous approach */
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
        /* get number of elements to process in this stripe */
        /* debug 
        t2=opal_timer_base_get_cycles();
         end debug */
        count_this_stripe=n_dts_per_buffer;
        if( count_processed + count_this_stripe > count )
            count_this_stripe=count-count_processed;

        /* get unique set of tags for this stripe.
         *  Assume only one collective
         *  per communicator at a given time, so no locking needed
         *  for atomic update of the tag */
        base_tag=sm_module->collective_tag;
        sm_module->collective_tag+=my_exchange_node->n_tags;

        /* get pointers to my work buffers */
        my_ctl_pointer=sm_buffer_desc->proc_memory[my_rank].control_region;
            my_write_pointer=sm_buffer_desc->proc_memory[my_rank].data_segment;
            my_read_pointer=my_write_pointer+len_data_buffer;
            my_tmp_data_buffer[0]=my_write_pointer;
            my_tmp_data_buffer[1]=my_read_pointer;

            /* copy data into the write buffer */
            rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)my_write_pointer,
                    (char *)((char *)sbuf+dt_extent*count_processed));
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
            /* debug 
            t3=opal_timer_base_get_cycles();
            timers[1]+=(t3-t2);
             end debug */
            
            /* copy data in from the "extra" source, if need be */
            tag=base_tag;
            if(0 < my_exchange_node->n_extra_sources)  {
                int n_my_count;

                if ( EXCHANGE_NODE == my_exchange_node->node_type ) {

                    /* signal to partner that I am ready */
                    MB();
                    /*
                     * Signal extra node that data is ready
                     */
                    my_ctl_pointer->flag=tag;

                    /* figure out my portion of the reduction */
                    n_my_count=count_this_stripe/2;

                    extra_rank=my_exchange_node->rank_extra_source;
                    extra_ctl_pointer=
                        sm_buffer_desc->proc_memory[extra_rank].control_region;
                    extra_rank_write_data_pointer=
                        sm_buffer_desc->proc_memory[extra_rank].data_segment;
                    
                    /* wait until remote data is read */
                    while( extra_ctl_pointer->flag < tag ) {
                        opal_progress();
                    }

                    /* apply collective operation to first half of the data */
                    if( 0 < n_my_count ) {
                        ompi_op_reduce(op,(void *)extra_rank_write_data_pointer,
                                (void *)my_write_pointer, n_my_count,dtype);
                    }


                    /* wait for my partner to finish reducing the data */
                    tag=base_tag+1;
                    while( extra_ctl_pointer->flag < tag ) {
                        opal_progress();
                    }


                    /* read my partner's data */
   
                    /* adjust read an write pointers */
                    extra_rank_write_data_pointer+=(n_my_count*dt_extent);

                    if( 0 < (count_this_stripe-n_my_count) ) {
                        rc=ompi_datatype_copy_content_same_ddt(dtype, 
                                count_this_stripe-n_my_count,
                                (char *)(my_write_pointer+n_my_count*dt_extent),
                                (char *)extra_rank_write_data_pointer);
                        if( 0 != rc ) {
                            return OMPI_ERROR;
                        }
                    }
                
                    /* now we are ready for the power of 2 portion of the
                     *   algorithm
                     */

            } else {
        
                /* set memory barriet to make sure data is in main memory before
                 *  the completion flgas are set.
                 */

                MB();
    
                /*
                 * Signal extra node that data is ready
                 */
                my_ctl_pointer->flag=tag;

                /* figure out my portion of the reduction */
                n_my_count=count_this_stripe-(count_this_stripe/2);

                /* get the pointer to the partners data that needs to be reduced */
                extra_rank=my_exchange_node->rank_extra_source;
                extra_ctl_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].control_region;
                extra_rank_write_data_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].data_segment;
                /* offset into my half of the data */
                extra_rank_write_data_pointer+= 
                        ((count_this_stripe/2)*dt_extent);
                my_extra_write_pointer=my_write_pointer+
                    ((count_this_stripe/2)*dt_extent);
                    
                /* wait until remote data is read */
                while( extra_ctl_pointer->flag < tag ) {
                    opal_progress();
                }

                /* apply collective operation to second half of the data  */
                if( 0 < n_my_count ) {
                    ompi_op_reduce(op,(void *)extra_rank_write_data_pointer,
                            (void *)my_extra_write_pointer, n_my_count,dtype);
                }

                /* signal that I am done, so my partner can read my data */
                MB();
                tag=base_tag+1;
                my_ctl_pointer->flag=tag;

            }
        }

        MB();
        /*
         * Signal parent that data is ready
         */
        tag=base_tag+1;
        my_ctl_pointer->flag=tag;

        /* loop over data exchanges */
        for(exchange=0 ; exchange < my_exchange_node->n_exchanges ; exchange++) {

            /* debug 
            t4=opal_timer_base_get_cycles();
             end debug */

            index_read=(exchange&1);
            index_write=((exchange+1)&1);

            my_write_pointer=my_tmp_data_buffer[index_write];
            my_read_pointer=my_tmp_data_buffer[index_read];

            /* is the remote data read */
            pair_rank=my_exchange_node->rank_exchanges[exchange];
            partner_ctl_pointer=
                sm_buffer_desc->proc_memory[pair_rank].control_region;
            partner_read_pointer=
                sm_buffer_desc->proc_memory[pair_rank].data_segment;
            if( 1 == index_read ) {
                partner_read_pointer+=len_data_buffer;
            }
                
            /* wait until remote data is read */
            while(  partner_ctl_pointer->flag < tag  ) {
                opal_progress();
            }
            /* debug 
            t5=opal_timer_base_get_cycles();
        timers[2]+=(t5-t4);
             end debug */

            /* reduce data into my write buffer */
            /* apply collective operation */
           
        ompi_3buff_op_reduce(op,my_read_pointer,partner_read_pointer,
                my_write_pointer,count_this_stripe,dtype);

            /* debug 
            t6=opal_timer_base_get_cycles();
        timers[3]+=(t6-t5);
             end debug */

            /* end test */
            
            /* signal that I am done reading my peer's data */
            tag++;
            MB();
            my_ctl_pointer->flag=tag;

            /* wait for my peer to finish - other wise buffers may be
             *   reused too early */
            while(  partner_ctl_pointer->flag < tag  ) {
                opal_progress();
            }
            /* debug 
            t7=opal_timer_base_get_cycles();
        timers[4]+=(t7-t6);
             end debug */

        }

        /* copy data in from the "extra" source, if need be */
        if(0 < my_exchange_node->n_extra_sources)  {
            tag=base_tag+my_exchange_node->n_tags-1;

            if ( EXTRA_NODE == my_exchange_node->node_type ) {

                extra_rank=my_exchange_node->rank_extra_source;
                extra_ctl_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].control_region;
                extra_rank_read_data_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].data_segment;
                index_read=(my_exchange_node->log_2&1);
                if( index_read ) {
                    extra_rank_read_data_pointer+=len_data_buffer;
                }
                    
                /* wait until remote data is read */
                while(! ( extra_ctl_pointer->flag == tag ) ) {
                    opal_progress();
                }
    
            
                /* write the data into my read buffer */
                rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                        (char *)my_write_pointer,
                        (char *)extra_rank_read_data_pointer);
                if( 0 != rc ) {
                    return OMPI_ERROR;
                }

                /* signal that I am done */
                my_ctl_pointer->flag=tag;

            } else {
        
                tag=base_tag+my_exchange_node->n_tags-1;
                /* set memory barriet to make sure data is in main memory before
                 *  the completion flgas are set.
                 */

                MB();
    
                /*
                 * Signal parent that data is ready
                 */
                my_ctl_pointer->flag=tag;

                /* wait until child is done to move on - this buffer will
                 *   be reused for the next stripe, so don't want to move
                 *   on too quick.
                 */
                extra_rank=my_exchange_node->rank_extra_source;
                extra_ctl_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].control_region;

                /* wait until remote data is read */
                while( extra_ctl_pointer->flag < tag  ) {
                    opal_progress();
                }
            }
        }

            /* debug 
            t8=opal_timer_base_get_cycles();
             end debug */
        /* copy data into the destination buffer */
        rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                (char *)((char *)rbuf+dt_extent*count_processed),
                (char *)my_write_pointer);
        if( 0 != rc ) {
            return OMPI_ERROR;
        }
    
        /* update the count of elements processed */
        count_processed+=count_this_stripe;
    }


    /* debug 

    t9=opal_timer_base_get_cycles();
    timers[5]+=(t9-t8);
     end debug */


    /* "free" the shared-memory working buffer */
    rc=free_sm2_shared_buffer(sm_module);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

    /* debug 
    t10=opal_timer_base_get_cycles();
    timers[6]+=(t10-t9);
     end debug */

    /* return */
    return rc;

Error:
    return rc;
}

/**
 * Shared memory blocking allreduce.
 */
int mca_coll_sm2_allreduce_intra_reducescatter_allgather(void *sbuf, void *rbuf, 
        int count, struct ompi_datatype_t *dtype, 
        struct ompi_op_t *op, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{
    /* local varibles */
    int i,rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    int pair_rank,exchange,extra_rank,n_proc_data,tmp;
    int starting_proc;
    int n_elements_per_proc, n_residual_elements;
    int cnt_offset,n_copy;
    pair_exchange_node_t *my_exchange_node;
    int my_rank,comm_size,count_processed,count_this_stripe;
    int count_this_exchange;
    int done_copy_tag,ok_to_copy_tag;
    size_t len_data_buffer;
    ptrdiff_t dt_extent;
    long long tag, base_tag;
    sm_work_buffer_t *sm_buffer_desc;
    volatile char * extra_rank_write_data_pointer;
    volatile char * extra_rank_read_data_pointer;
    volatile char * partner_base_pointer;
    volatile char * my_pointer;
    volatile char * my_base_pointer;
    volatile char * my_extra_write_pointer;
    volatile char * partner_pointer;
    volatile char * source_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * 
        partner_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * 
        extra_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * 
        source_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;

    sm_module=(mca_coll_sm2_module_t *) module;

    /* get size of data needed - same layout as user data, so that
     *   we can apply the reudction routines directly on these buffers
     */
    rc=ompi_datatype_type_extent(dtype, &dt_extent);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

    /* lenght of control and data regions */
    len_data_buffer=sm_module->data_memory_per_proc_per_segment;

    /* number of data types copies that the scratch buffer can hold */
    n_dts_per_buffer=((int) len_data_buffer)/dt_extent;
    if ( 0 == n_dts_per_buffer ) {
        rc=OMPI_ERROR;
        goto Error;
    }

    len_data_buffer=n_dts_per_buffer*dt_extent;

    /* compute number of stripes needed to process this collective */
    n_data_segments=(count+n_dts_per_buffer -1 ) / n_dts_per_buffer ;

    /* get my node for the reduction tree */
    my_exchange_node=&(sm_module->recursive_doubling_tree);
    my_rank=ompi_comm_rank(comm);
    comm_size=ompi_comm_size(comm);

    /* get access to shared memory working buffer */
    sm_buffer_desc=alloc_sm2_shared_buffer(sm_module);
    my_ctl_pointer=sm_buffer_desc->proc_memory[my_rank].control_region;
    my_base_pointer=sm_buffer_desc->proc_memory[my_rank].data_segment;

    count_processed=0;
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
        /* get number of elements to process in this stripe */
        /* debug 
        t2=opal_timer_base_get_cycles();
         end debug */
        count_this_stripe=n_dts_per_buffer;
        if( count_processed + count_this_stripe > count )
            count_this_stripe=count-count_processed;

        /* compute the number of elements "owned" by each process */
        n_elements_per_proc=(count_this_stripe/my_exchange_node->n_largest_pow_2);
        n_residual_elements=count_this_stripe-
            n_elements_per_proc*my_exchange_node->n_largest_pow_2;
        for(i=0 ; i < my_exchange_node->n_largest_pow_2 ; i++ ) {
            sm_module->scratch_space[i]=n_elements_per_proc;
            if( i < n_residual_elements) {
                sm_module->scratch_space[i]++;
            }
        }

        /* get unique set of tags for this stripe.
         *  Assume only one collective
         *  per communicator at a given time, so no locking needed
         *  for atomic update of the tag */
        base_tag=sm_module->collective_tag;
        /* log_2 tags for recursive doubling, one for the non-power of 2
         *   initial send, 1 for first copy into shared memory, and
         *   one for completing the copyout.
         */
        sm_module->collective_tag+=(my_exchange_node->log_2+3);


        /* copy data into the write buffer */
        rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                (char *)my_base_pointer,
                (char *)((char *)sbuf+dt_extent*count_processed));
        if( 0 != rc ) {
            return OMPI_ERROR;
        }

            /* debug 
            t3=opal_timer_base_get_cycles();
            timers[1]+=(t3-t2);
             end debug */
            
        /* copy data in from the "extra" source, if need be */
        tag=base_tag;
            if(0 < my_exchange_node->n_extra_sources)  {
                int n_my_count;

                if ( EXCHANGE_NODE == my_exchange_node->node_type ) {

                    /* signal to partner that I am ready */
                    MB();
                    /*
                     * Signal extra node that data is ready
                     */
                    my_ctl_pointer->flag=tag;

                    /* figure out my portion of the reduction */
                    n_my_count=count_this_stripe/2;

                    extra_rank=my_exchange_node->rank_extra_source;
                    extra_ctl_pointer=
                        sm_buffer_desc->proc_memory[extra_rank].control_region;
                    extra_rank_write_data_pointer=
                        sm_buffer_desc->proc_memory[extra_rank].data_segment;
                    
                    /* wait until remote data is read */
                    while( extra_ctl_pointer->flag < tag ) {
                        opal_progress();
                    }

                    /* apply collective operation to first half of the data */
                    if( 0 < n_my_count ) {
                        ompi_op_reduce(op,(void *)extra_rank_write_data_pointer,
                                (void *)my_base_pointer, n_my_count,dtype);
                    }


                    /* wait for my partner to finish reducing the data */
                    tag=base_tag+1;
                    while( extra_ctl_pointer->flag < tag ) {
                        opal_progress();
                    }


                    /* read my partner's data */
   
                    /* adjust read an write pointers */
                    extra_rank_write_data_pointer+=(n_my_count*dt_extent);

                    if( 0 < (count_this_stripe-n_my_count) ) {
                        rc=ompi_datatype_copy_content_same_ddt(dtype, 
                                count_this_stripe-n_my_count,
                                (char *)(my_base_pointer+n_my_count*dt_extent),
                                (char *)extra_rank_write_data_pointer);
                        if( 0 != rc ) {
                            return OMPI_ERROR;
                        }
                    }
                
                    /* now we are ready for the power of 2 portion of the
                     *   algorithm
                     */

            } else {
        
                /* set memory barriet to make sure data is in main memory before
                 *  the completion flgas are set.
                 */

                MB();
    
                /*
                 * Signal extra node that data is ready
                 */
                my_ctl_pointer->flag=tag;

                /* figure out my portion of the reduction */
                n_my_count=count_this_stripe-(count_this_stripe/2);

                /* get the pointer to the partners data that needs to be reduced */
                extra_rank=my_exchange_node->rank_extra_source;
                extra_ctl_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].control_region;
                extra_rank_write_data_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].data_segment;
                /* offset into my half of the data */
                extra_rank_write_data_pointer+= 
                        ((count_this_stripe/2)*dt_extent);
                my_extra_write_pointer=my_base_pointer+
                    ((count_this_stripe/2)*dt_extent);
                    
                /* wait until remote data is read */
                while( extra_ctl_pointer->flag < tag ) {
                    opal_progress();
                }

                /* apply collective operation to second half of the data  */
                if( 0 < n_my_count ) {
                    ompi_op_reduce(op,(void *)extra_rank_write_data_pointer,
                            (void *)my_extra_write_pointer, n_my_count,dtype);
                }

                /* signal that I am done, so my partner can read my data */
                MB();
                tag=base_tag+1;
                my_ctl_pointer->flag=tag;

            }
        }
        MB();

        /* 
         * reduce-scatter
         */
        /*
         * Signal parent that data is ready
         */
        tag=base_tag+1;
        my_ctl_pointer->flag=tag;

        /* 
         * loop over data exchanges 
         */
        /* set the number of procs whos's data I will manipulate - this starts
         * at the number of procs in the exchange, so a divide by two at each
         * iteration will give the right number of proc for the given iteration 
         */
        n_proc_data=my_exchange_node->n_largest_pow_2;
        starting_proc=0;
        for(exchange=my_exchange_node->n_exchanges-1;exchange>=0;exchange--) {
                        
            /* is the remote data read */
            pair_rank=my_exchange_node->rank_exchanges[exchange];

            partner_ctl_pointer=
                sm_buffer_desc->proc_memory[pair_rank].control_region;
            partner_base_pointer=
                sm_buffer_desc->proc_memory[pair_rank].data_segment;

            /* wait until remote data is read */
            while(  partner_ctl_pointer->flag < tag  ) {
                opal_progress();
            }

            /* figure out the base address to use : the lower rank gets
             *   the upper data, with the higher rank getting the lower half
             *   of the current chunk */
            n_proc_data=n_proc_data/2;
            if(pair_rank <  my_rank ) {
                starting_proc+=n_proc_data;
            }

            /* figure out my staring pointer */
            tmp=0;
            for(i=0 ; i < starting_proc ; i++ ) {
                tmp+=sm_module->scratch_space[i];
            }
            my_pointer=my_base_pointer+tmp*dt_extent;
            /* figure out partner's staring pointer */
            partner_pointer=partner_base_pointer+tmp*dt_extent;

            /* figure out how much to read */
            tmp=0;
            for(i=starting_proc ; i < starting_proc+n_proc_data ; i++ ) {
                tmp+=sm_module->scratch_space[i];
            }
            count_this_exchange=tmp;

            /* reduce data into my write buffer */
            /* apply collective operation */
            ompi_op_reduce(op,(void *)partner_pointer,
                    (void *)my_pointer, count_this_exchange,dtype);
        /* debug 
        { int *int_tmp=(int *)my_pointer;
            int i;
            fprintf(stderr,"  result my rank %d data in tmp :: ",my_rank);
            for (i=0 ; i < count_this_exchange ; i++ ) {
                fprintf(stderr," %d ",int_tmp[i]);
            }
            fprintf(stderr,"\n");
            int_tmp=(int *)partner_pointer;
            fprintf(stderr,"  partner data my rank %d data in tmp :: ",my_rank);
            for (i=0 ; i < count_this_exchange ; i++ ) {
                fprintf(stderr," %d ",int_tmp[i]);
            }
            fprintf(stderr,"\n");
            fflush(stderr);
        }
         end debug */

            /* signal that I am done reading my peer's data */
            tag++;
            MB();
            my_ctl_pointer->flag=tag;


        } /* end exchange loop */

           /* debug 
            t8=opal_timer_base_get_cycles();
             end debug */
        /* copy data out to final destination.  Could do some sort of
         * recursive doubleing in the sm, then copy to process private,
         * which reduces memory contention.  However, this also almost
         * doubles the number of copies.
         */
        ok_to_copy_tag=base_tag+1+my_exchange_node->log_2;

        /* read from the result buffers directly to the final destinaion */
        cnt_offset=0;
        for(n_copy=0 ; n_copy < my_exchange_node->n_largest_pow_2 ; n_copy++ ) {
        
            if( 0 >= sm_module->scratch_space[n_copy] )
                continue;

            source_ctl_pointer=
                sm_buffer_desc->proc_memory[n_copy].control_region;
            source_pointer=
                sm_buffer_desc->proc_memory[n_copy].data_segment;

            /* wait until remote data is read */
            while( source_ctl_pointer->flag < ok_to_copy_tag  ) {
                opal_progress();
            }
            /* copy data into the destination buffer */
            rc=ompi_datatype_copy_content_same_ddt(dtype, 
                    sm_module->scratch_space[n_copy],
                    (char *)((char *)rbuf+
                             dt_extent*(count_processed+cnt_offset)),
                    (char *)((char *)source_pointer+
                             dt_extent*cnt_offset));
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
            cnt_offset+=sm_module->scratch_space[n_copy];
        }
        done_copy_tag=base_tag+2+my_exchange_node->log_2;
        my_ctl_pointer->flag=done_copy_tag;

        /* wait for all to read the data, before re-using this buffer */
        if( stripe_number < (n_data_segments-1) ) {
            for(n_copy=0 ; n_copy < comm_size ; n_copy++ ) {
                source_ctl_pointer=
                    sm_buffer_desc->proc_memory[n_copy].control_region;
                while( source_ctl_pointer-> flag < done_copy_tag ) {
                    opal_progress();
                }
            }
        }
    
        /* update the count of elements processed */
        count_processed+=count_this_stripe;
    }
    /* return */
    return rc;

Error:
    return rc;
}
#if 0
/* just storing various versions of the recursive doubling algorithm, 
 * so can compare them later on.
 */
/**
 * Shared memory blocking allreduce.
 */
static
int mca_coll_sm2_allreduce_intra_recursive_doubling(void *sbuf, void *rbuf, 
        int count, struct ompi_datatype_t *dtype, 
        struct ompi_op_t *op, struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{
    /* local variables */
    int rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    int pair_rank,exchange,extra_rank;
    int index_read,index_write;
    pair_exchange_node_t *my_exchange_node;
    int my_rank,count_processed,count_this_stripe;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag, base_tag;
    sm_work_buffer_t *sm_buffer_desc;
    volatile char * my_tmp_data_buffer[2];
    volatile char * my_write_pointer;
    volatile char * my_read_pointer;
    volatile char * extra_rank_write_data_pointer;
    volatile char * extra_rank_read_data_pointer;
    volatile char * partner_read_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * 
        partner_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * 
        extra_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    /* debug 
    opal_timer_t t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
     end debug */

    sm_module=(mca_coll_sm2_module_t *) module;

    /* get size of data needed - same layout as user data, so that
     *   we can apply the reudction routines directly on these buffers
     */
    rc=ompi_datatype_type_size(dtype, &dt_extent);
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

    /* need a read and a write buffer for a pair-wise exchange of data */
    n_dts_per_buffer/=2;
    len_data_buffer=n_dts_per_buffer*dt_extent;

    /* compute number of stripes needed to process this collective */
    n_data_segments=(count+n_dts_per_buffer -1 ) / n_dts_per_buffer ;

    /* get my node for the reduction tree */
    my_exchange_node=&(sm_module->recursive_doubling_tree);
    my_rank=ompi_comm_rank(comm);

    count_processed=0;

    /* get a pointer to the shared-memory working buffer */
    /* NOTE: starting with a rather synchronous approach */

   
    /* debug 
    t0=opal_timer_base_get_cycles();
     end debug */

    /* use the same set of buffers for a single reduction */
    sm_buffer_desc=alloc_sm2_shared_buffer(sm_module);

    /* get pointers to my work buffers */
    my_ctl_pointer=sm_buffer_desc->proc_memory[my_rank].control_region;
    my_write_pointer=sm_buffer_desc->proc_memory[my_rank].data_segment;
    my_read_pointer=my_write_pointer+len_data_buffer;
    my_tmp_data_buffer[0]=my_write_pointer;
    my_tmp_data_buffer[1]=my_read_pointer;

    /* debug 
    t1=opal_timer_base_get_cycles();
     end debug */
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
        /* get number of elements to process in this stripe */
        count_this_stripe=n_dts_per_buffer;
        if( count_processed + count_this_stripe > count )
            count_this_stripe=count-count_processed;

        /* get unique set of tags for this stripe.
         *  Assume only one collective
         *  per communicator at a given time, so no locking needed
         *  for atomic update of the tag */
        base_tag=sm_module->collective_tag;
        sm_module->collective_tag+=my_exchange_node->n_tags;

        /* debug 
        t2=opal_timer_base_get_cycles();
        timers[0]+=(t2-t1);
         end debug */

        /* copy data into the write buffer */
        rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                (char *)my_write_pointer,
                (char *)((char *)sbuf+dt_extent*count_processed));
        if( 0 != rc ) {
            return OMPI_ERROR;
        }
        /* debug 
        t3=opal_timer_base_get_cycles();
        timers[1]+=(t3-t2);
         end debug */
        
        /* copy data in from the "extra" source, if need be */
        tag=base_tag;
        if(0 < my_exchange_node->n_extra_sources)  {

            if ( EXCHANGE_NODE == my_exchange_node->node_type ) {

                extra_rank=my_exchange_node->rank_extra_source;
                extra_ctl_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].control_region;
                extra_rank_write_data_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].data_segment;
                    
                /* wait until remote data is read */
                while( extra_ctl_pointer->flag < tag ) {
                    opal_progress();
                }
    
                /* apply collective operation */
                ompi_op_reduce(op,(void *)extra_rank_write_data_pointer,
                        (void *)my_write_pointer, count_this_stripe,dtype);
            } else {
        
                /* set memory barriet to make sure data is in main memory before
                 *  the completion flgas are set.
                 */

                MB();
    
                /*
                 * Signal parent that data is ready
                 */
                my_ctl_pointer->flag=tag;

            }
        }

        MB();
        /*
         * Signal parent that data is ready
         */
        tag=base_tag+1;
        my_ctl_pointer->flag=tag;

        /* loop over data exchanges */
        for(exchange=0 ; exchange < my_exchange_node->n_exchanges ; exchange++) {
            /* debug 
            t4=opal_timer_base_get_cycles();
             end debug */

            index_read=(exchange&1);
            index_write=((exchange+1)&1);

            my_write_pointer=my_tmp_data_buffer[index_write];
            my_read_pointer=my_tmp_data_buffer[index_read];

            /* is the remote data read */
            pair_rank=my_exchange_node->rank_exchanges[exchange];
            partner_ctl_pointer=
                sm_buffer_desc->proc_memory[pair_rank].control_region;
            partner_read_pointer=
                sm_buffer_desc->proc_memory[pair_rank].data_segment;
            if( 1 == index_read ) {
                partner_read_pointer+=len_data_buffer;
            }
                
            /* wait until remote data is read */
            while(  partner_ctl_pointer->flag < tag  ) {
                opal_progress();
            }
            /* debug 
            t5=opal_timer_base_get_cycles();
        timers[2]+=(t5-t4);
             end debug */

            /* reduce data into my write buffer */
            /* apply collective operation */
            /*
            ompi_op_reduce(op,(void *)partner_read_pointer,
                    (void *)my_write_pointer, count_this_stripe,dtype);
                    */
                
            /* test */
           
               {
                   int ii,n_ints;
                   int * restrict my_read=(int *)my_read_pointer;
                   int * restrict my_write=(int *)my_write_pointer;
                   int * restrict exchange_read=(int *)partner_read_pointer;
                   n_ints=count_this_stripe;
                   for(ii=0 ; ii < n_ints ; ii++ ) {
                       my_write[ii]=my_read[ii]+exchange_read[ii];
                   }

               }
            /* debug 
            t6=opal_timer_base_get_cycles();
        timers[3]+=(t6-t5);
             end debug */

            /* end test */
            
            /* signal that I am done reading my peer's data */
            tag++;
            MB();
            my_ctl_pointer->flag=tag;

            /* wait for my peer to finish - other wise buffers may be
             *   reused too early */
            while(  partner_ctl_pointer->flag < tag  ) {
                opal_progress();
            }
            /* debug 
            t7=opal_timer_base_get_cycles();
        timers[4]+=(t7-t6);
             end debug */

        }

        /* copy data in from the "extra" source, if need be */
        if(0 < my_exchange_node->n_extra_sources)  {
            tag=base_tag+my_exchange_node->n_tags-1;

            if ( EXTRA_NODE == my_exchange_node->node_type ) {

                extra_rank=my_exchange_node->rank_extra_source;
                extra_ctl_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].control_region;
                extra_rank_read_data_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].data_segment;
                index_read=(my_exchange_node->log_2&1);
                if( index_read ) {
                    extra_rank_read_data_pointer+=len_data_buffer;
                }
                    
                /* wait until remote data is read */
                while(! ( extra_ctl_pointer->flag == tag ) ) {
                    opal_progress();
                }
    
            
                /* write the data into my read buffer */
                rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                        (char *)my_write_pointer,
                        (char *)extra_rank_read_data_pointer);
                if( 0 != rc ) {
                    return OMPI_ERROR;
                }

                /* signal that I am done */
                my_ctl_pointer->flag=tag;


            } else {
        
                tag=base_tag+my_exchange_node->n_tags-1;
                /* set memory barriet to make sure data is in main memory before
                 *  the completion flgas are set.
                 */

                MB();
    
                /*
                 * Signal parent that data is ready
                 */
                my_ctl_pointer->flag=tag;

                /* wait until child is done to move on - this buffer will
                 *   be reused for the next stripe, so don't want to move
                 *   on too quick.
                 */
                extra_rank=my_exchange_node->rank_extra_source;
                extra_ctl_pointer=
                    sm_buffer_desc->proc_memory[extra_rank].control_region;
                /* wait until remote data is read */
                while(! ( extra_ctl_pointer->flag < tag ) ) {
                    opal_progress();
                }

            }
        }

            /* debug 
            t8=opal_timer_base_get_cycles();
             end debug */
        /* copy data into the destination buffer */
        rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                (char *)((char *)rbuf+dt_extent*count_processed),
                (char *)my_write_pointer);
        if( 0 != rc ) {
            return OMPI_ERROR;
        }

        /* update the count of elements processed */
        count_processed+=count_this_stripe;
    }
        
    rc=free_sm2_shared_buffer(sm_module);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

    /* return */
    return rc;

Error:
    return rc;
}
#endif
/**
 * Shared memory blocking allreduce.
 */
int mca_coll_sm2_allreduce_intra(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    /* local variables */
    int rc;
    mca_coll_sm2_module_t *sm_module;
    ptrdiff_t dt_extent;
    size_t len_data_buffer;

    sm_module=(mca_coll_sm2_module_t *) module;

    /* get size of data needed - same layout as user data, so that
     *   we can apply the reudction routines directly on these buffers
     */
    rc=ompi_datatype_type_extent(dtype, &dt_extent);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

    len_data_buffer=count*dt_extent;
    
    if( len_data_buffer <= sm_module->short_message_size) {
        rc=sm_module->allreduce_functions[SHORT_DATA_FN_ALLREDUCE]
            (sbuf, rbuf, count, dtype, op, comm, module);
    }
    else {
        rc=sm_module->allreduce_functions[LONG_DATA_FN_ALLREDUCE]
            (sbuf, rbuf, count, dtype, op, comm, module);
    }

    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

    return OMPI_SUCCESS;

Error:
    return rc;
}

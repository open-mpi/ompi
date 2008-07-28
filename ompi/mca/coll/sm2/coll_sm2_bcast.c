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
int mca_coll_sm2_fanout(void *buf, int count,
        struct ompi_datatype_t *dtype,  int root,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{
    /* local variables */
    int rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    int comm_size,process_shift,my_node_index;
    int my_rank;
    int count_processed,count_this_stripe;
    int my_fanout_parent;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag;
    volatile char * my_data_pointer;
    volatile char * parent_data_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * parent_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    tree_node_t *my_fanout_read_tree;
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
    my_fanout_read_tree=&(sm_module->fanout_read_tree[my_node_index]);
    my_fanout_parent=my_fanout_read_tree->parent_rank+process_shift;
    if( comm_size <= my_fanout_parent ){
        my_fanout_parent-=comm_size;
    }

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

        /*
         * Fan out from root - let the memory copies at each
         *   stage help reduce memory contention.
         */
        if( ROOT_NODE == my_fanout_read_tree->my_node_type ) {

            /* copy data to user supplied buffer */
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)my_data_pointer,
                    (char *)((char *)buf+dt_extent*count_processed));
            if( 0 != rc ) {
                return OMPI_ERROR;
            }

            /* I am the root - so copy  signal children, and then
             *   start reading
             */
            MB();
            my_ctl_pointer->flag=tag;
    
        } else if( LEAF_NODE == my_fanout_read_tree->my_node_type ) {
    
            parent_data_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].data_segment;
            parent_ctl_pointer=
                sm_buffer_desc->proc_memory[my_fanout_parent].control_region;

            /*
             * wait on Parent to signal that data is ready
             */
            while( parent_ctl_pointer->flag != tag) {
                opal_progress();
            }

            /* copy data to user supplied buffer */
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)buf+dt_extent*count_processed,
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
            while( parent_ctl_pointer->flag != tag) {
                opal_progress();
            }

            /* copy the data to my shared buffer, for access by children */
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)my_data_pointer,(char *)parent_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
        
            /* set memory barriet to make sure data is in main memory before
             *  the completion flgas are set.
             */
            MB();

            /* signal children that they may read the result data */
            my_ctl_pointer->flag=tag;

            /* copy data to user supplied buffer */
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)buf+dt_extent*count_processed,
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

/**
 * Shared memory blocking broadcast.
 */
int mca_coll_sm2_bcast_intra(void *buf, int count,
        struct ompi_datatype_t *dtype, int root,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{
    /* local variables */
    int rc;

        rc= mca_coll_sm2_fanout(buf, count, dtype, root, comm, module);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }

    return OMPI_SUCCESS;

Error:
    return rc;
}

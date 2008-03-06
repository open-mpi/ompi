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
static
int mca_coll_sm2_allreduce_intra_fanin_fanout(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                struct mca_coll_base_module_1_1_0_t *module)
{
    /* local variables */
    int rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    int my_rank, child_rank, child, n_parents, n_children;
    int my_fanin_parent,count_processed,count_this_stripe;
    int my_fanout_parent;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag;
    volatile char * sm_buffer;
    volatile char * my_data_pointer;
    volatile char * child_data_pointer;
    volatile char * parent_data_pointer;
    char *my_base_temp_pointer;
    volatile char * child_base_temp_pointer;
    volatile char * parent_base_temp_pointer; 
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * child_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * parent_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    tree_node_t *my_reduction_node, *my_fanout_read_tree;


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
    my_fanout_read_tree=&(sm_module->fanout_read_tree[my_rank]);
    n_children=my_reduction_node->n_children;
    n_parents=my_reduction_node->n_parents;
    my_fanin_parent=my_reduction_node->parent_rank;
    my_fanout_parent=my_fanout_read_tree->parent_rank;
    count_processed=0;

    /* get a pointer to the shared-memory working buffer */
    /* NOTE: starting with a rather synchronous approach */
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
        sm_buffer=alloc_sm2_shared_buffer(sm_module);
        if( NULL == sm_buffer) {
            rc=OMPI_ERR_OUT_OF_RESOURCE;
            goto Error;
        }
        /* get number of elements to process in this stripe */
        count_this_stripe=n_dts_per_buffer;
        if( count_processed + count_this_stripe > count )
            count_this_stripe=count-count_processed;

        /* get base address to "my" memory segment */
        my_base_temp_pointer=(char *)
            ((char *)sm_buffer+sm_module->sm_buffer_mgmt_barrier_tree.my_rank*
             sm_module->segement_size_per_process);
        /* offset to data segment */
        my_data_pointer=my_base_temp_pointer+ctl_size;
        my_ctl_pointer=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            my_base_temp_pointer;

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
                        ( (child_ctl_pointer->flag == tag) &
                         (child_ctl_pointer->index== stripe_number) ) ) {
                    /* Note: Actually need to make progress here */
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
            my_ctl_pointer->index=stripe_number;
        }
    
        /***************************
         * Fan into root phase
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
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)((char *)rbuf+dt_extent*count_processed),
                    (char *)my_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
    
        } else if( LEAF_NODE == my_fanout_read_tree->my_node_type ) {
    
            parent_base_temp_pointer=(char *)
                ((char *)sm_buffer+my_fanout_parent*
                 sm_module->segement_size_per_process);
   
            parent_data_pointer=(volatile char *)
                ((char *)parent_base_temp_pointer+ctl_size);
            parent_ctl_pointer=(volatile 
                    mca_coll_sm2_nb_request_process_shared_mem_t *)
                parent_base_temp_pointer;

            child_ctl_pointer=
                (volatile  mca_coll_sm2_nb_request_process_shared_mem_t *)
                parent_data_pointer;
       
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
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)rbuf+dt_extent*count_processed,
                    (char *)parent_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
    
        } else {
            /* interior nodes */
            parent_base_temp_pointer=(char *)
                ((char *)sm_buffer+my_fanout_parent*
                 sm_module->segement_size_per_process);
   
            parent_data_pointer=(volatile char *)
                ((char *)parent_base_temp_pointer+ctl_size);
            parent_ctl_pointer=(volatile 
                    mca_coll_sm2_nb_request_process_shared_mem_t *)
                parent_base_temp_pointer;

            child_ctl_pointer=
                (volatile  mca_coll_sm2_nb_request_process_shared_mem_t *)
                parent_data_pointer;
       
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
            my_ctl_pointer->flag=-tag;

            /* copy data to user supplied buffer */
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
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


/**
 * Shared memory blocking allreduce.
 */
static
int mca_coll_sm2_allreduce_intra_recursive_doubling(void *sbuf, void *rbuf, 
        int count, struct ompi_datatype_t *dtype, 
        struct ompi_op_t *op, struct ompi_communicator_t *comm,
        struct mca_coll_base_module_1_1_0_t *module)
{
    /* local variables */
    int rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    int my_rank, child_rank, child, n_parents, n_children;
    int my_fanin_parent,count_processed,count_this_stripe;
    int my_fanout_parent;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag;
    volatile char * sm_buffer;
    volatile char * my_data_pointer;
    volatile char * child_data_pointer;
    volatile char * parent_data_pointer;
    char *my_base_temp_pointer;
    volatile char * child_base_temp_pointer;
    volatile char * parent_base_temp_pointer; 
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * child_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * parent_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    tree_node_t *my_reduction_node, *my_fanout_read_tree;


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
    my_fanout_read_tree=&(sm_module->fanout_read_tree[my_rank]);
    n_children=my_reduction_node->n_children;
    n_parents=my_reduction_node->n_parents;
    my_fanin_parent=my_reduction_node->parent_rank;
    my_fanout_parent=my_fanout_read_tree->parent_rank;
    count_processed=0;

    /* get a pointer to the shared-memory working buffer */
    /* NOTE: starting with a rather synchronous approach */
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
        sm_buffer=alloc_sm2_shared_buffer(sm_module);
        if( NULL == sm_buffer) {
            rc=OMPI_ERR_OUT_OF_RESOURCE;
            goto Error;
        }
        /* get number of elements to process in this stripe */
        count_this_stripe=n_dts_per_buffer;
        if( count_processed + count_this_stripe > count )
            count_this_stripe=count-count_processed;

        /* get base address to "my" memory segment */
        my_base_temp_pointer=(char *)
            ((char *)sm_buffer+sm_module->sm_buffer_mgmt_barrier_tree.my_rank*
             sm_module->segement_size_per_process);
        /* offset to data segment */
        my_data_pointer=my_base_temp_pointer+ctl_size;
        my_ctl_pointer=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            my_base_temp_pointer;

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
                        ( (child_ctl_pointer->flag == tag) &
                         (child_ctl_pointer->index== stripe_number) ) ) {
                    /* Note: Actually need to make progress here */
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
            my_ctl_pointer->index=stripe_number;
        }
    
        /***************************
         * Fan into root phase
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
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)((char *)rbuf+dt_extent*count_processed),
                    (char *)my_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
    
        } else if( LEAF_NODE == my_fanout_read_tree->my_node_type ) {
    
            parent_base_temp_pointer=(char *)
                ((char *)sm_buffer+my_fanout_parent*
                 sm_module->segement_size_per_process);
   
            parent_data_pointer=(volatile char *)
                ((char *)parent_base_temp_pointer+ctl_size);
            parent_ctl_pointer=(volatile 
                    mca_coll_sm2_nb_request_process_shared_mem_t *)
                parent_base_temp_pointer;

            child_ctl_pointer=
                (volatile  mca_coll_sm2_nb_request_process_shared_mem_t *)
                parent_data_pointer;
       
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
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)rbuf+dt_extent*count_processed,
                    (char *)parent_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
    
        } else {
            /* interior nodes */
            parent_base_temp_pointer=(char *)
                ((char *)sm_buffer+my_fanout_parent*
                 sm_module->segement_size_per_process);
   
            parent_data_pointer=(volatile char *)
                ((char *)parent_base_temp_pointer+ctl_size);
            parent_ctl_pointer=(volatile 
                    mca_coll_sm2_nb_request_process_shared_mem_t *)
                parent_base_temp_pointer;

            child_ctl_pointer=
                (volatile  mca_coll_sm2_nb_request_process_shared_mem_t *)
                parent_data_pointer;
       
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
            my_ctl_pointer->flag=-tag;

            /* copy data to user supplied buffer */
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
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
        rc= mca_coll_sm2_allreduce_intra_recursive_doubling(sbuf, rbuf, count,
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
    return rc;
}

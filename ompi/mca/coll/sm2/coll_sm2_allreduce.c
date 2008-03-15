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
/* debug */
#include "opal/sys/timer.h"

extern uint64_t timers[7];
/* end debug */



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
    sm_work_buffer_t *sm_buffer_desc;


    sm_module=(mca_coll_sm2_module_t *) module;

    /* get unique tag for this collective - assume only one collective
     *  per communicator at a given time, so no locking needed
     *  for atomic update of the tag */
    tag=sm_module->collective_tag;
    sm_module->collective_tag++;

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
    
        sm_buffer_desc=alloc_sm2_shared_buffer(sm_module);
        sm_buffer=sm_buffer_desc->base_segment_address;
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
    int pair_rank,exchange,extra_rank;
    int index_read,index_write;
    pair_exchange_node_t *my_exchange_node;
    int my_rank,count_processed,count_this_stripe;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag, base_tag;
    sm_work_buffer_t *sm_buffer_desc;
    volatile char * sm_buffer;
    volatile char * my_tmp_data_buffer[2];
    volatile char * my_write_pointer;
    volatile char * my_read_pointer;
    volatile char * extra_rank_write_data_pointer;
    volatile char * extra_rank_read_data_pointer;
    volatile char * partner_read_pointer;
    volatile char * partner_write_pointer;
    char *my_base_temp_pointer;
    volatile char * partner_base_temp_pointer;
    volatile char * extra_rank_base_temp_pointer; 
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * 
        partner_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * 
        extra_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    /* debug */
    opal_timer_t t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
    /* end debug */

    sm_module=(mca_coll_sm2_module_t *) module;

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
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {
            /* debug */
            t0=opal_sys_timer_get_cycles();
            /* end debug */
        sm_buffer_desc=alloc_sm2_shared_buffer(sm_module);
        sm_buffer=sm_buffer_desc->base_segment_address;
        if( NULL == sm_buffer) {
            rc=OMPI_ERR_OUT_OF_RESOURCE;
            goto Error;
        }
            /* debug */
            t1=opal_sys_timer_get_cycles();
            /* end debug */
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

        /* get base address to "my" memory segment */
        my_base_temp_pointer=(char *)
            ((char *)sm_buffer+sm_module->sm_buffer_mgmt_barrier_tree.my_rank*
             sm_module->segement_size_per_process);
        /* offset to data segment */
        my_ctl_pointer=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            my_base_temp_pointer;
        /* my partner will read my data, as I am reducing it's data into
         * my buffer
         */
        my_write_pointer=my_base_temp_pointer+ctl_size;
        my_read_pointer=my_write_pointer+len_data_buffer;
        my_tmp_data_buffer[0]=my_write_pointer;
        my_tmp_data_buffer[1]=my_read_pointer;
        /* debug */
        t2=opal_sys_timer_get_cycles();
        timers[0]+=(t2-t1);
        /* end debug */

        /* copy data into the write buffer */
        rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                (char *)my_write_pointer,
                (char *)((char *)sbuf+dt_extent*count_processed));
        if( 0 != rc ) {
            return OMPI_ERROR;
        }
        /* debug */
        t3=opal_sys_timer_get_cycles();
        timers[1]+=(t3-t2);
        /* end debug */
        
        /* copy data in from the "extra" source, if need be */
        tag=base_tag;
        if(0 < my_exchange_node->n_extra_sources)  {

            if ( EXCHANGE_NODE == my_exchange_node->node_type ) {

                extra_rank=my_exchange_node->rank_extra_source;
                extra_rank_base_temp_pointer=(char *)
                    ((char *)sm_buffer+extra_rank*
                     sm_module->segement_size_per_process);
           
                extra_ctl_pointer=
                    ( mca_coll_sm2_nb_request_process_shared_mem_t * volatile)
                    extra_rank_base_temp_pointer;
                extra_rank_write_data_pointer=extra_rank_base_temp_pointer+
                    ctl_size;
                    
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

            /* debug */
            t4=opal_sys_timer_get_cycles();
            /* end debug */

            index_read=(exchange&1);
            index_write=((exchange+1)&1);

            my_write_pointer=my_tmp_data_buffer[index_write];
            my_read_pointer=my_tmp_data_buffer[index_read];

            /* is the remote data read */
            pair_rank=my_exchange_node->rank_exchanges[exchange];
            partner_base_temp_pointer=(char *)
                ((char *)sm_buffer+pair_rank*
                 sm_module->segement_size_per_process);
       
            partner_ctl_pointer=
                ( mca_coll_sm2_nb_request_process_shared_mem_t * volatile)
                partner_base_temp_pointer;
            partner_read_pointer=(char *)partner_ctl_pointer+ctl_size;
            if( 1 == index_read ) {
                partner_read_pointer+=len_data_buffer;
            }
                
            /* wait until remote data is read */
            while(  partner_ctl_pointer->flag < tag  ) {
                opal_progress();
            }
            /* debug */
            t5=opal_sys_timer_get_cycles();
        timers[2]+=(t5-t4);
            /* end debug */

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
            /* debug */
            t6=opal_sys_timer_get_cycles();
        timers[3]+=(t6-t5);
            /* end debug */

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
            /* debug */
            t7=opal_sys_timer_get_cycles();
        timers[4]+=(t7-t6);
            /* end debug */

        }

        /* copy data in from the "extra" source, if need be */
        if(0 < my_exchange_node->n_extra_sources)  {
            tag=base_tag+my_exchange_node->n_tags-1;

            if ( EXTRA_NODE == my_exchange_node->node_type ) {

                extra_rank=my_exchange_node->rank_extra_source;
                extra_rank_base_temp_pointer=(char *)
                    ((char *)sm_buffer+extra_rank*
                     sm_module->segement_size_per_process);
           
                extra_ctl_pointer=
                    ( mca_coll_sm2_nb_request_process_shared_mem_t * volatile)
                    extra_rank_base_temp_pointer;
                index_read=(my_exchange_node->log_2&1);
                extra_rank_read_data_pointer=extra_rank_base_temp_pointer+
                    ctl_size;
                if( index_read ) {
                    extra_rank_read_data_pointer+=len_data_buffer;
                }
                    
                /* wait until remote data is read */
                while(! ( extra_ctl_pointer->flag == tag ) ) {
                    opal_progress();
                }
    
            
                /* write the data into my read buffer */
                rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                        (char *)my_write_pointer,
                        (char *)extra_rank_read_data_pointer);
                if( 0 != rc ) {
                    return OMPI_ERROR;
                }

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

            }
        }

            /* debug */
            t8=opal_sys_timer_get_cycles();
            /* end debug */
        /* copy data into the destination buffer */
        rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                (char *)((char *)rbuf+dt_extent*count_processed),
                (char *)my_write_pointer);
        if( 0 != rc ) {
            return OMPI_ERROR;
        }
            /* debug */
            t9=opal_sys_timer_get_cycles();
        timers[5]+=(t9-t8);
            /* end debug */

        /* "free" the shared-memory working buffer */
        rc=free_sm2_shared_buffer(sm_module);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }
            /* debug */
            t10=opal_sys_timer_get_cycles();
        timers[6]+=(t10-t9);
            /* end debug */
    
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

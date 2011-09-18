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
int mca_coll_sm2_reduce_intra_fanin(void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype, 
                                struct ompi_op_t *op,
                                int root,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
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

    /* debug 
    last_root=root;
     end debug */
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
    my_reduction_node=&(sm_module->reduction_tree[my_node_index]);
    n_children=my_reduction_node->n_children;
    /* debug 
    node_type=my_reduction_node->my_node_type;
     end debug */

    if( 1 == n_data_segments ) {
        /* single data segment */
        
            /* get unique tag for this stripe - assume only one collective
             *  per communicator at a given time, so no locking needed
             *  for atomic update of the tag */
            tag=sm_module->collective_tag;
            sm_module->collective_tag++;
            /* debug 
            assert(tag);
             end debug */
    
            /* get a pointer to the shared-memory working buffer */
            sm_buffer_desc=alloc_sm2_shared_buffer(sm_module);
            /* debug 
            free_buff_free_index=tag;
             end debug */
    
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
                rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
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
        
            /* debug 
                    if( 0 == child_ctl_pointer->flag ) {
                        fprintf(stderr,"TTT 2 count %d root %d child_rank %d \n",
                                count,root,child_rank);
                        debug_module();
                    }
             end debug */
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
                rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
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
            /* debug 
                    if( 0 == child_ctl_pointer->flag ) {
                        fprintf(stderr,"TTT 3 count %d root %d child_rank \n",
                                count,root,child_rank);
                        debug_module();
                    }
             end debug */
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
                rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
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
            /* debug 
                    if( 0 == child_ctl_pointer->flag ) {
                        fprintf(stderr,"TTT 1 count %d root %d child_rank %d \n",
                                count,root,child_rank);
                        debug_module();
                    }
             end debug */
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
                    rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
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
int mca_coll_sm2_reduce_intra_reducescatter_gather(void *sbuf, void *rbuf, 
        int count, struct ompi_datatype_t *dtype, 
        struct ompi_op_t *op, 
        int root,
        struct ompi_communicator_t *comm,
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
    volatile char * my_extra_write_pointer;
    volatile char * partner_base_pointer;
    volatile char * my_pointer;
    volatile char * my_base_pointer;
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
        { int *int_tmp=(int *)my_base_pointer;
            int i;
            fprintf(stderr," my rank %d data in tmp :: ",my_rank);
            for (i=0 ; i < count_this_stripe ; i++ ) {
                fprintf(stderr," %d ",int_tmp[i]);
            }
            fprintf(stderr,"\n");
            fflush(stderr);
        }
         end debug */

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
        /* debug 
        { int *int_tmp=(int *)my_base_pointer;
            int i;
            fprintf(stderr,"  GGG my rank %d data in tmp :: ",my_rank);
            for (i=0 ; i < count_this_stripe ; i++ ) {
                fprintf(stderr," %d ",int_tmp[i]);
            }
            fprintf(stderr,"\n");
            fflush(stderr);
        }
         end debug */
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

        /* only root reads the results */
        if( root == my_rank) {
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

/**
 * Shared memory blocking reduce.
 */
int mca_coll_sm2_reduce_intra(void *sbuf, void *rbuf, int count,
        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
        int root, struct ompi_communicator_t *comm,
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
        rc=sm_module->reduce_functions[SHORT_DATA_FN_REDUCE]
            (sbuf, rbuf, count, dtype, op, root, comm, module);
    }
    else {
        rc=sm_module->reduce_functions[LONG_DATA_FN_REDUCE]
            (sbuf, rbuf, count, dtype, op, root, comm, module);
    }

    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

#if 0
        rc= mca_coll_sm2_reduce_intra_fanin(sbuf, rbuf, count,
                dtype, op, root, comm, module);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }

        rc= mca_coll_sm2_reduce_intra_reducescatter_gather(sbuf, rbuf, count,
                dtype, op, root, comm, module);
        if( OMPI_SUCCESS != rc ) {
            goto Error;
        }
#endif

    return OMPI_SUCCESS;

Error:
    return rc;
}

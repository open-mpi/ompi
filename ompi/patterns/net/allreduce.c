/*
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
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
#include "ompi/mca/rte/rte.h"

void send_completion(nt status, struct ompi_process_name_t* peer, struct iovec* msg, 
                     int count, ompi_rml_tag_t tag, void* cbdata)
{
    /* set send completion flag */
    *(int *)cbdata=1;
}


void recv_completion(nt status, struct ompi_process_name_t* peer, struct iovec* msg, 
                     int count, ompi_rml_tag_t tag, void* cbdata)
{
    /* set receive completion flag */
    MB();
    *(int *)cbdata=1;
}


static void op_reduce(int op_type,(void *)src_dest_buf,(void *) src_buf, int count,
        int data_type)
{
    /* local variables */
    int ret;

    /* op type */
    switch (op_type) {

        case OP_SUM:

            
            switch (data_type) {
                case TYPE_INT4:
                    int *int_src_ptr=(int *)src_ptr;
                    int *int_src_dst_ptr=(int *)src_dst_ptr;
                    int cnt;
                    for(cnt=0 ; cnt < count ; ) {
                        (*(int_src_dst_ptr))+=(*(int_src_ptr));
                    break;
                default:
                    ret=OMPI_ERROR;
                    goto Error;
            }

            break;

        default:
        ret=OMPI_ERROR;
        goto Error;
    }
Error:
    return ret;
}

/**
 * All-reduce for contigous primitive types
 */
static
comm_allreduce(void *sbuf, void *rbuf, int count, opal_datatype_t *dtype, 
        int op_type, opal_list_t *peers)
{
    /* local variables */
    int rc=OMPI_SUCCESS,n_dts_per_buffer,n_data_segments,stripe_number;
    int pair_rank,exchange,extra_rank;
    int index_read,index_write;
    netpatterns_pair_exchange_node_t my_exchange_node;
    int my_rank,count_processed,count_this_stripe;
    size_t n_peers,message_extent,len_data_buffer;
    size_t dt_size;
    long long tag, base_tag;
    sm_work_buffer_t *sm_buffer_desc;
    opal_list_item_t *item;
    char scratch_bufers[2][MAX_TMP_BUFFER];
    int send_buffer=0;recv_buffer=1;
    char *sbuf_current,*rbuf_current;
    ompi_proc_t **proc_array;
    struct iovec send_iov, recv_iov;
    volatile int *recv_done, *send_done;
    int recv_completion_flag, send_completion_flag;
    int data_type;

    /* get size of data needed - same layout as user data, so that
     *   we can apply the reudction routines directly on these buffers
     */
    rc=opal_datatype_type_size(dtype, &dt_size);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }
    message_extent=dt_extent*count;

    /* lenght of control and data regions */
    len_data_buffer=sm_module->data_memory_per_proc_per_segment;

    /* number of data types copies that the scratch buffer can hold */
    n_dts_per_buffer=((int) MAX_TMP_BUFFER)/dt_size;
    if ( 0 == n_dts_per_buffer ) {
        rc=OMPI_ERROR;
        goto Error;
    }

    /* need a read and a write buffer for a pair-wise exchange of data */
    n_dts_per_buffer/=2;
    len_data_buffer=n_dts_per_buffer*dt_size;

    /* compute number of stripes needed to process this collective */
    n_data_segments=(count+n_dts_per_buffer -1 ) / n_dts_per_buffer ;

    /* */
    n_peers=opal_list_get_size(peers);

    /* get my rank in the list */
    my_rank=0;
    for (item = opal_list_get_first(peers) ;
            item != opal_list_get_end(peers) ;
            item = opal_list_get_next(peers)) {
        if(ompi_proc_local()==(ompi_proc_t *)item){
            /* this is the pointer to my proc strucuture */
            break;
        }
        my_rank++;
    }
    proc_array=(ompi_proc_t **)malloc(sizeof(ompi_proc_t *)*n_peers);
    if( NULL == proc_array) {
        goto Error;
    }
    cnt=0;
    for (item = opal_list_get_first(peers) ;
            item != opal_list_get_end(peers) ;
            item = opal_list_get_next(peers)) {
        proc_array[cnt]=(ompi_proc_t *)item;
        cnt++;
    }

    /* get my reduction communication pattern */
    ret=netpatterns_setup_recursive_doubling_tree_node(n_peers,my_rank,&my_exchange_node);
    if(OMPI_SUCCESS != ret){
        return ret;
    }

    /* setup flags for non-blocking communications */    
    recv_done=&recv_completion_flag;
    send_done=&send_completion_flag;

    /* set data type */
    if(&opal_datatype_int4==dtype) {
        data_type=TYPE_INT4;
    }

    count_processed=0;

    /* get a pointer to the shared-memory working buffer */
    /* NOTE: starting with a rather synchronous approach */
    for( stripe_number=0 ; stripe_number < n_data_segments ; stripe_number++ ) {

        /* get number of elements to process in this stripe */
        count_this_stripe=n_dts_per_buffer;
        if( count_processed + count_this_stripe > count )
            count_this_stripe=count-count_processed;

        /* copy data from the input buffer into the temp buffer */
        sbuf_current=(char *)sbuf+count_processed*dt_size;
        memcopy(scratch_bufers[send_buffer],sbuf_current,count_this_stripe*dt_size);

        /* copy data in from the "extra" source, if need be */
        if(0 < my_exchange_node->n_extra_sources)  {

            if ( EXCHANGE_NODE == my_exchange_node->node_type ) {
                
                /*
                ** Receive data from extra node
                */
                
                extra_rank=my_exchange_node.rank_extra_source;
                recv_iov.iov_base=scratch_bufers[recv_buffer];
                recv_iov.iov_len=count_this_stripe*dt_size;
                rc = ompi_rte_recv(&(proc_array[extra_rank]->proc_name), &recv_iov, 1,
                        OMPI_RML_TAG_ALLREDUCE , 0);
                if(OMPI_SUCCESS != rc ) {
                    goto  Error;
                }

                /* apply collective operation to first half of the data */
                if( 0 < count_this_stripe ) {
                    op_reduce(op_type,(void *)scratch_bufers[recv_buffer],
                            (void *)scratch_bufers[send_buffer], n_my_count,TYPE_INT4);
                }


            } else {
        
                /*
                ** Send data to "partner" node
                */
                extra_rank=my_exchange_node.rank_extra_source;
                send_iov.iov_base=scratch_bufers[send_buffer];
                send_iov.iov_len=count_this_stripe*dt_size;
                rc = ompi_rte_send(&(proc_array[extra_rank]->proc_name), &send_iov, 1,
                        OMPI_RML_TAG_ALLREDUCE , 0);
                if(OMPI_SUCCESS != rc ) {
                    goto  Error;
                }
            }

            /* change pointer to scratch buffer - this was we can send data
            ** that we have summed w/o a memory copy, and receive data into the
            ** other buffer, w/o fear of over writting data that has not yet
            ** completed being send
            */
            recv_buffer^=1;
            send_buffer^=1;
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
            t4=opal_sys_timer_get_cycles();
             end debug */


            my_write_pointer=my_tmp_data_buffer[index_write];
            my_read_pointer=my_tmp_data_buffer[index_read];

            /* is the remote data read */
            pair_rank=my_exchange_node->rank_exchanges[exchange];

            *recv_done=0; 
            *send_done=0;
            MB();

            /* post non-blocking receive */
            recv_iov.iov_base=scratch_bufers[send_buffer];
            recv_iov.iov_len=count_this_stripe*dt_size;
            rc = ompi_rte_recv_nb(&(proc_array[extra_rank]->proc_name), recv_iov, 1,
                        OMPI_RML_TAG_ALLREDUCE , 0, recv_completion, recv_done);

            /* post non-blocking send */
            send_iov.iov_base=scratch_bufers[send_buffer];
            send_iov.iov_len=count_this_stripe*dt_size;
            rc = ompi_rte_send_nb(&(proc_array[extra_rank]->proc_name), send_iov, 1,
                        OMPI_RML_TAG_ALLREDUCE , 0, send_completion, send_done);

            /* wait on receive completion */
            while(!(*recv_done) ) {
                opal_progress();
            }
                
            /* reduce the data */
            if( 0 < count_this_stripe ) {
                op_reduce(op_type,(void *)scratch_bufers[recv_buffer],
                        (void *)scratch_bufers[send_buffer], n_my_count,TYPE_INT4);
            }

            
            /* get ready for next step */
            index_read=(exchange&1);
            index_write=((exchange+1)&1);

            /* wait on send completion */
            while(!(*send_done) ) {
                opal_progress();
            }
                
        }

        /* copy data in from the "extra" source, if need be */
        if(0 < my_exchange_node->n_extra_sources)  {

            if ( EXTRA_NODE == my_exchange_node->node_type ) {
                /* 
                ** receive the data 
                ** */
                extra_rank=my_exchange_node->rank_extra_source;

                recv_iov.iov_base=scratch_bufers[recv_buffer];
                recv_iov.iov_len=count_this_stripe*dt_size;
                rc = ompi_rte_recv(&(proc_array[extra_rank]->proc_name), &recv_iov, 1,
                        OMPI_RML_TAG_ALLREDUCE , 0);
                if(OMPI_SUCCESS != rc ) {
                    goto  Error;
                }

            } else {
                /* send the data to the pair-rank outside of the power of 2 set
                ** of ranks
                */

                extra_rank=my_exchange_node->rank_extra_source;
                send_iov.iov_base=scratch_bufers[recv_buffer];
                send_iov.iov_len=count_this_stripe*dt_size;
                rc = ompi_rte_recv(&(proc_array[extra_rank]->proc_name), &send_iov, 1,
                        OMPI_RML_TAG_ALLREDUCE , 0);
                if(OMPI_SUCCESS != rc ) {
                    goto  Error;
                }
            }
        }

        /* copy data into the destination buffer */
        rc=ompi_datatype_copy_content_same_ddt(dtype, count_this_stripe,
                (char *)((char *)rbuf+dt_extent*count_processed),
                (char *)my_write_pointer);
        if( 0 != rc ) {
            return OMPI_ERROR;
        }

        /* copy data from the temp buffer into the output buffer */
        rbuf_current=(char *)rbuf+count_processed*dt_size;
        memcopy(scratch_bufers[recv_buffer],rbuf_current,count_this_stripe*dt_size);
    
        /* update the count of elements processed */
        count_processed+=count_this_stripe;
    }

    /* return */
    return rc;

Error:
    return rc;
}

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
    int my_fanin_parent,count_processed,count_this_stripe;
    int my_fanout_parent;
    size_t message_extent,dt_extent,ctl_size,len_data_buffer;
    long long tag;
    volatile char * sm_buffer;
    volatile char * my_data_pointer;
    volatile char * child_data_pointer;
    volatile char * parent_data_pointer;
    char *my_base_temp_pointer, * volatile child_base_temp_pointer;
    char * volatile parent_base_temp_pointer, * volatile root_base_temp_pointer;
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * child_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * parent_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    tree_node_t *my_reduction_node, *my_fanout_read_tree;

    /* debug */
    fprintf(stderr," GGGG sm2 allreduce called r %d \n",ompi_comm_rank(comm));
    fflush(stderr);
    /* end debug */

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
        /* debug */
        fprintf(stderr," GGGG strip_number %d r %d \n",
                stripe_number,ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */
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

        /* debug */
        fprintf(stderr," GGGG before fan in r %d \n",
                ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */
        /*
         * Fan into root phase
         */

        /* copy segment into shared buffer - later on will optimize to
         *   eliminate extra copies.
         */
        rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                my_data_pointer, sbuf+dt_extent*count_processed);
        if( 0 != rc ) {
            return OMPI_ERROR;
        }

        /* debug */
        fprintf(stderr," GGGG copied my data to sm r %d \n",
                ompi_comm_rank(comm));
        fprintf(stderr," GGGG  tag %lld index %d r %d \n",
                tag,stripe_number,ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */
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

        /* debug */
        fprintf(stderr," GGGG before wait tag %lld index %d p %p r %d \n",
                tag,stripe_number,&(child_ctl_pointer->flag),
                ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */
            /* wait until child flag is set */
            while(! 
                    (child_ctl_pointer->flag == tag &
                     child_ctl_pointer->index== stripe_number) ) {
                /* Note: Actually need to make progress here */
                ;
            }
        /* debug */
        fprintf(stderr," GGGG after wait %d \n",
                ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */

            /* apply collective operation */
            ompi_op_reduce(op,child_data_pointer,my_data_pointer,
                    count,dtype);
        }
        /* debug */
        fprintf(stderr," GGGG got data from kids  r %d \n",
                ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */

        /* set memory barriet to make sure data is in main memory before
         *  the completion flgas are set.
         */
        MB();

        /*
         * Signal parent that data is ready
         */
        my_ctl_pointer->flag=tag;
        my_ctl_pointer->index=stripe_number;
    
        /* debug */
        fprintf(stderr," GGGG signaled parent p %p val %lld - r %d \n",
                &(my_ctl_pointer->flag),
                my_ctl_pointer->flag,
                ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */
    
        /*
         * Fan out from root - let the memory copies at each
         *   stage help reduce memory contention.
         */
        if( 0 == my_fanout_read_tree->n_parents ) {
            /* I am the root - so copy  signal children, and then
             *   start reading
             */
            my_ctl_pointer->flag=-tag;
        /* debug */
        fprintf(stderr," GGGG reset flag to %lld - r %d \n",
                my_ctl_pointer->flag,
                ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */

            /* copy data to user supplied buffer */
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)rbuf+dt_extent*count_processed,my_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }

        } else {
            parent_base_temp_pointer=(char *)
                ((char *)sm_buffer+my_fanout_parent*
                 sm_module->segement_size_per_process);
   
            parent_data_pointer=parent_base_temp_pointer+ctl_size;
            parent_ctl_pointer=parent_base_temp_pointer;

            child_ctl_pointer=
                ( mca_coll_sm2_nb_request_process_shared_mem_t * volatile)
                parent_data_pointer;
       
            /*
             * wait on Parent to signal that data is ready
             */
        /* debug */
        fprintf(stderr," GGGG waiting on parent - r %d \n",
                ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */
            while(! 
                    /* in fan-in, index was already set correctly, so
                     *   no need to check it
                     */
                    (parent_ctl_pointer->flag == -tag ) ) {
                /* debug */
                fprintf(stderr," VVVV flag %lld index %lld - r %d \n",
                        parent_ctl_pointer->flag,
                        parent_ctl_pointer->index,
                        ompi_comm_rank(comm));
                fflush(stderr);
                /* end debug */
                /* Note: Actually need to make progress here */
                ;
            }
        /* debug */
        fprintf(stderr," GGGG done waiting on parent - r %d \n",
                ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */

            /* copy the data to my shared buffer, for access by children */
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    my_data_pointer,parent_data_pointer);
            if( 0 != rc ) {
                return OMPI_ERROR;
            }
        
            /* set memory barriet to make sure data is in main memory before
             *  the completion flgas are set.
             */
            MB();

        /* debug */
        fprintf(stderr," GGGG about to signal children - r %d \n",
                ompi_comm_rank(comm));
        fflush(stderr);
        /* end debug */
            /* signal children that they may read the result data */
            my_ctl_pointer->flag=-tag;

            /* copy data to user supplied buffer */
            rc=ompi_ddt_copy_content_same_ddt(dtype, count_this_stripe,
                    (char *)rbuf+dt_extent*count_processed,my_data_pointer);
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

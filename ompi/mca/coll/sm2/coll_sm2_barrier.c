/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
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
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "opal/sys/atomic.h"
#include "coll_sm2.h"
/* debug 
extern int debug_print;
 end debug */

/**
 * Shared memory barrier.
 *
 * Tree-based algorithm for a barrier: a fan in to rank 0 followed by
 * a fan out using the barrier segments in the shared memory area.
 *
 * There are 2 sets of barrier buffers -- since there can only be, at
 * most, 2 outstanding barriers at any time, there is no need for more
 * than this.  The generalized in-use flags, control, and data
 * segments are not used.
 *
 * The general algorithm is for a given process to wait for its N
 * children to fan in by monitoring a uint32_t in its barrier "in"
 * buffer.  When this value reaches N (i.e., each of the children have
 * atomically incremented the value), then the process atomically
 * increases the uint32_t in its parent's "in" buffer.  Then the
 * process waits for the parent to set a "1" in the process' "out"
 * buffer.  Once this happens, the process writes a "1" in each of its
 * children's "out" buffers, and returns.
 *
 * There's corner cases, of course, such as the root that has no
 * parent, and the leaves that have no children.  But that's the
 * general idea.
 */

/* non-blocking barrier - init function */
int mca_coll_sm2_nbbarrier_intra(struct ompi_communicator_t *comm,
        mca_coll_sm2_nb_request_process_private_mem_t *request,
        struct mca_coll_base_module_1_1_0_t *module)
{

    /* since completion must be in-order for the sm collective buffer allocation
     * to work correctly, no barrier completion will happen here.  The most
     * that will be done is for the leaf processes, to signal their presence.
     */
    /* local variables */
    int index;
    long long tag;
    mca_coll_sm2_module_t *sm_module;
    mca_coll_sm2_nb_request_process_shared_mem_t *sm_barrier_region;
    mca_coll_sm2_nb_request_process_shared_mem_t *sm_address;

    /* get pointer to nb-barrier structure */
    index=request->sm_index;
    sm_barrier_region=(mca_coll_sm2_nb_request_process_shared_mem_t *)
        (request->barrier_base_address[index]);

    /* set barrier tag - no atomicity needed as only only one outstanding
     *   collective per communicator exists
     */
    sm_module=(mca_coll_sm2_module_t *)module;
    sm_module->nb_barrier_tag++;
    request->tag=sm_module->nb_barrier_tag;
    tag=sm_module->nb_barrier_tag;

    if( LEAF_NODE == sm_module->sm_buffer_mgmt_barrier_tree.my_node_type ) {
        /*
         * Fan-in phase
         */
    
        /* Set my completion flag */
        sm_address=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            ((char *)sm_barrier_region+
             sm_module->sm_buffer_mgmt_barrier_tree.my_rank*
             sm_module->sm2_size_management_region_per_proc);
        sm_address->flag=tag;
        request->sm2_barrier_phase=NB_BARRIER_FAN_OUT;
    
    } else if( INTERIOR_NODE  == sm_module->sm_buffer_mgmt_barrier_tree.my_node_type ) {
        /*
         * Fan-in phase
         */
        request->sm2_barrier_phase=NB_BARRIER_FAN_IN;

    } else {
    
        /*
         * Fan-in phase
         */
        request->sm2_barrier_phase=NB_BARRIER_FAN_IN;
    }

    /* return - successful completion */
    return OMPI_SUCCESS;
}


/* non-blocking barrier - completion function */
int mca_coll_sm2_nbbarrier_intra_progress(struct ompi_communicator_t *comm,
        mca_coll_sm2_nb_request_process_private_mem_t *request,
        struct mca_coll_base_module_1_1_0_t *module)
{

    /* local variables */
    int index;
    int child,cnt,phase;
    long long tag;
    mca_coll_sm2_module_t *sm_module;
    mca_coll_sm2_nb_request_process_shared_mem_t *sm_barrier_region;
    mca_coll_sm2_nb_request_process_shared_mem_t *sm_address;

    /* get pointer to nb-barrier structure */
    index=request->sm_index;
    sm_barrier_region=request->barrier_base_address[index];

    /* set barrier tag - no atomicity needed as only only one outstanding
     *   collective per communicator exists
     */
    sm_module=(mca_coll_sm2_module_t *)module;
    tag=request->tag;

    if( LEAF_NODE == sm_module->sm_buffer_mgmt_barrier_tree.my_node_type ) {
        phase=request->sm2_barrier_phase;
        if( NB_BARRIER_FAN_OUT == phase ) {
            goto FANOUT_LEAF;
        } else if ( (NB_BARRIER_DONE == phase) || (NB_BARRIER_INACTIVE == phase) ) {
            goto DONE;
        }
        /* defult - NB_BARRIER_FAN_IN */
    
        /*
         * Fan-in phase
         */
    
    FANOUT_LEAF:
        /*
         * Fan-out phase
         */
    
        /*
         * check to see if parent has checked in
         */
        sm_address=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            ((char *)sm_barrier_region+
            sm_module->sm_buffer_mgmt_barrier_tree.parent_rank*
            sm_module->sm2_size_management_region_per_proc);
        if( sm_address->flag != -tag ) {
            /* if parent has not checked in - set parameters for async
             *   completion, incomplet barrier flag, and bail
             */
            request->sm2_barrier_phase=NB_BARRIER_FAN_OUT;
            return OMPI_SUCCESS;
        }
    
        /*
         * set my completion flag
         */
        request->sm2_barrier_phase=NB_BARRIER_DONE;
    } else if( INTERIOR_NODE == sm_module->sm_buffer_mgmt_barrier_tree.my_node_type ) {
        phase=request->sm2_barrier_phase;
        if( NB_BARRIER_FAN_OUT == phase ) {
            goto FANOUT_INTERIOR;
        } else if ( (NB_BARRIER_DONE == phase) || (NB_BARRIER_INACTIVE == phase) ) {
            goto DONE;
        }
        /* defult - NB_BARRIER_FAN_IN */
    
        /*
         * Fan-in phase
         */
    
        /* check to see if children have checked in */
        cnt=0;
        for( child=0 ; child < sm_module->sm_buffer_mgmt_barrier_tree.n_children ; child++ ) {
            /* compute flag address */
            sm_address=(mca_coll_sm2_nb_request_process_shared_mem_t *)
                ((char *)sm_barrier_region+
                sm_module->sm_buffer_mgmt_barrier_tree.children_ranks[child] *
                sm_module->sm2_size_management_region_per_proc);
            if(sm_address->flag == tag ) {
                /* child arrived */
                cnt++;
            } else {
                /* child not arrived, just break out */
                break;
            }
        }
    
        /* if children have not checked in - set paramenters for async
         *   completion, incomplet barrier flag, and bail
         */
        if( cnt != sm_module->sm_buffer_mgmt_barrier_tree.n_children ) {
            /* set restart parameters, and exit */
            request->sm2_barrier_phase=NB_BARRIER_FAN_IN;
            return OMPI_SUCCESS;
        }
    
        /* Set my completion flag */
        sm_address=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            ((char *)sm_barrier_region+
             sm_module->sm_buffer_mgmt_barrier_tree.my_rank *
             sm_module->sm2_size_management_region_per_proc);
        sm_address->flag=tag;
        /* don't need memory barrier here, as we are not setting any other sm
         * data for someone else to read
         */
    
    FANOUT_INTERIOR:
        /*
         * Fan-out phase
         */
    
        /*
         * check to see if parent has checked in
         */
        sm_address=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            ((char *)sm_barrier_region+
            sm_module->sm_buffer_mgmt_barrier_tree.parent_rank*
            sm_module->sm2_size_management_region_per_proc);
        if( sm_address->flag != -tag ) {
            /* if parent has not checked in - set parameters for async
             *   completion, incomplet barrier flag, and bail
             */
            request->sm2_barrier_phase=NB_BARRIER_FAN_OUT;
            return OMPI_SUCCESS;
        }
    
        sm_address=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            ((char *)sm_barrier_region+
             sm_module->sm_buffer_mgmt_barrier_tree.my_rank *
             sm_module->sm2_size_management_region_per_proc);
        sm_address->flag=-tag;
    
        /*
         * set my completion flag
         */
        request->sm2_barrier_phase=NB_BARRIER_DONE;
    } else {
        /* root node */
        phase=request->sm2_barrier_phase;
        if ( (NB_BARRIER_DONE == phase) || (NB_BARRIER_INACTIVE == phase) ) {
            goto DONE;
        }
        /* defult - NB_BARRIER_FAN_IN */
    
        /*
         * Fan-in phase
         */
    
        /* check to see if children have checked in */
        cnt=0;
        for( child=0 ; child < sm_module->sm_buffer_mgmt_barrier_tree.n_children ; child++ ) {
            /* compute flag address */
            sm_address=(mca_coll_sm2_nb_request_process_shared_mem_t *)
                ((char *)sm_barrier_region+
                sm_module->sm_buffer_mgmt_barrier_tree.children_ranks[child] *
                sm_module->sm2_size_management_region_per_proc);
            if(sm_address->flag == tag ) {
                /* child arrived */
                cnt++;
            } else {
                /* child not arrived, just break out */
                break;
            }
        }
    
        /* if children have not checked in - set paramenters for async
         *   completion, incomplet barrier flag, and bail
         */
        if( cnt != sm_module->sm_buffer_mgmt_barrier_tree.n_children ) {
            /* set restart parameters, and exit */
            request->sm2_barrier_phase=NB_BARRIER_FAN_IN;
            return OMPI_SUCCESS;
        }
    
        /* Set my completion flag */
        sm_address=(mca_coll_sm2_nb_request_process_shared_mem_t *)
            ((char *)sm_barrier_region+
             sm_module->sm_buffer_mgmt_barrier_tree.my_rank *
             sm_module->sm2_size_management_region_per_proc);
        sm_address->flag=-tag;

        /*
         * set my completion flag
         */
        request->sm2_barrier_phase=NB_BARRIER_DONE;
    }
    
DONE:
    /* return - successful completion */
    return OMPI_SUCCESS;
}

/**
 * Shared memory blocking allreduce.
 */
static
int mca_coll_sm2_barrier_intra_fanin_fanout(
        struct ompi_communicator_t *comm,
        struct mca_coll_base_module_1_1_0_t *module)
{
    /* local variables */
    int rc=OMPI_SUCCESS;
    int my_rank, child_rank, child, n_parents, n_children;
    int my_fanin_parent;
    int my_fanout_parent;
    long long tag;
    mca_coll_sm2_nb_request_process_shared_mem_t *my_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * child_ctl_pointer;
    volatile mca_coll_sm2_nb_request_process_shared_mem_t * parent_ctl_pointer;
    mca_coll_sm2_module_t *sm_module;
    tree_node_t *my_reduction_node, *my_fanout_read_tree;
    sm_work_buffer_t *sm_buffer_desc;

    sm_module=(mca_coll_sm2_module_t *) module;



    /* get my node for the reduction tree */
    my_rank=ompi_comm_rank(comm);
    my_reduction_node=&(sm_module->reduction_tree[my_rank]);
    my_fanout_read_tree=&(sm_module->fanout_read_tree[my_rank]);
    n_children=my_reduction_node->n_children;
    n_parents=my_reduction_node->n_parents;
    my_fanin_parent=my_reduction_node->parent_rank;
    my_fanout_parent=my_fanout_read_tree->parent_rank;

    /* get unique tag for this stripe - assume only one collective
     *  per communicator at a given time, so no locking needed
     *  for atomic update of the tag */
    tag=sm_module->collective_tag;
    sm_module->collective_tag++;

    sm_buffer_desc=alloc_sm2_shared_buffer(sm_module);

    /* offset to data segment */
    my_ctl_pointer=sm_buffer_desc->proc_memory[my_rank].control_region;

    /***************************
     * Fan into root phase
     ***************************/

    if( LEAF_NODE != my_reduction_node->my_node_type ) {

        /*
         * Wait on children, and apply op to their data
         */
        for( child=0 ; child < n_children ; child++ ) {

            child_rank=my_reduction_node->children_ranks[child];
            child_ctl_pointer=
                sm_buffer_desc->proc_memory[child_rank].control_region;

            /* wait until child flag is set */
            while(  child_ctl_pointer->flag != tag ) {
                /* Note: Actually need to make progress here */
                opal_progress();
            }


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
    } else {
        /* leaf node */

        /* set memory barriet to make sure data is in main memory before
         *  the completion flgas are set.
         */
        MB();

        /*
         * Signal parent that data is ready
         */
        my_ctl_pointer->flag=tag;
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


    } else if( LEAF_NODE == my_fanout_read_tree->my_node_type ) {

        parent_ctl_pointer=
            sm_buffer_desc->proc_memory[my_fanout_parent].control_region;

        /*
         * wait on Parent to signal that data is ready
         */
        while( parent_ctl_pointer->flag != -tag ) {
            opal_progress();
        }

    } else {
        /* interior nodes */
   
        parent_ctl_pointer=
            sm_buffer_desc->proc_memory[my_fanout_parent].control_region;

        /*
         * wait on Parent to signal that data is ready
         */
        while( parent_ctl_pointer->flag != -tag ) {
            opal_progress();
        }

        /* set memory barriet to make sure data is in main memory before
         *  the completion flgas are set.
         */
        MB();

        /* signal children that they may read the result data */
        my_ctl_pointer->flag=-tag;

    }

    /* "free" the shared-memory working buffer */
    rc=free_sm2_shared_buffer(sm_module);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }
    
    /* return */
    return rc;

Error:
    return rc;
}

/**
 * Shared memory blocking barrier
 */
int mca_coll_sm2_barrier_intra( struct ompi_communicator_t *comm,
                                struct mca_coll_base_module_1_1_0_t *module)
{
    /* local variables */
    int rc;

    rc= mca_coll_sm2_barrier_intra_fanin_fanout(comm, module);
    if( OMPI_SUCCESS != rc ) {
        goto Error;
    }

    return OMPI_SUCCESS;

Error:
    return rc;
}

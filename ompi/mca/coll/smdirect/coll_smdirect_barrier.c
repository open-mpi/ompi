/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
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
#include "coll_smdirect.h"

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
int mca_coll_smdirect_barrier_intra(struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    int rank, op_count;
    mca_coll_smdirect_comm_t *data;
    uint32_t num_children;
    size_t control_size;
    struct mca_coll_smdirect_tree_node_t *parent, *me;
    mca_coll_smdirect_procdata_t *parent_data;
    mca_coll_smdirect_module_t *sm_module = (mca_coll_smdirect_module_t*) module;

    /* Lazily enable the module the first time we invoke a collective
       on it */
    if (!sm_module->enabled) {
        int ret;
        if (OMPI_SUCCESS != (ret = ompi_coll_smdirect_lazy_enable(module, comm))) {
            return ret;
        }
    }


    control_size = mca_coll_smdirect_component.sm_control_size;
    data = sm_module->sm_comm_data;
    rank = ompi_comm_rank(comm);
    me = &data->mcb_tree[rank];
    parent = me->mcstn_parent;
    num_children = me->mcstn_num_children;
    op_count = ++data->mcb_operation_count;

    /* wait for processes from the previous op to finish */
    FLAG_WAIT_FOR_IDLE(&data->procdata->mcsp_op_flag);

    /* we will receive signals from our children, pass that info on, and wait
     * for our parent to do the same in reverse */
    FLAG_RETAIN(&data->procdata->mcsp_segment_down_flag, 1, 0);

    FLAG_RETAIN(&data->procdata->mcsp_segment_up_flag, num_children, 0);

    opal_atomic_wmb();
    FLAG_RETAIN(&data->procdata->mcsp_op_flag, num_children, op_count);

    /* wait for my children to signal */
    if (num_children > 0) {
        FLAG_WAIT_FOR_IDLE(&data->procdata->mcsp_segment_up_flag);
    }

    if (NULL != parent) {
        parent_data = (mca_coll_smdirect_procdata_t *)(data->sm_bootstrap_meta->module_data_addr
                                                          + control_size * parent->mcstn_id);
        /* make sure my parent is on the same op */
        FLAG_WAIT_FOR_OP(&parent_data->mcsp_op_flag, op_count);
        //FLAG_WAIT_FOR_OP(&parent_data->mcsp_segment_up_flag, op_count);
        /* signal up */
        FLAG_RELEASE(&parent_data->mcsp_segment_up_flag);
        /* wait for parent to signal back down */
        FLAG_WAIT_FOR_IDLE(&data->procdata->mcsp_segment_down_flag);
    }

    /* signal back down to our children */
    for (int i = 0; i < num_children; ++i) {
        mca_coll_smdirect_procdata_t *child_data = (mca_coll_smdirect_procdata_t *)(data->sm_bootstrap_meta->module_data_addr
                                                          + control_size * me->mcstn_children[i]->mcstn_id);
        /* make sure the child is on the same op */
        FLAG_WAIT_FOR_OP(&child_data->mcsp_op_flag, op_count);
        /* non-atomic reset is fine, there is only one parent */
        FLAG_RESET(&child_data->mcsp_segment_down_flag);
    }

    /* finally, signal my parent that we're done (to sync between ops) */
    if (NULL != parent) {
        FLAG_RELEASE(&parent_data->mcsp_op_flag);
    }

    return OMPI_SUCCESS;
}

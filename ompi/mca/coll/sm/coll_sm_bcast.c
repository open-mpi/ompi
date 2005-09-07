/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi/include/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"
#include "ompi/mca/coll/coll.h"
#include "opal/include/sys/atomic.h"
#include "coll_sm.h"

/**
 * Shared memory broadcast.
 *
 * For the root, the general algorithm is to wait for the segment to
 * be available.  Once it is, it copies a fragment of the user's
 * buffer into the shared data segment and then write a 1 into its
 * childrens' "out" control buffers.  The process is repeated until
 * all fragments have been written.
 *
 * For non-roots, they wait for a 1 to appear into their "out" control
 * buffers.  If they have children, they copy the data from their
 * parent's shared data segment into their shared data segment, and
 * write a 1 into each of its childrens' "out" control buffers.  They
 * then copy the data from their shared [local] data segment into the
 * user's buffer.  The process is repeated until all fragments have
 * been received.
 */
int mca_coll_sm_bcast_intra(void *buff, int count, 
                            struct ompi_datatype_t *datatype, int root, 
                            struct ompi_communicator_t *comm)
{
    struct iovec iov;
    mca_coll_base_comm_t *data = comm->c_coll_selected_data;
    int i, ret, rank, size, num_children, src_rank;
    int flag_num, segment_num, max_segment_num;
    int parent_rank;
    size_t total_size, max_data, bytes;
    mca_coll_sm_in_use_flag_t *flag;
    ompi_convertor_t convertor;
    mca_coll_sm_tree_node_t *me, *parent, **children;
    mca_coll_base_mpool_index_t *index;

    /* Setup some identities */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    OBJ_CONSTRUCT(&convertor, ompi_convertor_t);
    iov.iov_len = mca_coll_sm_component.sm_fragment_size;
    bytes = 0;

    me = &data->mcb_tree[(rank + size - root) % size];
    D(("rank %d: virtual rank %d\n", rank, me - data->mcb_tree));
    parent = me->mcstn_parent;
    children = me->mcstn_children;
    num_children = me->mcstn_num_children;

    /* Only have one top-level decision as to whether I'm the root or
       not.  Do this at the slight expense of repeating a little logic
       -- but it's better than a conditional branch in every loop
       iteration. */

    /*********************************************************************
     * Root
     *********************************************************************/

    if (root == rank) {

        /* The root needs a send convertor to pack from the user's
           buffer to shared memory */

        if (OMPI_SUCCESS != 
            (ret = 
             ompi_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                                                      datatype,
                                                      count, 
                                                      buff,
                                                      &convertor))) {
            return ret;
        }
        ompi_convertor_get_packed_size(&convertor, &total_size);
        D(("root got send convertor w/ total_size == %lu\n", 
           (unsigned long) total_size));

        /* Main loop over sending fragments */

        do {
            flag_num = (data->mcb_operation_count++ % 
                        mca_coll_sm_component.sm_comm_num_in_use_flags);

            FLAG_SETUP(flag_num, flag, data);
            FLAG_WAIT_FOR_IDLE(flag);
            FLAG_RETAIN(flag, size - 1, data->mcb_operation_count - 1);

            /* Loop over all the segments in this set */
            
            segment_num = flag_num * 
                mca_coll_sm_component.sm_comm_num_in_use_flags; 
            max_segment_num = (flag_num + 1) * 
                mca_coll_sm_component.sm_comm_num_in_use_flags; 
            do {
                index = &(data->mcb_mpool_index[segment_num]);

                /* Copy the fragment from the user buffer to my fragment
                   in the current segment */
                COPY_FRAGMENT_IN(convertor, index, iov, max_data);
                bytes += max_data;
                
                /* Wait for the write to absolutely complete */
                opal_atomic_wmb();
                
                /* Tell my children that this fragment is ready */
                PARENT_NOTIFY_CHILDREN(children, num_children, index,
                                       max_data);

                ++segment_num;
            } while (bytes < total_size && segment_num < max_segment_num);
        } while (bytes < total_size);
    }

    /*********************************************************************
     * Non-root
     *********************************************************************/

    else {

        /* Non-root processes need a receive convertor to unpack from
           shared mmory to the user's buffer */

        OBJ_CONSTRUCT(&convertor, ompi_convertor_t);
        if (OMPI_SUCCESS != 
            (ret = 
             ompi_convertor_copy_and_prepare_for_recv(ompi_mpi_local_convertor,
                                                      datatype,
                                                      count, 
                                                      buff,
                                                      &convertor))) {
            return ret;
        }
        ompi_convertor_get_packed_size(&convertor, &total_size);
        D(("rank %d got recv convertor w/ total_size == %lu\n", 
           rank, (unsigned long) total_size));

        /* Loop over receiving (and possibly re-sending) the
           fragments */
        
        do {
            flag_num = (data->mcb_operation_count % 
                        mca_coll_sm_component.sm_comm_num_in_use_flags);

            /* Wait for the root to mark this set of segments as
               ours */
            FLAG_SETUP(flag_num, flag, data);
            FLAG_WAIT_FOR_OP(flag, data->mcb_operation_count);
            ++data->mcb_operation_count;

            /* Loop over all the segments in this set */
            
            segment_num = flag_num * 
                mca_coll_sm_component.sm_comm_num_in_use_flags; 
            max_segment_num = (flag_num + 1) * 
                mca_coll_sm_component.sm_comm_num_in_use_flags; 
            do {

                /* Pre-calculate some values */
                parent_rank = (parent->mcstn_id + root) % size;
                index = &(data->mcb_mpool_index[segment_num]);

                /* Wait for my parent to tell me that the segment is ready */
                CHILD_WAIT_FOR_NOTIFY(rank, index, max_data);
                
                /* If I have children, send the data to them */
                if (num_children > 0) {
                    /* Copy the fragment from the parent's portion in
                       the segment to my portion in the segment. */
                    COPY_FRAGMENT_BETWEEN(parent_rank, rank, index, max_data);
                    
                    /* Wait for the write to absolutely complete */
                    opal_atomic_wmb();
                    
                    /* Tell my children that this fragment is ready */
                    PARENT_NOTIFY_CHILDREN(children, num_children, index,
                                           max_data);
                    
                    /* Set the "copy from buffer" to be my local
                       segment buffer so that we don't potentially
                       incur a non-local memory copy from the parent's
                       fan out data segment [again] when copying to
                       the user's buffer */
                    src_rank = rank;
                }
                
                /* If I don't have any children, set the "copy from
                   buffer" to be my parent's fan out segment to copy
                   directly from my parent */
                
                else {
                    src_rank = parent_rank;
                }
                
                /* Copy to my output buffer */
                COPY_FRAGMENT_OUT(convertor, src_rank, index, iov, max_data);

                bytes += max_data;
                ++segment_num;
            } while (bytes < total_size && segment_num < max_segment_num);

            /* Wait for all copy-out writes to complete before I say
               I'm done with the segments */
            opal_atomic_wmb();

            /* We're finished with this set of segments */
            FLAG_RELEASE(flag);
        } while (bytes < total_size);
    }

    /* Kill the convertor */

    OBJ_DESTRUCT(&convertor);

    /* All done */

    D(("rank %d done with bcast\n", rank));
    return OMPI_SUCCESS;
}

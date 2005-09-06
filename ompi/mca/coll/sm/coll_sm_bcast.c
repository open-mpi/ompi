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
    uint32_t iov_size = 1;
    mca_coll_base_comm_t *data = comm->c_coll_selected_data;
    int i, ret, rank, size, num_children;
    int flag_num, segment_num, max_segment_num;
    int parent_rank, child_rank;
    size_t total_size, max_data, bytes;
    volatile uint32_t *my_control;
    mca_coll_sm_in_use_flag_t *flag;
    ompi_convertor_t convertor;
    mca_coll_sm_tree_node_t *me, *parent, **children;
    int32_t bogus_free_after = 0;

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

            /* Wait for the set of segments to become available */
            flag = (mca_coll_sm_in_use_flag_t*)
                (((char *) data->mcb_in_use_flags) +
                 (flag_num * mca_coll_sm_component.sm_control_size));
            D(("root waiting for in_use flag %d (value %d), %p\n", 
               flag_num, flag->mcsiuf_num_procs_using, flag));
            while (0 != flag->mcsiuf_num_procs_using) {
                continue;
            }
            D(("root got in_use flag %d (value %d), %p\n", 
               flag_num, flag->mcsiuf_num_procs_using, flag));

            /* Now that the set of segments is availble, mark it as
               used.  No need to include the root in the count (we'd
               only have to decrement it later).  Don't need a write
               barrier here -- we have another later that will
               guarantee that the write has completed, if
               necessary. */

            flag->mcsiuf_num_procs_using = size - 1;
            flag->mcsiuf_operation_count = data->mcb_operation_count - 1;

            /* Loop over all the segments in this set */
            
            segment_num = flag_num * 
                mca_coll_sm_component.sm_comm_num_in_use_flags; 
            max_segment_num = (flag_num + 1) * 
                mca_coll_sm_component.sm_comm_num_in_use_flags; 
            do {

                /* Copy the fragment from the user buffer to my fragment
                   in the current segment */
                iov.iov_base = 
                    data->mcb_mpool_index[segment_num].mcbmi_data +
                    (rank * mca_coll_sm_component.sm_fragment_size);
                max_data = iov.iov_len;
                D(("root copying %lu bytes to data fan out, seg %d: %p\n",
                   (unsigned long) iov.iov_len, segment_num, iov.iov_base));
                ompi_convertor_pack(&convertor, &iov, &iov_size,
                                    &max_data, &bogus_free_after);
                bytes += max_data;
                
                /* Wait for the write to absolutely complete */
                opal_atomic_wmb();
                
                /* Tell my children that this fragment is ready (be
                   sure to normalize the child's ID based on the shift
                   we did above to calculate the "me" node in the
                   tree) */
                for (i = 0; i < num_children; ++i) {
                    child_rank = (children[i]->mcstn_id + root) % size;
                    *((size_t*) 
                      (((char*) 
                        data->mcb_mpool_index[segment_num].mcbmi_control) +
                       (mca_coll_sm_component.sm_control_size * 
                        child_rank))) = max_data;
                    D(("root sent notice to child %d (vrank %d), control to %p\n",
                       i, children[i]->mcstn_id,
                       (((char*) 
                         data->mcb_mpool_index[segment_num].mcbmi_control) +
                        (mca_coll_sm_component.sm_control_size * 
                         child_rank))));
                }

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
            flag = (mca_coll_sm_in_use_flag_t*)
                (((char *) data->mcb_in_use_flags) +
                 (flag_num * mca_coll_sm_component.sm_control_size));
            D(("rank %d waiting for root to claim in-use flag %d, %p (op count %d)\n", rank, flag_num, flag, data->mcb_operation_count));
            while (data->mcb_operation_count != flag->mcsiuf_operation_count) {
                continue;
            }
            ++data->mcb_operation_count;

            /* Loop over all the segments in this set */
            
            segment_num = flag_num * 
                mca_coll_sm_component.sm_comm_num_in_use_flags; 
            max_segment_num = (flag_num + 1) * 
                mca_coll_sm_component.sm_comm_num_in_use_flags; 
            do {

                /* Pre-calculate some pointers */
                
                parent_rank = (parent->mcstn_id + root) % size;
                D(("rank %d parent rank is %d\n", rank, parent_rank));
                my_control = (uint32_t *)
                    (((char*) 
                      data->mcb_mpool_index[segment_num].mcbmi_control) +
                     (rank * mca_coll_sm_component.sm_control_size));

                /* Wait for the fragment: the parent will mark the segment
                   as ready */
                D(("rank %d waiting for fragment in segment %d (control %p)\n",
                   rank, segment_num, (char*) my_control));
                while (0 == *my_control) {
                    continue;
                }
                max_data = *my_control;
                D(("rank %d: fragment ready in segment %d\n", rank, segment_num));
                
                /* If I have children, send the data to them */
                if (num_children > 0) {
                    /* No need to wait for the segment to become
                       available -- the root has already claimed it
                       and we're all already using it.  So copy the
                       fragment from the parent's portion in the
                       segment to my portion in the segment.  This is
                       a simply memcpy because it's already been
                       packed into the parent's segment. */
                    memcpy(/* my data fan out section in the segment */
                           (data->mcb_mpool_index[segment_num].mcbmi_data +
                            (rank * mca_coll_sm_component.sm_fragment_size)),
                           /* parent's fan out section in the segment */
                           (data->mcb_mpool_index[segment_num].mcbmi_data +
                            (parent_rank *
                             mca_coll_sm_component.sm_fragment_size)),
                           /* length */
                           *my_control);
                    D(("rank %d memcopy'ed fragment (%p) to my data fan out (%lu bytes)\n", rank, 
                       (data->mcb_mpool_index[segment_num].mcbmi_data +
                        (parent_rank * mca_coll_sm_component.sm_fragment_size)),
                       (unsigned long) *my_control));
                    
                    /* Wait for the write to absolutely complete */
                    opal_atomic_wmb();
                    
                    /* Tell my children that this fragment is ready */
                    for (i = 0; i < num_children; ++i) {
                        child_rank = (children[i]->mcstn_id + root) % size;
                        *((size_t*) 
                          (((char*) 
                            data->mcb_mpool_index[segment_num].mcbmi_control) +
                           (mca_coll_sm_component.sm_control_size * 
                            child_rank))) = *my_control;
                        D(("rank %d notifying child %d (vrank %d, rank %d)\n",
                           rank, i, children[i]->mcstn_id, child_rank));
                    }
                    
                    /* Set the "copy from buffer" to be my local
                       segment buffer so that we don't potentially
                       incur a non-local memory copy from the parent's
                       fan out data segment [again] when copying to
                       the user's buffer */
                    iov.iov_base =
                        data->mcb_mpool_index[segment_num].mcbmi_data +
                        (rank * mca_coll_sm_component.sm_fragment_size);
                }
                
                /* If I don't have any children, set the "copy from
                   buffer" to be my parent's fan out segment to copy
                   directly from my parent */
                
                else {
                    iov.iov_base =
                        (((char*) 
                          data->mcb_mpool_index[segment_num].mcbmi_data) +
                         (parent_rank * 
                          mca_coll_sm_component.sm_fragment_size));
                }
                
                /* Copy to my output buffer */
                D(("rank %d convertor copied from parent data %p to user buffer (%lu bytes)\n",
                   rank, iov.iov_base, (unsigned long) iov.iov_len));
                ompi_convertor_unpack(&convertor, &iov, &iov_size,
                                      &max_data, &bogus_free_after);

                *my_control = 0;
                bytes += max_data;
                ++segment_num;
            } while (bytes < total_size && segment_num < max_segment_num);

            /* Wait for all copy-out writes to complete before I say
               I'm done with the segments */
            opal_atomic_wmb();

            /* We're finished with this set of segments */

            D(("rank %d done with in-use flag %d (value %d)\n",
               rank, flag_num, flag->mcsiuf_num_procs_using));
            opal_atomic_add(&flag->mcsiuf_num_procs_using, -1);
        } while (bytes < total_size);
    }

    /* Kill the convertor */

    OBJ_DESTRUCT(&convertor);

    /* All done */

    D(("rank %d done with bcast\n", rank));
    return OMPI_SUCCESS;
}

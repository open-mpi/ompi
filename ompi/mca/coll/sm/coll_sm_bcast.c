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

#if 0
#define D(foo) { printf foo ; fflush(stdout); }
#else
#define D(foo)
#endif


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
    int i, ret, rank, size, segment, num_children;
    size_t total_size, max_data, bytes;
    uint32_t *my_control, *parent_control;
    ompi_convertor_t send_convertor;
    ompi_convertor_t recv_convertor;
    mca_coll_sm_tree_node_t *me, *parent, **children;
    int32_t bogus_free_after;

    /* Setup some identities */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    me = &data->mcb_tree[(rank + size - root) % size];
    parent = me->mcstn_parent;
    children = me->mcstn_children;
    num_children = me->mcstn_num_children;

    /* If I'm the root, I need a send convertor to pack from the
       user's buffer to shared memory */

    if (root == rank) {
        OBJ_CONSTRUCT(&send_convertor, ompi_convertor_t);
        if (OMPI_SUCCESS != 
            (ret = 
             ompi_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                                                      datatype,
                                                      count, 
                                                      buff,
                                                      &send_convertor))) {
            return ret;
        }
        ompi_convertor_get_packed_size(&send_convertor, &total_size);
        D(("root got send convertor w/ total_size == %lu\n", 
           (unsigned long) total_size));
    }

    /* If I'm not the root, I need a receive convertor to unpack from
       shared mmory to the user's buffer */

    else {
        OBJ_CONSTRUCT(&recv_convertor, ompi_convertor_t);
        if (OMPI_SUCCESS != 
            (ret = 
             ompi_convertor_copy_and_prepare_for_recv(ompi_mpi_local_convertor,
                                                      datatype,
                                                      count, 
                                                      buff,
                                                      &recv_convertor))) {
            return ret;
        }
        ompi_convertor_get_packed_size(&recv_convertor, &total_size);
        D(("rank %d got recv convertor w/ total_size == %lu\n", 
           rank, (unsigned long) total_size));
    } 

    /* Setup some more constants */

    iov.iov_len = mca_coll_sm_component.sm_fragment_size;
    bytes = 0;

    /* Loop over the fragments */

    do {
        segment = (++data->mcb_operation_count % 
                   data->mcb_mpool_num_segments);

        /* Root */

        if (root == rank) {

            /* Wait for the segment to become available */
            /* JMS */
            
            /* Copy the fragment from the user buffer to the segment */
            iov.iov_base = data->mcb_mpool_index[segment].mcbmi_data +
                (root * mca_coll_sm_component.sm_fragment_size);
            max_data = iov.iov_len;
            ompi_convertor_pack(&send_convertor, &iov, &iov_size,
                                &max_data, &bogus_free_after);
            D(("root sent %lu bytes to data fan out: %p\n",
               (unsigned long) max_data,
               data->mcb_mpool_index[segment].mcbmi_data +
               (root * mca_coll_sm_component.sm_fragment_size)));

            /* Wait for the write to absolutely complete */
            opal_atomic_wmb();
            
            /* Tell my children that this fragment is ready */
            for (i = 0; i < num_children; ++i) {
                /* JMS: TEMPORARILY HARDWIRED FOR ROOT==0 */
                *((size_t*) 
                  (((char*) 
                    data->mcb_mpool_index[segment].mcbmi_control) +
                   (mca_coll_sm_component.sm_control_size * 
                    children[i]->mcstn_id))) = max_data;
                D(("root sent notice to child %d (rank %d)\n",
                   i, children[i]->mcstn_id));
            }
        }

        /* Non-root */

        else {
            
            /* Pre-calculate some pointers */
            
            my_control = (uint32_t *)
                (((char*) 
                  data->mcb_mpool_index[segment].mcbmi_control) +
                 (rank * mca_coll_sm_component.sm_control_size));
            parent_control = (uint32_t *)
                (((char*) 
                  data->mcb_mpool_index[segment].mcbmi_control) +
                 (parent->mcstn_id * mca_coll_sm_component.sm_control_size));
            
            /* Wait for the fragment: the parent will mark the segment
               as ready */
            D(("rank %d waiting for fragment in segment %d\n", rank, segment));
            while (0 == *my_control) {
                continue;
            }
            max_data = *my_control;
            D(("rank %d: fragment ready in segment %d\n", rank, segment));
            
            /* If I have children, send the data to them */
            if (num_children > 0) {
                max_data = iov.iov_len;

                /* Wait for the segment to become available */
                /* JMS */

                /* Copy the fragment from the parent's portion in the
                   segment to my portion in the segment.  This is a
                   simply memcpy because it's already been packed
                   into the parent's segment. */
                memcpy(/* my data fan out section in the segment */
                       (data->mcb_mpool_index[segment].mcbmi_data +
                        (me->mcstn_id * 
                         mca_coll_sm_component.sm_fragment_size)),
                       /* parent's fan out section in the segment */
                       (data->mcb_mpool_index[segment].mcbmi_data +
                        (parent->mcstn_id * 
                         mca_coll_sm_component.sm_fragment_size)),
                       /* length */
                       *my_control);
                D(("rank %d copied fragment (%p) to my data fan out (%lu bytes)\n", rank, 
                   (data->mcb_mpool_index[segment].mcbmi_data +
                    (parent->mcstn_id * 
                     mca_coll_sm_component.sm_fragment_size)),
                   (unsigned long) *my_control));
            
                /* Wait for the write to absolutely complete */
                opal_atomic_wmb();

                /* Tell my children that this fragment is ready */
                for (i = 0; i < num_children; ++i) {
                    /* JMS: TEMPORARILY HARDWIRED FOR ROOT==0 */
                    *((uint32_t*) 
                      (((char*) 
                        data->mcb_mpool_index[segment].mcbmi_control) +
                       (mca_coll_sm_component.sm_control_size * 
                        children[i]->mcstn_id))) = 1;
                    D(("rank %d notifying child %d (rank %d)\n",
                       rank, i, children[i]->mcstn_id));
                }

                /* Set the "copy from buffer" to be my local segment
                   buffer so that we don't potentially incur a
                   non-local memory copy from the parent's fan out
                   data segment [again] when copying to the user's
                   buffer */
                iov.iov_base =
                    data->mcb_mpool_index[segment].mcbmi_data +
                    (me->mcstn_id * 
                     mca_coll_sm_component.sm_fragment_size);
                D(("rank %d convertor copying from my data fan out\n", rank));
            }

            /* If I don't have any children, set the "copy from
               buffer" to be my parent's fan out segment to copy
               directly from my parent */

            else {
                iov.iov_base =
                    (((char*) 
                      data->mcb_mpool_index[segment].mcbmi_data) +
                     (parent->mcstn_id * 
                      mca_coll_sm_component.sm_fragment_size));
            }
            
            /* Copy to my output buffer */
            ompi_convertor_unpack(&recv_convertor, &iov, &iov_size,
                                  &max_data, &bogus_free_after);
            D(("rank %d convertor copied into user buffer (%lu bytes)\n", rank, max_data));
        }

        /* It's ok to only look at the max_data from the last
           operation because it will be the same value for all of
           them */
        bytes += max_data;
    } while (bytes < total_size);
    D(("rank %d done sending/receiving\n", rank));

    if (root == rank) {
        D(("root destroying send convertor\n"));
        OBJ_DESTRUCT(&send_convertor);
    } else {
        D(("rank %d destroying recv_convertor\n", rank));
        OBJ_DESTRUCT(&recv_convertor);
    }
    D(("rank %d done with bcast\n", rank));

    /* All done */

    return OMPI_SUCCESS;
}

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "ompi_config.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"
#include "ompi/mca/coll/coll.h"
#include "opal/sys/atomic.h"
#include "ompi/op/op.h"
#include "coll_sm.h"


/*
 * Local functions
 */
static int reduce_inorder(void *sbuf, void* rbuf, int count, 
                          struct ompi_datatype_t *dtype, 
                          struct ompi_op_t *op, 
                          int root, struct ompi_communicator_t *comm);
#define WANT_REDUCE_NO_ORDER 0
#if WANT_REDUCE_NO_ORDER
static int reduce_no_order(void *sbuf, void* rbuf, int count, 
                           struct ompi_datatype_t *dtype, 
                           struct ompi_op_t *op, 
                           int root, struct ompi_communicator_t *comm);
#endif

/*
 * Useful utility routine
 */
#if !defined(__WINDOWS__)
static inline int min(int a, int b)
{
    return (a < b) ? a : b;
}
#endif  /* !defined(__WINDOWS__) */

/**
 * Shared memory reduction.
 *
 * Simply farms out to the associative or non-associative functions.
 */
int mca_coll_sm_reduce_intra(void *sbuf, void* rbuf, int count, 
                             struct ompi_datatype_t *dtype, 
                             struct ompi_op_t *op, 
                             int root, struct ompi_communicator_t *comm)
{
    size_t size;

    /* There are several possibilities:
     *

     * 0. If the datatype is larger than a segment, fall back to basic
     * 1. If the op is user-defined, use the strict order
     * 2. If the op is intrinsic:
     *    a. If the op is float-associative, use the unordered
     *    b. If the op is not float-asociative:
     *       i. if the data is floating point, use the strict order
     *       ii. if the data is not floating point, use the unordered
     */

    ompi_ddt_type_size(dtype, &size);
    if ((int)size > mca_coll_sm_component.sm_control_size) {
        return comm->c_coll_basic_module->coll_reduce(sbuf, rbuf, count,
                                                      dtype, op, root, comm);
    } 
#if WANT_REDUCE_NO_ORDER
    else if (!ompi_op_is_intrinsic(op) ||
        (ompi_op_is_intrinsic(op) && !ompi_op_is_float_assoc(op) &&
         0 != (dtype->flags & DT_FLAG_DATA_FLOAT))) {
        return reduce_inorder(sbuf, rbuf, count, dtype, op, root, comm);
    } else {
        return reduce_no_order(sbuf, rbuf, count, dtype, op, root, comm);
    }
#else
    else {
        return reduce_inorder(sbuf, rbuf, count, dtype, op, root, comm);
    }
#endif
}


/**
 * In-order shared memory reduction.
 *
 * This function performs the reduction in order -- combining elements
 * starting with (0 operation 1), then (result operation 2), then
 * (result operation 3), etc.
 *
 * Root's algorithm:
 * 
 * If our datatype is "friendly" (i.e., the representation of the
 * buffer is the same packed as it is unpacked), then the root doesn't
 * need a temporary buffer -- we can combine the operands directly
 * from the shared memory segments to the root's rbuf.  Otherwise, we
 * need a receive convertor and receive each fragment into a temporary
 * buffer where we can combine that operan with the root's rbuf.
 *
 * In general, there are two loops:
 *
 * 1. loop over all fragments (which must be done in units of an
 * integer number of datatypes -- remember that if this function is
 * called, we know that the datattype is smaller than the max size of
 * a fragment, so this is definitely possible)
 *
 * 2. loop over all the processes -- 0 to (comm_size-1).  
 * For process 0:
 * - if the root==0, copy the *entire* buffer (i.e., don't copy
 *   fragment by fragment -- might as well copy the entire thing) the
 *   first time through the algorithm, and no-op every other time
 * - else, copy from the shmem fragment to the out buffer
 * For all other proceses:
 * - if root==i, combine the relevant fragment from the sbuf to the
 *   relevant fragment on the rbuf
 * - else, if the datatype is friendly, combine relevant fragment from
 *   the shmem segment to the relevant fragment in the rbuf.  Otherwise,
 *   use the convertor to copy the fragment out of shmem into a temp
 *   buffer and do the combination from there to the rbuf.
 *
 * If we don't have a friendly datatype, then free the temporary
 * buffer at the end.
 */
static int reduce_inorder(void *sbuf, void* rbuf, int count, 
                          struct ompi_datatype_t *dtype, 
                          struct ompi_op_t *op, 
                          int root, struct ompi_communicator_t *comm)
{
    struct iovec iov;
    mca_coll_base_comm_t *data = comm->c_coll_selected_data;
    int ret, rank, size;
    int flag_num, segment_num, max_segment_num;
    size_t total_size, max_data, bytes;
    mca_coll_sm_in_use_flag_t *flag;
    ompi_convertor_t convertor;
    mca_coll_base_mpool_index_t *index;
    size_t ddt_size;
    size_t segment_ddt_count, segment_ddt_bytes, zero = 0;

    /* Setup some identities */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* Figure out how much we should have the convertor copy.  We need
       to have it be in units of a datatype -- i.e., we only want to
       copy a whole datatype worth of data or none at all (we've
       already guaranteed above that the datatype is not larger than a
       segment, so we'll at least get 1). */

    ompi_ddt_type_size(dtype, &ddt_size);
    segment_ddt_count = mca_coll_sm_component.sm_fragment_size / ddt_size;
    iov.iov_len = segment_ddt_bytes = segment_ddt_count * ddt_size;
    total_size = ddt_size * count;

    bytes = 0;

    /* Only have one top-level decision as to whether I'm the root or
       not.  Do this at the slight expense of repeating a little logic
       -- but it's better than a conditional branch in every loop
       iteration. */

    /*********************************************************************
     * Root
     *********************************************************************/
    
    if (root == rank) {
        char *reduce_temp_buffer, *free_buffer, *reduce_target;
        ptrdiff_t true_lb, true_extent, lb, extent;
        char *inplace_temp;
        int peer;
        size_t count_left = (size_t)count;
        int frag_num = 0;
        bool first_operation = true;
        
        /* If the datatype is the same packed as it is unpacked, we
           can save a memory copy and just do the reduction operation
           directly from the shared memory segment.  However, if the
           representation is not the same, then we need to get a
           receive convertor and a temporary buffer to receive
           into. */
        
        ompi_ddt_get_extent(dtype, &lb, &extent);
        ompi_ddt_get_true_extent(dtype, &true_lb, &true_extent);
        if (ompi_ddt_is_contiguous_memory_layout(dtype, count)) {
            reduce_temp_buffer = free_buffer = NULL;
        } else {
            OBJ_CONSTRUCT(&convertor, ompi_convertor_t);

            /* See lengthy comment in coll basic reduce about
               explanation for how to malloc the extra buffer.  Note
               that we do not need a buffer big enough to hold "count"
               instances of the datatype (i.e., big enough to hold the
               entire user buffer) -- we only need to be able to hold
               "segment_ddt_count" instances (i.e., the number of
               instances that can be held in a single fragment) */
            
            free_buffer = (char*)malloc(true_extent + 
                                        (segment_ddt_count - 1) * extent);
            if (NULL == free_buffer) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            reduce_temp_buffer = free_buffer - lb;
            
            /* Trickery here: we use a potentially smaller count than
               the user count -- use the largest count that is <=
               user's count that will fit within a single segment. */
            
            if (OMPI_SUCCESS != 
                (ret = ompi_convertor_copy_and_prepare_for_recv(ompi_mpi_local_convertor,
                                                                dtype,
                                                                segment_ddt_count, 
                                                                reduce_temp_buffer,
                                                                0,
                                                                &convertor))) {
                free(free_buffer);
                return ret;
            }
        }

        /* If we're a) doing MPI_IN_PLACE (which means we're the root
           -- wouldn't have gotten down here with MPI_IN_PLACE if we
           weren't the root), and b) we're not rank 0, then we need to
           copy the rbuf into a temporary buffer and use that as the
           sbuf */

        if (MPI_IN_PLACE == sbuf && 0 != rank) {
            inplace_temp = (char*)malloc(true_extent + (count - 1) * extent);
            if (NULL == inplace_temp) {
                if (NULL != free_buffer) {
                    free(free_buffer);
                }
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            sbuf = inplace_temp - lb;
        } else {
            inplace_temp = NULL;
        }
               
        /* Main loop over receiving / reducing fragments */

        do {
            flag_num = (data->mcb_operation_count % 
                        mca_coll_sm_component.sm_comm_num_in_use_flags);
            FLAG_SETUP(flag_num, flag, data);
            FLAG_WAIT_FOR_IDLE(flag);
            FLAG_RETAIN(flag, size, data->mcb_operation_count);
            ++data->mcb_operation_count;

            /* Loop over all the segments in this set */
            
            segment_num = 
                flag_num * mca_coll_sm_component.sm_segs_per_inuse_flag;
            max_segment_num = 
                (flag_num + 1) * mca_coll_sm_component.sm_segs_per_inuse_flag;
            reduce_target = (((char*) rbuf) + (frag_num * segment_ddt_bytes));
            do {

                /* Process 0 (special case) */

                if (rank == 0) {
                    /* If we're the root *and* the first process to be
                       combined *and* this is the first segment in the
                       entire algorithm, then just copy the whole
                       buffer.  That way, we never need to copy from
                       this process again (i.e., do the copy all at
                       once since all the data is local, and then
                       don't worry about it for the rest of the
                       algorithm) */
                    if (first_operation) {
                        first_operation = false;
                        if (MPI_IN_PLACE != sbuf) {
                            ompi_ddt_copy_content_same_ddt(dtype,
                                                           count,
                                                           reduce_target, (char*)sbuf);
                            D(("root copied entire buffer to rbuf (contig ddt, count %d) FIRST OPERATION\n", count));
                        }
                    }
                } else {
                    index = &(data->mcb_mpool_index[segment_num]);
                    PARENT_WAIT_FOR_NOTIFY_SPECIFIC(0, rank, index, max_data);
                        
                    /* If we don't need an extra buffer, memcpy the
                       fragment straight to the output buffer.
                       Otherwise, unpack. */
                        
                    if (NULL == free_buffer) {
                        D(("root: special case -- copy from rank 0 shemem to reduce_target (%d bytes)\n", max_data));
                        memcpy(reduce_target, index->mcbmi_data, max_data);
                    } else {
                        /* This is somethat inefficient -- should be
                           able to avoid one of the memory copies
                           here, but doing so would violate an
                           abstraction barrier in the convertor (i.e.,
                           directly manipulate some of the private
                           data on the convertor struct) */
                        D(("root: special case -- unpack and copy from rank 0 to reduce_target\n"));
                        COPY_FRAGMENT_OUT(convertor, 0, index, 
                                          iov, max_data);
                        ompi_convertor_set_position(&convertor, &zero);
                        
                        ompi_ddt_copy_content_same_ddt(dtype, 
                                                       max_data / ddt_size,
                                                       reduce_target,
                                                       iov.iov_base);
                    }
                }

                /* Loop over all the remaining processes, receiving
                   and reducing them in order */

                for (peer = 1; peer < size; ++peer) {

                    /* Handle the case where the source is this process */

                    if (rank == peer) {
                        /* Otherwise, I'm not the first process, so
                           instead of copying, combine in the next
                           fragment */
                        D(("root combining fragment from shmem (contig ddt): count %d (left %d, seg %d)\n", min(count_left, segment_ddt_count), count_left, segment_ddt_count));
                        ompi_op_reduce(op, 
                                       ((char *) sbuf) +
                                       frag_num * segment_ddt_bytes,
                                       reduce_target,
                                       min(count_left, segment_ddt_count),
                                       dtype);
                    }

                    /* Now handle the case where the source is not
                       this process.  Wait for the process to copy to
                       the segment. */

                    else {
                        index = &(data->mcb_mpool_index[segment_num]);
                        PARENT_WAIT_FOR_NOTIFY_SPECIFIC(peer, rank, 
                                                        index, max_data);
                        
                        /* If we don't need an extra buffer, then do the
                           reduction operation on the fragment straight
                           from the shmem. */
                        
                        if (NULL == free_buffer) {
                            D(("root combining %d elements in shmem from peer %d\n",
                               max_data / ddt_size, peer));
                            ompi_op_reduce(op,
                                           (index->mcbmi_data + 
                                            (peer * mca_coll_sm_component.sm_fragment_size)),
                                           reduce_target, max_data / ddt_size,
                                           dtype);
                        }
                        
                        /* Otherwise, unpack the fragment to the temporary
                           buffer and then do the reduction from there */
                        
                        else {
                            D(("root combining %d elements in copy out buffer from peer %d\n",
                               max_data / ddt_size, peer));
                            /* Unpack the fragment into my temporary
                               buffer */
                            COPY_FRAGMENT_OUT(convertor, peer, index, 
                                              iov, max_data);
                            ompi_convertor_set_position(&convertor, &zero);
                            
                            /* Do the reduction on this fragment */
                            ompi_op_reduce(op, reduce_temp_buffer,
                                           reduce_target, 
                                           max_data / ddt_size,
                                           dtype);
                        }
                    } /* whether this process was me or not */
                } /* loop over all proceses */

                /* We've iterated through all the processes -- now we
                   move on to the next segment */

                count_left -= segment_ddt_count;
                bytes += segment_ddt_bytes;
                ++segment_num;
                ++frag_num;
                reduce_target += segment_ddt_bytes;
            } while (bytes < total_size && segment_num < max_segment_num);

            /* Root is now done with this set of segments */
            FLAG_RELEASE(flag);
        } while (bytes < total_size);

        /* Kill the convertor, if we had one */
        
        if (NULL != free_buffer) {
            OBJ_DESTRUCT(&convertor);
            free(free_buffer);
        }
        if (NULL != inplace_temp) {
            free(inplace_temp);
        }
    }

    /*********************************************************************
     * Non-root
     *********************************************************************/

    else {

        /* Here we get a convertor for the full count that the user
           provided (as opposed to the convertor that the root got) */

        OBJ_CONSTRUCT(&convertor, ompi_convertor_t);
        if (OMPI_SUCCESS != 
            (ret = 
             ompi_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                                                      dtype,
                                                      count, 
                                                      sbuf,
                                                      0,
                                                      &convertor))) {
            return ret;
        }

        /* Loop over sending fragments to the root */
        
        do {
            flag_num = (data->mcb_operation_count % 
                        mca_coll_sm_component.sm_comm_num_in_use_flags);

            /* Wait for the root to mark this set of segments as
               ours */
            FLAG_SETUP(flag_num, flag, data);
            FLAG_WAIT_FOR_OP(flag, data->mcb_operation_count);
            ++data->mcb_operation_count;

            /* Loop over all the segments in this set */

            segment_num = 
                flag_num * mca_coll_sm_component.sm_segs_per_inuse_flag;
            max_segment_num = 
                (flag_num + 1) * mca_coll_sm_component.sm_segs_per_inuse_flag;
            do {
                index = &(data->mcb_mpool_index[segment_num]);

                /* Copy from the user's buffer to my shared mem
                   segment */
                COPY_FRAGMENT_IN(convertor, index, rank, iov, max_data);
                bytes += max_data;

                /* Wait for the write to absolutely complete */
                opal_atomic_wmb();
                
                /* Tell my parent (always the reduction root -- we're
                   ignoring the mcb_tree parent/child relationships
                   here) that this fragment is ready */
                CHILD_NOTIFY_PARENT(rank, root, index, max_data);

                ++segment_num;
            } while (bytes < total_size && segment_num < max_segment_num);

            /* We're finished with this set of segments */
            FLAG_RELEASE(flag);
        } while (bytes < total_size);

        /* Kill the convertor */

        OBJ_DESTRUCT(&convertor);
    }

    /* All done */

    return OMPI_SUCCESS;
}


#if WANT_REDUCE_NO_ORDER
/**
 * Unordered shared memory reduction.
 *
 * This function performs the reduction in whatever order the operands
 * arrive.
 */
static int reduce_no_order(void *sbuf, void* rbuf, int count, 
                           struct ompi_datatype_t *dtype, 
                           struct ompi_op_t *op, 
                           int root, struct ompi_communicator_t *comm)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
#endif

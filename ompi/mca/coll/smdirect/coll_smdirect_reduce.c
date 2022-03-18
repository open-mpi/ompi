/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/datatype/opal_convertor.h"
#include "opal/sys/atomic.h"
#include "opal/util/minmax.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/op/op.h"
#include "coll_smdirect.h"

#include "opal/mca/smsc/smsc.h"


/*
 * Local functions
 */
static int reduce_inorder(const void *sbuf, void* rbuf, int count,
                          struct ompi_datatype_t *dtype,
                          struct ompi_op_t *op,
                          int root, struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module);

static int reduce_inorder_map(const void *sbuf, void* rbuf, int count,
                              struct ompi_datatype_t *dtype,
                              struct ompi_op_t *op,
                              int root, struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module);
#define WANT_REDUCE_NO_ORDER 0
#if WANT_REDUCE_NO_ORDER
static int reduce_no_order(const void *sbuf, void* rbuf, int count,
                           struct ompi_datatype_t *dtype,
                           struct ompi_op_t *op,
                           int root, struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);
#endif

/**
 * Shared memory reduction.
 *
 * Simply farms out to the associative or non-associative functions.
 */
int mca_coll_smdirect_reduce_intra(const void *sbuf, void* rbuf, int count,
                             struct ompi_datatype_t *dtype,
                             struct ompi_op_t *op,
                             int root, struct ompi_communicator_t *comm,
                             mca_coll_base_module_t *module)
{
    size_t size;
    mca_coll_smdirect_module_t *sm_module = (mca_coll_smdirect_module_t*) module;


    /* We currently only support predefined datatypes and ops.
     * We know that only predefined datatypes can be used with predefined ops.
     */

    if (!ompi_op_is_intrinsic(op)) {
        return sm_module->previous_reduce(sbuf, rbuf, count,
                                          dtype, op, root, comm,
                                          sm_module->previous_reduce_module);
    }

    /* There are several possibilities:
     *
     * 0. If the datatype is larger than a segment, fall back to
     *    underlying module
     * 1. If the op is user-defined, use the strict order
     * 2. If the op is intrinsic:
     *    a. If the op is float-associative, use the unordered
     *    b. If the op is not float-associative:
     *       i. if the data is floating point, use the strict order
     *       ii. if the data is not floating point, use the unordered
     */

    ompi_datatype_type_size(dtype, &size);
    if ((int)size > mca_coll_smdirect_component.sm_control_size) {
        return sm_module->previous_reduce(sbuf, rbuf, count,
                                          dtype, op, root, comm,
                                          sm_module->previous_reduce_module);
    }
#if WANT_REDUCE_NO_ORDER
    else {
        /* Lazily enable the module the first time we invoke a
           collective on it */
        if (!sm_module->enabled) {
            if (OMPI_SUCCESS !=
                (ret = ompi_coll_smdirect_lazy_enable(module, comm))) {
                return ret;
            }
        }

        if (!ompi_op_is_intrinsic(op) ||
            (ompi_op_is_intrinsic(op) && !ompi_op_is_float_assoc(op) &&
             0 != (dtype->flags & OMPI_DATATYPE_FLAG_DATA_FLOAT))) {
            return reduce_inorder(sbuf, rbuf, count, dtype, op,
                                  root, comm, module);
        } else {
            return reduce_no_order(sbuf, rbuf, count, dtype, op,
                                   root, comm, module);
        }
    }
#else
    else {
        /* Lazily enable the module the first time we invoke a
           collective on it */
        if (!sm_module->enabled) {
            int ret;

            if (OMPI_SUCCESS !=
                (ret = ompi_coll_smdirect_lazy_enable(module, comm))) {
                return ret;
            }
        }

        return reduce_inorder_map(sbuf, rbuf, count, dtype, op, root, comm, module);
    }
#endif
}


/**
 * In-order shared memory reduction using mapped memory.
 *
 * Each child's output memory is mapped into our memory space and
 * accessed directly, avoiding unnecessary copies of data.
 *
 * For each segment, we combine (reduce) the data of child 0...num_children
 * into our result buffer and signal to our parent once the data is available.
 * While our parent consumes a segment, we will start processing
 * the next segment.
 * At then end, we have to wait for our parent to complete consuming all our
 * segments before we can return.
 * Leaf nodes successively mark segments as complete, since they do not
 * perform any reduction operations.
 */

static int reduce_inorder_map(const void *sbuf, void* rbuf, int count,
                              struct ompi_datatype_t *dtype,
                              struct ompi_op_t *op,
                              int root, struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    mca_coll_smdirect_module_t *sm_module = (mca_coll_smdirect_module_t*) module;
    mca_coll_smdirect_comm_t *data = sm_module->sm_comm_data;
    size_t ddt_size;
    ptrdiff_t extent;
    char *tmpbuf = NULL;

    mca_coll_smdirect_tree_node_t *me, **children, *parent;

    /* Setup some identities */

    const int rank = ompi_comm_rank(comm);
    const int size = ompi_comm_size(comm);

    /* ddt_size is the packed size (e.g., MPI_SHORT_INT is 6) */
    ompi_datatype_type_size(dtype, &ddt_size);
    /* extent is from lb to ub (e.g., MPI_SHORT_INT is 8) */
    ompi_datatype_type_extent(dtype, &extent);
    /* ddts per segment, or 1 if ddt size exceeds fragment size */
    const size_t segment_ddt_count = (mca_coll_smdirect_component.sm_fragment_size + ddt_size - 1) / ddt_size;
    const size_t segment_ddt_bytes = segment_ddt_count * extent;
    const size_t num_segments = (count + segment_ddt_count - 1) / segment_ddt_count;
    const size_t total_extent = count * extent;
    const size_t control_size = mca_coll_smdirect_component.sm_control_size;

    /* get the current operation */
    int op_count = ++data->mcb_operation_count;


    me = &data->mcb_tree[(rank + size - root) % size];
    children = me->mcstn_children;
    parent = me->mcstn_parent;
    const int num_children = me->mcstn_num_children;

    if (root != rank && num_children > 0) {
        /* double buffering: while our parent reads a segment we
         * can compute the next one */
        tmpbuf = malloc(segment_ddt_bytes*2);
    }

    /* wait for processes from the previous op to finish */
    FLAG_WAIT_FOR_IDLE(&data->procdata->mcsp_op_flag);

    /* set our input buffer information */
    if (0 == num_children) {
        /* leafs provide the full input buffer */
        data->procdata->mcsp_indata = (void*)sbuf;
        data->procdata->mcsp_insize = total_extent;
    } else {
        /* inner nodes provide the temporary buffer spanning one segment */
        data->procdata->mcsp_indata = tmpbuf;
        data->procdata->mcsp_insize = segment_ddt_bytes*2;
    }

    /* initialize the segment flag so that our parent blocks until
     * we completed the first segment */
    FLAG_RETAIN(&data->procdata->mcsp_segment_up_flag, 0, -1);

    /* signal that our procdata for this op is ready */
    opal_atomic_wmb();
    /* we will wait for our parent to signal completion */
    FLAG_RETAIN(&data->procdata->mcsp_op_flag, (NULL != parent ? 1 : 0), op_count);

    /**
     * Get our children's SMSC endpoint and procdata
     */
    mca_coll_smdirect_peerdata_t *peerdata = data->peerdata;
    for (int i = 0; i < num_children; ++i) {
        mca_coll_smdirect_peerdata_t *peer = &peerdata[i];
        int peerid = children[i]->mcstn_id;
        if (NULL == (peer->endpoint = data->endpoints[peerid])) {
            peer->endpoint = MCA_SMSC_CALL(get_endpoint, (&ompi_comm_peer_lookup(comm, peerid)->super));
            data->endpoints[peerid] = peer->endpoint;
        }
        peer->procdata = (mca_coll_smdirect_procdata_t *)(data->sm_bootstrap_meta->module_data_addr
                                                          + control_size * peerid);
        /* make sure we're all on the same op */
        FLAG_WAIT_FOR_OP(&peer->procdata->mcsp_op_flag, op_count);
        opal_atomic_rmb();
        /* map the children's memory region */
        peer->mapping_ctx = MCA_SMSC_CALL(map_peer_region,
                                          peer->endpoint,
                                          0,
                                          peer->procdata->mcsp_indata,
                                          peer->procdata->mcsp_insize,
                                          &peer->mapping_ptr);
        assert(peer->mapping_ptr != NULL);
        assert(peer->mapping_ctx != NULL);
        peer->num_children = children[i]->mcstn_num_children;
    }

    /**
     * The standard asks MPI implementations to ensure that results are reproducible,
     * so always reduce in the order 0...num_children
     */
    if (num_children > 0) {
        int32_t segment_id;
        size_t count_left;
        for (segment_id = 0, count_left = count;
             segment_id < num_segments;
             segment_id++, count_left -= segment_ddt_count) {
            /* figure out the buffers to use */
            void *reduce_target;
            void *reduce_source1 = ((char *) sbuf) + segment_id * segment_ddt_bytes;
            size_t segcount = opal_min(count_left, segment_ddt_count);
            if (rank != root) {
                reduce_target  = tmpbuf + (segment_id % 2)*(segment_ddt_bytes);
            } else {
                /* root reduces directly into the receive buffer */
                reduce_target = ((char*)rbuf) + (segment_id * segment_ddt_bytes);
                if (MPI_IN_PLACE == sbuf) {
                    /* root with in-place takes data from the receive buffer */
                    reduce_source1 = reduce_target;
                }
            }
            for (int i = 0; i < num_children; ++i) {
                mca_coll_smdirect_peerdata_t *peer = &peerdata[i];
                /* wait for this peer to make their data available */
                FLAG_WAIT_FOR_OP(&peer->procdata->mcsp_segment_up_flag, segment_id);
                opal_atomic_rmb();
                /* handle segment data from this peer */
                void *peer_ptr = ((char*)peer->mapping_ptr) + (segment_id % 2)*(segment_ddt_bytes);
                if (peer->num_children == 0) {
                    /* peers without children expose their full send buffer */
                    peer_ptr = ((char*)peer->mapping_ptr) + segment_id * segment_ddt_bytes;
                }
                if (reduce_target == reduce_source1) {
                    /* Some of our input are already in the output buffer */
                    ompi_op_reduce(op, peer_ptr, reduce_target, segcount, dtype);
                } else {
                    /* with the first child, we can use the 3buff variant */
                    ompi_3buff_op_reduce(op, reduce_source1, peer_ptr, reduce_target, segcount, dtype);
                    /* after the first child we continue only with the reduce_target */
                    reduce_source1 = reduce_target;
                }
                /* tell peer that we're done reading this segment */
                opal_atomic_wmb();
                FLAG_RELEASE(&peer->procdata->mcsp_segment_up_flag);
            }
            opal_atomic_wmb();
            if (rank != root) {
                /* wait for our parent to complete reading the previous segment */
                FLAG_WAIT_FOR_IDLE(&data->procdata->mcsp_segment_up_flag);
                /* signal that the new segment is available */
                FLAG_RETAIN(&data->procdata->mcsp_segment_up_flag, 1, segment_id);
            }
        }
        if (rank != root) {
            /* wait for the last segment to be consumed */
            FLAG_WAIT_FOR_IDLE(&data->procdata->mcsp_segment_up_flag);
        }
    } else {
        /* progressively signal that our data is available and wait for them to be consumed */
        for (int32_t segment_id = 0; segment_id < num_segments; ++segment_id) {
            FLAG_RETAIN(&data->procdata->mcsp_segment_up_flag, 1, segment_id);
            FLAG_WAIT_FOR_IDLE(&data->procdata->mcsp_segment_up_flag);
        }
    }

    /* tell children that we're done and unmap their regions */
    for (int i = 0; i < num_children; ++i) {
        /* we're done with this peer */
        FLAG_RELEASE(&peerdata[i].procdata->mcsp_op_flag);
        MCA_SMSC_CALL(unmap_peer_region, peerdata[i].mapping_ctx);
    }
    free(tmpbuf);

    return OMPI_SUCCESS;
}

#if WANT_REDUCE_NO_ORDER
/**
 * Unordered shared memory reduction.
 *
 * This function performs the reduction in whatever order the operands
 * arrive.
 */
static int reduce_no_order(const void *sbuf, void* rbuf, int count,
                           struct ompi_datatype_t *dtype,
                           struct ompi_op_t *op,
                           int root, struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
#endif

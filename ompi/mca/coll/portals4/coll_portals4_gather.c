/*
 * Copyright (c) 2015      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/util/bit_ops.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

#include "coll_portals4.h"
#include "coll_portals4_request.h"


#undef RTR_USES_TRIGGERED_PUT


#define VRANK(ra, ro, si) ((ra - ro + si) % si)

/*
 * Borrowed with thanks from the coll-tuned component, then modified for Portals4.
 *
 *
 * Constructs in-order binomial tree which can be used for gather/scatter
 * operations.
 *
 * Here are some of the examples of this tree:
 * size = 2                    size = 4                 size = 8
 *      0                           0                        0
 *     /                          / |                      / | \
 *    1                          1  2                     1  2  4
 *                                  |                        |  | \
 *                                  3                        3  5  6
 *                                                                 |
 *                                                                 7
 *
 * size = 16
 *      0
 *    / | \        \
 *   1  2  4        8
 *      |  | \    / | \
 *      3  5  6  9  10 12
 *            |     |  | \
 *            7     11 13 14
 *                        |
 *                        15
 */
static ompi_coll_portals4_tree_t*
ompi_coll_portals4_build_in_order_bmtree( struct ompi_communicator_t* comm,
                                            int root )
{
    int childs = 0, rank, vrank, vparent, size, mask = 1, remote, i;
    ompi_coll_portals4_tree_t *bmtree;

    /*
     * Get size and rank of the process in this communicator
     */
    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    vrank = VRANK(rank, root, size);

    bmtree = (ompi_coll_portals4_tree_t*)malloc(sizeof(ompi_coll_portals4_tree_t));
    if (!bmtree) {
        opal_output(ompi_coll_base_framework.framework_output,
                    "coll:portals4:build_bmtree PANIC out of memory");
        return NULL;
    }

    bmtree->tree_bmtree   = 1;
    bmtree->tree_root     = MPI_UNDEFINED;
    bmtree->tree_nextsize = MPI_UNDEFINED;
    for(i=0;i<MAXTREEFANOUT;i++) {
        bmtree->tree_next[i] = -1;
    }

    if (root == rank) {
        bmtree->tree_prev = root;
    }

    while (mask < size) {
        remote = vrank ^ mask;
        if (remote < vrank) {
            bmtree->tree_prev = (remote + root) % size;
            break;
        } else if (remote < size) {
            bmtree->tree_next[childs] = (remote + root) % size;
            childs++;
            if (childs==MAXTREEFANOUT) {
                opal_output(ompi_coll_base_framework.framework_output,
                             "coll:portals4:build_bmtree max fanout incorrect %d needed %d",
                             MAXTREEFANOUT, childs);
                return NULL;
            }
        }
        mask <<= 1;
    }
    bmtree->tree_nextsize = childs;
    bmtree->tree_root     = root;

    vparent = VRANK(bmtree->tree_prev, root, size);
    if (root == rank) {
        bmtree->tree_numdescendants = size - 1;
    } else if (bmtree->tree_nextsize > 0) {
        int possible_descendants = vrank - vparent - 1;
        if ((vrank + possible_descendants) > size) {
            bmtree->tree_numdescendants = size - vrank - 1;
        } else {
            bmtree->tree_numdescendants = possible_descendants;
        }
    } else {
        bmtree->tree_numdescendants = 0;
    }

    opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                        "%d: bmtree result - size(%d)  rank(%d)  vrank(%d)  root(%d)  parent(%d) vparent(%d)  numkids(%d)  numdescendants(%d)",
                        __LINE__,
                        size, rank, vrank, bmtree->tree_root, bmtree->tree_prev, vparent, bmtree->tree_nextsize, bmtree->tree_numdescendants);

    return bmtree;
}

/*
 * Borrowed with thanks from the coll-tuned component.
 */
static int
ompi_coll_portals4_destroy_tree( ompi_coll_portals4_tree_t** tree )
{
    ompi_coll_portals4_tree_t *ptr;

    if ((!tree)||(!*tree)) {
        return OMPI_SUCCESS;
    }

    ptr = *tree;

    free (ptr);
    *tree = NULL;   /* mark tree as gone */

    return OMPI_SUCCESS;
}


static int
setup_gather_buffers_binomial(struct ompi_communicator_t   *comm,
                              ompi_coll_portals4_request_t *request,
                              mca_coll_portals4_module_t   *portals4_module)
{
    int ret, line;

    uint32_t iov_count = 1;
    struct iovec iov;
    size_t max_data;

    ompi_coll_portals4_tree_t* bmtree = portals4_module->cached_in_order_bmtree;

    int vrank = VRANK(request->u.gather.my_rank, request->u.gather.root_rank, request->u.gather.size);

    ompi_coll_portals4_create_send_converter (&request->u.gather.send_converter,
                                              request->u.gather.pack_src_buf + request->u.gather.pack_src_offset,
                                              ompi_comm_peer_lookup(comm, request->u.gather.my_rank),
                                              request->u.gather.pack_src_count,
                                              request->u.gather.pack_src_dtype);
    opal_convertor_get_packed_size(&request->u.gather.send_converter, &request->u.gather.packed_size);

    /**********************************/
    /* Setup Gather Buffers           */
    /**********************************/
    if (vrank == 0) {
        request->u.gather.gather_bytes=request->u.gather.packed_size * (ptrdiff_t)request->u.gather.size;

        /*
         * root node, needs to allocate temp buffer to gather
         * packed bytes from all nodes including self.
         * rotate will occur after transfer during unpack.
         */
        request->u.gather.gather_buf = (char *) malloc(request->u.gather.gather_bytes);
        if (NULL == request->u.gather.gather_buf) {
            ret = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
        }
        request->u.gather.free_after = 1;

        /* pack local data into request->u.gather.gather_buf */
        iov.iov_len = request->u.gather.gather_bytes;
        iov.iov_base = (IOVBASE_TYPE *) request->u.gather.gather_buf;
        opal_convertor_pack(&request->u.gather.send_converter, &iov, &iov_count, &max_data);

        opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                            "%s:%d:vrank(%d): root - gather_buf(%p) - gather_bytes(%lu)=packed_size(%ld) * size(%d)",
                            __FILE__, __LINE__, vrank,
                            request->u.gather.gather_buf, request->u.gather.gather_bytes,
                            request->u.gather.packed_size, request->u.gather.size);
    } else if (bmtree->tree_nextsize) {
        /*
         * other non-leaf nodes, allocate temp buffer to receive data from
         * children.  we need space for data from tree_numdescendants + 1
         * processes.
         */
        request->u.gather.gather_bytes=request->u.gather.packed_size * ((ptrdiff_t)bmtree->tree_numdescendants + 1);

        request->u.gather.gather_buf = (char *) malloc(request->u.gather.gather_bytes);
        if (NULL == request->u.gather.gather_buf) {
            ret = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
        }
        request->u.gather.free_after = 1;

        iov.iov_len = request->u.gather.gather_bytes;
        iov.iov_base = (IOVBASE_TYPE *) request->u.gather.gather_buf;
        opal_convertor_pack(&request->u.gather.send_converter, &iov, &iov_count, &max_data);

        opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                            "%s:%d:vrank(%d): nonleaf - gather_buf(%p) - gather_bytes(%lu)=packed_size(%ld) * (bmtree->tree_numdescendants(%d) + 1)",
                            __FILE__, __LINE__, vrank,
                            request->u.gather.gather_buf, request->u.gather.gather_bytes,
                            request->u.gather.packed_size, bmtree->tree_numdescendants);
    } else {
        /* leaf nodes, allocate space to pack into and put from */
        request->u.gather.gather_bytes=request->u.gather.packed_size;

        request->u.gather.gather_buf = (char *) malloc(request->u.gather.gather_bytes);
        if (NULL == request->u.gather.gather_buf) {
            ret = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
        }
        request->u.gather.free_after = 1;

        iov.iov_len = request->u.gather.gather_bytes;
        iov.iov_base = (IOVBASE_TYPE *) request->u.gather.gather_buf;
        opal_convertor_pack(&request->u.gather.send_converter, &iov, &iov_count, &max_data);

        opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                            "%s:%d:vrank(%d): leaf - gather_buf(%p) - gather_bytes(%lu)=packed_size(%ld)",
                            __FILE__, __LINE__, vrank,
                            request->u.gather.gather_buf, request->u.gather.gather_bytes,
                            request->u.gather.packed_size);
    }

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

static int
setup_gather_buffers_linear(struct ompi_communicator_t   *comm,
                            ompi_coll_portals4_request_t *request,
                            mca_coll_portals4_module_t   *portals4_module)
{
    int ret, line;

    uint32_t iov_count = 1;
    struct iovec iov;
    size_t max_data;

    int8_t i_am_root = (request->u.gather.my_rank == request->u.gather.root_rank);

    ompi_coll_portals4_create_send_converter (&request->u.gather.send_converter,
                                              request->u.gather.pack_src_buf + request->u.gather.pack_src_offset,
                                              ompi_comm_peer_lookup(comm, request->u.gather.my_rank),
                                              request->u.gather.pack_src_count,
                                              request->u.gather.pack_src_dtype);
    opal_convertor_get_packed_size(&request->u.gather.send_converter, &request->u.gather.packed_size);

    /**********************************/
    /* Setup Gather Buffers           */
    /**********************************/
    if (i_am_root) {
        request->u.gather.gather_bytes=request->u.gather.packed_size * (ptrdiff_t)request->u.gather.size;

        /*
         * root node, needs to allocate temp buffer to gather
         * packed bytes from all nodes including self.
         */
        request->u.gather.gather_buf = (char *) malloc(request->u.gather.gather_bytes);
        if (NULL == request->u.gather.gather_buf) {
            ret = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
        }
        request->u.gather.free_after = 1;

        /* pack local data into request->u.gather.gather_buf */
        uint64_t gather_buf_offset = (ptrdiff_t)request->u.gather.my_rank * request->u.gather.packed_size;
        iov.iov_len = request->u.gather.gather_bytes - gather_buf_offset;
        iov.iov_base = (IOVBASE_TYPE *) (request->u.gather.gather_buf + gather_buf_offset);
        opal_convertor_pack(&request->u.gather.send_converter, &iov, &iov_count, &max_data);

        opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                            "%s:%d:rank(%d): root - gather_buf(%p) - gather_bytes(%lu)=packed_size(%ld) * size(%d)",
                            __FILE__, __LINE__, request->u.gather.my_rank,
                            request->u.gather.gather_buf, request->u.gather.gather_bytes,
                            request->u.gather.packed_size, request->u.gather.size);
    } else {
        /* non-root nodes, allocate space to pack into and put from */
        request->u.gather.gather_bytes=request->u.gather.packed_size;
        request->u.gather.gather_buf = (char *) malloc(request->u.gather.gather_bytes);
        if (NULL == request->u.gather.gather_buf) {
            ret = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
        }
        request->u.gather.free_after = 1;

        iov.iov_len = request->u.gather.gather_bytes;
        iov.iov_base = (IOVBASE_TYPE *) request->u.gather.gather_buf;
        opal_convertor_pack(&request->u.gather.send_converter, &iov, &iov_count, &max_data);

        opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                            "%s:%d:rank(%d): leaf - gather_buf(%p) - gather_bytes(%lu)=packed_size(%ld)",
                            __FILE__, __LINE__, request->u.gather.my_rank,
                            request->u.gather.gather_buf, request->u.gather.gather_bytes,
                            request->u.gather.packed_size);
    }

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

static int
setup_gather_handles(struct ompi_communicator_t   *comm,
                     ompi_coll_portals4_request_t *request,
                     mca_coll_portals4_module_t   *portals4_module)
{
    int ret, line;

    ptl_me_t  me;

    /**********************************/
    /* Setup Gather Handles           */
    /**********************************/
    COLL_PORTALS4_SET_BITS(request->u.gather.gather_match_bits, ompi_comm_get_cid(comm),
            0, 0, COLL_PORTALS4_GATHER, 0, request->u.gather.coll_count);

    ret = PtlCTAlloc(mca_coll_portals4_component.ni_h,
                     &request->u.gather.gather_cth);
    if (PTL_OK != ret) { ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr; }

    request->u.gather.gather_mdh = mca_coll_portals4_component.data_md_h;
    request->u.gather.gather_offset = (ptl_size_t)request->u.gather.gather_buf;

    /* children put here */
    me.start = request->u.gather.gather_buf;
    me.length = request->u.gather.gather_bytes;
    me.ct_handle = request->u.gather.gather_cth;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
        PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_CT_COMM;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = request->u.gather.gather_match_bits;
    me.ignore_bits = 0;
    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                      mca_coll_portals4_component.pt_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &request->u.gather.gather_meh);
    if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

static int
setup_sync_handles(struct ompi_communicator_t   *comm,
                   ompi_coll_portals4_request_t *request,
                   mca_coll_portals4_module_t   *portals4_module)
{
    int ret, line;

    ptl_me_t  me;

    /**********************************/
    /* Setup Sync Handles             */
    /**********************************/
    COLL_PORTALS4_SET_BITS(request->u.gather.sync_match_bits, ompi_comm_get_cid(comm),
            0, 1, COLL_PORTALS4_GATHER, 0, request->u.gather.coll_count);

    ret = PtlCTAlloc(mca_coll_portals4_component.ni_h,
                     &request->u.gather.sync_cth);
    if (PTL_OK != ret) { ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr; }

    request->u.gather.sync_mdh = mca_coll_portals4_component.zero_md_h;

    me.start = NULL;
    me.length = 0;
    me.ct_handle = request->u.gather.sync_cth;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
        PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_CT_COMM | PTL_ME_EVENT_CT_OVERFLOW;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = request->u.gather.sync_match_bits;
    me.ignore_bits = 0;
    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                      mca_coll_portals4_component.pt_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &request->u.gather.sync_meh);
    if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

static int
cleanup_gather_handles(ompi_coll_portals4_request_t *request)
{
    int ret, line;

    /**********************************/
    /* Cleanup Gather Handles             */
    /**********************************/
    ret = PtlMEUnlink(request->u.gather.gather_meh);
    if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    ret = PtlCTFree(request->u.gather.gather_cth);
    if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

static int
cleanup_sync_handles(ompi_coll_portals4_request_t *request)
{
    int ret, line;

    /**********************************/
    /* Cleanup Sync Handles             */
    /**********************************/
    ret = PtlMEUnlink(request->u.gather.sync_meh);
    if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    ret = PtlCTFree(request->u.gather.sync_cth);
    if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

static int
ompi_coll_portals4_gather_intra_binomial_top(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                             void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                             int root,
                                             struct ompi_communicator_t *comm,
                                             ompi_coll_portals4_request_t *request,
                                             mca_coll_base_module_t *module)
{
    mca_coll_portals4_module_t *portals4_module = (mca_coll_portals4_module_t*) module;
    int ret, line;
    ptl_ct_event_t ct;

    ptl_ct_event_t sync_incr_event;

    int vrank=-1;

    int32_t i=0;

    ompi_coll_portals4_tree_t* bmtree;

    int32_t expected_ops =0;
    int32_t expected_acks=0;


    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra_binomial_top enter rank %d", request->u.gather.my_rank));

    request->type = OMPI_COLL_PORTALS4_TYPE_GATHER;
    request->u.gather.gather_buf=NULL;
    request->u.gather.gather_mdh=PTL_INVALID_HANDLE;
    request->u.gather.gather_cth=PTL_INVALID_HANDLE;
    request->u.gather.gather_meh=PTL_INVALID_HANDLE;
    request->u.gather.sync_mdh=PTL_INVALID_HANDLE;
    request->u.gather.sync_cth=PTL_INVALID_HANDLE;
    request->u.gather.sync_meh=PTL_INVALID_HANDLE;

    request->u.gather.my_rank   = ompi_comm_rank(comm);
    request->u.gather.size      = ompi_comm_size(comm);
    request->u.gather.root_rank = root;
    request->u.gather.sbuf      = sbuf;
    request->u.gather.rbuf      = rbuf;
    if ((root == request->u.gather.my_rank) && (sbuf == MPI_IN_PLACE)) {
        request->u.gather.pack_src_buf   = rbuf;
        request->u.gather.pack_src_count = rcount;
        request->u.gather.pack_src_dtype = rdtype;
    } else {
        request->u.gather.pack_src_buf    = sbuf;
        request->u.gather.pack_src_count  = scount;
        request->u.gather.pack_src_dtype  = sdtype;
        request->u.gather.pack_src_offset = 0;
    }
    ompi_datatype_get_extent(request->u.gather.pack_src_dtype,
                             &request->u.gather.pack_src_lb,
                             &request->u.gather.pack_src_extent);
    ompi_datatype_get_true_extent(request->u.gather.pack_src_dtype,
                                  &request->u.gather.pack_src_true_lb,
                                  &request->u.gather.pack_src_true_extent);
    request->u.gather.unpack_dst_buf   = rbuf;
    request->u.gather.unpack_dst_count = rcount;
    request->u.gather.unpack_dst_dtype = rdtype;
    ompi_datatype_get_extent(request->u.gather.unpack_dst_dtype,
                             &request->u.gather.unpack_dst_lb,
                             &request->u.gather.unpack_dst_extent);
    ompi_datatype_get_true_extent(request->u.gather.unpack_dst_dtype,
                                  &request->u.gather.unpack_dst_true_lb,
                                  &request->u.gather.unpack_dst_true_extent);

    if ((root == request->u.gather.my_rank) && (sbuf == MPI_IN_PLACE)) {
        request->u.gather.pack_src_offset = request->u.gather.pack_src_extent * request->u.gather.pack_src_count * request->u.gather.my_rank;
    }

    opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                        "%s:%d:vrank(%d): request->u.gather.pack_src_offset(%lu)",
                        __FILE__, __LINE__, vrank,
                        request->u.gather.pack_src_offset);

    /**********************************/
    /* Setup Common Parameters        */
    /**********************************/

    request->u.gather.coll_count = opal_atomic_add_size_t(&portals4_module->coll_count, 1);

    COLL_PORTALS4_UPDATE_IN_ORDER_BMTREE( comm, portals4_module, request->u.gather.root_rank );
    bmtree = portals4_module->cached_in_order_bmtree;

    vrank = VRANK(request->u.gather.my_rank, request->u.gather.root_rank, request->u.gather.size);

    ret = setup_gather_buffers_binomial(comm, request, portals4_module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = setup_gather_handles(comm, request, portals4_module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = setup_sync_handles(comm, request, portals4_module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    /***********************************************/
    /* Chain the RTR and Recv-ACK to the Gather CT */
    /***********************************************/
    if (vrank != 0) {
        sync_incr_event.success=1;
        sync_incr_event.failure=0;
        ret = PtlTriggeredCTInc(request->u.gather.gather_cth,
                                sync_incr_event,
                                request->u.gather.sync_cth,
                                1);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
        ret = PtlTriggeredCTInc(request->u.gather.gather_cth,
                                sync_incr_event,
                                request->u.gather.sync_cth,
                                2);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

    /**********************************/
    /* do the gather                  */
    /**********************************/
    if (vrank == 0) {
        /* root, so do nothing */

        expected_ops=bmtree->tree_nextsize; /* gather put from each child */
        expected_acks=0;

    } else {
        int32_t parent = bmtree->tree_prev;
        int32_t vparent = VRANK(parent, request->u.gather.root_rank, request->u.gather.size);

        ptl_size_t remote_offset=(vrank-vparent) * request->u.gather.packed_size;

        opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                            "%s:%d:vrank(%d): remote_offset(%lu)=(vrank(%d)-vparent(%d)) * packed_size(%ld)",
                            __FILE__, __LINE__, vrank,
                            remote_offset, vrank, vparent, request->u.gather.packed_size);

        expected_ops=bmtree->tree_nextsize + 1; /* gather put from each child + a chained RTR */
        expected_acks=1;                        /* Recv-ACK from parent */

        ret = PtlTriggeredPut(request->u.gather.gather_mdh,
                              request->u.gather.gather_offset,
                              request->u.gather.gather_bytes,
                              PTL_NO_ACK_REQ,
                              ompi_coll_portals4_get_peer(comm, parent),
                              mca_coll_portals4_component.pt_idx,
                              request->u.gather.gather_match_bits,
                              remote_offset,
                              NULL,
                              0,
                              request->u.gather.gather_cth,
                              expected_ops);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

    /************************************/
    /* put Recv-ACK to each child       */
    /************************************/
    for (i=0;i<bmtree->tree_nextsize;i++) {
        int32_t child=bmtree->tree_next[i];
        ret = PtlTriggeredPut(request->u.gather.sync_mdh,
                              0,
                              0,
                              PTL_NO_ACK_REQ,
                              ompi_coll_portals4_get_peer(comm, child),
                              mca_coll_portals4_component.pt_idx,
                              request->u.gather.sync_match_bits,
                              0,
                              NULL,
                              0,
                              request->u.gather.gather_cth,
                              expected_ops);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

    expected_ops+=expected_acks;

    if (!request->u.gather.is_sync) {
        /******************************************/
        /* put to finish pt when all ops complete */
        /******************************************/
        ret = PtlTriggeredPut(mca_coll_portals4_component.zero_md_h,
                0,
                0,
                PTL_NO_ACK_REQ,
                ompi_coll_portals4_get_peer(comm, request->u.gather.my_rank),
                mca_coll_portals4_component.finish_pt_idx,
                0,
                0,
                NULL,
                (uintptr_t) request,
                request->u.gather.gather_cth,
                expected_ops);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

#ifdef RTR_USES_TRIGGERED_PUT
    /**********************************/
    /* put RTR to each child          */
    /**********************************/
    for (i=0;i<bmtree->tree_nextsize;i++) {
        int32_t child=bmtree->tree_next[i];
        ret = PtlTriggeredPut(request->u.gather.sync_mdh,
                              0,
                              0,
                              PTL_NO_ACK_REQ,
                              ompi_coll_portals4_get_peer(comm, child),
                              mca_coll_portals4_component.pt_idx,
                              request->u.gather.sync_match_bits,
                              0,
                              NULL,
                              0,
                              request->u.gather.sync_cth,
                              0);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }
#else
    /**********************************/
    /* put RTR to each child          */
    /**********************************/
    for (i=0;i<bmtree->tree_nextsize;i++) {
        int32_t child=bmtree->tree_next[i];
        ret = PtlPut(request->u.gather.sync_mdh,
                     0,
                     0,
                     PTL_NO_ACK_REQ,
                     ompi_coll_portals4_get_peer(comm, child),
                     mca_coll_portals4_component.pt_idx,
                     request->u.gather.sync_match_bits,
                     0,
                     NULL,
                     0);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }
#endif

    if (request->u.gather.is_sync) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "%s:%d:vrank(%d): calling CTWait(expected_ops=%d)\n",
                            __FILE__, __LINE__, vrank, expected_ops);

        /********************************/
        /* Wait for all ops to complete */
        /********************************/
        ret = PtlCTWait(request->u.gather.gather_cth, expected_ops, &ct);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "%s:%d:vrank(%d): completed CTWait(expected_ops=%d)\n",
                            __FILE__, __LINE__, vrank, expected_ops);
    }

    ompi_coll_portals4_destroy_tree(&(portals4_module->cached_in_order_bmtree));

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra_binomial_top exit rank %d", request->u.gather.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    if (NULL != request->u.gather.gather_buf)
        free(request->u.gather.gather_buf);

    ompi_coll_portals4_destroy_tree(&(portals4_module->cached_in_order_bmtree));

    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

static int
ompi_coll_portals4_gather_intra_linear_top(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                           void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                           int root,
                                           struct ompi_communicator_t *comm,
                                           ompi_coll_portals4_request_t *request,
                                           mca_coll_base_module_t *module)
{
    mca_coll_portals4_module_t *portals4_module = (mca_coll_portals4_module_t*) module;
    int ret, line;
    ptl_ct_event_t ct;

    ptl_ct_event_t sync_incr_event;

    int8_t i_am_root;

    int32_t i=0;

    int32_t expected_ops =0;
    int32_t expected_acks=0;


    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra_linear_top enter rank %d", request->u.gather.my_rank));

    request->type = OMPI_COLL_PORTALS4_TYPE_GATHER;
    request->u.gather.gather_buf=NULL;
    request->u.gather.gather_mdh=PTL_INVALID_HANDLE;
    request->u.gather.gather_cth=PTL_INVALID_HANDLE;
    request->u.gather.gather_meh=PTL_INVALID_HANDLE;
    request->u.gather.sync_mdh=PTL_INVALID_HANDLE;
    request->u.gather.sync_cth=PTL_INVALID_HANDLE;
    request->u.gather.sync_meh=PTL_INVALID_HANDLE;

    request->u.gather.my_rank   = ompi_comm_rank(comm);
    request->u.gather.size      = ompi_comm_size(comm);
    request->u.gather.root_rank = root;
    request->u.gather.sbuf      = sbuf;
    request->u.gather.rbuf      = rbuf;
    if ((root == request->u.gather.my_rank) && (sbuf == MPI_IN_PLACE)) {
        request->u.gather.pack_src_buf   = rbuf;
        request->u.gather.pack_src_count = rcount;
        request->u.gather.pack_src_dtype = rdtype;
    } else {
        request->u.gather.pack_src_buf    = sbuf;
        request->u.gather.pack_src_count  = scount;
        request->u.gather.pack_src_dtype  = sdtype;
        request->u.gather.pack_src_offset = 0;
    }
    ompi_datatype_get_extent(request->u.gather.pack_src_dtype,
                             &request->u.gather.pack_src_lb,
                             &request->u.gather.pack_src_extent);
    ompi_datatype_get_true_extent(request->u.gather.pack_src_dtype,
                                  &request->u.gather.pack_src_true_lb,
                                  &request->u.gather.pack_src_true_extent);
    request->u.gather.unpack_dst_buf   = rbuf;
    request->u.gather.unpack_dst_count = rcount;
    request->u.gather.unpack_dst_dtype = rdtype;
    ompi_datatype_get_extent(request->u.gather.unpack_dst_dtype,
                             &request->u.gather.unpack_dst_lb,
                             &request->u.gather.unpack_dst_extent);
    ompi_datatype_get_true_extent(request->u.gather.unpack_dst_dtype,
                                  &request->u.gather.unpack_dst_true_lb,
                                  &request->u.gather.unpack_dst_true_extent);

    if ((root == request->u.gather.my_rank) && (sbuf == MPI_IN_PLACE)) {
        request->u.gather.pack_src_offset = request->u.gather.pack_src_extent * request->u.gather.pack_src_count * request->u.gather.my_rank;
    }

    opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                        "%s:%d:rank(%d): request->u.gather.pack_src_offset(%lu)",
                        __FILE__, __LINE__, request->u.gather.my_rank,
                        request->u.gather.pack_src_offset);

    /**********************************/
    /* Setup Common Parameters        */
    /**********************************/

    i_am_root = (request->u.gather.my_rank == request->u.gather.root_rank);

    request->u.gather.coll_count = opal_atomic_add_size_t(&portals4_module->coll_count, 1);

    ret = setup_gather_buffers_linear(comm, request, portals4_module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = setup_gather_handles(comm, request, portals4_module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = setup_sync_handles(comm, request, portals4_module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    /***********************************************/
    /* Chain the RTR and Recv-ACK to the Gather CT */
    /***********************************************/
    if (!i_am_root) {
        sync_incr_event.success=1;
        sync_incr_event.failure=0;
        ret = PtlTriggeredCTInc(request->u.gather.gather_cth,
                                sync_incr_event,
                                request->u.gather.sync_cth,
                                1);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
        ret = PtlTriggeredCTInc(request->u.gather.gather_cth,
                                sync_incr_event,
                                request->u.gather.sync_cth,
                                2);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

    /**********************************/
    /* do the gather                  */
    /**********************************/
    if (i_am_root) {
        /* root, so do nothing */

        expected_ops=request->u.gather.size-1; /* gather put from all other ranks */
        expected_acks=0;

    } else {
        ptl_size_t remote_offset=request->u.gather.my_rank * request->u.gather.packed_size;

        opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                            "%s:%d:rank(%d): remote_offset(%lu)=rank(%d) * packed_size(%ld)",
                            __FILE__, __LINE__, request->u.gather.my_rank,
                            remote_offset, request->u.gather.my_rank, request->u.gather.packed_size);

        expected_ops=1;  /* chained RTR */
        expected_acks=1; /* Recv-ACK from root */

        ret = PtlTriggeredPut(request->u.gather.gather_mdh,
                              request->u.gather.gather_offset,
                              request->u.gather.gather_bytes,
                              PTL_NO_ACK_REQ,
                              ompi_coll_portals4_get_peer(comm, request->u.gather.root_rank),
                              mca_coll_portals4_component.pt_idx,
                              request->u.gather.gather_match_bits,
                              remote_offset,
                              NULL,
                              0,
                              request->u.gather.gather_cth,
                              expected_ops);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

    /*****************************************/
    /* root puts Recv-ACK to all other ranks */
    /*****************************************/
    if (i_am_root) {
        for (i=0;i<request->u.gather.size;i++) {
            if (i == request->u.gather.root_rank) { continue; }
            ret = PtlTriggeredPut(request->u.gather.sync_mdh,
                                  0,
                                  0,
                                  PTL_NO_ACK_REQ,
                                  ompi_coll_portals4_get_peer(comm, i),
                                  mca_coll_portals4_component.pt_idx,
                                  request->u.gather.sync_match_bits,
                                  0,
                                  NULL,
                                  0,
                                  request->u.gather.gather_cth,
                                  expected_ops);
            if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
        }
    }

    expected_ops+=expected_acks;

    if (!request->u.gather.is_sync) {
        /******************************************/
        /* put to finish pt when all ops complete */
        /******************************************/
        ret = PtlTriggeredPut(mca_coll_portals4_component.zero_md_h,
                0,
                0,
                PTL_NO_ACK_REQ,
                ompi_coll_portals4_get_peer(comm, request->u.gather.my_rank),
                mca_coll_portals4_component.finish_pt_idx,
                0,
                0,
                NULL,
                (uintptr_t) request,
                request->u.gather.gather_cth,
                expected_ops);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

#ifdef RTR_USES_TRIGGERED_PUT
    /************************************/
    /* root puts RTR to all other ranks */
    /************************************/
    if (i_am_root) {
        for (i=0;i<request->u.gather.size;i++) {
            if (i == request->u.gather.root_rank) { continue; }
            ret = PtlTriggeredPut(request->u.gather.sync_mdh,
                                  0,
                                  0,
                                  PTL_NO_ACK_REQ,
                                  ompi_coll_portals4_get_peer(comm, i),
                                  mca_coll_portals4_component.pt_idx,
                                  request->u.gather.sync_match_bits,
                                  0,
                                  NULL,
                                  0,
                                  request->u.gather.sync_cth,
                                  0);
            if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
        }
    }
#else
    /************************************/
    /* root puts RTR to all other ranks */
    /************************************/
    if (i_am_root) {
        for (i=0;i<request->u.gather.size;i++) {
            if (i == request->u.gather.root_rank) { continue; }
            ret = PtlPut(request->u.gather.sync_mdh,
                         0,
                         0,
                         PTL_NO_ACK_REQ,
                         ompi_coll_portals4_get_peer(comm, i),
                         mca_coll_portals4_component.pt_idx,
                         request->u.gather.sync_match_bits,
                         0,
                         NULL,
                         0);
            if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
        }
    }
#endif

    if (request->u.gather.is_sync) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                "calling CTWait(expected_ops=%d)\n", expected_ops);

        /********************************/
        /* Wait for all ops to complete */
        /********************************/
        ret = PtlCTWait(request->u.gather.gather_cth, expected_ops, &ct);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                "completed CTWait(expected_ops=%d)\n", expected_ops);
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra_linear_top exit rank %d", request->u.gather.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    if (NULL != request->u.gather.gather_buf)
        free(request->u.gather.gather_buf);

    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

static int
ompi_coll_portals4_gather_intra_binomial_bottom(struct ompi_communicator_t *comm,
                                                ompi_coll_portals4_request_t *request)
{
    int ret, line;
    int i;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra_binomial_bottom enter rank %d", request->u.gather.my_rank));

    ret = cleanup_gather_handles(request);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = cleanup_sync_handles(request);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    if (request->u.gather.my_rank == request->u.gather.root_rank) {
        uint32_t iov_count = 1;
        struct iovec iov;
        size_t max_data;

        for (i=0;i<request->u.gather.size;i++) {
            uint64_t offset = request->u.gather.unpack_dst_extent * request->u.gather.unpack_dst_count * ((request->u.gather.my_rank + i) % request->u.gather.size);

            opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                                "%s:%d:rank(%d): offset(%lu)",
                                __FILE__, __LINE__, request->u.gather.my_rank,
                                offset);

            ompi_coll_portals4_create_recv_converter (&request->u.gather.recv_converter,
                                                      request->u.gather.unpack_dst_buf + offset,
                                                      ompi_comm_peer_lookup(comm, request->u.gather.my_rank),
                                                      request->u.gather.unpack_dst_count,
                                                      request->u.gather.unpack_dst_dtype);

            iov.iov_len = request->u.gather.packed_size;
            iov.iov_base = (IOVBASE_TYPE *) ((char *)request->u.gather.gather_buf + (request->u.gather.packed_size*i));
            opal_convertor_unpack(&request->u.gather.recv_converter, &iov, &iov_count, &max_data);

            OBJ_DESTRUCT(&request->u.gather.recv_converter);
        }
    }

    if (request->u.gather.free_after)
        free(request->u.gather.gather_buf);

    request->super.req_status.MPI_ERROR = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_complete(&request->super, true);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra_binomial_bottom exit rank %d", request->u.gather.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    request->super.req_status.MPI_ERROR = ret;

    if (request->u.gather.free_after)
        free(request->u.gather.gather_buf);

    opal_output(ompi_coll_base_framework.framework_output,
            "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
            __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

static int
ompi_coll_portals4_gather_intra_linear_bottom(struct ompi_communicator_t *comm,
                                              ompi_coll_portals4_request_t *request)
{
    int ret, line;
    int i;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra_linear_bottom enter rank %d", request->u.gather.my_rank));

    ret = cleanup_gather_handles(request);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = cleanup_sync_handles(request);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    if (request->u.gather.my_rank == request->u.gather.root_rank) {
        uint32_t iov_count = 1;
        struct iovec iov;
        size_t max_data;

        for (i=0;i<request->u.gather.size;i++) {
            ompi_coll_portals4_create_recv_converter (&request->u.gather.recv_converter,
                                                      request->u.gather.unpack_dst_buf + (request->u.gather.unpack_dst_extent*request->u.gather.unpack_dst_count*i),
                                                      ompi_comm_peer_lookup(comm, request->u.gather.my_rank),
                                                      request->u.gather.unpack_dst_count,
                                                      request->u.gather.unpack_dst_dtype);

            iov.iov_len = request->u.gather.packed_size;
            iov.iov_base = (IOVBASE_TYPE *) ((char *)request->u.gather.gather_buf + (request->u.gather.packed_size*i));
            opal_convertor_unpack(&request->u.gather.recv_converter, &iov, &iov_count, &max_data);

            OBJ_DESTRUCT(&request->u.gather.recv_converter);
        }
    }

    if (request->u.gather.free_after)
        free(request->u.gather.gather_buf);

    request->super.req_status.MPI_ERROR = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_complete(&request->super, true);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra_linear_bottom exit rank %d", request->u.gather.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    request->super.req_status.MPI_ERROR = ret;

    if (request->u.gather.free_after)
        free(request->u.gather.gather_buf);

    opal_output(ompi_coll_base_framework.framework_output,
            "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
            __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

int
ompi_coll_portals4_gather_intra(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                int root,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    int ret, line;

    ompi_coll_portals4_request_t *request;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra enter rank %d", ompi_comm_rank(comm)));

    /*
     *  allocate a portals4 request
     */
    OMPI_COLL_PORTALS4_REQUEST_ALLOC(comm, request);
    if (NULL == request) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
    }
    request->u.gather.is_sync = 1;

    /*
     *  initiate the gather
     *
     *  this request is marked synchronous (is_sync==1), so PtlCTWait()
     *  will be called to wait for completion.
     */
    if (1 == mca_coll_portals4_component.use_binomial_gather_algorithm) {
        ret = ompi_coll_portals4_gather_intra_binomial_top(sbuf, scount, sdtype,
                                                           rbuf, rcount, rdtype,
                                                           root,
                                                           comm,
                                                           request,
                                                           module);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

        ret = ompi_coll_portals4_gather_intra_binomial_bottom(comm, request);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }
    } else {
        ret = ompi_coll_portals4_gather_intra_linear_top(sbuf, scount, sdtype,
                                                         rbuf, rcount, rdtype,
                                                         root,
                                                         comm,
                                                         request,
                                                         module);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

        ret = ompi_coll_portals4_gather_intra_linear_bottom(comm, request);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }
    }

    /*
     *  return the portals4 request
     */
    OMPI_COLL_PORTALS4_REQUEST_RETURN(request);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:gather_intra exit rank %d", request->u.gather.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
            "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
            __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}


int
ompi_coll_portals4_igather_intra(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                 void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                 int root,
                                 struct ompi_communicator_t *comm,
                                 ompi_request_t **ompi_request,
                                 mca_coll_base_module_t *module)
{
    int ret, line;

    ompi_coll_portals4_request_t *request;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:igather_intra enter rank %d", ompi_comm_rank(comm)));

    /*
     *  allocate a portals4 request
     */
    OMPI_COLL_PORTALS4_REQUEST_ALLOC(comm, request);
    if (NULL == request) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
    }
    *ompi_request = &request->super;
    request->u.gather.is_sync = 0;

    /*
     *  initiate the gather
     *
     *  this request is marked asynchronous (is_sync==0), so
     *  portals4_progress() will handle completion.
     */
    if (1 == mca_coll_portals4_component.use_binomial_gather_algorithm) {
        ret = ompi_coll_portals4_gather_intra_binomial_top(sbuf, scount, sdtype,
                                                           rbuf, rcount, rdtype,
                                                           root,
                                                           comm,
                                                           request,
                                                           module);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }
    } else {
        ret = ompi_coll_portals4_gather_intra_linear_top(sbuf, scount, sdtype,
                                                         rbuf, rcount, rdtype,
                                                         root,
                                                         comm,
                                                         request,
                                                         module);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:igather_intra exit rank %d", request->u.gather.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
            "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
            __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}


int
ompi_coll_portals4_igather_intra_fini(ompi_coll_portals4_request_t *request)
{
    int ret, line;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:igather_intra_fini enter rank %d", request->u.gather.my_rank));

    /*
     *  cleanup the gather
     */
    if (1 == mca_coll_portals4_component.use_binomial_gather_algorithm) {
        ret = ompi_coll_portals4_gather_intra_binomial_bottom(request->super.req_mpi_object.comm, request);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }
    } else {
        ret = ompi_coll_portals4_gather_intra_linear_bottom(request->super.req_mpi_object.comm, request);
        if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:igather_intra_fini exit rank %d", request->u.gather.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
            "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
            __FILE__, __LINE__, line, ret, request->u.gather.my_rank);

    return ret;
}

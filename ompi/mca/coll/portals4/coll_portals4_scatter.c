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


static int
setup_scatter_buffers_linear(struct ompi_communicator_t   *comm,
                             ompi_coll_portals4_request_t *request,
                             mca_coll_portals4_module_t   *portals4_module)
{
    int ret, line;

    int8_t i_am_root = (request->u.scatter.my_rank == request->u.scatter.root_rank);

    ompi_coll_portals4_create_send_converter (&request->u.scatter.send_converter,
                                              request->u.scatter.pack_src_buf,
                                              ompi_comm_peer_lookup(comm, request->u.scatter.my_rank),
                                              request->u.scatter.pack_src_count,
                                              request->u.scatter.pack_src_dtype);
    opal_convertor_get_packed_size(&request->u.scatter.send_converter, &request->u.scatter.packed_size);
    OBJ_DESTRUCT(&request->u.scatter.send_converter);

    /**********************************/
    /* Setup Scatter Buffers           */
    /**********************************/
    if (i_am_root) {

        /*
         * calculate the total size of the packed data
         */
        request->u.scatter.scatter_bytes=request->u.scatter.packed_size * (ptrdiff_t)request->u.scatter.size;

        /* all transfers done using request->u.scatter.sdtype.
         * allocate temp buffer for recv, copy and/or rotate data at the end */
        request->u.scatter.scatter_buf = (char *) malloc(request->u.scatter.scatter_bytes);
        if (NULL == request->u.scatter.scatter_buf) {
            ret = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
        }
        request->u.scatter.free_after = 1;

        for (int32_t i=0;i<request->u.scatter.size;i++) {
            uint32_t iov_count = 1;
            struct iovec iov;
            size_t max_data;

            uint64_t offset = request->u.scatter.pack_src_extent * request->u.scatter.pack_src_count * i;

            opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                                "%s:%d:rank(%d): offset(%lu)",
                                __FILE__, __LINE__, request->u.scatter.my_rank,
                                offset);

            ompi_coll_portals4_create_send_converter (&request->u.scatter.send_converter,
                                                      request->u.scatter.pack_src_buf + offset,
                                                      ompi_comm_peer_lookup(comm, request->u.scatter.my_rank),
                                                      request->u.scatter.pack_src_count,
                                                      request->u.scatter.pack_src_dtype);

            iov.iov_len = request->u.scatter.packed_size;
            iov.iov_base = (IOVBASE_TYPE *) ((char *)request->u.scatter.scatter_buf + (request->u.scatter.packed_size*i));
            opal_convertor_pack(&request->u.scatter.send_converter, &iov, &iov_count, &max_data);

            OBJ_DESTRUCT(&request->u.scatter.send_converter);
        }

        opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                            "%s:%d:rank(%d): root - scatter_buf(%p) - scatter_bytes(%lu)=packed_size(%ld) * size(%d)",
                            __FILE__, __LINE__, request->u.scatter.my_rank,
                            request->u.scatter.scatter_buf, request->u.scatter.scatter_bytes,
                            request->u.scatter.packed_size, request->u.scatter.size);
    } else {
        request->u.scatter.scatter_bytes=request->u.scatter.packed_size;
        request->u.scatter.scatter_buf = (char *) malloc(request->u.scatter.scatter_bytes);
        if (NULL == request->u.scatter.scatter_buf) {
            ret = OMPI_ERR_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
        }
        request->u.scatter.free_after = 1;

        opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                            "%s:%d:rank(%d): leaf - scatter_buf(%p) - scatter_bytes(%lu)=packed_size(%ld)",
                            __FILE__, __LINE__, request->u.scatter.my_rank,
                            request->u.scatter.scatter_buf, request->u.scatter.scatter_bytes,
                            request->u.scatter.packed_size);
    }

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.scatter.my_rank);

    return ret;
}

static int
setup_scatter_handles(struct ompi_communicator_t   *comm,
                      ompi_coll_portals4_request_t *request,
                      mca_coll_portals4_module_t   *portals4_module)
{
    int ret, line;

    ptl_me_t  me;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:setup_scatter_handles enter rank %d", request->u.scatter.my_rank));

    /**********************************/
    /* Setup Scatter Handles           */
    /**********************************/
    COLL_PORTALS4_SET_BITS(request->u.scatter.scatter_match_bits, ompi_comm_get_cid(comm),
            0, 0, COLL_PORTALS4_SCATTER, 0, request->u.scatter.coll_count);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:setup_scatter_handles rank(%d) scatter_match_bits(0x%016lX)",
                 request->u.scatter.my_rank, request->u.scatter.scatter_match_bits));

    ret = PtlCTAlloc(mca_coll_portals4_component.ni_h,
                     &request->u.scatter.scatter_cth);
    if (PTL_OK != ret) { ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr; }

    request->u.scatter.scatter_mdh = mca_coll_portals4_component.data_md_h;

    me.start = request->u.scatter.scatter_buf;
    me.length = request->u.scatter.scatter_bytes;
    me.ct_handle = request->u.scatter.scatter_cth;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
        PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_CT_COMM;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = request->u.scatter.scatter_match_bits;
    me.ignore_bits = 0;
    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                      mca_coll_portals4_component.pt_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &request->u.scatter.scatter_meh);
    if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:setup_scatter_handles exit rank %d", request->u.scatter.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.scatter.my_rank);

    return ret;
}

static int
setup_sync_handles(struct ompi_communicator_t   *comm,
                   ompi_coll_portals4_request_t *request,
                   mca_coll_portals4_module_t   *portals4_module)
{
    int ret, line;

    ptl_me_t  me;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:setup_sync_handles enter rank %d", request->u.scatter.my_rank));

    /**********************************/
    /* Setup Sync Handles             */
    /**********************************/
    COLL_PORTALS4_SET_BITS(request->u.scatter.sync_match_bits, ompi_comm_get_cid(comm),
            0, 1, COLL_PORTALS4_SCATTER, 0, request->u.scatter.coll_count);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:setup_sync_handles rank(%d) sync_match_bits(0x%016lX)",
                 request->u.scatter.my_rank, request->u.scatter.sync_match_bits));

    ret = PtlCTAlloc(mca_coll_portals4_component.ni_h,
                     &request->u.scatter.sync_cth);
    if (PTL_OK != ret) { ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr; }

    request->u.scatter.sync_mdh = mca_coll_portals4_component.zero_md_h;

    me.start = NULL;
    me.length = 0;
    me.ct_handle = request->u.scatter.sync_cth;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
        PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_CT_COMM | PTL_ME_EVENT_CT_OVERFLOW;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = request->u.scatter.sync_match_bits;
    me.ignore_bits = 0;
    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                      mca_coll_portals4_component.pt_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &request->u.scatter.sync_meh);
    if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:setup_sync_handles exit rank %d", request->u.scatter.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.scatter.my_rank);

    return ret;
}

static int
cleanup_scatter_handles(ompi_coll_portals4_request_t *request)
{
    int ret, line;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:cleanup_scatter_handles enter rank %d", request->u.scatter.my_rank));

    /**********************************/
    /* Cleanup Scatter Handles             */
    /**********************************/
    do {
        ret = PtlMEUnlink(request->u.scatter.scatter_meh);
        if (PTL_IN_USE == ret) {
            opal_output(ompi_coll_base_framework.framework_output,
                        "%s:%4d: scatter_meh still in use (ret=%d, rank %2d)",
                        __FILE__, __LINE__, ret, request->u.scatter.my_rank);
            continue;
        }
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    } while (ret == PTL_IN_USE);

    ret = PtlCTFree(request->u.scatter.scatter_cth);
    if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:cleanup_scatter_handles exit rank %d", request->u.scatter.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.scatter.my_rank);

    return ret;
}

static int
cleanup_sync_handles(ompi_coll_portals4_request_t *request)
{
    int ret, line;
    int ptl_ret;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:cleanup_sync_handles enter rank %d", request->u.scatter.my_rank));

    /**********************************/
    /* Cleanup Sync Handles             */
    /**********************************/
    do {
        ret = PtlMEUnlink(request->u.scatter.sync_meh);
        if (PTL_IN_USE == ret) {
            opal_output(ompi_coll_base_framework.framework_output,
                        "%s:%4d: sync_meh still in use (ret=%d, rank %2d)",
                        __FILE__, __LINE__, ret, request->u.scatter.my_rank);
            continue;
        }
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    } while (ret == PTL_IN_USE);

    ret = PtlCTFree(request->u.scatter.sync_cth);
    if (PTL_OK != ret) { ptl_ret = ret; ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:cleanup_sync_handles exit rank %d", request->u.scatter.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred (ptl_ret=%d) ret=%d, rank %2d",
                __FILE__, __LINE__, line, ptl_ret, ret, request->u.scatter.my_rank);

    return ret;
}

static int
ompi_coll_portals4_scatter_intra_linear_top(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
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

    int32_t expected_rtrs = 0;
    int32_t expected_puts = 0;
    int32_t expected_acks = 0;
    int32_t expected_ops  = 0;

    int32_t expected_chained_rtrs = 0;
    int32_t expected_chained_acks = 0;


    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:scatter_intra_linear_top enter rank %d", request->u.scatter.my_rank));

    request->type                   = OMPI_COLL_PORTALS4_TYPE_SCATTER;
    request->u.scatter.scatter_buf  = NULL;
    request->u.scatter.scatter_mdh  = PTL_INVALID_HANDLE;
    request->u.scatter.scatter_cth  = PTL_INVALID_HANDLE;
    request->u.scatter.scatter_meh  = PTL_INVALID_HANDLE;
    request->u.scatter.sync_mdh     = PTL_INVALID_HANDLE;
    request->u.scatter.sync_cth     = PTL_INVALID_HANDLE;
    request->u.scatter.sync_meh     = PTL_INVALID_HANDLE;

    request->u.scatter.my_rank   = ompi_comm_rank(comm);
    request->u.scatter.size      = ompi_comm_size(comm);
    request->u.scatter.root_rank = root;
    request->u.scatter.sbuf      = sbuf;
    request->u.scatter.rbuf      = rbuf;

    request->u.scatter.pack_src_buf    = sbuf;
    request->u.scatter.pack_src_count  = scount;
    request->u.scatter.pack_src_dtype  = sdtype;
    ompi_datatype_get_extent(request->u.scatter.pack_src_dtype,
                             &request->u.scatter.pack_src_lb,
                             &request->u.scatter.pack_src_extent);
    ompi_datatype_get_true_extent(request->u.scatter.pack_src_dtype,
                                  &request->u.scatter.pack_src_true_lb,
                                  &request->u.scatter.pack_src_true_extent);

    if ((root == request->u.scatter.my_rank) && (rbuf == MPI_IN_PLACE)) {
        request->u.scatter.unpack_dst_buf   = NULL;
        request->u.scatter.unpack_dst_count = 0;
        request->u.scatter.unpack_dst_dtype = MPI_DATATYPE_NULL;
    } else {
        request->u.scatter.unpack_dst_buf   = rbuf;
        request->u.scatter.unpack_dst_count = rcount;
        request->u.scatter.unpack_dst_dtype = rdtype;
        request->u.scatter.unpack_dst_offset = 0;
        ompi_datatype_get_extent(request->u.scatter.unpack_dst_dtype,
                                 &request->u.scatter.unpack_dst_lb,
                                 &request->u.scatter.unpack_dst_extent);
        ompi_datatype_get_true_extent(request->u.scatter.unpack_dst_dtype,
                                      &request->u.scatter.unpack_dst_true_lb,
                                      &request->u.scatter.unpack_dst_true_extent);
    }

    opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                        "%s:%d:rank(%d): request->u.scatter.unpack_dst_offset(%lu)",
                        __FILE__, __LINE__, request->u.scatter.my_rank,
                        request->u.scatter.unpack_dst_offset);

    /**********************************/
    /* Setup Common Parameters        */
    /**********************************/

    i_am_root = (request->u.scatter.my_rank == request->u.scatter.root_rank);

    request->u.scatter.coll_count = opal_atomic_add_size_t(&portals4_module->coll_count, 1);

    ret = setup_scatter_buffers_linear(comm, request, portals4_module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = setup_scatter_handles(comm, request, portals4_module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = setup_sync_handles(comm, request, portals4_module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    /**********************************/
    /* do the scatter                 */
    /**********************************/
    if (i_am_root) {
        /* operations on the sync counter */
        expected_rtrs = request->u.scatter.size - 1; /* expect RTRs from non-root ranks */
        expected_acks = request->u.scatter.size - 1; /* expect Recv-ACKs from non-root ranks */

        /* operations on the scatter counter */
        expected_puts         = 0;
        expected_chained_rtrs = 1;
        expected_chained_acks = 1;

        /* Chain the RTR and Recv-ACK to the Scatter CT */
        sync_incr_event.success=1;
        sync_incr_event.failure=0;
        ret = PtlTriggeredCTInc(request->u.scatter.scatter_cth,
                                sync_incr_event,
                                request->u.scatter.sync_cth,
                                expected_rtrs);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

        ret = PtlTriggeredCTInc(request->u.scatter.scatter_cth,
                                sync_incr_event,
                                request->u.scatter.sync_cth,
                                expected_rtrs + expected_acks);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

        /* root, so put packed bytes to other ranks */
        for (int32_t i=0;i<request->u.scatter.size;i++) {
            /* do not put to my scatter_buf.  my data gets unpacked into my out buffer in linear_bottom(). */
            if (i == request->u.scatter.my_rank) {
                continue;
            }

            ptl_size_t offset = request->u.scatter.packed_size * i;

            opal_output_verbose(30, ompi_coll_base_framework.framework_output,
                                "%s:%d:rank(%d): offset(%lu)=rank(%d) * packed_size(%ld)",
                                __FILE__, __LINE__, request->u.scatter.my_rank,
                                offset, i, request->u.scatter.packed_size);

            ret = PtlTriggeredPut(request->u.scatter.scatter_mdh,
                                  (ptl_size_t)request->u.scatter.scatter_buf + offset,
                                  request->u.scatter.packed_size,
                                  PTL_NO_ACK_REQ,
                                  ompi_coll_portals4_get_peer(comm, i),
                                  mca_coll_portals4_component.pt_idx,
                                  request->u.scatter.scatter_match_bits,
                                  0,
                                  NULL,
                                  0,
                                  request->u.scatter.scatter_cth,
                                  expected_chained_rtrs);
            if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
        }
    } else {
        /* non-root, so do nothing */

        /* operations on the sync counter */
        expected_rtrs = 0;
        expected_acks = 0;

        /* operations on the scatter counter */
        expected_puts         = 1;  /* scatter put from root */
        expected_chained_rtrs = 0;
        expected_chained_acks = 0;
    }

    expected_ops = expected_chained_rtrs + expected_puts;

    /**********************************************/
    /* only non-root ranks are PUT to, so only    */
    /* non-root ranks must PUT a Recv-ACK to root */
    /**********************************************/
    if (!i_am_root) {
        ret = PtlTriggeredPut(request->u.scatter.sync_mdh,
                              0,
                              0,
                              PTL_NO_ACK_REQ,
                              ompi_coll_portals4_get_peer(comm, request->u.scatter.root_rank),
                              mca_coll_portals4_component.pt_idx,
                              request->u.scatter.sync_match_bits,
                              0,
                              NULL,
                              0,
                              request->u.scatter.scatter_cth,
                              expected_ops);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

    expected_ops += expected_chained_acks;

    if (!request->u.scatter.is_sync) {
        /******************************************/
        /* put to finish pt when all ops complete */
        /******************************************/
        ret = PtlTriggeredPut(mca_coll_portals4_component.zero_md_h,
                0,
                0,
                PTL_NO_ACK_REQ,
                ompi_coll_portals4_get_peer(comm, request->u.scatter.my_rank),
                mca_coll_portals4_component.finish_pt_idx,
                0,
                0,
                NULL,
                (uintptr_t) request,
                request->u.scatter.scatter_cth,
                expected_ops);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

    /**************************************/
    /* all non-root ranks put RTR to root */
    /**************************************/
    if (!i_am_root) {
        ret = PtlPut(request->u.scatter.sync_mdh,
                     0,
                     0,
                     PTL_NO_ACK_REQ,
                     ompi_coll_portals4_get_peer(comm, request->u.scatter.root_rank),
                     mca_coll_portals4_component.pt_idx,
                     request->u.scatter.sync_match_bits,
                     0,
                     NULL,
                     0);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
    }

    if (request->u.scatter.is_sync) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "calling CTWait(expected_ops=%d)\n", expected_ops);

        /********************************/
        /* Wait for all ops to complete */
        /********************************/
        ret = PtlCTWait(request->u.scatter.scatter_cth, expected_ops, &ct);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "completed CTWait(expected_ops=%d)\n", expected_ops);
    }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:scatter_intra_linear_top exit rank %d", request->u.scatter.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    if (NULL != request->u.scatter.scatter_buf)
        free(request->u.scatter.scatter_buf);

    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
                __FILE__, __LINE__, line, ret, request->u.scatter.my_rank);

    return ret;
}

static int
ompi_coll_portals4_scatter_intra_linear_bottom(struct ompi_communicator_t *comm,
                                               ompi_coll_portals4_request_t *request)
{
    int ret, line;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:scatter_intra_linear_bottom enter rank %d", request->u.scatter.my_rank));

    ret = cleanup_scatter_handles(request);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = cleanup_sync_handles(request);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    if (NULL != request->u.scatter.unpack_dst_buf) {
        uint32_t iov_count = 1;
        struct iovec iov;
        size_t max_data;

        ompi_coll_portals4_create_recv_converter (&request->u.scatter.recv_converter,
                                                  request->u.scatter.unpack_dst_buf,
                                                  ompi_comm_peer_lookup(comm, request->u.scatter.my_rank),
                                                  request->u.scatter.unpack_dst_count,
                                                  request->u.scatter.unpack_dst_dtype);

        iov.iov_len = request->u.scatter.packed_size;
        if (request->u.scatter.my_rank == request->u.scatter.root_rank) {
            /* unpack my data from the location in scatter_buf where is was packed */
            uint64_t offset = request->u.scatter.pack_src_extent * request->u.scatter.pack_src_count * request->u.scatter.my_rank;
            iov.iov_base = (IOVBASE_TYPE *)((char *)request->u.scatter.scatter_buf + offset);
        } else {
            iov.iov_base = (IOVBASE_TYPE *)request->u.scatter.scatter_buf;
        }
        opal_convertor_unpack(&request->u.scatter.recv_converter, &iov, &iov_count, &max_data);

        OBJ_DESTRUCT(&request->u.scatter.recv_converter);
    }

    if (request->u.scatter.free_after)
        free(request->u.scatter.scatter_buf);

    request->super.req_status.MPI_ERROR = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_complete(&request->super, true);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:scatter_intra_linear_bottom exit rank %d", request->u.scatter.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    request->super.req_status.MPI_ERROR = ret;

    if (request->u.scatter.free_after)
        free(request->u.scatter.scatter_buf);

    opal_output(ompi_coll_base_framework.framework_output,
            "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
            __FILE__, __LINE__, line, ret, request->u.scatter.my_rank);

    return ret;
}

int
ompi_coll_portals4_scatter_intra(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                 void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                 int root,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module)
{
    int ret, line;

    ompi_coll_portals4_request_t *request;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:scatter_intra enter rank %d", ompi_comm_rank(comm)));

    /*
     *  allocate a portals4 request
     */
    OMPI_COLL_PORTALS4_REQUEST_ALLOC(comm, request);
    if (NULL == request) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
    }
    request->u.scatter.is_sync = 1;

    /*
     *  initiate the scatter
     *
     *  this request is marked synchronous (is_sync==1), so PtlCTWait()
     *  will be called to wait for completion.
     */
    ret = ompi_coll_portals4_scatter_intra_linear_top(sbuf, scount, sdtype,
                                                     rbuf, rcount, rdtype,
                                                     root,
                                                     comm,
                                                     request,
                                                     module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    ret = ompi_coll_portals4_scatter_intra_linear_bottom(comm, request);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    /*
     *  return the portals4 request
     */
    OMPI_COLL_PORTALS4_REQUEST_RETURN(request);

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:scatter_intra exit rank %d", request->u.scatter.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
            "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
            __FILE__, __LINE__, line, ret, request->u.scatter.my_rank);

    return ret;
}


int
ompi_coll_portals4_iscatter_intra(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                 void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                 int root,
                                 struct ompi_communicator_t *comm,
                                 ompi_request_t **ompi_request,
                                 mca_coll_base_module_t *module)
{
    int ret, line;

    ompi_coll_portals4_request_t *request;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:iscatter_intra enter rank %d", ompi_comm_rank(comm)));

    /*
     *  allocate a portals4 request
     */
    OMPI_COLL_PORTALS4_REQUEST_ALLOC(comm, request);
    if (NULL == request) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE; line = __LINE__; goto err_hdlr;
    }
    *ompi_request = &request->super;
    request->u.scatter.is_sync = 0;

    /*
     *  initiate the scatter
     *
     *  this request is marked asynchronous (is_sync==0), so
     *  portals4_progress() will handle completion.
     */
    ret = ompi_coll_portals4_scatter_intra_linear_top(sbuf, scount, sdtype,
                                                      rbuf, rcount, rdtype,
                                                      root,
                                                      comm,
                                                      request,
                                                      module);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:iscatter_intra exit rank %d", request->u.scatter.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
            "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
            __FILE__, __LINE__, line, ret, request->u.scatter.my_rank);

    return ret;
}


int
ompi_coll_portals4_iscatter_intra_fini(ompi_coll_portals4_request_t *request)
{
    int ret, line;

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:iscatter_intra_fini enter rank %d", request->u.scatter.my_rank));

    /*
     *  cleanup the scatter
     */
    ret = ompi_coll_portals4_scatter_intra_linear_bottom(request->super.req_mpi_object.comm, request);
    if (MPI_SUCCESS != ret) { line = __LINE__; goto err_hdlr; }

    OPAL_OUTPUT((ompi_coll_base_framework.framework_output,
                 "coll:portals4:iscatter_intra_fini exit rank %d", request->u.scatter.my_rank));

    return OMPI_SUCCESS;

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
            "%s:%4d:%4d\tError occurred ret=%d, rank %2d",
            __FILE__, __LINE__, line, ret, request->u.scatter.my_rank);

    return ret;
}

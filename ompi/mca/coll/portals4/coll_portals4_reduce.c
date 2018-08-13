/*
 * Copyright (c) 2015      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_portals4.h"
#include "coll_portals4_request.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/op/op.h"
#include "opal/util/bit_ops.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

#define COLL_PORTALS4_REDUCE_MAX_CHILDREN	2


static int
reduce_kary_tree_top(const void *sendbuf, void *recvbuf, int count,
        MPI_Datatype dtype, MPI_Op op,
        int root,
        struct ompi_communicator_t *comm,
        ompi_coll_portals4_request_t *request,
        mca_coll_portals4_module_t *module)
{
    bool is_sync = request->is_sync;
    int ret;
    unsigned int i;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    ptl_rank_t parent, child[COLL_PORTALS4_REDUCE_MAX_CHILDREN];
    size_t internal_count, length;
    ptl_handle_md_t zero_md_h, data_md_h;
    ptl_handle_me_t me_h;
    ptl_me_t me;
    ptl_match_bits_t match_bits_ack, match_bits_rtr, match_bits;
    ptl_ct_event_t ct;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dtype;


    request->type = OMPI_COLL_PORTALS4_TYPE_REDUCE;

    /*
     ** Initialization
     */

    for (i = 0 ; i < COLL_PORTALS4_REDUCE_MAX_CHILDREN ; i++) {
        child[i] = PTL_INVALID_RANK;
    }

    parent = PTL_INVALID_RANK;

    zero_md_h = mca_coll_portals4_component.zero_md_h;
    data_md_h = mca_coll_portals4_component.data_md_h;

    internal_count = opal_atomic_add_fetch_size_t(&module->coll_count, 1);

    /*
     ** DATATYPE and SIZES
     */
    ret = ompi_datatype_type_size(dtype, &length);
    length *= count;

    request->u.reduce.is_optim = is_reduce_optimizable(dtype, length, op, &ptl_dtype, &ptl_op);

    if (request->u.reduce.is_optim) {

        /*
         * TOPOLOGY
         */

        /* this function is dependent on the number of segments,
         * if we use segmentation pipe-line is preferred, and
         * binary tree otherwise */

        get_k_ary_tree(COLL_PORTALS4_REDUCE_MAX_CHILDREN,
                rank, size, root, &parent, child, &request->u.reduce.child_nb);

        /*
         * PORTALS4 RESOURCE ALLOCATION
         */

        /* Compute match bits */
        COLL_PORTALS4_SET_BITS(match_bits_ack, ompi_comm_get_cid(comm), 1, 0,
                COLL_PORTALS4_REDUCE, 0, internal_count);

        COLL_PORTALS4_SET_BITS(match_bits_rtr, ompi_comm_get_cid(comm), 0, 1,
                COLL_PORTALS4_REDUCE, 0, internal_count);

        COLL_PORTALS4_SET_BITS(match_bits, ompi_comm_get_cid(comm), 0, 0,
                COLL_PORTALS4_REDUCE, 0, internal_count);

        if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.reduce.trig_ct_h)) != 0) {
            return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
        }

        /* warning : all the operations will be executed on the recvbuf */
        if (rank != root) {
            request->u.reduce.free_buffer = malloc(length);
            if (NULL == request->u.reduce.free_buffer) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            recvbuf = (void*)request->u.reduce.free_buffer;

            memcpy(recvbuf, sendbuf, length);
        }
        else {
            request->u.reduce.free_buffer = NULL;
            if (sendbuf != MPI_IN_PLACE) {
                memcpy(recvbuf, sendbuf, length);
            }
        }

        if (request->u.reduce.child_nb) {

            /*
             ** Prepare Data ME
             */
            memset(&me, 0, sizeof(ptl_me_t));
            me.start = recvbuf;
            me.length = length;
            me.ct_handle = request->u.reduce.trig_ct_h;
            me.uid = mca_coll_portals4_component.uid;
            me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
                    PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
                    PTL_ME_EVENT_CT_COMM;
            me.match_id.phys.nid = PTL_NID_ANY;
            me.match_id.phys.pid = PTL_PID_ANY;
            me.match_bits = match_bits;
            me.ignore_bits = 0;

            if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                    mca_coll_portals4_component.pt_idx,
                    &me, PTL_PRIORITY_LIST, NULL,
                    &request->u.reduce.data_me_h)) != 0) {
                return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
            }
        }

        if (rank != root) {
            request->u.reduce.use_ack_ct_h = true;

            if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.reduce.ack_ct_h)) != 0) {
                return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
            }

            /*
             ** Prepare ME for data ACK Put
             ** Priority List
             */

            memset(&me, 0, sizeof(ptl_me_t));
            me.start = NULL;
            me.length = 0;
            me.min_free = 0;
            me.uid = mca_coll_portals4_component.uid;
            me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
                    PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
                    PTL_ME_USE_ONCE |
                    PTL_ME_EVENT_CT_COMM;
            me.match_id.phys.nid = PTL_NID_ANY;
            me.match_id.phys.pid = PTL_PID_ANY;
            me.match_bits = match_bits_ack;
            me.ignore_bits = 0;
            me.ct_handle = request->u.reduce.ack_ct_h;

            if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                    mca_coll_portals4_component.pt_idx,
                    &me, PTL_PRIORITY_LIST,
                    NULL,
                    &me_h)) != 0) {
                return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
            }

            /*
             ** Prepare ME for sending RTR Put
             ** Priority List, match also with "Overflow list Me" in coll_portals4_component
             */

            memset(&me, 0, sizeof(ptl_me_t));
            me.start = NULL;
            me.length = 0;
            me.min_free = 0;
            me.uid = mca_coll_portals4_component.uid;
            me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
                    PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
                    PTL_ME_USE_ONCE |
                    PTL_ME_EVENT_CT_COMM | PTL_ME_EVENT_CT_OVERFLOW;
            me.match_id.phys.nid = PTL_NID_ANY;
            me.match_id.phys.pid = PTL_PID_ANY;
            me.match_bits = match_bits_rtr;
            me.ignore_bits = 0;
            me.ct_handle = request->u.reduce.trig_ct_h;

            if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                    mca_coll_portals4_component.pt_idx,
                    &me, PTL_PRIORITY_LIST,
                    NULL,
                    &me_h)) != 0) {
                return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
            }

            /* Send Atomic operation to the parent */
            if ((ret = PtlTriggeredAtomic(data_md_h,
                    (uint64_t)recvbuf,
                    length, PTL_NO_ACK_REQ,
                    ompi_coll_portals4_get_peer(comm, parent),
                    mca_coll_portals4_component.pt_idx,
                    match_bits, 0, NULL, 0,
                    ptl_op, ptl_dtype, request->u.reduce.trig_ct_h,
                    request->u.reduce.child_nb + 1)) != 0) {
                return opal_stderr("PtlTriggeredAtomic failed", __FILE__, __LINE__, ret);
            }
        }
        else {
            request->u.reduce.use_ack_ct_h = false;
        }

        if (request->u.reduce.child_nb) {
            for (i = 0 ; i < COLL_PORTALS4_REDUCE_MAX_CHILDREN ; i++) {
                if (child[i] != PTL_INVALID_RANK) {
                    /*
                     * Prepare Triggered Put to ACK Data to children
                     *
                     */

                    if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                            ompi_coll_portals4_get_peer(comm, child[i]),
                            mca_coll_portals4_component.pt_idx,
                            match_bits_ack, 0, NULL, 0,
                            request->u.reduce.trig_ct_h,
                            (rank != root) ?
                                    request->u.reduce.child_nb + 1 :
                                    request->u.reduce.child_nb)) != 0) {
                        return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                    }

                    /*
                     * Send RTR to children
                     *
                     */

                    /* and there, we only send the RTR when all the MEs are ready */
                    if ((ret = PtlPut(zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                            ompi_coll_portals4_get_peer(comm, child[i]),
                            mca_coll_portals4_component.pt_idx,
                            match_bits_rtr, 0, NULL, 0)) != PTL_OK) {
                        return opal_stderr("Put RTR failed %d", __FILE__, __LINE__, ret);
                    }
                }
            }
        }

        if (rank != root) {
            if (is_sync) {
                if ((ret = PtlCTWait(request->u.reduce.ack_ct_h, 1, &ct)) != 0) {
                    opal_stderr("PtlCTWait failed", __FILE__, __LINE__, ret);
                }
            }
            else {
                if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                        ompi_coll_portals4_get_peer(comm, rank),
                        mca_coll_portals4_component.finish_pt_idx,
                        0, 0, NULL, (uintptr_t) request,
                        request->u.reduce.ack_ct_h,
                        1)) != 0) {
                    return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                }
            }
        }
        else {
            if (is_sync) {
                if ((ret = PtlCTWait(request->u.reduce.trig_ct_h,
                        request->u.reduce.child_nb, &ct)) != 0) {
                    opal_stderr("PtlCTWait failed", __FILE__, __LINE__, ret);
                }
            }
            else {
                if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                        ompi_coll_portals4_get_peer(comm, rank),
                        mca_coll_portals4_component.finish_pt_idx,
                        0, 0, NULL, (uintptr_t) request,
                        request->u.reduce.trig_ct_h,
                        request->u.reduce.child_nb)) != 0) {
                    return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                }
            }
        }
    }
    else {
        opal_output_verbose(100, ompi_coll_base_framework.framework_output,
                "rank %d - optimization not supported, falling back to previous handler\n", rank);

        if (request->is_sync) {
            if ((module->previous_reduce) && (module->previous_reduce_module)) {
                ret = module->previous_reduce(sendbuf, recvbuf, count, dtype, op, root,
                        comm, module->previous_reduce_module);
            }
            else {
                opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                        "rank %d - no previous reduce handler is available, aborting\n", rank);
                return (OMPI_ERROR);
            }
        }
        else {
            if ((module->previous_ireduce) && (module->previous_ireduce_module)) {
                ret =  module->previous_ireduce(sendbuf, recvbuf, count, dtype, op, root,
                        comm, request->fallback_request, module->previous_ireduce_module);
            }
            else {
                opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                        "rank %d - no previous ireduce handler is available, aborting\n", rank);
                return (OMPI_ERROR);
            }
        }
        return ret;
    }
    return (OMPI_SUCCESS);
}




static int
reduce_kary_tree_bottom(ompi_coll_portals4_request_t *request)
{
    int ret, line;

    if (request->u.reduce.is_optim) {
        PtlAtomicSync();

        if (request->u.reduce.use_ack_ct_h) {
            ret = PtlCTFree(request->u.reduce.ack_ct_h);
            if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
        }

        if (request->u.reduce.child_nb) {
            do {
                ret = PtlMEUnlink(request->u.reduce.data_me_h);
            } while (PTL_IN_USE == ret);
            if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }
        }

        ret = PtlCTFree(request->u.reduce.trig_ct_h);
        if (PTL_OK != ret) { ret = OMPI_ERROR; line = __LINE__; goto err_hdlr; }

        if (request->u.reduce.free_buffer) {
            free(request->u.reduce.free_buffer);
        }
    }
    return (OMPI_SUCCESS);

err_hdlr:
    opal_output(ompi_coll_base_framework.framework_output,
                "%s:%4d:%4d\tError occurred ret=%d",
                __FILE__, __LINE__, line, ret);

    return ret;
}


int
ompi_coll_portals4_reduce_intra(const void *sendbuf, void *recvbuf, int count,
        MPI_Datatype dtype, MPI_Op op,
        int root,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
{
    int ret;
    mca_coll_portals4_module_t *portals4_module = (mca_coll_portals4_module_t*) module;
    ompi_coll_portals4_request_t *request;

    OMPI_COLL_PORTALS4_REQUEST_ALLOC(comm, request);
    if (NULL == request) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: request alloc failed\n",
                __FILE__, __LINE__);
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    request->is_sync = true;
    request->fallback_request = NULL;

    ret = reduce_kary_tree_top(sendbuf, recvbuf, count,
            dtype, op, root, comm,  request,  portals4_module);
    if (OMPI_SUCCESS != ret)
        return ret;
    ret = reduce_kary_tree_bottom(request);
    if (OMPI_SUCCESS != ret)
        return ret;

    OMPI_COLL_PORTALS4_REQUEST_RETURN(request);
    return (OMPI_SUCCESS);
}


int
ompi_coll_portals4_ireduce_intra(const void* sendbuf, void* recvbuf, int count,
        MPI_Datatype dtype, MPI_Op op,
        int root,
        struct ompi_communicator_t *comm,
        ompi_request_t ** ompi_request,
        struct mca_coll_base_module_2_3_0_t *module)
{
    int ret;
    mca_coll_portals4_module_t *portals4_module = (mca_coll_portals4_module_t*) module;
    ompi_coll_portals4_request_t *request;

    OMPI_COLL_PORTALS4_REQUEST_ALLOC(comm, request);
    if (NULL == request) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: request alloc failed\n",
                __FILE__, __LINE__);
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    *ompi_request = &request->super;
    request->fallback_request = ompi_request;
    request->is_sync = false;

    ret = reduce_kary_tree_top(sendbuf, recvbuf, count,
            dtype, op, root, comm,  request,  portals4_module);
    if (OMPI_SUCCESS != ret)
        return ret;

    if (!request->u.reduce.is_optim) {
        OMPI_COLL_PORTALS4_REQUEST_RETURN(request);
    }

    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "ireduce");
    return (OMPI_SUCCESS);
}

int
ompi_coll_portals4_ireduce_intra_fini(ompi_coll_portals4_request_t *request)
{
    int ret;

    ret = reduce_kary_tree_bottom(request);
    if (OMPI_SUCCESS != ret)
        return ret;

    ompi_request_complete(&request->super, true);

    return (OMPI_SUCCESS);
}

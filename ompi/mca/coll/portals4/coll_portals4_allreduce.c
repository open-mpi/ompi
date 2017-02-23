/*
 * Copyright (c) 2015      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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


#define COLL_PORTALS4_ALLREDUCE_MAX_SEGMENT	128
#define COLL_PORTALS4_ALLREDUCE_MAX_CHILDREN	2

static int
allreduce_kary_tree_top(const void *sendbuf, void *recvbuf, int count,
        MPI_Datatype dtype, MPI_Op op,
        struct ompi_communicator_t *comm,
        ompi_coll_portals4_request_t *request,
        mca_coll_portals4_module_t *module)
{
    bool is_sync = request->is_sync;
    int ret, i;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    ptl_rank_t parent, child[COLL_PORTALS4_ALLREDUCE_MAX_CHILDREN];
    unsigned int child_nb;
    size_t internal_count, length;
    ptl_handle_md_t zero_md_h, data_md_h;
    ptl_handle_me_t me_h;
    ptl_me_t me;
    ptl_match_bits_t match_bits_ack, match_bits_rtr, match_bits;
    ptl_ct_event_t ct;
    ptl_op_t ptl_op;
    ptl_datatype_t ptl_dtype;

    request->type = OMPI_COLL_PORTALS4_TYPE_ALLREDUCE;

    /*
     ** Initialization
     */

    for (i = 0 ; i < COLL_PORTALS4_ALLREDUCE_MAX_CHILDREN ; i++) {
        child[i] = PTL_INVALID_RANK;
    }

    parent = PTL_INVALID_RANK;

    zero_md_h = mca_coll_portals4_component.zero_md_h;
    data_md_h = mca_coll_portals4_component.data_md_h;

    internal_count = opal_atomic_add_size_t(&module->coll_count, 1);

    /*
     ** DATATYPE and SIZES
     */
    ret = ompi_datatype_type_size(dtype, &length);
    length *= count;

    request->u.allreduce.is_optim = is_reduce_optimizable(dtype, length, op, &ptl_dtype, &ptl_op);

    if (request->u.allreduce.is_optim) {
        /*
         * TOPOLOGY
         */

        /* this function is dependent on the number of segments,
         * if we use segmentation pipe-line is preferred, and
         * binary tree otherwise */

        get_k_ary_tree(COLL_PORTALS4_ALLREDUCE_MAX_CHILDREN,
                rank, size, PTL_FIRST_RANK, &parent, child, &child_nb);
        request->u.allreduce.child_nb = child_nb;

        /*
         * PORTALS4 RESOURCE ALLOCATION
         */

        /* Compute match bits */
        COLL_PORTALS4_SET_BITS(match_bits_ack, ompi_comm_get_cid(comm), 1, 0,
                COLL_PORTALS4_ALLREDUCE, 0, internal_count);

        COLL_PORTALS4_SET_BITS(match_bits_rtr, ompi_comm_get_cid(comm), 0, 1,
                COLL_PORTALS4_ALLREDUCE, 0, internal_count);

        COLL_PORTALS4_SET_BITS(match_bits, ompi_comm_get_cid(comm), 0, 0,
                COLL_PORTALS4_ALLREDUCE, 0, internal_count);

        if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.allreduce.trig_ct_h)) != 0) {
            return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
        }

        if (sendbuf != MPI_IN_PLACE) {
            /*
             * copy the data from sendbuf to recvbuf
             */
            memcpy(recvbuf, sendbuf, length);
        }

        /*
         ** Prepare Data ME
         */
        memset(&me, 0, sizeof(ptl_me_t));
        me.start = recvbuf;
        me.length = length;
        me.ct_handle = request->u.allreduce.trig_ct_h;
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
                &request->u.allreduce.data_me_h)) != 0) {
            return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
        }

        if (child_nb) {
            if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.allreduce.ack_ct_h)) != 0) {
                return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
            }

            for (i = 0 ; i < COLL_PORTALS4_ALLREDUCE_MAX_CHILDREN ; i++) {
                if (child[i] != PTL_INVALID_RANK) {

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
                    me.ct_handle = request->u.allreduce.ack_ct_h;

                    if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                            mca_coll_portals4_component.pt_idx,
                            &me, PTL_PRIORITY_LIST,
                            NULL,
                            &me_h)) != 0) {
                        return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
                    }

                    /*
                     * Prepare Triggered Put to send the result to children
                     *
                     */

                    if ((ret = PtlTriggeredPut (data_md_h,
                            (uint64_t)recvbuf,
                            length, PTL_NO_ACK_REQ,
                            ompi_coll_portals4_get_peer(comm, child[i]),
                            mca_coll_portals4_component.pt_idx,
                            match_bits, 0, NULL, 0,
                            request->u.allreduce.trig_ct_h,
                            (rank != PTL_FIRST_RANK) ? child_nb + 2 : child_nb)) != 0) {
                        return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                    }
                }

            }
        }

        if (rank != PTL_FIRST_RANK) {

            /* Send Atomic operation to the parent */
            if ((ret = PtlTriggeredAtomic(data_md_h,
                    (uint64_t)recvbuf,
                    length, PTL_NO_ACK_REQ,
                    ompi_coll_portals4_get_peer(comm, parent),
                    mca_coll_portals4_component.pt_idx,
                    match_bits, 0, NULL, 0,
                    ptl_op, ptl_dtype, request->u.allreduce.trig_ct_h,
                    child_nb + 1)) != 0) {
                return opal_stderr("PtlTriggeredAtomic failed", __FILE__, __LINE__, ret);
            }

            if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                    ompi_coll_portals4_get_peer(comm, parent),
                    mca_coll_portals4_component.pt_idx,
                    match_bits_ack, 0, NULL, 0,
                    request->u.allreduce.trig_ct_h,
                    child_nb + 2)) != 0) {
                return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
            }

            /*
             ** Prepare ME for receving RTR
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
            me.ct_handle = request->u.allreduce.trig_ct_h;

            if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                    mca_coll_portals4_component.pt_idx,
                    &me, PTL_PRIORITY_LIST,
                    NULL,
                    &me_h)) != 0) {
                return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
            }
        }


        /*
         * OK, everything is ready for data and acknowledgement reception
         *
         */

        if (child_nb) {
            for (i = 0 ; i < COLL_PORTALS4_ALLREDUCE_MAX_CHILDREN ; i++) {
                if (child[i] != PTL_INVALID_RANK) {
                    /*
                     * Send RTR to children
                     *
                     */

                    /* and there, we only send the RTR when all the MEs are ready */
                    if ((ret = PtlPut(zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                            ompi_coll_portals4_get_peer(comm, child[i]),
                            mca_coll_portals4_component.pt_idx,
                            match_bits_rtr, 0, NULL, 0)) != PTL_OK)
                        return opal_stderr("Put RTR failed %d", __FILE__, __LINE__, ret);
                }
            }
        }

        if (child_nb) {
            if (is_sync) {
                if ((ret = PtlCTWait(request->u.allreduce.ack_ct_h,
                        child_nb, &ct)) != 0) {
                    opal_stderr("PtlCTWait failed", __FILE__, __LINE__, ret);
                }
            }
            else {
                if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                        ompi_coll_portals4_get_peer(comm, rank),
                        mca_coll_portals4_component.finish_pt_idx,
                        0, 0, NULL, (uintptr_t) request,
                        request->u.allreduce.ack_ct_h,
                        child_nb)) != 0) {
                    return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                }
            }
        }
        else {
            if (is_sync) {
                if ((ret = PtlCTWait(request->u.allreduce.trig_ct_h,
                        (rank != PTL_FIRST_RANK) ? 2 : 0, &ct)) != 0) {
                    opal_stderr("PtlCTWait failed", __FILE__, __LINE__, ret);
                }
            }
            else {
                if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                        ompi_coll_portals4_get_peer(comm, rank),
                        mca_coll_portals4_component.finish_pt_idx,
                        0, 0, NULL, (uintptr_t) request,
                        request->u.allreduce.trig_ct_h,
                        (rank != PTL_FIRST_RANK) ? 2 : 0)) != 0) {
                    return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                }
            }
        }

    }
    else {
        opal_output_verbose(100, ompi_coll_base_framework.framework_output,
                "rank %d - optimization not supported, falling back to previous handler\n", rank);

        if (request->is_sync) {
            if ((module->previous_allreduce) && (module->previous_allreduce_module)) {
                ret = module->previous_allreduce(sendbuf, recvbuf, count, dtype, op,
                        comm, module->previous_allreduce_module);
            }
            else {
                opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                        "rank %d - no previous allreduce handler is available, aborting\n", rank);
                return (OMPI_ERROR);
            }
        }
        else {
            if ((module->previous_iallreduce) && (module->previous_iallreduce_module)) {
                ret = module->previous_iallreduce(sendbuf, recvbuf, count, dtype, op,
                        comm, request->fallback_request, module->previous_iallreduce_module);
            }
            else {
                opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                        "rank %d - no previous iallreduce handler is available, aborting\n", rank);
                return (OMPI_ERROR);
            }
        }
        return ret;
    }
    return (OMPI_SUCCESS);
}

static int
allreduce_kary_tree_bottom(ompi_coll_portals4_request_t *request)
{
    if (request->u.allreduce.is_optim) {
        PtlAtomicSync();

        if (request->u.allreduce.child_nb) {
            PtlCTFree(request->u.allreduce.ack_ct_h);
        }

        PtlMEUnlink(request->u.allreduce.data_me_h);
        PtlCTFree(request->u.allreduce.trig_ct_h);
    }

    return (OMPI_SUCCESS);
}

int ompi_coll_portals4_allreduce_intra(const void* sendbuf, void* recvbuf, int count,
        MPI_Datatype dtype, MPI_Op op,
        struct ompi_communicator_t *comm,
        struct mca_coll_base_module_2_1_0_t *module)
{
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

    allreduce_kary_tree_top(sendbuf, recvbuf, count,
            dtype, op, comm, request, portals4_module);

    allreduce_kary_tree_bottom(request);

    OMPI_COLL_PORTALS4_REQUEST_RETURN(request);
    return (OMPI_SUCCESS);
}


int ompi_coll_portals4_iallreduce_intra(const void* sendbuf, void* recvbuf, int count,
        MPI_Datatype dtype, MPI_Op op,
        struct ompi_communicator_t *comm,
        ompi_request_t ** ompi_request,
        struct mca_coll_base_module_2_1_0_t *module)
{
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

    allreduce_kary_tree_top(sendbuf, recvbuf, count,
            dtype, op, comm, request, portals4_module);

    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "iallreduce");
    return (OMPI_SUCCESS);
}


int
ompi_coll_portals4_iallreduce_intra_fini(struct ompi_coll_portals4_request_t *request)
{
    allreduce_kary_tree_bottom(request);
    ompi_request_complete(&request->super, true);

    return (OMPI_SUCCESS);
}

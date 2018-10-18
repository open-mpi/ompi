/*
 * Copyright (c) 2013-2015 Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
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

#include "mpi.h"
#include "ompi/constants.h"
#include "opal/util/bit_ops.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"


static int
barrier_hypercube_top(struct ompi_communicator_t *comm,
        ompi_coll_portals4_request_t *request,
        mca_coll_portals4_module_t *portals4_module)
{
    bool is_sync = request->is_sync;
    int ret, i, dim, hibit, mask, num_msgs;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    ptl_me_t me;
    size_t count;
    ptl_match_bits_t match_bits_rtr, match_bits;
    ptl_ct_event_t event;
    ptl_handle_md_t md_h;

    md_h = mca_coll_portals4_component.zero_md_h;

    request->type = OMPI_COLL_PORTALS4_TYPE_BARRIER;

    count = opal_atomic_add_fetch_size_t(&portals4_module->coll_count, 1);

    ret = PtlCTAlloc(mca_coll_portals4_component.ni_h,
            &request->u.barrier.rtr_ct_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlCTAlloc failed: %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    COLL_PORTALS4_SET_BITS(match_bits_rtr, ompi_comm_get_cid(comm),
            0, 1, COLL_PORTALS4_BARRIER, 0, count);

    COLL_PORTALS4_SET_BITS(match_bits, ompi_comm_get_cid(comm),
            0, 0, COLL_PORTALS4_BARRIER, 0, count);

    /* Build "tree" out of hypercube */
    dim = comm->c_cube_dim;
    hibit = opal_hibit(rank, dim);
    --dim;
    /* calculate number of children to receive from */
    num_msgs = get_nchildren(dim + 1, hibit, rank, size);

    /* receive space */
    memset(&me, 0, sizeof(ptl_me_t));
    me.start = NULL;
    me.length = 0;
    me.ct_handle = request->u.barrier.rtr_ct_h;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
            PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
            PTL_ME_EVENT_CT_COMM | PTL_ME_EVENT_CT_OVERFLOW;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = match_bits;
    me.ignore_bits = COLL_PORTALS4_RTR_MASK;
    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
            mca_coll_portals4_component.pt_idx,
            &me,
            PTL_PRIORITY_LIST,
            NULL,
            &request->u.barrier.data_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlMEAppend failed: %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    /* send to parent when children have sent to us */
    if (rank > 0) {
        int parent = rank & ~(1 << hibit);

        ret = PtlTriggeredPut(md_h,
                0,
                0,
                PTL_NO_ACK_REQ,
                ompi_coll_portals4_get_peer(comm, parent),
                mca_coll_portals4_component.pt_idx,
                match_bits_rtr,
                0,
                NULL,
                0,
                request->u.barrier.rtr_ct_h,
                num_msgs);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                    "%s:%d: PtlTriggeredPut failed: %d\n",
                    __FILE__, __LINE__, ret);
            return OMPI_ERROR;
        }

        /* we'll need to wait for the parent response before the next set of comms */
        num_msgs++;
    }

    /* send to children when parent (or all children if root) has sent to us */
    for (i = hibit + 1, mask = 1 << i; i <= dim; ++i, mask <<= 1) {
        int peer = rank | mask;
        if (peer < size) {
            ret = PtlTriggeredPut(md_h,
                    0,
                    0,
                    PTL_NO_ACK_REQ,
                    ompi_coll_portals4_get_peer(comm, peer),
                    mca_coll_portals4_component.pt_idx,
                    match_bits,
                    0,
                    NULL,
                    0,
                    request->u.barrier.rtr_ct_h,
                    num_msgs);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                        "%s:%d: PtlTriggeredPut failed: %d\n",
                        __FILE__, __LINE__, ret);
                return OMPI_ERROR;
            }
        }
    }

    if (is_sync) {
        /* Each process has a pending PtlTriggeredPut. To be sure this request will be triggered, we must
           call PtlTriggeredCTInc twice. Otherwise, we could free the CT too early and the Put wouldn't be triggered */

        ptl_ct_event_t ct_inc;

        ct_inc.success = 1;
        ct_inc.failure = 0;

        if ((ret = PtlTriggeredCTInc(request->u.barrier.rtr_ct_h, ct_inc,
                request->u.barrier.rtr_ct_h, num_msgs)) != 0) {
            return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
        }

        if ((ret = PtlTriggeredCTInc(request->u.barrier.rtr_ct_h, ct_inc,
                request->u.barrier.rtr_ct_h, num_msgs + 1)) != 0) {
            return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
        }

        ret = PtlCTWait(request->u.barrier.rtr_ct_h, num_msgs + 2, &event);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                    "%s:%d: PtlCTWait failed: %d\n",
                    __FILE__, __LINE__, ret);
            return OMPI_ERROR;
        }
    }
    else {
        /* Send a put to self when we've received all our messages... */
        ret = PtlTriggeredPut(md_h,
                0,
                0,
                PTL_NO_ACK_REQ,
                ompi_coll_portals4_get_peer(comm, rank),
                mca_coll_portals4_component.finish_pt_idx,
                0,
                0,
                NULL,
                (uintptr_t) request,
                request->u.barrier.rtr_ct_h,
                num_msgs);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                    "%s:%d: PtlTriggeredPut failed: %d\n",
                    __FILE__, __LINE__, ret);
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}


static int
barrier_hypercube_bottom(ompi_coll_portals4_request_t *request)
{
    int ret;

    /* cleanup */
    do {
        ret = PtlMEUnlink(request->u.barrier.data_me_h);
    } while (PTL_IN_USE == ret);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlMEUnlink failed: %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    ret = PtlCTFree(request->u.barrier.rtr_ct_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlCTFree failed: %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
ompi_coll_portals4_barrier_intra(struct ompi_communicator_t *comm,
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

    ret = barrier_hypercube_top(comm, request, portals4_module);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: ompi_coll_portals4_barrier_hypercube_top failed %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    ret = barrier_hypercube_bottom(request);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: ompi_coll_portals4_barrier_hypercube_bottom failed %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    OMPI_COLL_PORTALS4_REQUEST_RETURN(request);
    return OMPI_SUCCESS;
}


int
ompi_coll_portals4_ibarrier_intra(struct ompi_communicator_t *comm,
        ompi_request_t **ompi_req,
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

    *ompi_req = &request->super;
    request->is_sync = false;

    ret = barrier_hypercube_top(comm, request, portals4_module);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: ompi_coll_portals4_barrier_hypercube_top failed %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
ompi_coll_portals4_ibarrier_intra_fini(ompi_coll_portals4_request_t *request)
{
    int ret;

    ret = barrier_hypercube_bottom(request);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: ompi_coll_portals4_barrier_hypercube_bottom failed %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    ompi_request_complete(&request->super, true);

    return OMPI_SUCCESS;
}

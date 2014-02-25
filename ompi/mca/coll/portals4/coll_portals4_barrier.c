/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
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


int
ompi_coll_portals4_barrier_intra(struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module)
{
    mca_coll_portals4_module_t *portals4_module = (mca_coll_portals4_module_t*) module;
    int ret, i, dim, hibit, mask, num_msgs;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    ptl_ct_event_t ct;
    ptl_handle_ct_t ct_h;
    ptl_handle_me_t me_h;
    ptl_me_t me;
    size_t count;
    ptl_match_bits_t match_bits;
    ptl_handle_md_t md_h;
    void *base;

    ompi_coll_portals4_get_md(0, &md_h, &base);

    count = opal_atomic_add_size_t(&portals4_module->barrier_count, 1);

    ret = PtlCTAlloc(mca_coll_portals4_component.ni_h,
                     &ct_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlCTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    COLL_PORTALS4_SET_BITS(match_bits, ompi_comm_get_cid(comm), 
                          0, COLL_PORTALS4_BARRIER, count);

    /* Build "tree" out of hypercube */
    dim = comm->c_cube_dim;
    hibit = opal_hibit(rank, dim);
    --dim;

    /* receive space */
    me.start = NULL;
    me.length = 0;
    me.ct_handle = ct_h;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
        PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_CT_COMM | PTL_ME_EVENT_CT_OVERFLOW;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = match_bits;
    me.ignore_bits = 0;
    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                      mca_coll_portals4_component.pt_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    /* calculate number of children to receive from */
    num_msgs = ompi_coll_portals4_get_nchildren(dim + 1, hibit, rank, size);
    
    /* send to parent when children have sent to us */
    if (rank > 0) {
        int parent = rank & ~(1 << hibit);
        ret = PtlTriggeredPut(md_h,
                              0,
                              0,
                              PTL_NO_ACK_REQ,
                              ompi_coll_portals4_get_peer(comm, parent),
                              mca_coll_portals4_component.pt_idx,
                              match_bits,
                              0,
                              NULL,
                              0,
                              ct_h,
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
                                  ct_h,
                                  num_msgs);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                    "%s:%d: PtlTriggeredPut failed: %d\n",
                                    __FILE__, __LINE__, ret);
                return OMPI_ERROR;
            }
        }
    }

    /* Wait for all incoming messages */
    ret = PtlCTWait(ct_h, num_msgs, &ct);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlCTWait failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    /* cleanup */
    ret = PtlMEUnlink(me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlMEUnlink failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }
    ret = PtlCTFree(ct_h); 
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlCTFree failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
ompi_coll_portals4_ibarrier_intra(struct ompi_communicator_t *comm, 
                                  ompi_request_t **ompi_req,
                                  struct mca_coll_base_module_2_0_0_t *module)
{
    mca_coll_portals4_module_t *portals4_module = (mca_coll_portals4_module_t*) module;
    int ret, i, dim, hibit, mask, num_msgs;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    ptl_me_t me;
    size_t count;
    ptl_match_bits_t match_bits;
    ompi_coll_portals4_request_t *request;
    ptl_handle_md_t md_h;
    void *base;

    ompi_coll_portals4_get_md(0, &md_h, &base);

    OMPI_COLL_PORTALS4_REQUEST_ALLOC(comm, request);
    if (NULL == request) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: request alloc failed\n",
                            __FILE__, __LINE__);
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }
    *ompi_req = &request->super;
    request->type = OMPI_COLL_PORTALS4_TYPE_BARRIER;

    count = opal_atomic_add_size_t(&portals4_module->barrier_count, 1);

    ret = PtlCTAlloc(mca_coll_portals4_component.ni_h,
                     &request->ct_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlCTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    COLL_PORTALS4_SET_BITS(match_bits, ompi_comm_get_cid(comm), 
                          0, COLL_PORTALS4_BARRIER, count);

    /* Build "tree" out of hypercube */
    dim = comm->c_cube_dim;
    hibit = opal_hibit(rank, dim);
    --dim;
    /* calculate number of children to receive from */
    num_msgs = ompi_coll_portals4_get_nchildren(dim + 1, hibit, rank, size);

    /* receive space */
    me.start = NULL;
    me.length = 0;
    me.ct_handle = request->ct_h;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
        PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_CT_COMM | PTL_ME_EVENT_CT_OVERFLOW;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = match_bits;
    me.ignore_bits = 0;
    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                      mca_coll_portals4_component.pt_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &request->me_h);
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
                              match_bits,
                              0,
                              NULL,
                              0,
                              request->ct_h,
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
                                  request->ct_h,
                                  num_msgs);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                    "%s:%d: PtlTriggeredPut failed: %d\n",
                                    __FILE__, __LINE__, ret);
                return OMPI_ERROR;
            }
        }
    }

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
                          request->ct_h,
                          num_msgs);

    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlTriggeredPut failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


int
ompi_coll_portals4_ibarrier_intra_fini(ompi_coll_portals4_request_t *request)
{
    int ret;

    /* cleanup */
    ret = PtlMEUnlink(request->me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlMEUnlink failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }
    ret = PtlCTFree(request->ct_h); 
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlCTFree failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_complete(&request->super, true);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    return OMPI_SUCCESS;
}

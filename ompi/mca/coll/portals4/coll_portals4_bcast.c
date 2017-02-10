/*
 * Copyright (c) 2015      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
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
#include "ompi/datatype/ompi_datatype.h"

/*
 * the bcast communication is based on 1 to N scheme
 *
 */

#define COLL_PORTALS4_BCAST_MAX_CHILDREN    2
#define COLL_PORTALS4_BCAST_ALGO_THRESHOLD	4


static int prepare_bcast_data (struct ompi_communicator_t *comm,
        void *buff, int count,
        struct ompi_datatype_t *datatype, int root,
        ompi_coll_portals4_request_t *request) {
    int rank = ompi_comm_rank(comm);
    int ret;
    size_t max_data;
    unsigned int iov_count;
    struct iovec iovec;

    request->u.bcast.is_root = (rank == root);
    request->u.bcast.needs_pack = !ompi_datatype_is_contiguous_memory_layout(datatype, count);

    if (request->u.bcast.needs_pack) {
        if (request->u.bcast.is_root) {
            OBJ_CONSTRUCT(&request->u.bcast.convertor, opal_convertor_t);
            opal_convertor_copy_and_prepare_for_send(ompi_mpi_local_convertor,
                    &(datatype->super), count,
                    buff, 0, &request->u.bcast.convertor);
            opal_convertor_get_packed_size(&request->u.bcast.convertor, &request->u.bcast.tmpsize);
            request->u.bcast.tmpbuf = malloc(request->u.bcast.tmpsize);
            if (OPAL_UNLIKELY(NULL == request->u.bcast.tmpbuf)) {
                OBJ_DESTRUCT(&request->u.bcast.convertor);
                return opal_stderr("malloc failed", __FILE__, __LINE__, OMPI_ERR_OUT_OF_RESOURCE);
            }

            iovec.iov_base = request->u.bcast.tmpbuf;
            iovec.iov_len = request->u.bcast.tmpsize;
            iov_count = 1;
            max_data = request->u.bcast.tmpsize;
            ret = opal_convertor_pack(&request->u.bcast.convertor, &iovec, &iov_count, &max_data);
            OBJ_DESTRUCT(&request->u.bcast.convertor);
            if (OPAL_UNLIKELY(ret < 0)) {
                return opal_stderr("opal_convertor_pack failed", __FILE__, __LINE__, ret);	}
        }
        else {
            OBJ_CONSTRUCT(&request->u.bcast.convertor, opal_convertor_t);
            opal_convertor_copy_and_prepare_for_recv(ompi_mpi_local_convertor,
                    &(datatype->super), count,
                    buff, 0, &request->u.bcast.convertor);

            max_data = request->u.bcast.tmpsize;
            opal_convertor_get_packed_size(&request->u.bcast.convertor, &max_data);

            request->u.bcast.tmpbuf = malloc(request->u.bcast.tmpsize);
            if (OPAL_UNLIKELY(NULL == request->u.bcast.tmpbuf)) {
                OBJ_DESTRUCT(&request->u.bcast.convertor);
                return opal_stderr("malloc failed", __FILE__, __LINE__, OMPI_ERR_OUT_OF_RESOURCE);
            }
        }
    }
    else {
        request->u.bcast.tmpbuf = buff;

        ompi_datatype_type_size(datatype, &request->u.bcast.tmpsize);
        request->u.bcast.tmpsize *= count;
    }

    /* Number of segments */
    {
        size_t max_msg_size = (COLL_PORTALS4_MAX_BW >  mca_coll_portals4_component.ni_limits.max_msg_size) ?
            mca_coll_portals4_component.ni_limits.max_msg_size :
            COLL_PORTALS4_MAX_BW;

        //TODO : Either make compatible Portals size limits and COLL_PORTALS4_MAX_SEGMENT or remove COLL_PORTALS4_MAX_SEGMENT
        request->u.bcast.segment_nb =  (request->u.bcast.tmpsize > max_msg_size) ?
            (((request->u.bcast.tmpsize + max_msg_size -1)  / max_msg_size) < COLL_PORTALS4_MAX_SEGMENT ?
                ((request->u.bcast.tmpsize + max_msg_size -1)  / max_msg_size) : COLL_PORTALS4_MAX_SEGMENT) :
                    1;

        OPAL_OUTPUT_VERBOSE((10, ompi_coll_base_framework.framework_output,
                "seg_number=%d , seg_size_max=%lu", request->u.bcast.segment_nb, max_msg_size));
    }
    if (request->u.bcast.segment_nb > COLL_PORTALS4_BCAST_ALGO_THRESHOLD) {
        request->u.bcast.algo = OMPI_COLL_PORTALS4_BCAST_PIPELINE_ALGO;
    }
    else {
        request->u.bcast.algo = OMPI_COLL_PORTALS4_BCAST_KARY_TREE_ALGO;
    }
    return (OMPI_SUCCESS);
}

static int post_bcast_data(	ompi_coll_portals4_request_t *request) {

    int ret;
    size_t max_data;
    unsigned int iov_count;
    struct iovec iovec;

    if (request->u.bcast.needs_pack) {
        if (!request->u.bcast.is_root) {
            opal_convertor_get_packed_size(&request->u.bcast.convertor, &request->u.bcast.tmpsize);

            iovec.iov_base = request->u.bcast.tmpbuf;
            iovec.iov_len = request->u.bcast.tmpsize;
            iov_count = 1;
            ret = opal_convertor_unpack(&request->u.bcast.convertor, &iovec, &iov_count, &max_data);
            OBJ_DESTRUCT(&request->u.bcast.convertor);
            if (OPAL_UNLIKELY(ret < 0)) {
                return opal_stderr("opal_convertor_unpack failed", __FILE__, __LINE__, ret);
            }
        }
        free(request->u.bcast.tmpbuf);
    }
    return (OMPI_SUCCESS);
}

static int
bcast_kary_tree_top(void *buff, int count,
        struct ompi_datatype_t *datatype, int root,
        struct ompi_communicator_t *comm,
        ompi_coll_portals4_request_t *request,
        mca_coll_portals4_module_t *portals4_module)
{
    bool is_sync = request->is_sync;
    int ret;
    unsigned int i, seg, seg_size, nb_long;
    unsigned int segment_nb = request->u.bcast.segment_nb;
    unsigned int child_nb;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    ptl_rank_t parent, child[COLL_PORTALS4_BCAST_MAX_CHILDREN];
    size_t internal_count, length, offset;
    ptl_handle_md_t zero_md_h, data_md_h;
    ptl_handle_me_t me_h;
    ptl_ct_event_t ct_inc;
    ptl_me_t me;
    ptl_match_bits_t match_bits_ack, match_bits_rtr, match_bits;
    ptl_ct_event_t ct;
    ptl_size_t trig_thr, ack_thr;

    /*
     ** Initialization
     */

    request->type = OMPI_COLL_PORTALS4_TYPE_BCAST;

    for (i = 0 ; i < COLL_PORTALS4_BCAST_MAX_CHILDREN ; i++) {
        child[i] = PTL_INVALID_RANK;
    }

    parent = PTL_INVALID_RANK;

    zero_md_h = mca_coll_portals4_component.zero_md_h;
    data_md_h = mca_coll_portals4_component.data_md_h;

    internal_count = opal_atomic_add_size_t(&portals4_module->coll_count, 1);


    /*
     ** DATATYPE and SIZES
     */

    get_k_ary_tree(COLL_PORTALS4_BCAST_MAX_CHILDREN,
            rank, size, root, &parent, child, &child_nb);
    request->u.bcast.u.child_nb = child_nb;

    /*
     * TOPOLOGY
     */

    /*
     * PORTALS4 RESOURCE ALLOCATION
     */

    if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.bcast.trig_ct_h)) != 0) {
        return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
    }

    /* Compute match bits */
    COLL_PORTALS4_SET_BITS(match_bits_ack, ompi_comm_get_cid(comm), 1, 0,
            COLL_PORTALS4_BCAST, 0, internal_count);

    COLL_PORTALS4_SET_BITS(match_bits_rtr, ompi_comm_get_cid(comm), 0, 1,
            COLL_PORTALS4_BCAST, 0, internal_count);

    COLL_PORTALS4_SET_BITS(match_bits, ompi_comm_get_cid(comm), 0, 0,
            COLL_PORTALS4_BCAST, 0, internal_count);

    /* The data will be cut in segment_nb segments.
     * nb_long segments will have a size of (seg_size + 1)
     * and (segment_nb - nb_long) segments will have a size of seg_size
     */
    seg_size = request->u.bcast.tmpsize / segment_nb;
    nb_long = request->u.bcast.tmpsize % segment_nb;
    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "seg_size=%d nb_long=%d segment_nb=%d", seg_size, nb_long, segment_nb);

    if (rank != root) {
        for (seg = 1, offset = 0, length = 0 ;
                seg <= segment_nb ;
                seg++, offset += length) {

            /* Divide buffer into segments */
            if (seg <= nb_long) length = seg_size + 1;
            else length = seg_size;

            /*
             ** Prepare Data ME
             */

            memset(&me, 0, sizeof(ptl_me_t));
            me.start = ((uint8_t*) request->u.bcast.tmpbuf) + offset;
            me.length = length;
            me.ct_handle = request->u.bcast.trig_ct_h;
            me.uid = mca_coll_portals4_component.uid;
            me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
                    PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
                    PTL_ME_USE_ONCE |
                    PTL_ME_EVENT_CT_COMM;
            me.match_id.phys.nid = PTL_NID_ANY;
            me.match_id.phys.pid = PTL_PID_ANY;
            me.match_bits = match_bits;
            me.ignore_bits = 0;
            if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                    mca_coll_portals4_component.pt_idx,
                    &me,
                    PTL_PRIORITY_LIST,
                    NULL,
                    &me_h)) != 0) {
                return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
            }
        }

        /*
         * Send RTR to parent
         *
         * the root does not to have to do it, since it does not have parent.
         * WE can do such an operation by now, since we are able to receive data,
         * even if we are not able to receive the others.
         *
         */

        /* and there, we only send the RTR when all the MEs are ready */
        if ((ret = PtlPut(zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                ompi_coll_portals4_get_peer(comm, parent),
                mca_coll_portals4_component.pt_idx, match_bits_rtr,
                0, NULL, 0)) != PTL_OK) {
            return opal_stderr("Put RTR failed %d", __FILE__, __LINE__, ret);
        }

        /*
         * Prepare Triggered Put to ACK Data to parent
         *
         */

        trig_thr = child_nb ? (segment_nb * 2) :
                segment_nb;

        if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                ompi_coll_portals4_get_peer(comm, parent),
                mca_coll_portals4_component.pt_idx,
                match_bits_ack, 0, NULL, 0,
                request->u.bcast.trig_ct_h, trig_thr)) != 0) {
            return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
        }
    }

    if (child_nb) {
        if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.bcast.rtr_ct_h)) != 0) {
            return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
        }
        ct_inc.success = segment_nb;
        ct_inc.failure = 0;

        if ((ret = PtlTriggeredCTInc(request->u.bcast.trig_ct_h, ct_inc,
                request->u.bcast.rtr_ct_h, child_nb)) != 0) {
            return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
        }

        if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.bcast.ack_ct_h)) != 0) {
            return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
        }

        /*
         ** Prepare ME for receiving data ACK Put
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
        me.ct_handle = request->u.bcast.ack_ct_h;

        for (i = 0 ; i < child_nb ; i++) {
            if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                    mca_coll_portals4_component.pt_idx,
                    &me, PTL_PRIORITY_LIST,  NULL,
                    &me_h)) != 0) {
                return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
            }
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
        me.ct_handle = request->u.bcast.rtr_ct_h;

        for (i = 0 ; i < child_nb ; i++) {
            if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                    mca_coll_portals4_component.pt_idx,
                    &me, PTL_PRIORITY_LIST,
                    NULL,
                    &me_h)) != 0) {
                return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
            }
        }

        for (seg = 1, offset = 0, length = 0 ;
                seg <= segment_nb ;
                seg++, offset += length) {

            /* Divide buffer into segments */
            if (seg <= nb_long) length = seg_size + 1;
            else length = seg_size;
            opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                "bcast with k-ary tree : segment of size %ld", length);

            /* compute the triggering threshold to send data to the children */
            trig_thr = segment_nb + seg - 1; /* To be sure the set of PtlTriggeredPut of DATA will be executed in order */
            if (rank != root) trig_thr ++;

            /*
             ** Send Data to children
             */

            for (i = 0 ; i < COLL_PORTALS4_BCAST_MAX_CHILDREN ; i++) {
                if (child[i] != PTL_INVALID_RANK) {

                    if ((ret = PtlTriggeredPut (data_md_h,
                            (uint64_t) request->u.bcast.tmpbuf + offset,
                            length, PTL_NO_ACK_REQ,
                            ompi_coll_portals4_get_peer(comm, child[i]),
                            mca_coll_portals4_component.pt_idx,
                            match_bits, 0,
                            NULL,
                            0, request->u.bcast.trig_ct_h, trig_thr)) != 0) {
                        return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                    }
                }
            }
        }

        if (rank == root) {
            trig_thr = segment_nb;
            ct_inc.success = segment_nb;
            ct_inc.failure = 0;

            if ((ret = PtlTriggeredCTInc(request->u.bcast.trig_ct_h, ct_inc,
                   request->u.bcast.trig_ct_h, trig_thr)) != 0) {
                return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
            }
        }

        ack_thr = child_nb;

        if (is_sync) {
            if ((ret = PtlCTWait(request->u.bcast.ack_ct_h, ack_thr, &ct)) != 0)
                opal_stderr("PtlCTWait failed", __FILE__, __LINE__, ret);
        }
        else {
            if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                    ompi_coll_portals4_get_peer(comm, rank),
                    mca_coll_portals4_component.finish_pt_idx,
                    0, 0, NULL, (uintptr_t) request,
                    request->u.bcast.ack_ct_h,
                    ack_thr)) != 0) {
                return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
            }
        }
    }
    else {
        /* A leaf of the tree does not need to send data to its children */
        request->u.bcast.rtr_ct_h = PTL_INVALID_HANDLE;
        request->u.bcast.ack_ct_h = PTL_INVALID_HANDLE;

        /* a leaf does not expect counting events from its children,
         * the threshold is computed using the number of segments received
         * from the parent
         */

        if (rank != root) {
            trig_thr = segment_nb;
            if (is_sync) {
                /* Each leaf has a pending PtlTriggeredPut (to send the final ACK). We must call PtlTriggeredCTInc twice.
                   Otherwise, we could pass the PtlCTWait and then free the CT too early and the Put wouldn't be triggered.

                   This is necessary because portals4 does not insure the order in the triggered operations associated
                   with the same threshold. In the case where PtlCTWait is not called (else case), this is not necessary. */

                ct_inc.success = 1;
                ct_inc.failure = 0;

                if ((ret = PtlTriggeredCTInc(request->u.bcast.trig_ct_h, ct_inc,
                        request->u.bcast.trig_ct_h, trig_thr)) != 0) {
                    return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
                }

                if ((ret = PtlTriggeredCTInc(request->u.bcast.trig_ct_h, ct_inc,
                        request->u.bcast.trig_ct_h, trig_thr + 1)) != 0) {
                    return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
                }

                if ((ret = PtlCTWait(request->u.bcast.trig_ct_h, trig_thr + 2, &ct)) != 0) {
                    opal_stderr("PtlCTWait failed", __FILE__, __LINE__, ret);
                }
            }
            else {
                if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                        ompi_coll_portals4_get_peer(comm, rank),
                        mca_coll_portals4_component.finish_pt_idx,
                        0, 0, NULL, (uintptr_t) request,
                        request->u.bcast.trig_ct_h,
                        trig_thr)) != 0) {
                    return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                }

            }
        }
    }
    return (OMPI_SUCCESS);
}


static int
bcast_pipeline_top(void *buff, int count,
        struct ompi_datatype_t *datatype, int root,
        struct ompi_communicator_t *comm,
        ompi_coll_portals4_request_t *request,
        mca_coll_portals4_module_t *portals4_module)
{
    bool is_sync = request->is_sync;
    int ret;
    unsigned int seg, seg_size, nb_long;
    unsigned int segment_nb = request->u.bcast.segment_nb;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);
    ptl_rank_t parent, child;
    size_t internal_count, length, offset;
    ptl_handle_md_t zero_md_h, data_md_h;
    ptl_handle_me_t me_h;
    ptl_ct_event_t ct_inc;
    ptl_me_t me;
    ptl_match_bits_t match_bits_ack, match_bits_rtr, match_bits;
    ptl_ct_event_t ct;
    ptl_size_t trig_thr;

    /*
     ** Initialization
     */

    request->type = OMPI_COLL_PORTALS4_TYPE_BCAST;

    child = PTL_INVALID_RANK;
    parent = PTL_INVALID_RANK;

    zero_md_h = mca_coll_portals4_component.zero_md_h;
    data_md_h = mca_coll_portals4_component.data_md_h;

    internal_count = opal_atomic_add_size_t(&portals4_module->coll_count, 1);

    /*
     ** DATATYPE and SIZES
     */

    get_pipeline(rank, size, root, &parent, &child);
    request->u.bcast.u.child = child;

    /*
     * PORTALS4 RESOURCE ALLOCATION
     */

    if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.bcast.trig_ct_h)) != 0) {
        return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
    }

    /* Compute match bits */
    COLL_PORTALS4_SET_BITS(match_bits_ack, ompi_comm_get_cid(comm), 1, 0,
            COLL_PORTALS4_BCAST, 0, internal_count);

    COLL_PORTALS4_SET_BITS(match_bits_rtr, ompi_comm_get_cid(comm), 0, 1,
            COLL_PORTALS4_BCAST, 0, internal_count);

    COLL_PORTALS4_SET_BITS(match_bits, ompi_comm_get_cid(comm), 0, 0,
            COLL_PORTALS4_BCAST, 0, internal_count);
    /* The data will be cut in segment_nb segments.
     * nb_long segments will have a size of (seg_size + 1)
     * and (segment_nb - nb_long) segments will have a size of seg_size
     */
    seg_size = request->u.bcast.tmpsize / segment_nb;
    nb_long = request->u.bcast.tmpsize % segment_nb;
    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "seg_size=%d nb_long=%d", seg_size, nb_long);

    if (rank != root) {
        for (seg = 1, offset = 0, length = 0 ;
                seg <= segment_nb ;
                seg++, offset += length) {

            /* Divide buffer into segments */
            if (seg <= nb_long) length = seg_size + 1;
            else length = seg_size;

            /*
             ** Prepare Data ME
             */

            memset(&me, 0, sizeof(ptl_me_t));
            me.start = ((uint8_t*) request->u.bcast.tmpbuf) + offset;
            me.length = length;
            me.ct_handle = request->u.bcast.trig_ct_h;
            me.uid = mca_coll_portals4_component.uid;
            me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
                    PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE |
                    PTL_ME_USE_ONCE |
                    PTL_ME_EVENT_CT_COMM;
            me.match_id.phys.nid = PTL_NID_ANY;
            me.match_id.phys.pid = PTL_PID_ANY;
            me.match_bits = match_bits;
            me.ignore_bits = 0;
            if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                    mca_coll_portals4_component.pt_idx,
                    &me,
                    PTL_PRIORITY_LIST,
                    NULL,
                    &me_h)) != 0) {
                return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
            }
        }

        /*
         * Send RTR to parent
         *
         * the root does not to have to do it, since it does not have parent.
         * WE can do such an operation by now, since we are able to receive data,
         * even if we are not able to receive the others.
         *
         */

        /* and there, we only send the RTR when all the MEs are ready */
        if ((ret = PtlPut(zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                ompi_coll_portals4_get_peer(comm, parent),
                mca_coll_portals4_component.pt_idx, match_bits_rtr,
                0, NULL, 0)) != PTL_OK) {
            return opal_stderr("Put RTR failed %d", __FILE__, __LINE__, ret);
        }

        /*
         * Prepare Triggered Put to ACK Data to parent
         *
         */

        trig_thr = (child != PTL_INVALID_RANK) ?
                (segment_nb * 2) :
                segment_nb;

        if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                ompi_coll_portals4_get_peer(comm, parent),
                mca_coll_portals4_component.pt_idx,
                match_bits_ack, 0, NULL, 0,
                request->u.bcast.trig_ct_h, trig_thr)) != 0) {
            return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
        }
    }

    if (child != PTL_INVALID_RANK) {
        if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.bcast.rtr_ct_h)) != 0) {
            return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
        }

        ct_inc.success = segment_nb;
        ct_inc.failure = 0;

        if ((ret = PtlTriggeredCTInc(request->u.bcast.trig_ct_h, ct_inc,
                request->u.bcast.rtr_ct_h, 1)) != 0) {
            return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
        }

        if ((ret = PtlCTAlloc(mca_coll_portals4_component.ni_h, &request->u.bcast.ack_ct_h)) != 0) {
            return opal_stderr("PtlCTAlloc failed", __FILE__, __LINE__, ret);
        }

        /*
         ** Prepare ME for receiving data ACK Put
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
        me.ct_handle = request->u.bcast.ack_ct_h;

        if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                mca_coll_portals4_component.pt_idx,
                &me, PTL_PRIORITY_LIST,  NULL,
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
        me.ct_handle = request->u.bcast.rtr_ct_h;

        if ((ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                mca_coll_portals4_component.pt_idx,
                &me, PTL_PRIORITY_LIST,
                NULL,
                &me_h)) != 0) {
            return opal_stderr("PtlMEAppend failed", __FILE__, __LINE__, ret);
        }

        for (seg = 1, offset = 0, length = 0 ;
                seg <= segment_nb ;
                seg++, offset += length) {

            /* Divide buffer into segments */
            if (seg <= nb_long) length = seg_size + 1;
            else length = seg_size;
            opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                "bcast with pipeline  :  segment of size %ld \n", length);

            /* compute the triggering threshold to send data to the children */
            trig_thr = segment_nb + seg - 1; /* To be sure the PtlTriggeredPut will be executed in order */
            if (rank != root) trig_thr ++;

            /*
             ** Send Data to children
             */

            if (child != PTL_INVALID_RANK) {

                if ((ret = PtlTriggeredPut (data_md_h,
                        (uint64_t) request->u.bcast.tmpbuf + offset,
                        length, PTL_NO_ACK_REQ,
                        ompi_coll_portals4_get_peer(comm, child),
                        mca_coll_portals4_component.pt_idx,
                        match_bits, 0,
                        NULL,
                        0, request->u.bcast.trig_ct_h, trig_thr)) != 0) {
                    return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                }
            }
        }
        if (rank == root) {
            trig_thr = segment_nb;
            ct_inc.success = segment_nb;
            ct_inc.failure = 0;

            if ((ret = PtlTriggeredCTInc(request->u.bcast.trig_ct_h, ct_inc,
                   request->u.bcast.trig_ct_h, trig_thr)) != 0) {
                return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
            }
        }

        if (is_sync) {
            if ((ret = PtlCTWait(request->u.bcast.ack_ct_h, 1, &ct)) != 0) {
                opal_stderr("PtlCTWait failed", __FILE__, __LINE__, ret);
            }
        }
        else {
            if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                    ompi_coll_portals4_get_peer(comm, rank),
                    mca_coll_portals4_component.finish_pt_idx,
                    0, 0, NULL, (uintptr_t) request,
                    request->u.bcast.ack_ct_h,
                    1)) != 0) {
                return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
            }
        }
    }
    else {
        /* A leaf of the tree does not need to send data to its children */
        request->u.bcast.rtr_ct_h = PTL_INVALID_HANDLE;
        request->u.bcast.ack_ct_h = PTL_INVALID_HANDLE;

        /* a leaf does not expect counting events from its children,
         * the threshold is computed using the number of segments received
         * from the parent
         */

        if (rank != root) {
            trig_thr = segment_nb;

            if (is_sync) {
                /* Each leaf has a pending PtlTriggeredPut (to send the final ACK). We must call PtlTriggeredCTInc twice.
                   Otherwise, we could pass the PtlCTWait and then free the CT too early and the Put wouldn't be triggered.

                   This is necessary because portals4 does not insure the order in the triggered operations associated
                   with the same threshold. In the case where PtlCTWait is not called (else case), this is not necessary. */

                ct_inc.success = 1;
                ct_inc.failure = 0;

                if ((ret = PtlTriggeredCTInc(request->u.bcast.trig_ct_h, ct_inc,
                        request->u.bcast.trig_ct_h, trig_thr)) != 0) {
                    return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
                }

                if ((ret = PtlTriggeredCTInc(request->u.bcast.trig_ct_h, ct_inc,
                        request->u.bcast.trig_ct_h, trig_thr + 1)) != 0) {
                    return opal_stderr("PtlTriggeredCTInc failed", __FILE__, __LINE__, ret);
                }

                if ((ret = PtlCTWait(request->u.bcast.trig_ct_h, trig_thr + 2, &ct)) != 0) {
                    opal_stderr("PtlCTWait failed", __FILE__, __LINE__, ret);
                }
            }
            else {
                if ((ret = PtlTriggeredPut (zero_md_h, 0, 0, PTL_NO_ACK_REQ,
                        ompi_coll_portals4_get_peer(comm, rank),
                        mca_coll_portals4_component.finish_pt_idx,
                        0, 0, NULL, (uintptr_t) request,
                        request->u.bcast.trig_ct_h,
                        trig_thr)) != 0) {
                    return opal_stderr("PtlTriggeredPut failed", __FILE__, __LINE__, ret);
                }
            }
        }
    }

    return (OMPI_SUCCESS);
}



static int
bcast_kary_tree_bottom(ompi_coll_portals4_request_t *request)
{
    /* release all Portals4 resources for this request */
    if (request->u.bcast.u.child_nb) {
        PtlCTFree(request->u.bcast.rtr_ct_h);
        PtlCTFree(request->u.bcast.ack_ct_h);
    }

    PtlCTFree(request->u.bcast.trig_ct_h);

    return (OMPI_SUCCESS);
}


static int
bcast_pipeline_bottom(ompi_coll_portals4_request_t *request)
{
    /* release all Portals4 resources for this request */
    if (request->u.bcast.u.child != PTL_INVALID_RANK) {
        PtlCTFree(request->u.bcast.rtr_ct_h);
        PtlCTFree(request->u.bcast.ack_ct_h);
    }

    PtlCTFree(request->u.bcast.trig_ct_h);
    return (OMPI_SUCCESS);
}


int
ompi_coll_portals4_bcast_intra(void *buff, int count,
        struct ompi_datatype_t *datatype, int root,
        struct ompi_communicator_t *comm,
        mca_coll_base_module_t *module)
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

    prepare_bcast_data(comm, buff, count, datatype, root, request);

    switch (request->u.bcast.algo) {
    case OMPI_COLL_PORTALS4_BCAST_KARY_TREE_ALGO:
        bcast_kary_tree_top(buff, count, datatype, root,
                comm, request, portals4_module);
        bcast_kary_tree_bottom(request);
        break;
    case OMPI_COLL_PORTALS4_BCAST_PIPELINE_ALGO:
        bcast_pipeline_top(buff, count, datatype, root,
                comm, request, portals4_module);
        bcast_pipeline_bottom(request);
        break;
    default:
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: unknown bcast algorithm %d\n",
                __FILE__, __LINE__, request->u.bcast.algo);
        return OMPI_ERROR;
    }
    post_bcast_data(request);

    OMPI_COLL_PORTALS4_REQUEST_RETURN(request);
    return (OMPI_SUCCESS);
}


int
ompi_coll_portals4_ibcast_intra(void *buff, int count,
        struct ompi_datatype_t *datatype, int root,
        struct ompi_communicator_t *comm,
        ompi_request_t **ompi_request,
        mca_coll_base_module_t *module)
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
    request->is_sync = false;

    prepare_bcast_data(comm, buff, count, datatype, root, request);

    switch (request->u.bcast.algo) {
    case OMPI_COLL_PORTALS4_BCAST_KARY_TREE_ALGO:
        bcast_kary_tree_top(buff, count, datatype, root,
                comm, request, portals4_module);
        break;
    case OMPI_COLL_PORTALS4_BCAST_PIPELINE_ALGO:
        bcast_pipeline_top(buff, count, datatype, root,
                comm, request, portals4_module);
        break;
    default:
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: unknown bcast algorithm %d\n",
                __FILE__, __LINE__, request->u.bcast.algo);
        return OMPI_ERROR;
    }

    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "ibcast_intra");
    return (OMPI_SUCCESS);
}


int
ompi_coll_portals4_ibcast_intra_fini(ompi_coll_portals4_request_t *request)
{

    switch (request->u.bcast.algo) {
    case OMPI_COLL_PORTALS4_BCAST_KARY_TREE_ALGO:
        bcast_kary_tree_bottom(request);
        break;
    case OMPI_COLL_PORTALS4_BCAST_PIPELINE_ALGO:
        bcast_pipeline_bottom(request);
        break;
    default:
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: unknown bcast algorithm %d\n",
                __FILE__, __LINE__, request->u.bcast.algo);
        return OMPI_ERROR;
    }

    post_bcast_data(request);

    ompi_request_complete(&request->super, true);

    opal_output_verbose(10, ompi_coll_base_framework.framework_output, "ibcast_intra_fini");
    return (OMPI_SUCCESS);
}

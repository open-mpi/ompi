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

#include "ompi_config.h"

#include "osc_pt2pt.h"
#include "osc_pt2pt_sendreq.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_data_move.h"
#include "osc_pt2pt_obj_convert.h"

#include "opal/util/output.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/datatype/datatype.h"

static inline int32_t
create_send_tag(ompi_osc_pt2pt_module_t *module)
{
#if OMPI_HAVE_THREAD_SUPPORT && OPAL_HAVE_ATOMIC_CMPSET_32
    int32_t newval, oldval;
    do {
        oldval = module->p2p_tag_counter;
        newval = (oldval + 1) % mca_pml.pml_max_tag;
    } while (0 == opal_atomic_cmpset_32(&module->p2p_tag_counter, oldval, newval));
    return newval;
#elif OMPI_HAVE_THREAD_SUPPORT 
    int32_t ret;
    /* no compare and swap - have to lock the module */
    OPAL_THREAD_LOCK(&module->p2p_lock);
    module->p2p_tag_counter = (module->p2p_tag_counter + 1) % mca_pml.pml_max_tag;
    ret = module->p2p_tag_counter;
    OPAL_THREAD_UNLOCK(&module->p2p_lock);
    return ret;
#else
    module->p2p_tag_counter = (module->p2p_tag_counter + 1) % mca_pml.pml_max_tag;
    return module->p2p_tag_counter;
#endif
}


/**********************************************************************
 *
 * Sending a sendreq to target
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_sendreq_send_long_cb(ompi_osc_pt2pt_longreq_t *longreq)
{
    ompi_osc_pt2pt_sendreq_t *sendreq = 
        (ompi_osc_pt2pt_sendreq_t*) longreq->req_comp_cbdata;

    opal_output(-1, "%d completed long sendreq to %d",
                sendreq->req_module->p2p_comm->c_my_rank,
                sendreq->req_target_rank);

    opal_list_remove_item(&(sendreq->req_module->p2p_long_msgs), &(longreq->super));

    ompi_osc_pt2pt_longreq_free(longreq);

    OPAL_THREAD_ADD32(&(sendreq->req_module->p2p_num_pending_out), -1);
    ompi_osc_pt2pt_sendreq_free(sendreq);
}


static void
ompi_osc_pt2pt_sendreq_send_cb(struct mca_btl_base_module_t* btl, 
                           struct mca_btl_base_endpoint_t *endpoint,
                           struct mca_btl_base_descriptor_t* descriptor,
                           int status)
{
    ompi_osc_pt2pt_sendreq_t *sendreq = 
        (ompi_osc_pt2pt_sendreq_t*) descriptor->des_cbdata;
    ompi_osc_pt2pt_send_header_t *header =
        (ompi_osc_pt2pt_send_header_t*) descriptor->des_src[0].seg_addr.pval;

    if (OMPI_SUCCESS != status) {
        /* requeue and return */
        /* BWB - FIX ME - figure out where to put this bad boy */
        abort();
        return;
    }

    /* have to look at header, and not the sendreq because in the case
       of get, it's possible that the sendreq has been freed already
       (if the remote side replies before we get our send completion
       callback) and already allocated to another request.  We don't
       wait for this completion before exiting a synchronization point
       in the case of get, as we really don't care when it completes -
       only when the data arrives. */
    if (OMPI_OSC_PT2PT_HDR_GET != header->hdr_base.hdr_type) {
        /* do we need to post a send? */
        if (header->hdr_msg_length != 0) {
            /* sendreq is done.  Mark it as so and get out of here */
            OPAL_THREAD_ADD32(&(sendreq->req_module->p2p_num_pending_out), -1);
            ompi_osc_pt2pt_sendreq_free(sendreq);
        } else {
            ompi_osc_pt2pt_longreq_t *longreq;
            ompi_osc_pt2pt_longreq_alloc(&longreq);

            longreq->req_comp_cb = ompi_osc_pt2pt_sendreq_send_long_cb;
            longreq->req_comp_cbdata = sendreq;
            opal_output(-1, "%d starting long sendreq to %d (%d)",
                        sendreq->req_module->p2p_comm->c_my_rank,
                        sendreq->req_target_rank,
                        header->hdr_origin_tag);
                        
            mca_pml.pml_isend(sendreq->req_origin_convertor.pBaseBuf,
                              sendreq->req_origin_convertor.count,
                              sendreq->req_origin_datatype,
                              sendreq->req_target_rank,
                              header->hdr_origin_tag,
                              MCA_PML_BASE_SEND_STANDARD,
                              sendreq->req_module->p2p_comm,
                              &(longreq->req_pml_req));

            /* put the send request in the waiting list */
            OPAL_THREAD_LOCK(&(sendreq->req_module->p2p_lock));
            opal_list_append(&(sendreq->req_module->p2p_long_msgs), &(longreq->super));
            OPAL_THREAD_UNLOCK(&(sendreq->req_module->p2p_lock));
        }
    }
    
    /* release the descriptor and sendreq */
    btl->btl_free(btl, descriptor);

    /* any other sendreqs to restart? */
    /* BWB - FIX ME - implement sending the next sendreq here */
}


/* create the initial fragment, pack header, datatype, and payload (if
   size fits) and send */
int
ompi_osc_pt2pt_sendreq_send(ompi_osc_pt2pt_module_t *module,
                            ompi_osc_pt2pt_sendreq_t *sendreq)
{
    int ret = OMPI_SUCCESS;
    mca_bml_base_endpoint_t *endpoint = NULL;
    mca_bml_base_btl_t *bml_btl = NULL;
    mca_btl_base_descriptor_t *descriptor = NULL;
    ompi_osc_pt2pt_send_header_t *header = NULL;
    size_t written_data = 0;
    size_t needed_len = sizeof(ompi_osc_pt2pt_send_header_t);
    const void *packed_ddt;
    size_t packed_ddt_len = ompi_ddt_pack_description_length(sendreq->req_target_datatype);

    /* we always need to send the ddt */
    needed_len += packed_ddt_len;
    if (OMPI_OSC_PT2PT_GET != sendreq->req_type) {
        needed_len += sendreq->req_origin_bytes_packed;
    }

    /* Get a BTL so we have the eager limit */
    endpoint = (mca_bml_base_endpoint_t*) sendreq->req_target_proc->proc_pml;
    bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
    descriptor = bml_btl->btl_alloc(bml_btl->btl,
                                    needed_len < bml_btl->btl_eager_limit ? needed_len :
                                    bml_btl->btl_eager_limit);
    if (NULL == descriptor) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* verify at least enough space for header */
    if (descriptor->des_src[0].seg_len < sizeof(ompi_osc_pt2pt_send_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup descriptor */
    descriptor->des_cbfunc = ompi_osc_pt2pt_sendreq_send_cb;
    descriptor->des_cbdata = (void*) sendreq;
    descriptor->des_flags = MCA_BTL_DES_FLAGS_PRIORITY;

    /* pack header */
    header = (ompi_osc_pt2pt_send_header_t*) descriptor->des_src[0].seg_addr.pval;
    written_data += sizeof(ompi_osc_pt2pt_send_header_t);
    header->hdr_windx = sendreq->req_module->p2p_comm->c_contextid;
    header->hdr_origin = sendreq->req_module->p2p_comm->c_my_rank;
    header->hdr_origin_sendreq.pval = (void*) sendreq;
    header->hdr_origin_tag = 0;
    header->hdr_target_disp = sendreq->req_target_disp;
    header->hdr_target_count = sendreq->req_target_count;

    switch (sendreq->req_type) {
    case OMPI_OSC_PT2PT_PUT:
        header->hdr_base.hdr_type = OMPI_OSC_PT2PT_HDR_PUT;
#if OMPI_ENABLE_MEM_DEBUG
        header->hdr_target_op = 0;
#endif
        break;

    case OMPI_OSC_PT2PT_ACC:
        header->hdr_base.hdr_type = OMPI_OSC_PT2PT_HDR_ACC;
        header->hdr_target_op = sendreq->req_op_id;
        break;

    case OMPI_OSC_PT2PT_GET:
        header->hdr_base.hdr_type = OMPI_OSC_PT2PT_HDR_GET;
#if OMPI_ENABLE_MEM_DEBUG
        header->hdr_target_op = 0;
#endif
        break;
    }

    /* Set datatype id and / or pack datatype */
    ret = ompi_ddt_get_pack_description(sendreq->req_target_datatype, &packed_ddt);
    if (OMPI_SUCCESS != ret) goto cleanup;
    memcpy((unsigned char*) descriptor->des_src[0].seg_addr.pval + written_data,
           packed_ddt, packed_ddt_len);
    written_data += packed_ddt_len;

    if (OMPI_OSC_PT2PT_GET != sendreq->req_type) {
        /* if sending data and it fits, pack payload */
        if (descriptor->des_src[0].seg_len >=
            written_data + sendreq->req_origin_bytes_packed) {
            struct iovec iov;
            uint32_t iov_count = 1;
            int32_t free_after;
            size_t max_data = sendreq->req_origin_bytes_packed;

            iov.iov_len = max_data;
            iov.iov_base = (unsigned char*) descriptor->des_src[0].seg_addr.pval + written_data;

            ret = ompi_convertor_pack(&sendreq->req_origin_convertor, &iov, &iov_count,
                                      &max_data, &free_after);
            if (ret < 0) {
                ret = OMPI_ERR_FATAL;
                goto cleanup;
            }

            assert(max_data == sendreq->req_origin_bytes_packed);
            written_data += max_data;
            descriptor->des_src[0].seg_len = written_data;

            header->hdr_msg_length = sendreq->req_origin_bytes_packed;
        } else {
            header->hdr_msg_length = 0;
            header->hdr_origin_tag = create_send_tag(module);
        }
    } else {
        descriptor->des_src[0].seg_len = written_data;
        header->hdr_msg_length = 0;
    }

#if 0 /* BWB - FIX ME */
    /* put in network byte order */
    OMPI_OSC_PT2PT_REQ_HDR_HTON(header);
#endif

    /* send fragment */
    opal_output(-1, "%d sending sendreq to %d",
                sendreq->req_module->p2p_comm->c_my_rank,
                sendreq->req_target_rank);
                
    ret = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_OSC_PT2PT);
    goto done;

 cleanup:
    if (descriptor != NULL) {
        mca_bml_base_free(bml_btl, descriptor);
    }

 done:
    return ret;
}


/**********************************************************************
 *
 * Sending a replyreq back to origin
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_replyreq_send_long_cb(ompi_osc_pt2pt_longreq_t *longreq)
{
    ompi_osc_pt2pt_replyreq_t *replyreq = 
        (ompi_osc_pt2pt_replyreq_t*) longreq->req_comp_cbdata;

    opal_list_remove_item(&(replyreq->rep_module->p2p_long_msgs), &(longreq->super));

    ompi_osc_pt2pt_longreq_free(longreq);

    OPAL_THREAD_ADD32(&(replyreq->rep_module->p2p_num_pending_in), -1);
    ompi_osc_pt2pt_replyreq_free(replyreq);
}


static void
ompi_osc_pt2pt_replyreq_send_cb(struct mca_btl_base_module_t* btl, 
                             struct mca_btl_base_endpoint_t *endpoint,
                             struct mca_btl_base_descriptor_t* descriptor,
                             int status)
{
    ompi_osc_pt2pt_replyreq_t *replyreq = 
        (ompi_osc_pt2pt_replyreq_t*) descriptor->des_cbdata;
    ompi_osc_pt2pt_reply_header_t *header =
        (ompi_osc_pt2pt_reply_header_t*) descriptor->des_src[0].seg_addr.pval;

    if (OMPI_SUCCESS != status) {
        /* requeue and return */
        /* BWB - FIX ME - figure out where to put this bad boy */
        abort();
        return;
    }

    /* do we need to post a send? */
    if (header->hdr_msg_length != 0) {
        /* sendreq is done.  Mark it as so and get out of here */
        OPAL_THREAD_ADD32(&(replyreq->rep_module->p2p_num_pending_in), -1);
        ompi_osc_pt2pt_replyreq_free(replyreq);
    } else {
            ompi_osc_pt2pt_longreq_t *longreq;
            ompi_osc_pt2pt_longreq_alloc(&longreq);

            longreq->req_comp_cb = ompi_osc_pt2pt_replyreq_send_long_cb;
            longreq->req_comp_cbdata = replyreq;

            mca_pml.pml_isend(replyreq->rep_target_convertor.pBaseBuf,
                              replyreq->rep_target_convertor.count,
                              replyreq->rep_target_datatype,
                              replyreq->rep_origin_rank,
                              header->hdr_target_tag,
                              MCA_PML_BASE_SEND_STANDARD,
                              replyreq->rep_module->p2p_comm,
                              &(longreq->req_pml_req));

            /* put the send request in the waiting list */
            OPAL_THREAD_LOCK(&(replyreq->rep_module->p2p_lock));
            opal_list_append(&(replyreq->rep_module->p2p_long_msgs), &(longreq->super));
            OPAL_THREAD_UNLOCK(&(replyreq->rep_module->p2p_lock));
    }
    
    /* release the descriptor and replyreq */
    btl->btl_free(btl, descriptor);

    /* any other replyreqs to restart? */
}


int
ompi_osc_pt2pt_replyreq_send(ompi_osc_pt2pt_module_t *module,
                             ompi_osc_pt2pt_replyreq_t *replyreq)
{
    int ret = OMPI_SUCCESS;
    mca_bml_base_endpoint_t *endpoint = NULL;
    mca_bml_base_btl_t *bml_btl = NULL;
    mca_btl_base_descriptor_t *descriptor = NULL;
    ompi_osc_pt2pt_reply_header_t *header = NULL;
    size_t written_data = 0;
        
    /* Get a BTL and a fragment to go with it */
    endpoint = (mca_bml_base_endpoint_t*) replyreq->rep_origin_proc->proc_pml;
    bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
    descriptor = bml_btl->btl_alloc(bml_btl->btl,
                                    bml_btl->btl_eager_limit);
    if (NULL == descriptor) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* verify at least enough space for header */
    if (descriptor->des_src[0].seg_len < sizeof(ompi_osc_pt2pt_reply_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup descriptor */
    descriptor->des_cbfunc = ompi_osc_pt2pt_replyreq_send_cb;
    descriptor->des_cbdata = (void*) replyreq;
    descriptor->des_flags = MCA_BTL_DES_FLAGS_PRIORITY;

    /* pack header */
    header = (ompi_osc_pt2pt_reply_header_t*) descriptor->des_src[0].seg_addr.pval;
    written_data += sizeof(ompi_osc_pt2pt_reply_header_t);
    header->hdr_base.hdr_type = OMPI_OSC_PT2PT_HDR_REPLY;
    header->hdr_origin_sendreq = replyreq->rep_origin_sendreq;
    header->hdr_target_tag = 0;

    /* if sending data fits, pack payload */
    if (descriptor->des_src[0].seg_len >=
        written_data + replyreq->rep_target_bytes_packed) {
        struct iovec iov;
        uint32_t iov_count = 1;
        int32_t free_after;
        size_t max_data = replyreq->rep_target_bytes_packed;

        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) descriptor->des_src[0].seg_addr.pval + written_data;

        ret = ompi_convertor_pack(&replyreq->rep_target_convertor, &iov, &iov_count,
                                  &max_data, &free_after);
        if (ret < 0) {
            ret = OMPI_ERR_FATAL;
            goto cleanup;
        }

        assert(max_data == replyreq->rep_target_bytes_packed);
        written_data += max_data;
        descriptor->des_src[0].seg_len = written_data;

        header->hdr_msg_length = replyreq->rep_target_bytes_packed;
    } else {
        header->hdr_msg_length = 0;
        header->hdr_target_tag = create_send_tag(module);
    }

#if 0 /* BWB - FIX ME */
    /* put in network byte order */
    OMPI_OSC_PT2PT_REPLYREQ_HDR_HTON(header);
#endif

    /* send fragment */
    ret = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_OSC_PT2PT);
    goto done;

 cleanup:
    if (descriptor != NULL) {
        mca_bml_base_free(bml_btl, descriptor);
    }

 done:
    return ret;
}


/**********************************************************************
 *
 * Receive a put on the target side
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_sendreq_recv_put_long_cb(ompi_osc_pt2pt_longreq_t *longreq)
{
    opal_list_remove_item(&(longreq->req_module->p2p_long_msgs), &(longreq->super));

    OBJ_RELEASE(longreq->req_datatype);
    ompi_osc_pt2pt_longreq_free(longreq);

    OPAL_THREAD_ADD32(&(longreq->req_module->p2p_num_pending_in), -1);
}


int
ompi_osc_pt2pt_sendreq_recv_put(ompi_osc_pt2pt_module_t *module,
                                ompi_osc_pt2pt_send_header_t *header,
                                void *inbuf)
{
    int ret = OMPI_SUCCESS;
    void *target = (unsigned char*) module->p2p_win->w_baseptr + 
        (header->hdr_target_disp * module->p2p_win->w_disp_unit);    
    ompi_proc_t *proc = module->p2p_comm->c_pml_procs[header->hdr_origin]->proc_ompi;
    struct ompi_datatype_t *datatype = 
        ompi_osc_pt2pt_datatype_create(proc, &inbuf);

    if (header->hdr_msg_length > 0) {
        ompi_convertor_t convertor;
        struct iovec iov;
        uint32_t iov_count = 1;
        int32_t free_after = 0;
        size_t max_data;
        ompi_proc_t *proc;

        /* create convertor */
        OBJ_CONSTRUCT(&convertor, ompi_convertor_t);

        /* initialize convertor */
        proc = ompi_comm_peer_lookup(module->p2p_comm, header->hdr_origin);
        ompi_convertor_copy_and_prepare_for_recv(proc->proc_convertor,
                                                 datatype,
                                                 header->hdr_target_count,
                                                 target,
                                                 &convertor);
        iov.iov_len = header->hdr_msg_length;
        iov.iov_base = inbuf;
        max_data = iov.iov_len;
        ompi_convertor_unpack(&convertor, 
                              &iov,
                              &iov_count,
                              &max_data,
                              &free_after);
        OBJ_DESTRUCT(&convertor);
        OBJ_RELEASE(datatype);
        OPAL_THREAD_ADD32(&(module->p2p_num_pending_in), -1);
    
    } else {
            ompi_osc_pt2pt_longreq_t *longreq;
            ompi_osc_pt2pt_longreq_alloc(&longreq);

            longreq->req_comp_cb = ompi_osc_pt2pt_sendreq_recv_put_long_cb;
            longreq->req_comp_cbdata = NULL;
            longreq->req_datatype = datatype;
            longreq->req_module = module;

            ret = mca_pml.pml_irecv(target,
                                    header->hdr_target_count,
                                    datatype,
                                    header->hdr_origin,
                                    header->hdr_origin_tag,
                                    module->p2p_comm,
                                    &(longreq->req_pml_req));

            /* put the send request in the waiting list */
            OPAL_THREAD_LOCK(&(module->p2p_lock));
            opal_list_append(&(module->p2p_long_msgs), &(longreq->super));
            OPAL_THREAD_UNLOCK(&(module->p2p_lock));
    }

    return ret;
}




/**********************************************************************
 *
 * Receive an accumulate on the target side
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_sendreq_recv_accum_long_cb(ompi_osc_pt2pt_longreq_t *longreq)
{
    ompi_osc_pt2pt_send_header_t *header = 
        (ompi_osc_pt2pt_send_header_t*) longreq->req_comp_cbdata;
    void *payload = (void*) (header + 1);
    int ret;

    /* lock the window for accumulates */
    OPAL_THREAD_LOCK(&longreq->req_module->p2p_acc_lock);

    opal_list_remove_item(&(longreq->req_module->p2p_long_msgs), &(longreq->super));

    /* copy the data from the temporary buffer into the user window */
    ret = ompi_osc_pt2pt_process_op(longreq->req_module, 
                                    header, 
                                    longreq->req_datatype, 
                                    longreq->req_op, 
                                    payload,
                                    header->hdr_msg_length);

    /* unlock the window for accumulates */
    OPAL_THREAD_UNLOCK(&longreq->req_module->p2p_acc_lock);
    
    opal_output(-1, "%d finished receiving long accum message from %d",
                longreq->req_module->p2p_comm->c_my_rank, 
                header->hdr_origin);               

    /* free the temp buffer */
    free(longreq->req_comp_cbdata);

    /* Release datatype & op */
    OBJ_RELEASE(longreq->req_datatype);
    OBJ_RELEASE(longreq->req_op);

    OPAL_THREAD_ADD32(&(longreq->req_module->p2p_num_pending_in), -1);

    ompi_osc_pt2pt_longreq_free(longreq);
}


int
ompi_osc_pt2pt_sendreq_recv_accum(ompi_osc_pt2pt_module_t *module,
                                  ompi_osc_pt2pt_send_header_t *header,
                                  void *payload)
{
    int ret = OMPI_SUCCESS;
    struct ompi_op_t *op = ompi_osc_pt2pt_op_create(header->hdr_target_op);
    ompi_proc_t *proc = module->p2p_comm->c_pml_procs[header->hdr_origin]->proc_ompi;
    struct ompi_datatype_t *datatype = 
        ompi_osc_pt2pt_datatype_create(proc, &payload);

    if (header->hdr_msg_length > 0) {
        /* lock the window for accumulates */
        OPAL_THREAD_LOCK(&module->p2p_acc_lock);

        /* copy the data from the temporary buffer into the user window */
        ret = ompi_osc_pt2pt_process_op(module, header, datatype, op, payload, 
                                        header->hdr_msg_length);

        /* unlock the window for accumulates */
        OPAL_THREAD_UNLOCK(&module->p2p_acc_lock);

        /* Release datatype & op */
        OBJ_RELEASE(datatype);
        OBJ_RELEASE(op);

        OPAL_THREAD_ADD32(&(module->p2p_num_pending_in), -1);

        opal_output(-1, "%d received accum message from %d",
                    module->p2p_comm->c_my_rank,
                    header->hdr_origin);
        
    } else {
        ompi_osc_pt2pt_longreq_t *longreq;
        long lb, extent, true_lb, true_extent;
        size_t buflen;

        /* figure out how big a buffer we need */
        ompi_ddt_get_extent(datatype, &lb, &extent);
        ompi_ddt_get_true_extent(datatype, &true_lb, &true_extent);
        buflen = true_extent + (header->hdr_target_count - 1) * extent;

        /* get a longreq and fill it in */
        ompi_osc_pt2pt_longreq_alloc(&longreq);

        longreq->req_comp_cb = ompi_osc_pt2pt_sendreq_recv_accum_long_cb;
        longreq->req_datatype = datatype;
        longreq->req_op = op;
        longreq->req_module = module;

        /* allocate a buffer to receive into ... */
        longreq->req_comp_cbdata = malloc(buflen + sizeof(ompi_osc_pt2pt_send_header_t));
        
        if (NULL == longreq->req_comp_cbdata) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        /* fill in tmp header */
        memcpy(longreq->req_comp_cbdata, header,
               sizeof(ompi_osc_pt2pt_send_header_t));
        ((ompi_osc_pt2pt_send_header_t*) longreq->req_comp_cbdata)->hdr_msg_length = buflen;

        ret = mca_pml.pml_irecv(((char*) longreq->req_comp_cbdata) + sizeof(ompi_osc_pt2pt_send_header_t),
                                header->hdr_target_count,
                                datatype,
                                header->hdr_origin,
                                header->hdr_origin_tag,
                                module->p2p_comm,
                                &(longreq->req_pml_req));

        opal_output(-1, "%d started long recv accum message from %d (%d)",
                    module->p2p_comm->c_my_rank,
                    header->hdr_origin,
                    header->hdr_origin_tag);

        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&(module->p2p_lock));
        opal_list_append(&(module->p2p_long_msgs), &(longreq->super));
        OPAL_THREAD_UNLOCK(&(module->p2p_lock));
    }

    return ret;
}


/**********************************************************************
 *
 * Recveive a get on the origin side
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_replyreq_recv_long_cb(ompi_osc_pt2pt_longreq_t *longreq)
{
    ompi_osc_pt2pt_sendreq_t *sendreq =
        (ompi_osc_pt2pt_sendreq_t*) longreq->req_comp_cbdata;

    opal_list_remove_item(&(longreq->req_module->p2p_long_msgs), &(longreq->super));

    ompi_osc_pt2pt_longreq_free(longreq);

    OPAL_THREAD_ADD32(&(sendreq->req_module->p2p_num_pending_out), -1);
    ompi_osc_pt2pt_sendreq_free(sendreq);
}

int
ompi_osc_pt2pt_replyreq_recv(ompi_osc_pt2pt_module_t *module,
                             ompi_osc_pt2pt_sendreq_t *sendreq,
                             ompi_osc_pt2pt_reply_header_t *header,
                             void *payload)
{
    int ret = OMPI_SUCCESS;

    /* receive into user buffer */
    if (header->hdr_msg_length > 0) {
        /* short message.  woo! */

        struct iovec iov;
        uint32_t iov_count = 1;
        int32_t free_after = 0;
        size_t max_data;

        iov.iov_len = header->hdr_msg_length;
        iov.iov_base = payload;
        max_data = iov.iov_len;
        ompi_convertor_unpack(&sendreq->req_origin_convertor,
                              &iov,
                              &iov_count,
                              &max_data,
                              &free_after);

        OPAL_THREAD_ADD32(&(sendreq->req_module->p2p_num_pending_out), -1);
        ompi_osc_pt2pt_sendreq_free(sendreq);
    } else {
        ompi_osc_pt2pt_longreq_t *longreq;
        ompi_osc_pt2pt_longreq_alloc(&longreq);

        longreq->req_comp_cb = ompi_osc_pt2pt_replyreq_recv_long_cb;
        longreq->req_comp_cbdata = sendreq;
        longreq->req_module = module;

        /* BWB - FIX ME -  George is going to kill me for this */
        ret = mca_pml.pml_irecv(sendreq->req_origin_convertor.pBaseBuf,
                                sendreq->req_origin_convertor.count,
                                sendreq->req_origin_datatype,
                                sendreq->req_target_rank,
                                header->hdr_target_tag,
                                module->p2p_comm,
                                &(longreq->req_pml_req));
        
        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&(module->p2p_lock));
        opal_list_append(&(module->p2p_long_msgs), &(longreq->super));
        OPAL_THREAD_UNLOCK(&(module->p2p_lock));
    }

    return ret;
}


/**********************************************************************
 *
 * Control message communication
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_control_send_cb(struct mca_btl_base_module_t* btl, 
                               struct mca_btl_base_endpoint_t *endpoint,
                               struct mca_btl_base_descriptor_t* descriptor,
                               int status)
{
    /* release the descriptor and sendreq */
    btl->btl_free(btl, descriptor);
}


int
ompi_osc_pt2pt_control_send(ompi_osc_pt2pt_module_t *module,
                            ompi_proc_t *proc,
                            uint8_t type, int32_t value0, int32_t value1)
{
    int ret = OMPI_SUCCESS;
    mca_bml_base_endpoint_t *endpoint = NULL;
    mca_bml_base_btl_t *bml_btl = NULL;
    mca_btl_base_descriptor_t *descriptor = NULL;
    ompi_osc_pt2pt_control_header_t *header = NULL;
        
    /* Get a BTL and a fragment to go with it */
    endpoint = (mca_bml_base_endpoint_t*) proc->proc_pml;
    bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
    descriptor = bml_btl->btl_alloc(bml_btl->btl,
                                    sizeof(ompi_osc_pt2pt_control_header_t));
    if (NULL == descriptor) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* verify at least enough space for header */
    if (descriptor->des_src[0].seg_len < sizeof(ompi_osc_pt2pt_control_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup descriptor */
    descriptor->des_cbfunc = ompi_osc_pt2pt_control_send_cb;
    descriptor->des_cbdata = NULL;
    descriptor->des_flags = MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_src[0].seg_len = sizeof(ompi_osc_pt2pt_control_header_t);

    /* pack header */
    header = (ompi_osc_pt2pt_control_header_t*) descriptor->des_src[0].seg_addr.pval;
    header->hdr_base.hdr_type = type;
    header->hdr_value[0] = value0;
    header->hdr_value[1] = value1;
    header->hdr_windx = module->p2p_comm->c_contextid;

#if 0 /* BWB - FIX ME */
    /* put in network byte order */
    OMPI_OSC_PT2PT_CONTROL_HDR_HTON(header);
#endif

    /* send fragment */
    ret = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_OSC_PT2PT);
    goto done;

 cleanup:
    if (descriptor != NULL) {
        mca_bml_base_free(bml_btl, descriptor);
    }

 done:
    return ret;
}

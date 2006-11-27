/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
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
#include "osc_pt2pt_buffer.h"

#include "opal/util/output.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/dt_arch.h"
#include "ompi/mca/osc/base/base.h"


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

    opal_output_verbose(50, ompi_osc_base_output,
                        "%d completed long sendreq to %d",
                        sendreq->req_module->p2p_comm->c_my_rank,
                        sendreq->req_target_rank);

    opal_list_remove_item(&(sendreq->req_module->p2p_long_msgs), 
                          &(longreq->super.super));

    ompi_osc_pt2pt_longreq_free(longreq);

    OPAL_THREAD_ADD32(&(sendreq->req_module->p2p_num_pending_out), -1);
    ompi_osc_pt2pt_sendreq_free(sendreq);
}


static void
ompi_osc_pt2pt_sendreq_send_cb(ompi_osc_pt2pt_buffer_t *buffer)
{
    ompi_osc_pt2pt_sendreq_t *sendreq = 
        (ompi_osc_pt2pt_sendreq_t*) buffer->cbdata;
    ompi_osc_pt2pt_send_header_t *header =
        (ompi_osc_pt2pt_send_header_t*) buffer->payload;

    /* have to look at header, and not the sendreq because in the case
       of get, it's possible that the sendreq has been freed already
       (if the remote side replies before we get our send completion
       callback) and already allocated to another request.  We don't
       wait for this completion before exiting a synchronization point
       in the case of get, as we really don't care when it completes -
       only when the data arrives. */
    if (OMPI_OSC_PT2PT_HDR_GET != header->hdr_base.hdr_type) {
#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
        if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
            OMPI_OSC_PT2PT_SEND_HDR_NTOH(*header);
        }
#endif
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
            opal_output_verbose(50, ompi_osc_base_output,
                                "%d starting long sendreq to %d (%d)",
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
            opal_list_append(&(sendreq->req_module->p2p_long_msgs), 
                             &(longreq->super.super));
            OPAL_THREAD_UNLOCK(&(sendreq->req_module->p2p_lock));
        }
    }
    
    /* release the buffer */
    OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                          &buffer->super);
}


/* create the initial fragment, pack header, datatype, and payload (if
   size fits) and send */
int
ompi_osc_pt2pt_sendreq_send(ompi_osc_pt2pt_module_t *module,
                            ompi_osc_pt2pt_sendreq_t *sendreq)
{
    int ret = OMPI_SUCCESS;
    opal_free_list_item_t *item;
    ompi_osc_pt2pt_send_header_t *header = NULL;
    ompi_osc_pt2pt_buffer_t *buffer = NULL;
    size_t written_data = 0;
    size_t needed_len = sizeof(ompi_osc_pt2pt_send_header_t);
    const void *packed_ddt;
    size_t packed_ddt_len = ompi_ddt_pack_description_length(sendreq->req_target_datatype);

    /* we always need to send the ddt */
    needed_len += packed_ddt_len;
    if (OMPI_OSC_PT2PT_GET != sendreq->req_type) {
        needed_len += sendreq->req_origin_bytes_packed;
    }

    /* Get a buffer */
    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_buffers,
                       item, ret);
    if (NULL == item) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    buffer = (ompi_osc_pt2pt_buffer_t*) item;

    /* verify at least enough space for header */
    if (mca_osc_pt2pt_component.p2p_c_eager_size < sizeof(ompi_osc_pt2pt_send_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup buffer */
    buffer->cbfunc = ompi_osc_pt2pt_sendreq_send_cb;
    buffer->cbdata = (void*) sendreq;

    /* pack header */
    header = (ompi_osc_pt2pt_send_header_t*) buffer->payload;
    written_data += sizeof(ompi_osc_pt2pt_send_header_t);
    header->hdr_base.hdr_flags = 0;
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
    memcpy((unsigned char*) buffer->payload + written_data,
           packed_ddt, packed_ddt_len);
    written_data += packed_ddt_len;

    if (OMPI_OSC_PT2PT_GET != sendreq->req_type) {
        /* if sending data and it fits, pack payload */
        if (mca_osc_pt2pt_component.p2p_c_eager_size >=
            written_data + sendreq->req_origin_bytes_packed) {
            struct iovec iov;
            uint32_t iov_count = 1;
            size_t max_data = sendreq->req_origin_bytes_packed;

            iov.iov_len = max_data;
            iov.iov_base = (IOVBASE_TYPE*)((unsigned char*) buffer->payload + written_data);

            ret = ompi_convertor_pack(&sendreq->req_origin_convertor, &iov, &iov_count,
                                      &max_data );
            if (ret < 0) {
                ret = OMPI_ERR_FATAL;
                goto cleanup;
            }

            assert(max_data == sendreq->req_origin_bytes_packed);
            written_data += max_data;

            header->hdr_msg_length = sendreq->req_origin_bytes_packed;
        } else {
            header->hdr_msg_length = 0;
            header->hdr_origin_tag = create_send_tag(module);
        }
    } else {
        header->hdr_msg_length = 0;
    }

    buffer->len = written_data;

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
#elif OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    if (sendreq->req_target_proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
        OMPI_OSC_PT2PT_SEND_HDR_HTON(*header);
    }
#endif

    /* send fragment */
    opal_output_verbose(51, ompi_osc_base_output,
                        "%d sending sendreq to %d",
                        sendreq->req_module->p2p_comm->c_my_rank,
                        sendreq->req_target_rank);

    ret = MCA_PML_CALL(isend(buffer->payload,
                             buffer->len,
                             MPI_BYTE,
                             sendreq->req_target_rank,
                             -200,
                             MCA_PML_BASE_SEND_STANDARD,
                             module->p2p_comm,
                             &buffer->request));
    opal_list_append(&module->p2p_pending_control_sends, 
                     &buffer->super.super);
    goto done;

 cleanup:
    if (item != NULL) {
        OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                              item);
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

    opal_list_remove_item(&(replyreq->rep_module->p2p_long_msgs), 
                          &(longreq->super.super));

    ompi_osc_pt2pt_longreq_free(longreq);

    OPAL_THREAD_ADD32(&(replyreq->rep_module->p2p_num_pending_in), -1);
    ompi_osc_pt2pt_replyreq_free(replyreq);
}


static void
ompi_osc_pt2pt_replyreq_send_cb(ompi_osc_pt2pt_buffer_t *buffer)
{
    ompi_osc_pt2pt_replyreq_t *replyreq = 
        (ompi_osc_pt2pt_replyreq_t*) buffer->cbdata;
    ompi_osc_pt2pt_reply_header_t *header =
        (ompi_osc_pt2pt_reply_header_t*) buffer->payload;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
        if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
            OMPI_OSC_PT2PT_REPLY_HDR_NTOH(*header);
        }
#endif

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
            opal_list_append(&(replyreq->rep_module->p2p_long_msgs),
                             &(longreq->super.super));
            OPAL_THREAD_UNLOCK(&(replyreq->rep_module->p2p_lock));
    }
    
    /* release the descriptor and replyreq */
    OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                          &buffer->super);
}


int
ompi_osc_pt2pt_replyreq_send(ompi_osc_pt2pt_module_t *module,
                             ompi_osc_pt2pt_replyreq_t *replyreq)
{
    int ret = OMPI_SUCCESS;
    opal_free_list_item_t *item;
    ompi_osc_pt2pt_buffer_t *buffer = NULL;
    ompi_osc_pt2pt_reply_header_t *header = NULL;
    size_t written_data = 0;

    /* Get a buffer */
    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_buffers,
                       item, ret);
    if (NULL == item) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    buffer = (ompi_osc_pt2pt_buffer_t*) item;

    /* verify at least enough space for header */
    if (mca_osc_pt2pt_component.p2p_c_eager_size < sizeof(ompi_osc_pt2pt_reply_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup buffer */
    buffer->cbfunc = ompi_osc_pt2pt_replyreq_send_cb;
    buffer->cbdata = (void*) replyreq;

    /* pack header */
    header = (ompi_osc_pt2pt_reply_header_t*) buffer->payload;
    written_data += sizeof(ompi_osc_pt2pt_reply_header_t);
    header->hdr_base.hdr_type = OMPI_OSC_PT2PT_HDR_REPLY;
    header->hdr_base.hdr_flags = 0;
    header->hdr_origin_sendreq = replyreq->rep_origin_sendreq;
    header->hdr_target_tag = 0;

    /* if sending data fits, pack payload */
    if (mca_osc_pt2pt_component.p2p_c_eager_size >=
        written_data + replyreq->rep_target_bytes_packed) {
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data = replyreq->rep_target_bytes_packed;

        iov.iov_len = max_data;
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*) buffer->payload + written_data);

        ret = ompi_convertor_pack(&replyreq->rep_target_convertor, &iov, &iov_count,
                                  &max_data );
        if (ret < 0) {
            ret = OMPI_ERR_FATAL;
            goto cleanup;
        }

        assert(max_data == replyreq->rep_target_bytes_packed);
        written_data += max_data;

        header->hdr_msg_length = replyreq->rep_target_bytes_packed;
    } else {
        header->hdr_msg_length = 0;
        header->hdr_target_tag = create_send_tag(module);
    }

    buffer->len = written_data;

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
#elif OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    if (replyreq->rep_origin_proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
        OMPI_OSC_PT2PT_REPLY_HDR_HTON(*header);
    }
#endif

    /* send fragment */
    ret = MCA_PML_CALL(isend(buffer->payload,
                             buffer->len,
                             MPI_BYTE,
                             replyreq->rep_origin_rank,
                             -200,
                             MCA_PML_BASE_SEND_STANDARD,
                             module->p2p_comm,
                             &buffer->request));
    opal_list_append(&module->p2p_pending_control_sends, 
                     &buffer->super.super);

    goto done;

 cleanup:
    if (item != NULL) {
        OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                              item);
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
    opal_list_remove_item(&(longreq->req_module->p2p_long_msgs), 
                          &(longreq->super.super));

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
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->p2p_comm, header->hdr_origin );
    struct ompi_datatype_t *datatype = 
        ompi_osc_pt2pt_datatype_create(proc, &inbuf);

    if (header->hdr_msg_length > 0) {
        ompi_convertor_t convertor;
        struct iovec iov;
        uint32_t iov_count = 1;
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
                                                 0,
                                                 &convertor);
        iov.iov_len = header->hdr_msg_length;
        iov.iov_base = (IOVBASE_TYPE*)inbuf;
        max_data = iov.iov_len;
        ompi_convertor_unpack(&convertor, 
                              &iov,
                              &iov_count,
                              &max_data );
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
            opal_list_append(&(module->p2p_long_msgs), 
                             &(longreq->super.super));
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

    opal_list_remove_item(&(longreq->req_module->p2p_long_msgs), 
                          &(longreq->super.super));

    /* copy the data from the temporary buffer into the user window */
    ret = ompi_osc_pt2pt_process_op(longreq->req_module, 
                                    header, 
                                    longreq->req_datatype, 
                                    longreq->req_op, 
                                    payload,
                                    header->hdr_msg_length);

    /* unlock the window for accumulates */
    OPAL_THREAD_UNLOCK(&longreq->req_module->p2p_acc_lock);
    
    opal_output_verbose(50, ompi_osc_base_output,
                        "%d finished receiving long accum message from %d",
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
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->p2p_comm, header->hdr_origin );
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

        opal_output_verbose(50, ompi_osc_base_output,
                            "%d received accum message from %d",
                            module->p2p_comm->c_my_rank,
                            header->hdr_origin);
        
    } else {
        ompi_osc_pt2pt_longreq_t *longreq;
        ptrdiff_t lb, extent, true_lb, true_extent;
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

        opal_output_verbose(50, ompi_osc_base_output,
                            "%d started long recv accum message from %d (%d)",
                            module->p2p_comm->c_my_rank,
                            header->hdr_origin,
                            header->hdr_origin_tag);

        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&(module->p2p_lock));
        opal_list_append(&(module->p2p_long_msgs), 
                         &(longreq->super.super));
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

    opal_list_remove_item(&(longreq->req_module->p2p_long_msgs),
                          &(longreq->super.super));

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
        size_t max_data;

        iov.iov_len = header->hdr_msg_length;
        iov.iov_base = (IOVBASE_TYPE*)payload;
        max_data = iov.iov_len;
        ompi_convertor_unpack(&sendreq->req_origin_convertor,
                              &iov,
                              &iov_count,
                              &max_data );

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
        opal_list_append(&(module->p2p_long_msgs),
                         &(longreq->super.super));
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
ompi_osc_pt2pt_control_send_cb(ompi_osc_pt2pt_buffer_t *buffer)
{
    /* release the descriptor and sendreq */
    OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                          &buffer->super);
}


int
ompi_osc_pt2pt_control_send(ompi_osc_pt2pt_module_t *module,
                            ompi_proc_t *proc,
                            uint8_t type, int32_t value0, int32_t value1)
{
    int ret = OMPI_SUCCESS;
    opal_free_list_item_t *item;
    ompi_osc_pt2pt_buffer_t *buffer = NULL;
    ompi_osc_pt2pt_control_header_t *header = NULL;
    int rank = -1, i;

    /* find the rank */
    for (i = 0 ; i < module->p2p_comm->c_remote_group->grp_proc_count ; ++i) {
        if (proc == module->p2p_comm->c_remote_group->grp_proc_pointers[i]) {
            rank = i;
        }
    }

    /* Get a buffer */
    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_buffers,
                       item, ret);
    if (NULL == item) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    buffer = (ompi_osc_pt2pt_buffer_t*) item;

    /* verify at least enough space for header */
    if (mca_osc_pt2pt_component.p2p_c_eager_size < sizeof(ompi_osc_pt2pt_control_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup buffer */
    buffer->cbfunc = ompi_osc_pt2pt_control_send_cb;
    buffer->cbdata = NULL;
    buffer->len = sizeof(ompi_osc_pt2pt_control_header_t);

    /* pack header */
    header = (ompi_osc_pt2pt_control_header_t*) buffer->payload;
    header->hdr_base.hdr_type = type;
    header->hdr_base.hdr_flags = 0;
    header->hdr_value[0] = value0;
    header->hdr_value[1] = value1;
    header->hdr_windx = module->p2p_comm->c_contextid;

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
#elif OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    if (proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
        OMPI_OSC_PT2PT_CONTROL_HDR_HTON(*header);
    }
#endif

    /* send fragment */
    ret = MCA_PML_CALL(isend(buffer->payload,
                             buffer->len,
                             MPI_BYTE,
                             rank,
                             -200,
                             MCA_PML_BASE_SEND_STANDARD,
                             module->p2p_comm,
                             &buffer->request));
    opal_list_append(&module->p2p_pending_control_sends, 
                     &buffer->super.super);
    goto done;

 cleanup:
    if (item != NULL) {
        OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                              item);
    }

 done:
    return ret;
}

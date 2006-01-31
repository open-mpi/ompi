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
#include "osc_pt2pt_replyreq.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_obj_convert.h"
#include "osc_pt2pt_data_move.h"

#include "opal/threads/mutex.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"

ompi_osc_pt2pt_component_t mca_osc_pt2pt_component = {
    { /* ompi_osc_base_component_t */
        { /* ompi_base_component_t */
            OMPI_OSC_BASE_VERSION_1_0_0,
            "pt2pt",
            1,
            0,
            0,
            NULL,
            NULL
        },
        { /* mca_base_component_data */
            false /* checkpointable? */
        },
        ompi_osc_pt2pt_component_init,
        ompi_osc_pt2pt_component_query,
        ompi_osc_pt2pt_component_select,
        ompi_osc_pt2pt_component_finalize
    }
};


ompi_osc_pt2pt_module_t ompi_osc_pt2pt_module_template = {
    {
        ompi_osc_pt2pt_module_free,

        ompi_osc_pt2pt_module_put,
        ompi_osc_pt2pt_module_get,
        ompi_osc_pt2pt_module_accumulate,

        ompi_osc_pt2pt_module_fence,

        ompi_osc_pt2pt_module_start,
        ompi_osc_pt2pt_module_complete,
        ompi_osc_pt2pt_module_post,
        ompi_osc_pt2pt_module_wait,
        ompi_osc_pt2pt_module_test,

        ompi_osc_pt2pt_module_lock,
        ompi_osc_pt2pt_module_unlock,
    }
};


void ompi_osc_pt2pt_component_fragment_cb(struct mca_btl_base_module_t *btl,
                                          mca_btl_base_tag_t tag,
                                          mca_btl_base_descriptor_t *descriptor,
                                          void *cbdata);

static bool
want_locks(ompi_info_t *info)
{
    char *val;
    int vallen, ret, flag;
    bool no_locks;

    ret = ompi_info_get_valuelen(info, "no_locks", &vallen, &flag);
    if (OMPI_SUCCESS != ret)  return true;
    if (flag == 0) return true;
    vallen++;

    val = malloc(sizeof(char) * vallen);
    if (NULL == val) return true;

    ret = ompi_info_get(info, "no_locks", vallen, val, &flag);
    if (OMPI_SUCCESS != ret) {
        free(val);
        return true;
    }
    assert(flag != 0);
    ret = ompi_info_value_to_bool(val, &no_locks);
    free(val);
    if (OMPI_SUCCESS != ret) return true;

    return !no_locks;
}


int
ompi_osc_pt2pt_component_init(bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    int ret;

    /* we can run with either threads or not threads (may not be able
       to do win locks)... */
    mca_osc_pt2pt_component.p2p_c_have_progress_threads = 
        enable_progress_threads;

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_modules,
                  opal_hash_table_t);
    opal_hash_table_init(&mca_osc_pt2pt_component.p2p_c_modules, 2);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_sendreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_pt2pt_component.p2p_c_sendreqs,
                        sizeof(ompi_osc_pt2pt_sendreq_t),
                        OBJ_CLASS(ompi_osc_pt2pt_sendreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_replyreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_pt2pt_component.p2p_c_replyreqs,
                        sizeof(ompi_osc_pt2pt_replyreq_t),
                        OBJ_CLASS(ompi_osc_pt2pt_replyreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_longreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_pt2pt_component.p2p_c_longreqs,
                        sizeof(ompi_osc_pt2pt_longreq_t),
                        OBJ_CLASS(ompi_osc_pt2pt_longreq_t),
                        1, -1, 1);

    ret = mca_bml.bml_register(MCA_BTL_TAG_OSC_PT2PT,
                               ompi_osc_pt2pt_component_fragment_cb,
                               NULL);

    return ret;
}


int 
ompi_osc_pt2pt_component_finalize(void)
{
    size_t num_modules;

    if (0 != 
        (num_modules = opal_hash_table_get_size(&mca_osc_pt2pt_component.p2p_c_modules))) {
        opal_output(0, "WARNING: There were %d Windows created but not freed.",
                    num_modules);
    }

    mca_bml.bml_register(MCA_BTL_TAG_OSC_PT2PT, NULL, NULL);

    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_longreqs);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_replyreqs);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_sendreqs);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_modules);
    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_lock);

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_component_query(ompi_win_t *win,
                              ompi_info_t *info,
                              ompi_communicator_t *comm)
{
    if (!mca_osc_pt2pt_component.p2p_c_have_progress_threads) {
        /* if we don't have threads, we can only run if the user
           promises not to use locking by setting the no_locks key */
#if 0
        if (want_locks(info)) { 
            /* once the default build of Open MPI is to use progress
               threads, should enable this check again.  For now,
               though, we just abort when we get to the call to
               MPI_Lock() */
            return -1
        }
#endif
    }

    /* woo!  we can run! Return priority of 10 (low) */
    return 10;
}


int 
ompi_osc_pt2pt_component_select(ompi_win_t *win,
                               ompi_info_t *info,
                               ompi_communicator_t *comm)
{
    ompi_osc_pt2pt_module_t *module;
    int ret, i;

    /* create module structure */
    module = malloc(sizeof(ompi_osc_pt2pt_module_t));
    if (NULL == module) return OMPI_ERROR;

    /* fill in the function pointer part */
    memcpy(module, &ompi_osc_pt2pt_module_template, 
           sizeof(ompi_osc_base_module_t));

    /* initialize the p2p part */
    OBJ_CONSTRUCT(&(module->p2p_lock), opal_mutex_t);
    OBJ_CONSTRUCT(&(module->p2p_acc_lock), opal_mutex_t);

    module->p2p_win = win;

    ret = ompi_comm_dup(comm, &(module->p2p_comm));
    if (ret != OMPI_SUCCESS) {
        OBJ_DESTRUCT(&(module->p2p_acc_lock));
        OBJ_DESTRUCT(&(module->p2p_lock));
        free(module);
        return ret;
    }

    module->p2p_want_locks = want_locks(info);
    if (!mca_osc_pt2pt_component.p2p_c_have_progress_threads) {
        module->p2p_want_locks = false;
    }

    module->p2p_pending_out_sendreqs = malloc(sizeof(opal_list_t) *
                                              ompi_comm_size(module->p2p_comm));
    if (NULL == module) {
        ompi_comm_free(&comm);
        OBJ_DESTRUCT(&(module->p2p_acc_lock));
        OBJ_DESTRUCT(&(module->p2p_lock));
        free(module);
        return ret;
    }
    for (i = 0 ; i < ompi_comm_size(module->p2p_comm) ; ++i) {
        OBJ_CONSTRUCT(&(module->p2p_pending_out_sendreqs[i]), opal_list_t);
    }

    module->p2p_num_pending_out = 0;
    module->p2p_num_pending_in = 0;
    module->p2p_tag_counter = 0;

    OBJ_CONSTRUCT(&(module->p2p_long_msgs), opal_list_t);
    module->p2p_num_long_msgs = 0;

    /* update component data */
    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    opal_hash_table_set_value_uint32(&mca_osc_pt2pt_component.p2p_c_modules,
                                     module->p2p_comm->c_contextid,
                                     module);
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
                                     
    /* fill in window information */
    win->w_osc_module = (ompi_osc_base_module_t*) module;

    /* register to receive fragment callbacks */
    ret = mca_bml.bml_register(MCA_BTL_TAG_OSC_PT2PT,
                               ompi_osc_pt2pt_component_fragment_cb,
                               NULL);

    return ret;
}



/* dispatch for callback on message completion */
void
ompi_osc_pt2pt_component_fragment_cb(struct mca_btl_base_module_t *btl,
                                mca_btl_base_tag_t tag,
                                mca_btl_base_descriptor_t *descriptor,
                                void *cbdata)
{
    int ret;
    ompi_osc_pt2pt_module_t *module;
    void *payload;

    assert(descriptor->des_dst[0].seg_len >= sizeof(ompi_osc_pt2pt_type_header_t));

    /* handle message */
    switch (((ompi_osc_pt2pt_type_header_t*) descriptor->des_dst[0].seg_addr.pval)->hdr_type) {
    case OMPI_OSC_PT2PT_HDR_PUT:
        {
            ompi_osc_pt2pt_send_header_t *header;

            /* get our header and payload */
            header = (ompi_osc_pt2pt_send_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            payload = (void*) (header + 1);

            /* get our module pointer */
            module = ompi_osc_pt2pt_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            ret = ompi_osc_pt2pt_sendreq_recv_put(module, header, payload);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_ACC: 
        {
            ompi_osc_pt2pt_send_header_t *header;

            /* get our header and payload */
            header = (ompi_osc_pt2pt_send_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            payload = (void*) (header + 1);

            /* get our module pointer */
            module = ompi_osc_pt2pt_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            /* receive into temporary buffer */
            ret = ompi_osc_pt2pt_sendreq_recv_accum(module, header, payload);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_GET:
        {
            ompi_datatype_t *datatype;
            ompi_osc_pt2pt_send_header_t *header;
            ompi_osc_pt2pt_replyreq_t *replyreq;

            /* get our header and payload */
            header = (ompi_osc_pt2pt_send_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            payload = (void*) (header + 1);

            /* get our module pointer */
            module = ompi_osc_pt2pt_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            /* create or get a pointer to our datatype */
            datatype = ompi_osc_pt2pt_datatype_create(header->hdr_target_dt_id, &payload);

            /* create replyreq sendreq */
            ret = ompi_osc_pt2pt_replyreq_alloc_init(module,
                                                  header->hdr_origin,
                                                  header->hdr_origin_sendreq,
                                                  header->hdr_target_disp,
                                                  header->hdr_target_count,
                                                  datatype,
                                                  &replyreq);

            /* send replyreq */
            ompi_osc_pt2pt_replyreq_send(module, replyreq);

            /* sendreq does the right retain, so we can release safely */
            OBJ_RELEASE(datatype);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_REPLY:
        {
            ompi_osc_pt2pt_reply_header_t *header;
            ompi_osc_pt2pt_sendreq_t *sendreq;

            /* get our header and payload */
            header = (ompi_osc_pt2pt_reply_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            payload = (void*) (header + 1);

            /* get original sendreq pointer */
            sendreq = (ompi_osc_pt2pt_sendreq_t*) header->hdr_origin_sendreq.pval;
            module = sendreq->req_module;

            /* receive data */
            ompi_osc_pt2pt_replyreq_recv(module, sendreq, header, payload);
        }
        break;
    case OMPI_OSC_PT2PT_HDR_POST:
        {
            ompi_osc_pt2pt_control_header_t *header = 
                (ompi_osc_pt2pt_control_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;

            /* get our module pointer */
            module = ompi_osc_pt2pt_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            OPAL_THREAD_ADD32(&(module->p2p_num_pending_in), -1);
        }
        break;
    case OMPI_OSC_PT2PT_HDR_COMPLETE:
        {
            ompi_osc_pt2pt_control_header_t *header = 
                (ompi_osc_pt2pt_control_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;

            /* get our module pointer */
            module = ompi_osc_pt2pt_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            /* we've heard from one more place, and have value reqs to
               process */
            OPAL_THREAD_ADD32(&(module->p2p_num_pending_out), -1);
            OPAL_THREAD_ADD32(&(module->p2p_num_pending_in), header->hdr_value);
        }
        break;

    default:
        /* BWB - FIX ME - this sucks */
        opal_output(0, "received packet for Window with unknown type");
        abort();
   }
}

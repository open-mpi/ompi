/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "osc_rdma.h"
#include "osc_rdma_sendreq.h"
#include "osc_rdma_replyreq.h"
#include "osc_rdma_header.h"
#include "osc_rdma_obj_convert.h"
#include "osc_rdma_data_move.h"

#include "opal/threads/condition.h"
#include "opal/threads/mutex.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/datatype/dt_arch.h"

static int component_open(void);
static void component_fragment_cb(struct mca_btl_base_module_t *btl,
                                  mca_btl_base_tag_t tag,
                                  mca_btl_base_descriptor_t *descriptor,
                                  void *cbdata);
#if OMPI_ENABLE_PROGRESS_THREADS
static void* component_thread_fn(opal_object_t *obj);
#endif

ompi_osc_rdma_component_t mca_osc_rdma_component = {
    { /* ompi_osc_base_component_t */
        { /* ompi_base_component_t */
            OMPI_OSC_BASE_VERSION_1_0_0,
            "rdma",
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            component_open,
            NULL
        },
        { /* mca_base_component_data */
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        ompi_osc_rdma_component_init,
        ompi_osc_rdma_component_query,
        ompi_osc_rdma_component_select,
        ompi_osc_rdma_component_finalize
    }
};


ompi_osc_rdma_module_t ompi_osc_rdma_module_template = {
    {
        ompi_osc_rdma_module_free,

        ompi_osc_rdma_module_put,
        ompi_osc_rdma_module_get,
        ompi_osc_rdma_module_accumulate,

        ompi_osc_rdma_module_fence,

        ompi_osc_rdma_module_start,
        ompi_osc_rdma_module_complete,
        ompi_osc_rdma_module_post,
        ompi_osc_rdma_module_wait,
        ompi_osc_rdma_module_test,

        ompi_osc_rdma_module_lock,
        ompi_osc_rdma_module_unlock,
    }
};


/* look up parameters for configuring this window.  The code first
   looks in the info structure passed by the user, then through mca
   parameters. */
static bool
check_config_value_bool(char *key, ompi_info_t *info)
{
    char *value_string;
    int value_len, ret, flag, param;
    bool result;

    ret = ompi_info_get_valuelen(info, key, &value_len, &flag);
    if (OMPI_SUCCESS != ret) goto info_not_found;
    if (flag == 0) goto info_not_found;
    value_len++;

    value_string = (char*)malloc(sizeof(char) * value_len);
    if (NULL == value_string) goto info_not_found;

    ret = ompi_info_get(info, key, value_len, value_string, &flag);
    if (OMPI_SUCCESS != ret) {
        free(value_string);
        goto info_not_found;
    }
    assert(flag != 0);
    ret = ompi_info_value_to_bool(value_string, &result);
    free(value_string);
    if (OMPI_SUCCESS != ret) goto info_not_found;
    return result;

 info_not_found:
    param = mca_base_param_find("osc", "rdma", key);
    if (param == OPAL_ERROR) return false;

    ret = mca_base_param_lookup_int(param, &flag);
    if (OMPI_SUCCESS != ret) return false;

    return OPAL_INT_TO_BOOL(flag);
}


static int
component_open(void)
{
    mca_base_param_reg_int(&mca_osc_rdma_component.super.osc_version,
                           "eager_send",
                           "Attempt to start data movement during communication call, "
                           "instead of at synchrnoization time.  "
                           "Info key of same name overrides this value, "
                           "if info key given.",
                           false, false, 0, NULL);

    mca_base_param_reg_int(&mca_osc_rdma_component.super.osc_version,
                           "no_locks",
                           "Enable optimizations available only if MPI_LOCK is not used.",
                           false, false, 0, NULL);

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_component_init(bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    if (!mca_bml_base_inited()) return OMPI_ERROR;

    /* we can run with either threads or not threads (may not be able
       to do win locks)... */
    mca_osc_rdma_component.c_have_progress_threads = 
        enable_progress_threads;

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_modules,
                  opal_hash_table_t);
    opal_hash_table_init(&mca_osc_rdma_component.c_modules, 2);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_request_lock, 
                  opal_mutex_t);
    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_request_cond, 
                  opal_condition_t);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_sendreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_rdma_component.c_sendreqs,
                        sizeof(ompi_osc_rdma_sendreq_t),
                        OBJ_CLASS(ompi_osc_rdma_sendreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_replyreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_rdma_component.c_replyreqs,
                        sizeof(ompi_osc_rdma_replyreq_t),
                        OBJ_CLASS(ompi_osc_rdma_replyreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_longreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_rdma_component.c_longreqs,
                        sizeof(ompi_osc_rdma_longreq_t),
                        OBJ_CLASS(ompi_osc_rdma_longreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_pending_requests,
                  opal_list_t);

#if OMPI_ENABLE_PROGRESS_THREADS
    OBJ_CONSTRUCT(&mca_osc_rdma_component.c_thread, opal_thread_t);
    mca_osc_rdma_component.c_thread_run = false;
#endif

    mca_osc_rdma_component.c_btl_registered = false;

    return OMPI_SUCCESS;
}


int 
ompi_osc_rdma_component_finalize(void)
{
    size_t num_modules;

    if (0 != 
        (num_modules = opal_hash_table_get_size(&mca_osc_rdma_component.c_modules))) {
        opal_output(ompi_osc_base_output,
                    "WARNING: There were %d Windows created but not freed.",
                    (int) num_modules);
#if OMPI_ENABLE_PROGRESS_THREADS
        mca_osc_rdma_component.c_thread_run = false;
        opal_condition_broadcast(&ompi_request_cond);
        opal_thread_join(&mca_osc_rdma_component.c_thread, &ret);
#else
        opal_progress_unregister(ompi_osc_rdma_component_progress);
#endif
    }

    mca_bml.bml_register(MCA_BTL_TAG_OSC_RDMA, NULL, NULL);

#if OMPI_ENABLE_PROGRESS_THREADS
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_thread);
#endif
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_pending_requests);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_longreqs);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_replyreqs);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_sendreqs);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_request_cond);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_request_lock);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_modules);
    OBJ_DESTRUCT(&mca_osc_rdma_component.c_lock);

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_component_query(ompi_win_t *win,
                              ompi_info_t *info,
                              ompi_communicator_t *comm)
{
    /* if we inited, then the BMLs are available and we have a path to
       each peer.  Return slightly higher priority than the
       point-to-point code */
    
    /* lower priority below that of the pt2pt component until the btl
       redesign */
    return 0;
}


int 
ompi_osc_rdma_component_select(ompi_win_t *win,
                               ompi_info_t *info,
                               ompi_communicator_t *comm)
{
    ompi_osc_rdma_module_t *module = NULL;
    int ret, i;

    /* create module structure */
    module = (ompi_osc_rdma_module_t*)
        calloc(1, sizeof(ompi_osc_rdma_module_t));
    if (NULL == module) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    /* fill in the function pointer part */
    memcpy(module, &ompi_osc_rdma_module_template, 
           sizeof(ompi_osc_base_module_t));

    /* initialize the module part */
    OBJ_CONSTRUCT(&module->m_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->m_cond, opal_condition_t);
    OBJ_CONSTRUCT(&module->m_acc_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->m_pending_sendreqs, opal_list_t);
    OBJ_CONSTRUCT(&module->m_copy_pending_sendreqs, opal_list_t);
    OBJ_CONSTRUCT(&module->m_queued_sendreqs, opal_list_t);
    OBJ_CONSTRUCT(&module->m_locks_pending, opal_list_t);
    OBJ_CONSTRUCT(&module->m_unlocks_pending, opal_list_t);

    module->m_win = win;

    ret = ompi_comm_dup(comm, &module->m_comm, 0);
    if (ret != OMPI_SUCCESS) goto cleanup;

    opal_output_verbose(1, ompi_osc_base_output,
                        "rdma component creating window with id %d",
                        module->m_comm->c_contextid);

    module->m_num_pending_sendreqs = (unsigned int*)
        malloc(sizeof(unsigned int) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_num_pending_sendreqs) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    memset(module->m_num_pending_sendreqs, 0, 
           sizeof(unsigned int) * ompi_comm_size(module->m_comm));

    module->m_num_pending_out = 0;
    module->m_num_pending_in = 0;
    module->m_num_post_msgs = 0;
    module->m_num_complete_msgs = 0;
    module->m_tag_counter = 0;

    module->m_copy_num_pending_sendreqs = (unsigned int*)
        malloc(sizeof(unsigned int) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_copy_num_pending_sendreqs) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    memset(module->m_num_pending_sendreqs, 0, 
           sizeof(unsigned int) * ompi_comm_size(module->m_comm));

    module->m_eager_send = check_config_value_bool("eager_send", info);

    /* fence data */
    module->m_fence_coll_counts = (int*)
        malloc(sizeof(int) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_fence_coll_counts) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
        module->m_fence_coll_counts[i] = 1;
    }

    /* pwsc data */
    module->m_pw_group = NULL;
    module->m_sc_group = NULL;
    module->m_sc_remote_active_ranks = (bool*)
        malloc(sizeof(bool) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_sc_remote_active_ranks) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    module->m_sc_remote_ranks = (int*)
        malloc(sizeof(int) * ompi_comm_size(module->m_comm));
    if (NULL == module->m_sc_remote_ranks) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* lock data */
    module->m_lock_status = 0;
    module->m_shared_count = 0;
    module->m_lock_received_ack = 0;

    /* update component data */
    OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
    opal_hash_table_set_value_uint32(&mca_osc_rdma_component.c_modules,
                                     module->m_comm->c_contextid,
                                     module);
    ret = opal_hash_table_get_size(&mca_osc_rdma_component.c_modules);
    if (ret == 1) {
#if OMPI_ENABLE_PROGRESS_THREADS
        mca_osc_rdma_component.c_thread_run = true;
        mca_osc_rdma_component.c_thread.t_run = component_thread_fn;
        mca_osc_rdma_component.c_thread.t_arg = NULL;
        ret = opal_thread_start(&mca_osc_rdma_component.c_thread);
#else
        ret = opal_progress_register(ompi_osc_rdma_component_progress);
#endif
    } else {
        ret = OMPI_SUCCESS;
    }
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* fill in window information */
    win->w_osc_module = (ompi_osc_base_module_t*) module;
    if (check_config_value_bool("no_locks", info)) {
        win->w_flags |= OMPI_WIN_NO_LOCKS;
    }

    /* sync memory - make sure all initialization completed */
    opal_atomic_mb();

    /* register to receive fragment callbacks, if not already done */
    OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
    if (!mca_osc_rdma_component.c_btl_registered) {
        mca_osc_rdma_component.c_btl_registered = true;
        ret = mca_bml.bml_register(MCA_BTL_TAG_OSC_RDMA,
                                   component_fragment_cb,
                                   NULL);
    }
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
    if (OMPI_SUCCESS != ret) goto cleanup;

    /* need to make create a collective, or lock requests can come in
       before the window is fully created... */
    module->m_comm->c_coll.coll_barrier(module->m_comm);

    opal_output_verbose(50, ompi_osc_base_output,
                        "done creating window %d", module->m_comm->c_contextid);

    return OMPI_SUCCESS;

 cleanup:
    OBJ_DESTRUCT(&module->m_unlocks_pending);
    OBJ_DESTRUCT(&module->m_locks_pending);
    OBJ_DESTRUCT(&module->m_queued_sendreqs);
    OBJ_DESTRUCT(&module->m_copy_pending_sendreqs);
    OBJ_DESTRUCT(&module->m_pending_sendreqs);
    OBJ_DESTRUCT(&module->m_acc_lock);
    OBJ_DESTRUCT(&module->m_cond);
    OBJ_DESTRUCT(&module->m_lock);

    if (NULL != module->m_sc_remote_ranks) {
        free(module->m_sc_remote_ranks);
    }
    if (NULL != module->m_sc_remote_active_ranks) {
        free(module->m_sc_remote_active_ranks);
    }
    if (NULL != module->m_fence_coll_counts) {
        free(module->m_fence_coll_counts);
    }
    if (NULL != module->m_copy_num_pending_sendreqs) {
        free(module->m_copy_num_pending_sendreqs);
    }
    if (NULL != module->m_num_pending_sendreqs) {
        free(module->m_num_pending_sendreqs);
    }
    if (NULL != module->m_comm) ompi_comm_free(&module->m_comm);

    if (NULL != module) free(module);

    return ret;
}


/* dispatch for callback on message completion */
static void
component_fragment_cb(struct mca_btl_base_module_t *btl,
                                mca_btl_base_tag_t tag,
                                mca_btl_base_descriptor_t *descriptor,
                                void *cbdata)
{
    int ret;
    ompi_osc_rdma_module_t *module;
    void *payload;
    uint8_t hdr_type;

    assert(descriptor->des_dst[0].seg_len >= 
           sizeof(ompi_osc_rdma_base_header_t));

    hdr_type = ((ompi_osc_rdma_base_header_t*) 
                descriptor->des_dst[0].seg_addr.pval)->hdr_type;

    /* handle message */
    switch (hdr_type) {
    case OMPI_OSC_RDMA_HDR_PUT:
        {
            ompi_osc_rdma_send_header_t *header;

            /* get our header and payload */
            header = (ompi_osc_rdma_send_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_SEND_HDR_NTOH(*header);
            }
#endif

            /* get our module pointer */
            module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            if (!ompi_win_exposure_epoch(module->m_win)) {
                if (OMPI_WIN_FENCE & ompi_win_get_mode(module->m_win)) {
                    /* well, we're definitely in an access epoch now */
                    ompi_win_set_mode(module->m_win, 
                                      OMPI_WIN_FENCE | 
                                      OMPI_WIN_ACCESS_EPOCH |
                                      OMPI_WIN_EXPOSE_EPOCH);
                }
            }

            ret = ompi_osc_rdma_sendreq_recv_put(module, header, payload);
        }
        break;

    case OMPI_OSC_RDMA_HDR_ACC: 
        {
            ompi_osc_rdma_send_header_t *header;

            /* get our header and payload */
            header = (ompi_osc_rdma_send_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_SEND_HDR_NTOH(*header);
            }
#endif

            /* get our module pointer */
            module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            if (!ompi_win_exposure_epoch(module->m_win)) {
                if (OMPI_WIN_FENCE & ompi_win_get_mode(module->m_win)) {
                    /* well, we're definitely in an access epoch now */
                    ompi_win_set_mode(module->m_win, 
                                      OMPI_WIN_FENCE | 
                                      OMPI_WIN_ACCESS_EPOCH |
                                      OMPI_WIN_EXPOSE_EPOCH);
                }
            }

            /* receive into temporary buffer */
            ret = ompi_osc_rdma_sendreq_recv_accum(module, header, payload);
        }
        break;

    case OMPI_OSC_RDMA_HDR_GET:
        {
            ompi_datatype_t *datatype;
            ompi_osc_rdma_send_header_t *header;
            ompi_osc_rdma_replyreq_t *replyreq;
            ompi_proc_t *proc;

            /* get our header and payload */
            header = (ompi_osc_rdma_send_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_SEND_HDR_NTOH(*header);
            }
#endif

            /* get our module pointer */
            module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            if (!ompi_win_exposure_epoch(module->m_win)) {
                if (OMPI_WIN_FENCE & ompi_win_get_mode(module->m_win)) {
                    /* well, we're definitely in an access epoch now */
                    ompi_win_set_mode(module->m_win, 
                                      OMPI_WIN_FENCE | 
                                      OMPI_WIN_ACCESS_EPOCH |
                                      OMPI_WIN_EXPOSE_EPOCH);
                }
            }

            /* create or get a pointer to our datatype */
            proc = ompi_comm_peer_lookup( module->m_comm, header->hdr_origin );
            datatype = ompi_osc_rdma_datatype_create(proc, &payload);

            /* create replyreq sendreq */
            ret = ompi_osc_rdma_replyreq_alloc_init(module,
                                                  header->hdr_origin,
                                                  header->hdr_origin_sendreq,
                                                  header->hdr_target_disp,
                                                  header->hdr_target_count,
                                                  datatype,
                                                  &replyreq);

            /* send replyreq */
            ompi_osc_rdma_replyreq_send(module, replyreq);

            /* sendreq does the right retain, so we can release safely */
            OBJ_RELEASE(datatype);
        }
        break;

    case OMPI_OSC_RDMA_HDR_REPLY:
        {
            ompi_osc_rdma_reply_header_t *header;
            ompi_osc_rdma_sendreq_t *sendreq;

            /* get our header and payload */
            header = (ompi_osc_rdma_reply_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_REPLY_HDR_NTOH(*header);
            }
#endif

            /* get original sendreq pointer */
            sendreq = (ompi_osc_rdma_sendreq_t*) header->hdr_origin_sendreq.pval;
            module = sendreq->req_module;

            /* receive data */
            ompi_osc_rdma_replyreq_recv(module, sendreq, header, payload);
        }
        break;
    case OMPI_OSC_RDMA_HDR_POST:
        {
            ompi_osc_rdma_control_header_t *header = 
                (ompi_osc_rdma_control_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            int32_t count;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
            }
#endif

            /* get our module pointer */
            module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            OPAL_THREAD_LOCK(&module->m_lock);
            count = (module->m_num_post_msgs -= 1);
            OPAL_THREAD_UNLOCK(&module->m_lock);
            if (count == 0) opal_condition_broadcast(&module->m_cond);
        }
        break;
    case OMPI_OSC_RDMA_HDR_COMPLETE:
        {
            ompi_osc_rdma_control_header_t *header = 
                (ompi_osc_rdma_control_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            int32_t count;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
            }
#endif

            /* get our module pointer */
            module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            /* we've heard from one more place, and have value reqs to
               process */
            OPAL_THREAD_LOCK(&module->m_lock);
            count = (module->m_num_complete_msgs -= 1);
            count += (module->m_num_pending_in += header->hdr_value[0]);
            OPAL_THREAD_UNLOCK(&module->m_lock);

            if (count == 0) opal_condition_broadcast(&module->m_cond);
        }
        break;

    case OMPI_OSC_RDMA_HDR_LOCK_REQ:
        {
            ompi_osc_rdma_control_header_t *header = 
                (ompi_osc_rdma_control_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            int32_t count;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
            }
#endif

            /* get our module pointer */
            module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            if (header->hdr_value[1] > 0) {
                ompi_osc_rdma_passive_lock(module, header->hdr_value[0], 
                                            header->hdr_value[1]);
            } else {
                OPAL_THREAD_LOCK(&module->m_lock);
                count = (module->m_lock_received_ack += 1);
                OPAL_THREAD_UNLOCK(&module->m_lock);

                if (count != 0) opal_condition_broadcast(&module->m_cond);
            }
        }
        break;

    case OMPI_OSC_RDMA_HDR_UNLOCK_REQ:
        {
            ompi_osc_rdma_control_header_t *header = 
                (ompi_osc_rdma_control_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
            }
#endif

            /* get our module pointer */
            module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            ompi_osc_rdma_passive_unlock(module, header->hdr_value[0],
                                          header->hdr_value[1]);
        }
        break;

    case OMPI_OSC_RDMA_HDR_UNLOCK_REPLY:
        {
            ompi_osc_rdma_control_header_t *header = 
                (ompi_osc_rdma_control_header_t*) 
                descriptor->des_dst[0].seg_addr.pval;
            int32_t count;

            /* get our module pointer */
            module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            OPAL_THREAD_LOCK(&module->m_lock);
            count = (module->m_num_pending_out -= 1);
            OPAL_THREAD_UNLOCK(&module->m_lock);
            if (count == 0) opal_condition_broadcast(&module->m_cond);
        }
        break;

    default:
        /* BWB - FIX ME - this sucks */
        opal_output(ompi_osc_base_output,
                    "received packet for Window with unknown type");
   }
}

int
ompi_osc_rdma_component_progress(void)
{
    opal_list_item_t *item;
    int ret, done = 0;

#if OMPI_ENABLE_PROGRESS_THREADS
    ret = OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
#else
    ret = OPAL_THREAD_TRYLOCK(&mca_osc_rdma_component.c_lock);
#endif
    if (ret != 0) return 0;

    for (item = opal_list_get_first(&mca_osc_rdma_component.c_pending_requests) ;
         item != opal_list_get_end(&mca_osc_rdma_component.c_pending_requests) ;
         item = opal_list_get_next(item)) {
        ompi_osc_rdma_longreq_t *longreq = 
            (ompi_osc_rdma_longreq_t*) item;

        /* BWB - FIX ME */
#if OMPI_ENABLE_PROGRESS_THREADS == 0
        if (longreq->request->req_state == OMPI_REQUEST_INACTIVE ||
            longreq->request->req_complete) {
            ret = ompi_request_test(&longreq->request,
                                    &done,
                                    0);
        } else {
            done = 0;
            ret = OMPI_SUCCESS;
        }
#else
        ret = ompi_request_test(&longreq->request,
                                &done,
                                &longreq->status);
#endif
        if (OMPI_SUCCESS == ret && 0 != done) {
            opal_list_remove_item(&mca_osc_rdma_component.c_pending_requests,
                                  item);
            OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
            longreq->cbfunc(longreq);
            OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
            break;
        }
    }
        
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);

    return done;
}


#if OMPI_ENABLE_PROGRESS_THREADS
static void*
component_thread_fn(opal_object_t *obj)
{
    struct timespec waittime;

    while (mca_osc_rdma_component.c_thread_run) {
        /* wake up whenever a request completes, to make sure it's not
           for us */
        waittime.tv_sec = 1;
        waittime.tv_usec = 0;
        OPAL_THREAD_LOCK(&ompi_request_lock);
        opal_condition_timedwait(&ompi_request_cond, &ompi_request_lock, &waittime);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        ompi_osc_rdma_component_progress();
    }

    return NULL;
}
#endif

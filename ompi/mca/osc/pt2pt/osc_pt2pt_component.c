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

#include <string.h>

#include "osc_pt2pt.h"
#include "osc_pt2pt_sendreq.h"
#include "osc_pt2pt_replyreq.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_obj_convert.h"
#include "osc_pt2pt_data_move.h"
#include "osc_pt2pt_buffer.h"

#include "opal/threads/mutex.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/datatype/dt_arch.h"

static int ompi_osc_pt2pt_component_open(void);
static void ompi_osc_pt2pt_component_fragment_cb(struct ompi_osc_pt2pt_buffer_t *buffer);

ompi_osc_pt2pt_component_t mca_osc_pt2pt_component = {
    { /* ompi_osc_base_component_t */
        { /* ompi_base_component_t */
            OMPI_OSC_BASE_VERSION_1_0_0,
            "pt2pt",
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            ompi_osc_pt2pt_component_open,
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
    param = mca_base_param_find("osc", "pt2pt", key);
    if (param == OPAL_ERROR) return false;

    ret = mca_base_param_lookup_int(param, &flag);
    if (OMPI_SUCCESS != ret) return false;

    return OPAL_INT_TO_BOOL(flag);
}


static int
ompi_osc_pt2pt_component_open(void)
{
    int tmp;

    mca_base_param_reg_int(&mca_osc_pt2pt_component.super.osc_version,
                           "no_locks",
                           "Enable optimizations available only if MPI_LOCK is not used.",
                           false, false, 0, NULL);

    mca_base_param_reg_int(&mca_osc_pt2pt_component.super.osc_version,
                           "eager_limit",
                           "Max size of eagerly sent data",
                           false, false, 16 * 1024, 
                           &tmp);

    mca_osc_pt2pt_component.p2p_c_eager_size = tmp;

    return OMPI_SUCCESS;
}


int
ompi_osc_pt2pt_component_init(bool enable_progress_threads,
                             bool enable_mpi_threads)
{
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

    OBJ_CONSTRUCT(&mca_osc_pt2pt_component.p2p_c_buffers, opal_free_list_t);
    opal_free_list_init(&mca_osc_pt2pt_component.p2p_c_buffers,
                        mca_osc_pt2pt_component.p2p_c_eager_size +
                        sizeof(ompi_osc_pt2pt_buffer_t),
                        OBJ_CLASS(ompi_osc_pt2pt_buffer_t),
                        1, -1, 1);

    return OMPI_SUCCESS;
}


int 
ompi_osc_pt2pt_component_finalize(void)
{
    size_t num_modules;

    if (0 !=
        (num_modules = opal_hash_table_get_size(&mca_osc_pt2pt_component.p2p_c_modules))) {
        opal_output(ompi_osc_base_output,
                    "WARNING: There were %d Windows created but not freed.",
                    num_modules);
        opal_progress_unregister(ompi_osc_pt2pt_progress);
    }

    OBJ_DESTRUCT(&mca_osc_pt2pt_component.p2p_c_buffers);
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
    /* we can always run - return a low priority */
    return 5;
}


int 
ompi_osc_pt2pt_component_select(ompi_win_t *win,
                               ompi_info_t *info,
                               ompi_communicator_t *comm)
{
    ompi_osc_pt2pt_module_t *module;
    int ret, i;
    ompi_osc_pt2pt_buffer_t *buffer;
    opal_free_list_item_t *item;

    /* create module structure */
    module = (ompi_osc_pt2pt_module_t*)malloc(sizeof(ompi_osc_pt2pt_module_t));
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

    module->p2p_cb_request = NULL;

    OBJ_CONSTRUCT(&module->p2p_pending_control_sends, opal_list_t);

    OBJ_CONSTRUCT(&module->p2p_pending_sendreqs, opal_list_t);
    module->p2p_num_pending_sendreqs = (unsigned int*)malloc(sizeof(unsigned int) * 
                                                      ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_num_pending_sendreqs) {
        OBJ_DESTRUCT(&module->p2p_pending_sendreqs);
        ompi_comm_free(&comm);
        OBJ_DESTRUCT(&(module->p2p_acc_lock));
        OBJ_DESTRUCT(&(module->p2p_lock));
        free(module);
        return ret;
    }
    memset(module->p2p_num_pending_sendreqs, 0, 
           sizeof(unsigned int) * ompi_comm_size(module->p2p_comm));

    module->p2p_num_pending_out = 0;
    module->p2p_num_pending_in = 0;
    module->p2p_num_post_msgs = 0;
    module->p2p_num_complete_msgs = 0;
    module->p2p_tag_counter = 0;

    OBJ_CONSTRUCT(&(module->p2p_long_msgs), opal_list_t);

    OBJ_CONSTRUCT(&(module->p2p_copy_pending_sendreqs), opal_list_t);
    module->p2p_copy_num_pending_sendreqs = (unsigned int*)malloc(sizeof(unsigned int) * 
                                                           ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_copy_num_pending_sendreqs) {
        OBJ_DESTRUCT(&module->p2p_copy_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_long_msgs);
        free(module->p2p_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_pending_sendreqs);
        ompi_comm_free(&comm);
        OBJ_DESTRUCT(&(module->p2p_acc_lock));
        OBJ_DESTRUCT(&(module->p2p_lock));
        free(module);
        return ret;
    }
    memset(module->p2p_num_pending_sendreqs, 0, 
           sizeof(unsigned int) * ompi_comm_size(module->p2p_comm));

    /* fence data */
    module->p2p_fence_coll_counts = (int*)malloc(sizeof(int) * 
                                                 ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_fence_coll_counts) {
        free(module->p2p_copy_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_copy_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_long_msgs);
        free(module->p2p_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_pending_sendreqs);
        ompi_comm_free(&comm);
        OBJ_DESTRUCT(&(module->p2p_acc_lock));
        OBJ_DESTRUCT(&(module->p2p_lock));
        free(module);
        return ret;
    }
    for (i = 0 ; i < ompi_comm_size(module->p2p_comm) ; ++i) {
        module->p2p_fence_coll_counts[i] = 1;
    }

    module->p2p_fence_coll_results = (unsigned int*)malloc(sizeof(unsigned short) * 
                                                    ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_fence_coll_results) {
        free(module->p2p_fence_coll_counts);
        free(module->p2p_copy_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_copy_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_long_msgs);
        free(module->p2p_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_pending_sendreqs);
        ompi_comm_free(&comm);
        OBJ_DESTRUCT(&(module->p2p_acc_lock));
        OBJ_DESTRUCT(&(module->p2p_lock));
        free(module);
        return ret;
    }

    /* pwsc data */
    module->p2p_pw_group = NULL;
    module->p2p_sc_group = NULL;
    module->p2p_sc_remote_active_ranks =
        (bool*)malloc(sizeof(bool) * ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_sc_remote_active_ranks) {
        free(module->p2p_fence_coll_results);
        free(module->p2p_fence_coll_counts);
        free(module->p2p_copy_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_copy_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_long_msgs);
        free(module->p2p_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_pending_sendreqs);
        ompi_comm_free(&comm);
        OBJ_DESTRUCT(&(module->p2p_acc_lock));
        OBJ_DESTRUCT(&(module->p2p_lock));
        free(module);
        return OMPI_ERROR;
    }
    module->p2p_sc_remote_ranks =
        (int*)malloc(sizeof(int) * ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_sc_remote_ranks) {
        free(module->p2p_sc_remote_active_ranks);
        free(module->p2p_fence_coll_results);
        free(module->p2p_fence_coll_counts);
        free(module->p2p_copy_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_copy_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_long_msgs);
        free(module->p2p_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_pending_sendreqs);
        ompi_comm_free(&comm);
        OBJ_DESTRUCT(&(module->p2p_acc_lock));
        OBJ_DESTRUCT(&(module->p2p_lock));
        free(module);
        return OMPI_ERROR;
    }

    /* lock data */
    module->p2p_lock_status = 0;
    module->p2p_shared_count = 0;
    OBJ_CONSTRUCT(&(module->p2p_locks_pending), opal_list_t);
    module->p2p_lock_received_ack = 0;

    /* update component data */
    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    opal_hash_table_set_value_uint32(&mca_osc_pt2pt_component.p2p_c_modules,
                                     module->p2p_comm->c_contextid,
                                     module);

    if (1 == opal_hash_table_get_size(&mca_osc_pt2pt_component.p2p_c_modules)) {
        /* start progress thread */
        opal_progress_register(ompi_osc_pt2pt_progress);
    }
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);

    /* fill in window information */
    win->w_osc_module = (ompi_osc_base_module_t*) module;
    if (check_config_value_bool("no_locks", info)) {
        win->w_flags |= OMPI_WIN_NO_LOCKS;
    }

    /* sync memory - make sure all initialization completed */
    opal_atomic_mb();

    /* start up receive for protocol headers */
    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_buffers,
                       item, ret);
    if (NULL == item) {
        free(module->p2p_sc_remote_ranks);
        free(module->p2p_sc_remote_active_ranks);
        free(module->p2p_fence_coll_results);
        free(module->p2p_fence_coll_counts);
        free(module->p2p_copy_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_copy_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_long_msgs);
        free(module->p2p_num_pending_sendreqs);
        OBJ_DESTRUCT(&module->p2p_pending_sendreqs);
        ompi_comm_free(&comm);
        OBJ_DESTRUCT(&(module->p2p_acc_lock));
        OBJ_DESTRUCT(&(module->p2p_lock));
        free(module);
        return OMPI_ERROR;
    }
    buffer = (ompi_osc_pt2pt_buffer_t*) item;
    buffer->cbfunc = ompi_osc_pt2pt_component_fragment_cb;
    buffer->cbdata = (void*) module;
    
    ret = MCA_PML_CALL(irecv(buffer->payload,
                             mca_osc_pt2pt_component.p2p_c_eager_size,
                             MPI_BYTE,
                             MPI_ANY_SOURCE,
                             CONTROL_MSG_TAG,
                             module->p2p_comm,
                             &buffer->request));
    opal_list_append(&module->p2p_pending_control_sends, 
                     &buffer->super.super);

    return ret;
}


/* dispatch for callback on message completion */
static void
ompi_osc_pt2pt_component_fragment_cb(struct ompi_osc_pt2pt_buffer_t *pt2pt_buffer)
{
    int ret;
    void *payload, *buffer;
    size_t buffer_len;
    ompi_osc_pt2pt_module_t *module;
    ompi_osc_pt2pt_buffer_t *new_pt2pt_buffer;
    opal_free_list_item_t *item;

    buffer = pt2pt_buffer->payload;
    buffer_len = pt2pt_buffer->status._count;
    module = pt2pt_buffer->cbdata;

    /* post a new receive message */

    /* start up receive for protocol headers */
    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_buffers,
                       item, ret);
    assert(NULL != item);
    new_pt2pt_buffer = (ompi_osc_pt2pt_buffer_t*) item;
    new_pt2pt_buffer->cbfunc = ompi_osc_pt2pt_component_fragment_cb;
    new_pt2pt_buffer->cbdata = (void*) module;
    
    ret = MCA_PML_CALL(irecv(new_pt2pt_buffer->payload,
                             mca_osc_pt2pt_component.p2p_c_eager_size,
                             MPI_BYTE,
                             MPI_ANY_SOURCE,
                             CONTROL_MSG_TAG,
                             module->p2p_comm,
                             &new_pt2pt_buffer->request));
    assert(OMPI_SUCCESS == ret);
    opal_list_append(&module->p2p_pending_control_sends, 
                     &new_pt2pt_buffer->super.super);

    assert(buffer_len >=
           sizeof(ompi_osc_pt2pt_base_header_t));

    /* handle message */
    switch (((ompi_osc_pt2pt_base_header_t*) buffer)->hdr_type) {
    case OMPI_OSC_PT2PT_HDR_PUT:
        {
            ompi_osc_pt2pt_send_header_t *header;

            /* get our header and payload */
            header = (ompi_osc_pt2pt_send_header_t*) 
                buffer;
            payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_SEND_HDR_NTOH(*header);
            }
#endif

            assert(module == ompi_osc_pt2pt_windx_to_module(header->hdr_windx));

            if (!ompi_win_exposure_epoch(module->p2p_win)) {
                if (OMPI_WIN_FENCE & ompi_win_get_mode(module->p2p_win)) {
                    /* well, we're definitely in an access epoch now */
                    ompi_win_set_mode(module->p2p_win, 
                                      OMPI_WIN_FENCE | 
                                      OMPI_WIN_ACCESS_EPOCH |
                                      OMPI_WIN_EXPOSE_EPOCH);
                }
            }

            ret = ompi_osc_pt2pt_sendreq_recv_put(module, header, payload);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_ACC: 
        {
            ompi_osc_pt2pt_send_header_t *header;

            /* get our header and payload */
            header = (ompi_osc_pt2pt_send_header_t*) 
                buffer;
            payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_SEND_HDR_NTOH(*header);
            }
#endif

            assert(module == ompi_osc_pt2pt_windx_to_module(header->hdr_windx));

            if (!ompi_win_exposure_epoch(module->p2p_win)) {
                if (OMPI_WIN_FENCE & ompi_win_get_mode(module->p2p_win)) {
                    /* well, we're definitely in an access epoch now */
                    ompi_win_set_mode(module->p2p_win, 
                                      OMPI_WIN_FENCE | 
                                      OMPI_WIN_ACCESS_EPOCH |
                                      OMPI_WIN_EXPOSE_EPOCH);
                }
            }

            /* receive into temporary buffer */
            ret = ompi_osc_pt2pt_sendreq_recv_accum(module, header, payload);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_GET:
        {
            ompi_datatype_t *datatype;
            ompi_osc_pt2pt_send_header_t *header;
            ompi_osc_pt2pt_replyreq_t *replyreq;
            ompi_proc_t *proc;

            /* get our header and payload */
            header = (ompi_osc_pt2pt_send_header_t*) 
                buffer;
            payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_SEND_HDR_NTOH(*header);
            }
#endif

            assert(module == ompi_osc_pt2pt_windx_to_module(header->hdr_windx));

            if (!ompi_win_exposure_epoch(module->p2p_win)) {
                if (OMPI_WIN_FENCE & ompi_win_get_mode(module->p2p_win)) {
                    /* well, we're definitely in an access epoch now */
                    ompi_win_set_mode(module->p2p_win, 
                                      OMPI_WIN_FENCE | 
                                      OMPI_WIN_ACCESS_EPOCH |
                                      OMPI_WIN_EXPOSE_EPOCH);
                }
            }

            /* create or get a pointer to our datatype */
            proc = ompi_comm_peer_lookup( module->p2p_comm, header->hdr_origin );
            datatype = ompi_osc_pt2pt_datatype_create(proc, &payload);

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
                buffer;
            payload = (void*) (header + 1);

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_REPLY_HDR_NTOH(*header);
            }
#endif

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
                buffer;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_CONTROL_HDR_NTOH(*header);
            }
#endif

            assert(module == ompi_osc_pt2pt_windx_to_module(header->hdr_windx));

            OPAL_THREAD_ADD32(&(module->p2p_num_post_msgs), -1);
        }
        break;
    case OMPI_OSC_PT2PT_HDR_COMPLETE:
        {
            ompi_osc_pt2pt_control_header_t *header = 
                (ompi_osc_pt2pt_control_header_t*) 
                buffer;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_CONTROL_HDR_NTOH(*header);
            }
#endif

            assert(module = ompi_osc_pt2pt_windx_to_module(header->hdr_windx));

            /* we've heard from one more place, and have value reqs to
               process */
            OPAL_THREAD_ADD32(&(module->p2p_num_complete_msgs), -1);
            OPAL_THREAD_ADD32(&(module->p2p_num_pending_in), header->hdr_value[0]);
        }
        break;

    case OMPI_OSC_PT2PT_HDR_LOCK_REQ:
        {
            ompi_osc_pt2pt_control_header_t *header = 
                (ompi_osc_pt2pt_control_header_t*) 
                buffer;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_CONTROL_HDR_NTOH(*header);
            }
#endif

            assert(module == ompi_osc_pt2pt_windx_to_module(header->hdr_windx));

            if (header->hdr_value[1] > 0) {
                ompi_osc_pt2pt_passive_lock(module, header->hdr_value[0], 
                                            header->hdr_value[1]);
            } else {
                OPAL_THREAD_ADD32(&(module->p2p_lock_received_ack), 1);
            }
        }
        break;

    case OMPI_OSC_PT2PT_HDR_UNLOCK_REQ:
        {
            ompi_osc_pt2pt_control_header_t *header = 
                (ompi_osc_pt2pt_control_header_t*) 
                buffer;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_CONTROL_HDR_NTOH(*header);
            }
#endif

            assert(module == ompi_osc_pt2pt_windx_to_module(header->hdr_windx));

            ompi_osc_pt2pt_passive_unlock(module, header->hdr_value[0],
                                          header->hdr_value[1]);
        }
        break;
    case OMPI_OSC_PT2PT_HDR_UNLOCK_REPLY:
        {
            ompi_osc_pt2pt_control_header_t *header = 
                (ompi_osc_pt2pt_control_header_t*) 
                buffer;

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
                OMPI_OSC_PT2PT_CONTROL_HDR_NTOH(*header);
            }
#endif

            assert(module == ompi_osc_pt2pt_windx_to_module(header->hdr_windx));
            OPAL_THREAD_ADD32(&(module->p2p_num_pending_out), -1);
        }
        break;
    default:
        opal_output_verbose(5, ompi_osc_base_output,
                            "received packet for Window with unknown type");
   }

    item = &(pt2pt_buffer->super);
    OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                          item);
}



int
ompi_osc_pt2pt_request_test(ompi_request_t ** rptr,
                            int *completed,
                            ompi_status_public_t * status )
{
    ompi_request_t *request = *rptr;
    int ret = OMPI_SUCCESS;

#if OMPI_ENABLE_PROGRESS_THREADS == 0
    if (request->req_state == OMPI_REQUEST_INACTIVE ||
        request->req_complete) {
        ret = ompi_request_test(rptr, completed, status);
    } else {
        *completed = 0;
    }
#else
    ret = ompi_request_test(rptr, completed, status);
#endif

    return ret;
}

int
ompi_osc_pt2pt_progress(void)
{
    int ret, done, count = 0;
    void *node;
    uint32_t key;
    ompi_osc_pt2pt_module_t *module;
    opal_list_item_t *item;

    ret = opal_hash_table_get_first_key_uint32(&mca_osc_pt2pt_component.p2p_c_modules,
                                               &key,
                                               (void**) &module,
                                               &node);
    if (OMPI_SUCCESS != ret) return 0;

    do {
        /* loop through pending requests */
        for (item = opal_list_get_first(&module->p2p_pending_control_sends) ;
             item != opal_list_get_end(&module->p2p_pending_control_sends) ;
             item = opal_list_get_next(item)) {
            ompi_osc_pt2pt_buffer_t *buffer = 
                (ompi_osc_pt2pt_buffer_t*) item;

            ret = ompi_osc_pt2pt_request_test(&buffer->request, &done, &buffer->status);
            if (OMPI_SUCCESS == ret && done) {
                item = opal_list_remove_item(&module->p2p_pending_control_sends, 
                                             item);
                buffer->cbfunc(buffer);
                /* it's possible that cbfunc is going to do something
                   that calls progress, which means our loop is
                   probably hosed up because it's possible that the
                   list changed under us.  It's either exit the loop
                   through the list or start all over again.  I'm
                   going with exit. */
                break;
            }
        }
    } while (OMPI_SUCCESS ==
             opal_hash_table_get_next_key_uint32(&mca_osc_pt2pt_component.p2p_c_modules,
                                                 &key,
                                                 (void**) &module,
                                                 node,
                                                 &node));

    return count;
}

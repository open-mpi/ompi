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

#include <string.h>

#include "osc_rdma.h"
#include "osc_rdma_sendreq.h"
#include "osc_rdma_replyreq.h"
#include "osc_rdma_header.h"
#include "osc_rdma_obj_convert.h"
#include "osc_rdma_data_move.h"

#include "opal/threads/mutex.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/datatype/dt_arch.h"

static int ompi_osc_rdma_component_open(void);

ompi_osc_rdma_component_t mca_osc_rdma_component = {
    { /* ompi_osc_base_component_t */
        { /* ompi_base_component_t */
            OMPI_OSC_BASE_VERSION_1_0_0,
            "rdma",
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            ompi_osc_rdma_component_open,
            NULL
        },
        { /* mca_base_component_data */
            false /* checkpointable? */
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


void ompi_osc_rdma_component_fragment_cb(struct mca_btl_base_module_t *btl,
                                          mca_btl_base_tag_t tag,
                                          mca_btl_base_descriptor_t *descriptor,
                                          void *cbdata);

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

    value_string = malloc(sizeof(char) * value_len);
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

    result = flag;

    return result;
}


static int fence_sync_index;


static int
ompi_osc_rdma_component_open(void)
{
    fence_sync_index = 
        mca_base_param_reg_string(&mca_osc_rdma_component.super.osc_version,
                                  "fence_sync_method",
                                  "How to synchronize fence: reduce_scatter, allreduce, alltoall",
                                  false, false, "reduce_scatter", NULL);

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
    mca_osc_rdma_component.p2p_c_have_progress_threads = 
        enable_progress_threads;

    OBJ_CONSTRUCT(&mca_osc_rdma_component.p2p_c_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.p2p_c_modules,
                  opal_hash_table_t);
    opal_hash_table_init(&mca_osc_rdma_component.p2p_c_modules, 2);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.p2p_c_sendreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_rdma_component.p2p_c_sendreqs,
                        sizeof(ompi_osc_rdma_sendreq_t),
                        OBJ_CLASS(ompi_osc_rdma_sendreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.p2p_c_replyreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_rdma_component.p2p_c_replyreqs,
                        sizeof(ompi_osc_rdma_replyreq_t),
                        OBJ_CLASS(ompi_osc_rdma_replyreq_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&mca_osc_rdma_component.p2p_c_longreqs, opal_free_list_t);
    opal_free_list_init(&mca_osc_rdma_component.p2p_c_longreqs,
                        sizeof(ompi_osc_rdma_longreq_t),
                        OBJ_CLASS(ompi_osc_rdma_longreq_t),
                        1, -1, 1);

    return OMPI_SUCCESS;
}


int 
ompi_osc_rdma_component_finalize(void)
{
    size_t num_modules;

    if (0 != 
        (num_modules = opal_hash_table_get_size(&mca_osc_rdma_component.p2p_c_modules))) {
        opal_output(0, "WARNING: There were %d Windows created but not freed.",
                    num_modules);
    }

    mca_bml.bml_register(MCA_BTL_TAG_OSC_RDMA, NULL, NULL);

    OBJ_DESTRUCT(&mca_osc_rdma_component.p2p_c_longreqs);
    OBJ_DESTRUCT(&mca_osc_rdma_component.p2p_c_replyreqs);
    OBJ_DESTRUCT(&mca_osc_rdma_component.p2p_c_sendreqs);
    OBJ_DESTRUCT(&mca_osc_rdma_component.p2p_c_modules);
    OBJ_DESTRUCT(&mca_osc_rdma_component.p2p_c_lock);

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
    
    return 10;
}


int 
ompi_osc_rdma_component_select(ompi_win_t *win,
                               ompi_info_t *info,
                               ompi_communicator_t *comm)
{
    ompi_osc_rdma_module_t *module;
    int ret, i;
    char *sync_string;

    /* create module structure */
    module = malloc(sizeof(ompi_osc_rdma_module_t));
    if (NULL == module) return OMPI_ERROR;

    /* fill in the function pointer part */
    memcpy(module, &ompi_osc_rdma_module_template, 
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

    OBJ_CONSTRUCT(&module->p2p_pending_sendreqs, opal_list_t);
    module->p2p_num_pending_sendreqs = malloc(sizeof(short) * 
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
           sizeof(short) * ompi_comm_size(module->p2p_comm));

    module->p2p_num_pending_out = 0;
    module->p2p_num_pending_in = 0;
    module->p2p_tag_counter = 0;

    OBJ_CONSTRUCT(&(module->p2p_long_msgs), opal_list_t);

    OBJ_CONSTRUCT(&(module->p2p_copy_pending_sendreqs), opal_list_t);
    module->p2p_copy_num_pending_sendreqs = malloc(sizeof(short) * 
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
           sizeof(short) * ompi_comm_size(module->p2p_comm));

    module->p2p_eager_send = check_config_value_bool("eager_send", info);

    /* fence data */
    module->p2p_fence_coll_counts = malloc(sizeof(int) * 
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

    module->p2p_fence_coll_results = malloc(sizeof(int) * 
                                            ompi_comm_size(module->p2p_comm));
    if (NULL == module->p2p_fence_coll_counts) {
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

    /* figure out what sync method to use */
    mca_base_param_lookup_string(fence_sync_index, &sync_string);
    if (0 == strcmp(sync_string, "reduce_scatter")) {
        module->p2p_fence_sync_type = OSC_SYNC_REDUCE_SCATTER;
    } else if (0 == strcmp(sync_string, "allreduce")) {
        module->p2p_fence_sync_type = OSC_SYNC_ALLREDUCE;
    } else if (0 == strcmp(sync_string, "alltoall")) {
        module->p2p_fence_sync_type = OSC_SYNC_ALLTOALL;
    } else {
        opal_output(0, "invalid value for fence_sync_method parameter: %s\n", sync_string);
        return OMPI_ERROR;
    }

    /* pwsc data */
    module->p2p_pw_group = NULL;
    module->p2p_sc_group = NULL;

    /* lock data */
    module->p2p_lock_status = 0;
    module->p2p_shared_count = 0;
    OBJ_CONSTRUCT(&(module->p2p_locks_pending), opal_list_t);
    module->p2p_lock_received_ack = 0;

    /* update component data */
    OPAL_THREAD_LOCK(&mca_osc_rdma_component.p2p_c_lock);
    opal_hash_table_set_value_uint32(&mca_osc_rdma_component.p2p_c_modules,
                                     module->p2p_comm->c_contextid,
                                     module);
    OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.p2p_c_lock);

    /* fill in window information */
    win->w_osc_module = (ompi_osc_base_module_t*) module;
    if (check_config_value_bool("no_locks", info)) {
        win->w_flags |= OMPI_WIN_NO_LOCKS;
    }

    /* sync memory - make sure all initialization completed */
    opal_atomic_mb();

    /* register to receive fragment callbacks */
    ret = mca_bml.bml_register(MCA_BTL_TAG_OSC_RDMA,
                               ompi_osc_rdma_component_fragment_cb,
                               NULL);


    if (module->p2p_eager_send) {
        /* need to barrier if eager sending or we can receive before the
           other side has been fully setup, causing much gnashing of
           teeth. */
        module->p2p_comm->c_coll.coll_barrier(module->p2p_comm);
    }

    return ret;
}



/* dispatch for callback on message completion */
void
ompi_osc_rdma_component_fragment_cb(struct mca_btl_base_module_t *btl,
                                mca_btl_base_tag_t tag,
                                mca_btl_base_descriptor_t *descriptor,
                                void *cbdata)
{
    int ret;
    ompi_osc_rdma_module_t *module;
    void *payload;

    assert(descriptor->des_dst[0].seg_len >= 
           sizeof(ompi_osc_rdma_base_header_t));

    /* handle message */
    switch (((ompi_osc_rdma_base_header_t*) descriptor->des_dst[0].seg_addr.pval)->hdr_type) {
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

            /* create or get a pointer to our datatype */
            proc = module->p2p_comm->c_pml_procs[header->hdr_origin]->proc_ompi;
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

#if !defined(WORDS_BIGENDIAN) && OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_CONTROL_HDR_NTOH(*header);
            }
#endif

            /* get our module pointer */
            module = ompi_osc_rdma_windx_to_module(header->hdr_windx);
            if (NULL == module) return;

            OPAL_THREAD_ADD32(&(module->p2p_num_pending_in), -1);
        }
        break;
    case OMPI_OSC_RDMA_HDR_COMPLETE:
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

            /* we've heard from one more place, and have value reqs to
               process */
            OPAL_THREAD_ADD32(&(module->p2p_num_pending_out), -1);
            OPAL_THREAD_ADD32(&(module->p2p_num_pending_in), header->hdr_value[0]);
        }
        break;

    case OMPI_OSC_RDMA_HDR_LOCK_REQ:
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

            if (header->hdr_value[1] > 0) {
                ompi_osc_rdma_passive_lock(module, header->hdr_value[0], 
                                            header->hdr_value[1]);
            } else {
                OPAL_THREAD_ADD32(&(module->p2p_lock_received_ack), 1);
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

    default:
        /* BWB - FIX ME - this sucks */
        opal_output(0, "received packet for Window with unknown type");
        abort();
   }
}

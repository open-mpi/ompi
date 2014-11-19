/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"

#include <stdio.h>
#include <string.h>

#include "osc_pt2pt.h"
#include "osc_pt2pt_request.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_frag.h"
#include "osc_pt2pt_data_move.h"

#include "opal_stdint.h"
#include "ompi/memchecker.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "ompi/mca/osc/base/base.h"

/* progress an OSC request */
static int ompi_osc_pt2pt_req_comm_complete (ompi_request_t *request)
{
    ompi_osc_pt2pt_request_t *pt2pt_request = (ompi_osc_pt2pt_request_t *) request->req_complete_cb_data;
    ompi_osc_pt2pt_module_t *module = pt2pt_request->module;

    OPAL_OUTPUT_VERBOSE((10, ompi_osc_base_framework.framework_output,
                         "ompi_osc_pt2pt_req_comm_complete called tag = %d",
                         request->req_status.MPI_TAG));

    mark_outgoing_completion (module);

    if (0 == OPAL_THREAD_ADD32(&pt2pt_request->outstanding_requests, -1)) {
        ompi_osc_pt2pt_request_complete (pt2pt_request, request->req_status.MPI_ERROR);
    }

    /* put this request on the garbage colletion list */
    osc_pt2pt_gc_add_request (module, request);

    return OMPI_SUCCESS;
}

static int ompi_osc_pt2pt_dt_send_complete (ompi_request_t *request)
{
    ompi_datatype_t *datatype = (ompi_datatype_t *) request->req_complete_cb_data;
    ompi_osc_pt2pt_module_t *module = NULL;

    OBJ_RELEASE(datatype);

    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.lock);
    opal_hash_table_get_value_uint32(&mca_osc_pt2pt_component.modules,
                                     ompi_comm_get_cid(request->req_mpi_object.comm),
                                     (void **) &module);
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.lock);
    assert (NULL != module);

    /* put this request on the garbage colletion list */
    osc_pt2pt_gc_add_request (module, request);

    return OMPI_SUCCESS;
}

/* self communication optimizations */
static inline int ompi_osc_pt2pt_put_self (void *source, int source_count, ompi_datatype_t *source_datatype,
                                          OPAL_PTRDIFF_TYPE target_disp, int target_count, ompi_datatype_t *target_datatype,
                                          ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_request_t *request)
{
    void *target = (unsigned char*) module->baseptr +
        ((unsigned long) target_disp * module->disp_unit);
    int ret;

    /* if we are in active target mode wait until all post messages arrive */
    if (module->sc_group && !module->active_eager_send_active) {
        OPAL_THREAD_LOCK(&module->lock);
        while (0 != module->num_post_msgs) {
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    if (!(module->passive_target_access_epoch || module->active_eager_send_active)) {
        return OMPI_ERR_RMA_SYNC;
    }

    ret = ompi_datatype_sndrcv (source, source_count, source_datatype,
                                target, target_count, target_datatype);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (request) {
        ompi_osc_pt2pt_request_complete (request, MPI_SUCCESS);
    }

    return OMPI_SUCCESS;
}

static inline int ompi_osc_pt2pt_get_self (void *target, int target_count, ompi_datatype_t *target_datatype,
                                          OPAL_PTRDIFF_TYPE source_disp, int source_count, ompi_datatype_t *source_datatype,
                                          ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_request_t *request)
{
    void *source = (unsigned char*) module->baseptr +
        ((unsigned long) source_disp * module->disp_unit);
    int ret;

    /* if we are in active target mode wait until all post messages arrive */
    if (module->sc_group && !module->active_eager_send_active) {
        OPAL_THREAD_LOCK(&module->lock);
        while (0 != module->num_post_msgs) {
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    if (!(module->passive_target_access_epoch || module->active_eager_send_active)) {
        return OMPI_ERR_RMA_SYNC;
    }

    ret = ompi_datatype_sndrcv (source, source_count, source_datatype,
                                target, target_count, target_datatype);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    if (request) {
        ompi_osc_pt2pt_request_complete (request, MPI_SUCCESS);
    }

    return OMPI_SUCCESS;
}

static inline int ompi_osc_pt2pt_cas_self (void *source, void *compare, void *result, ompi_datatype_t *datatype,
                                          OPAL_PTRDIFF_TYPE target_disp, ompi_osc_pt2pt_module_t *module)
{
    void *target = (unsigned char*) module->baseptr +
        ((unsigned long) target_disp * module->disp_unit);

    /* if we are in active target mode wait until all post messages arrive */
    if (module->sc_group && !module->active_eager_send_active) {
        OPAL_THREAD_LOCK(&module->lock);
        while (0 != module->num_post_msgs) {
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    if (!(module->passive_target_access_epoch || module->active_eager_send_active)) {
        return OMPI_ERR_RMA_SYNC;
    }

    ompi_osc_pt2pt_accumulate_lock (module);

    memcpy (result, target, datatype->super.size);

    if (0 == memcmp (compare, target, datatype->super.size)) {
        memcpy (target, source, datatype->super.size);
    }

    ompi_osc_pt2pt_accumulate_unlock (module);

    return OMPI_SUCCESS;
}

static inline int ompi_osc_pt2pt_acc_self (void *source, int source_count, ompi_datatype_t *source_datatype,
                                          OPAL_PTRDIFF_TYPE target_disp, int target_count, ompi_datatype_t *target_datatype,
                                          ompi_op_t *op, ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_request_t *request)
{
    void *target = (unsigned char*) module->baseptr +
        ((unsigned long) target_disp * module->disp_unit);
    int ret;

    /* if we are in active target mode wait until all post messages arrive */
    if (module->sc_group && !module->active_eager_send_active) {
        OPAL_THREAD_LOCK(&module->lock);
        while (0 != module->num_post_msgs) {
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    if (!(module->passive_target_access_epoch || module->active_eager_send_active)) {
        return OMPI_ERR_RMA_SYNC;
    }

    ompi_osc_pt2pt_accumulate_lock (module);

    if (&ompi_mpi_op_replace.op != op) {
        ret = ompi_osc_base_sndrcv_op (source, source_count, source_datatype, target, target_count, target_datatype, op);
    } else {
        ret = ompi_datatype_sndrcv (source, source_count, source_datatype, target, target_count, target_datatype);
    }

    ompi_osc_pt2pt_accumulate_unlock (module);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OPAL_OUTPUT_VERBOSE((5, ompi_osc_base_framework.framework_output,
                             "ompi_osc_pt2pt_acc_self: failed performing accumulate operation. ret = %d", ret));
        return ret;
    }

    if (request) {
        ompi_osc_pt2pt_request_complete (request, MPI_SUCCESS);
    }

    return OMPI_SUCCESS;
}

static inline int ompi_osc_pt2pt_gacc_self (void *source, int source_count, ompi_datatype_t *source_datatype,
                                           void *result, int result_count, ompi_datatype_t *result_datatype,
                                           OPAL_PTRDIFF_TYPE target_disp, int target_count, ompi_datatype_t *target_datatype,
                                           ompi_op_t *op, ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_request_t *request)
{
    void *target = (unsigned char*) module->baseptr +
        ((unsigned long) target_disp * module->disp_unit);
    int ret;

    /* if we are in active target mode wait until all post messages arrive */
    if (module->sc_group && !module->active_eager_send_active) {
        OPAL_THREAD_LOCK(&module->lock);
        while (0 != module->num_post_msgs) {
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    if (!(module->passive_target_access_epoch || module->active_eager_send_active)) {
        return OMPI_ERR_RMA_SYNC;
    }

    ompi_osc_pt2pt_accumulate_lock (module);

    do {
        ret = ompi_datatype_sndrcv (target, target_count, target_datatype,
                                    result, result_count, result_datatype);

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            OPAL_OUTPUT_VERBOSE((5, ompi_osc_base_framework.framework_output,
                                 "ompi_osc_pt2pt_gacc_self: failed copying to the target buffer. ret = %d", ret));
            break;
        }

        if (&ompi_mpi_op_no_op.op != op) {
            if (&ompi_mpi_op_replace.op != op) {
                ret = ompi_osc_base_sndrcv_op (source, source_count, source_datatype, target, target_count, target_datatype, op);
            } else {
                ret = ompi_datatype_sndrcv (source, source_count, source_datatype, target, target_count, target_datatype);
            }
        }

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            OPAL_OUTPUT_VERBOSE((5, ompi_osc_base_framework.framework_output,
                                 "ompi_osc_pt2pt_gacc_self: failed performing accumulate operation. ret = %d", ret));
            break;
        }
    } while (0);

    ompi_osc_pt2pt_accumulate_unlock (module);

    if (request) {
        /* NTH: is it ok to use an ompi error code here? */
        ompi_osc_pt2pt_request_complete (request, ret);
    }

    return OMPI_SUCCESS;
}
/* end: self communication optimizations */

static inline int ompi_osc_pt2pt_put_w_req (void *origin_addr, int origin_count,
                                           struct ompi_datatype_t *origin_dt,
                                           int target, OPAL_PTRDIFF_TYPE target_disp,
                                           int target_count, struct ompi_datatype_t *target_dt,
                                           ompi_win_t *win, ompi_osc_pt2pt_request_t *request)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_proc_t *proc = ompi_comm_peer_lookup(module->comm, target);
    ompi_osc_pt2pt_frag_t *frag;
    ompi_osc_pt2pt_header_put_t *header;
    size_t ddt_len, payload_len, frag_len;
    bool is_long_datatype = false;
    bool is_long_msg = false;
    const void *packed_ddt;
    int tag = -1, ret;
    char *ptr;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "put: 0x%lx, %d, %s, %d, %d, %d, %s, %s",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name, win->w_name));

    if (!ompi_osc_pt2pt_check_access_epoch (module, target)) {
        return OMPI_ERR_RMA_SYNC;
    }

    /* short-circuit case */
    if (0 == origin_count || 0 == target_count) {
        if (request) {
            ompi_osc_pt2pt_request_complete (request, MPI_SUCCESS);
        }

        return OMPI_SUCCESS;
    }

    /* optimize self communication. TODO: optimize local communication */
    if (ompi_comm_rank (module->comm) == target) {
        return ompi_osc_pt2pt_put_self (origin_addr, origin_count, origin_dt,
                                       target_disp, target_count, target_dt,
                                       module, request);
    }

    /* Compute datatype and payload lengths.  Note that the datatype description
     * must fit in a single buffer */
    ddt_len = ompi_datatype_pack_description_length(target_dt);
    payload_len = origin_dt->super.size * origin_count;
    frag_len = sizeof(ompi_osc_pt2pt_header_put_t) + ddt_len + payload_len;

    ret = ompi_osc_pt2pt_frag_alloc(module, target, frag_len, &frag, &ptr);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        frag_len = sizeof(ompi_osc_pt2pt_header_put_t) + ddt_len;
        ret = ompi_osc_pt2pt_frag_alloc(module, target, frag_len, &frag, &ptr);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            /* allocate space for the header plus space to store ddt_len */
            frag_len = sizeof(ompi_osc_pt2pt_header_put_t) + 8;
            ret = ompi_osc_pt2pt_frag_alloc(module, target, frag_len, &frag, &ptr);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            is_long_datatype = true;
        }

        is_long_msg = true;
        tag = get_tag(module);
    }

    /* flush will be called at the end of this function. make sure the post message has
     * arrived. */
    if ((is_long_msg || request) && module->sc_group) {
        OPAL_THREAD_LOCK(&module->lock);
        while (0 != module->num_post_msgs) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "waiting for post messages. num_post_msgs = %d", module->num_post_msgs));
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: put long protocol: %d, large datatype: %d",
                         (int) is_long_msg, (int) is_long_datatype));

    header = (ompi_osc_pt2pt_header_put_t *) ptr;
    header->base.flags = 0;
    header->len = frag_len;
    header->count = target_count;
    header->displacement = target_disp;
    ptr += sizeof(ompi_osc_pt2pt_header_put_t);

    do {
        ret = ompi_datatype_get_pack_description(target_dt, &packed_ddt);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        if (is_long_datatype) {
            /* the datatype does not fit in an eager message. send it seperately */
            header->base.flags |= OMPI_OSC_PT2PT_HDR_FLAG_LARGE_DATATYPE;

            OBJ_RETAIN(target_dt);

            ret = ompi_osc_pt2pt_isend_w_cb ((void *) packed_ddt, ddt_len, MPI_BYTE, target,
                                            tag, module->comm, ompi_osc_pt2pt_dt_send_complete,
                                            target_dt);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }

            *((uint64_t *) ptr) = ddt_len;
            ptr += 8;
        } else {
            memcpy((unsigned char*) ptr, packed_ddt, ddt_len);
            ptr += ddt_len;
        }

        if (!is_long_msg) {
            header->base.type = OMPI_OSC_PT2PT_HDR_TYPE_PUT;

            osc_pt2pt_copy_for_send (ptr, payload_len, origin_addr, proc, origin_count,
                                    origin_dt);

            /* the user's buffer is no longer needed so mark the request as
             * complete. */
            if (request) {
                ompi_osc_pt2pt_request_complete (request, MPI_SUCCESS);
            }
        } else {
            header->base.type = OMPI_OSC_PT2PT_HDR_TYPE_PUT_LONG;

            header->tag = tag;

            /* increase the outgoing signal count */
            ompi_osc_signal_outgoing (module, target, 1);

            if (request) {
                request->outstanding_requests = 1;
                ret = ompi_osc_pt2pt_isend_w_cb (origin_addr, origin_count, origin_dt,
                                                target, tag, module->comm, ompi_osc_pt2pt_req_comm_complete,
                                                request);
            } else {
                ret = ompi_osc_pt2pt_component_isend (module,origin_addr, origin_count, origin_dt, target, tag,
                                                     module->comm);
            }
        }
    } while (0);

    if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
        header->base.flags |= OMPI_OSC_PT2PT_HDR_FLAG_VALID;
    }

    ret = ompi_osc_pt2pt_frag_finish(module, frag);

    if (request || is_long_msg) {
        /* need to flush now in case the caller decides to wait on the request */
        ompi_osc_pt2pt_frag_flush_target (module, target);
    }

    return ret;
}

int
ompi_osc_pt2pt_put(void *origin_addr, int origin_count,
                  struct ompi_datatype_t *origin_dt,
                  int target, OPAL_PTRDIFF_TYPE target_disp,
                  int target_count,
                  struct ompi_datatype_t *target_dt, ompi_win_t *win)
{
    return ompi_osc_pt2pt_put_w_req (origin_addr, origin_count,
                                    origin_dt, target, target_disp,
                                    target_count, target_dt, win, NULL);
}


static int
ompi_osc_pt2pt_accumulate_w_req (void *origin_addr, int origin_count,
                                struct ompi_datatype_t *origin_dt,
                                int target, OPAL_PTRDIFF_TYPE target_disp,
                                int target_count,
                                struct ompi_datatype_t *target_dt,
                                struct ompi_op_t *op, ompi_win_t *win,
                                ompi_osc_pt2pt_request_t *request)
{
    int ret;
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_proc_t *proc = ompi_comm_peer_lookup(module->comm, target);
    bool is_long_datatype = false;
    bool is_long_msg = false;
    ompi_osc_pt2pt_frag_t *frag;
    ompi_osc_pt2pt_header_acc_t *header;
    size_t ddt_len, payload_len, frag_len;
    char *ptr;
    const void *packed_ddt;
    int tag = -1;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "acc: 0x%lx, %d, %s, %d, %d, %d, %s, %s, %s",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name, op->o_name,
                         win->w_name));

    if (!ompi_osc_pt2pt_check_access_epoch (module, target)) {
        return OMPI_ERR_RMA_SYNC;
    }

    /* short-circuit case */
    if (0 == origin_count || 0 == target_count) {
        if (request) {
            ompi_osc_pt2pt_request_complete (request, MPI_SUCCESS);
        }

        return OMPI_SUCCESS;
    }

    /* optimize the self case. TODO: optimize the local case */
    if (ompi_comm_rank (module->comm) == target) {
        return ompi_osc_pt2pt_acc_self (origin_addr, origin_count, origin_dt,
                                       target_disp, target_count, target_dt,
                                       op, module, request);
    }

    /* Compute datatype and payload lengths.  Note that the datatype description
     * must fit in a single frag */
    ddt_len = ompi_datatype_pack_description_length(target_dt);
    payload_len = origin_dt->super.size * origin_count;

    frag_len = sizeof(*header) + ddt_len + payload_len;
    ret = ompi_osc_pt2pt_frag_alloc(module, target, frag_len, &frag, &ptr);
    if (OMPI_SUCCESS != ret) {
        frag_len = sizeof(*header) + ddt_len;
        ret = ompi_osc_pt2pt_frag_alloc(module, target, frag_len, &frag, &ptr);
        if (OMPI_SUCCESS != ret) {
            /* allocate space for the header plus space to store ddt_len */
            frag_len = sizeof(*header) + 8;
            ret = ompi_osc_pt2pt_frag_alloc(module, target, frag_len, &frag, &ptr);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            is_long_datatype = true;
         }

        is_long_msg = true;
        tag = get_tag (module);
    }

    /* flush will be called at the end of this function. make sure the post message has
     * arrived. */
    if ((is_long_msg || request) && module->sc_group) {
        OPAL_THREAD_LOCK(&module->lock);
        while (0 != module->num_post_msgs) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "waiting for post messages. num_post_msgs = %d", module->num_post_msgs));
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    header = (ompi_osc_pt2pt_header_acc_t*) ptr;
    header->base.flags = 0;
    header->len = frag_len;
    header->count = target_count;
    header->displacement = target_disp;
    header->op = op->o_f_to_c_index;
    ptr += sizeof (*header);

    do {
        ret = ompi_datatype_get_pack_description(target_dt, &packed_ddt);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        if (is_long_datatype) {
            /* the datatype does not fit in an eager message. send it seperately */
            header->base.flags |= OMPI_OSC_PT2PT_HDR_FLAG_LARGE_DATATYPE;

            OBJ_RETAIN(target_dt);

            ret = ompi_osc_pt2pt_isend_w_cb ((void *) packed_ddt, ddt_len, MPI_BYTE, target,
                                            tag, module->comm, ompi_osc_pt2pt_dt_send_complete,
                                            target_dt);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }

            *((uint64_t *) ptr) = ddt_len;
            ptr += 8;
        } else {
            memcpy((unsigned char*) ptr, packed_ddt, ddt_len);
            ptr += ddt_len;
        }

        if (!is_long_msg) {
            header->base.type = OMPI_OSC_PT2PT_HDR_TYPE_ACC;

            osc_pt2pt_copy_for_send (ptr, payload_len, origin_addr, proc,
                                    origin_count, origin_dt);

            /* the user's buffer is no longer needed so mark the request as
             * complete. */
            if (request) {
                ompi_osc_pt2pt_request_complete (request, MPI_SUCCESS);
            }
        } else {
            header->base.type = OMPI_OSC_PT2PT_HDR_TYPE_ACC_LONG;

            header->tag = tag;

            OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                                 "acc: starting long accumulate with tag %d", tag));

            /* increment the outgoing send count */
            ompi_osc_signal_outgoing (module, target, 1);

            if (request) {
                request->outstanding_requests = 1;
                ret = ompi_osc_pt2pt_isend_w_cb (origin_addr, origin_count, origin_dt,
                                                target, tag, module->comm, ompi_osc_pt2pt_req_comm_complete,
                                                request);
            } else {
                ret = ompi_osc_pt2pt_component_isend (module, origin_addr, origin_count, origin_dt, target, tag,
                                                     module->comm);
            }
        }
    } while (0);

    if (OMPI_SUCCESS != ret) {
        OPAL_OUTPUT_VERBOSE((25, ompi_osc_base_framework.framework_output,
                             "acc: failed with eror %d", ret));
    } else {
        /* mark the fragment as valid */
        header->base.flags |= OMPI_OSC_PT2PT_HDR_FLAG_VALID;
    }

    ret = ompi_osc_pt2pt_frag_finish(module, frag);

    if (is_long_msg || request) {
        /* need to flush now in case the caller decides to wait on the request */
        ompi_osc_pt2pt_frag_flush_target (module, target);
    }

    return ret;
}

int
ompi_osc_pt2pt_accumulate(void *origin_addr, int origin_count,
                         struct ompi_datatype_t *origin_dt,
                         int target, OPAL_PTRDIFF_TYPE target_disp,
                         int target_count,
                         struct ompi_datatype_t *target_dt,
                         struct ompi_op_t *op, ompi_win_t *win)
{
    return ompi_osc_pt2pt_accumulate_w_req (origin_addr, origin_count, origin_dt,
                                           target, target_disp, target_count,
                                           target_dt, op, win, NULL);
}

int ompi_osc_pt2pt_compare_and_swap (void *origin_addr, void *compare_addr,
                                    void *result_addr, struct ompi_datatype_t *dt,
                                    int target, OPAL_PTRDIFF_TYPE target_disp,
                                    struct ompi_win_t *win)
{
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_proc_t *proc = ompi_comm_peer_lookup(module->comm, target);
    ompi_osc_pt2pt_frag_t *frag;
    ompi_osc_pt2pt_header_cswap_t *header;
    size_t ddt_len, payload_len, frag_len;
    ompi_osc_pt2pt_request_t *request;
    const void *packed_ddt;
    int ret, tag;
    char *ptr;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "cswap: 0x%lx, 0x%lx, 0x%lx, %s, %d, %d, %s",
                         (unsigned long) origin_addr, (unsigned long) compare_addr,
                         (unsigned long) result_addr, dt->name, target, (int) target_disp,
                         win->w_name));

    if (!ompi_osc_pt2pt_check_access_epoch (module, target)) {
        return OMPI_ERR_RMA_SYNC;
    }

    /* optimize self case. TODO: optimize local case */
    if (ompi_comm_rank (module->comm) == target) {
        return ompi_osc_pt2pt_cas_self (origin_addr, compare_addr, result_addr, dt, target_disp,
                                       module);
    }

    /* compare-and-swaps are always request based, so that we know where to land the data */
    OMPI_OSC_PT2PT_REQUEST_ALLOC(win, request);
    if (NULL == request) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request->type = OMPI_OSC_PT2PT_HDR_TYPE_CSWAP;
    request->origin_addr = origin_addr;
    request->internal = true;
    OBJ_RETAIN(dt);
    request->origin_dt = dt;

    /* Compute datatype and payload lengths.  Note that the datatype description
     * must fit in a single frag. It should be small in this case. */
    ddt_len = ompi_datatype_pack_description_length(dt);

    /* we need to send both the origin and compare buffers */
    payload_len = dt->super.size * 2;

    frag_len = sizeof(ompi_osc_pt2pt_header_cswap_t) + ddt_len + payload_len;
    ret = ompi_osc_pt2pt_frag_alloc(module, target, frag_len, &frag, &ptr);
    if (OMPI_SUCCESS != ret) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    tag = get_tag (module);
    ompi_osc_signal_outgoing (module, target, 1);

    header = (ompi_osc_pt2pt_header_cswap_t *) ptr;
    header->base.type = OMPI_OSC_PT2PT_HDR_TYPE_CSWAP;
    header->base.flags = OMPI_OSC_PT2PT_HDR_FLAG_VALID;
    header->len = frag_len;
    header->displacement = target_disp;
    header->tag = tag;
    ptr += sizeof(ompi_osc_pt2pt_header_cswap_t);

    ret = ompi_datatype_get_pack_description(dt, &packed_ddt);
    memcpy((unsigned char*) ptr, packed_ddt, ddt_len);
    ptr += ddt_len;

    /* pack the origin and compare data */
    osc_pt2pt_copy_for_send (ptr, dt->super.size, origin_addr, proc, 1, dt);
    ptr += dt->super.size;
    osc_pt2pt_copy_for_send (ptr, dt->super.size, compare_addr, proc, 1, dt);

    request->outstanding_requests = 1;
    ret = ompi_osc_pt2pt_irecv_w_cb (result_addr, 1, dt, target, tag, module->comm,
                                    NULL, ompi_osc_pt2pt_req_comm_complete, request);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ret = ompi_osc_pt2pt_frag_finish(module, frag);

    return ret;
}


int ompi_osc_pt2pt_fetch_and_op(void *origin_addr, void *result_addr,
                               struct ompi_datatype_t *dt, int target,
                               OPAL_PTRDIFF_TYPE target_disp, struct ompi_op_t *op,
                               struct ompi_win_t *win)
{
    return ompi_osc_pt2pt_get_accumulate(origin_addr, 1, dt, result_addr, 1, dt,
                                        target, target_disp, 1, dt, op, win);
}

int ompi_osc_pt2pt_rput(void *origin_addr, int origin_count,
                       struct ompi_datatype_t *origin_dt,
                       int target, OPAL_PTRDIFF_TYPE target_disp,
                       int target_count, struct ompi_datatype_t *target_dt,
                       struct ompi_win_t *win, struct ompi_request_t **request)
{
    ompi_osc_pt2pt_request_t *pt2pt_request;
    int ret;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rput: 0x%lx, %d, %s, %d, %d, %d, %s, %s",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name, win->w_name));

    OMPI_OSC_PT2PT_REQUEST_ALLOC(win, pt2pt_request);
    if (NULL == pt2pt_request) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* short-circuit case */
    if (0 == origin_count || 0 == target_count) {
        ompi_osc_pt2pt_request_complete (pt2pt_request, MPI_SUCCESS);
        *request = &pt2pt_request->super;
        return OMPI_SUCCESS;
    }

    pt2pt_request->type = OMPI_OSC_PT2PT_HDR_TYPE_PUT;

    ret = ompi_osc_pt2pt_put_w_req (origin_addr, origin_count, origin_dt, target,
                                   target_disp, target_count, target_dt, win,
                                   pt2pt_request);
    if (OMPI_SUCCESS != ret) {
        OMPI_OSC_PT2PT_REQUEST_RETURN(pt2pt_request);
        return ret;
    }

    *request = (ompi_request_t *) pt2pt_request;

    return OMPI_SUCCESS;
}

static inline int ompi_osc_pt2pt_rget_internal (void *origin_addr, int origin_count,
                                               struct ompi_datatype_t *origin_dt,
                                               int target,
                                               OPAL_PTRDIFF_TYPE target_disp,
                                               int target_count,
                                               struct ompi_datatype_t *target_dt,
                                               struct ompi_win_t *win, bool release_req,
                                               struct ompi_request_t **request)
{
    int ret, tag;
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    bool is_long_datatype = false;
    ompi_osc_pt2pt_frag_t *frag;
    ompi_osc_pt2pt_header_get_t *header;
    size_t ddt_len, frag_len;
    char *ptr;
    const void *packed_ddt;
    ompi_osc_pt2pt_request_t *pt2pt_request;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "get: 0x%lx, %d, %s, %d, %d, %d, %s, %s",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name, win->w_name));

    if (!ompi_osc_pt2pt_check_access_epoch (module, target)) {
        return OMPI_ERR_RMA_SYNC;
    }

    /* gets are always request based, so that we know where to land the data */
    OMPI_OSC_PT2PT_REQUEST_ALLOC(win, pt2pt_request);
    if (NULL == pt2pt_request) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    pt2pt_request->internal = release_req;

    /* short-circuit case */
    if (0 == origin_count || 0 == target_count) {
        ompi_osc_pt2pt_request_complete (pt2pt_request, MPI_SUCCESS);
        *request = &pt2pt_request->super;
        return OMPI_SUCCESS;
    }

    /* optimize self communication. TODO: optimize local communication */
    if (ompi_comm_rank (module->comm) == target) {
        *request = &pt2pt_request->super;
        return ompi_osc_pt2pt_get_self (origin_addr, origin_count, origin_dt,
                                       target_disp, target_count, target_dt,
                                       module, pt2pt_request);
    }

    pt2pt_request->type = OMPI_OSC_PT2PT_HDR_TYPE_GET;
    pt2pt_request->origin_addr = origin_addr;
    pt2pt_request->origin_count = origin_count;
    OBJ_RETAIN(origin_dt);
    pt2pt_request->origin_dt = origin_dt;

    /* Compute datatype length.  Note that the datatype description
     * must fit in a single frag */
    ddt_len = ompi_datatype_pack_description_length(target_dt);

    frag_len = sizeof(ompi_osc_pt2pt_header_get_t) + ddt_len;
    ret = ompi_osc_pt2pt_frag_alloc(module, target, frag_len, &frag, &ptr);
    if (OMPI_SUCCESS != ret) {
        /* allocate space for the header plus space to store ddt_len */
        frag_len = sizeof(ompi_osc_pt2pt_header_put_t) + 8;
        ret = ompi_osc_pt2pt_frag_alloc(module, target, frag_len, &frag, &ptr);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        is_long_datatype = true;
    }

    tag = get_tag (module);

    /* for bookkeeping the get is "outgoing" */
    ompi_osc_signal_outgoing (module, target, 1);

    /* flush will be called at the end of this function. make sure the post message has
     * arrived. */
    if (!release_req && module->sc_group) {
        while (0 != module->num_post_msgs) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "waiting for post messages. num_post_msgs = %d", module->num_post_msgs));
            opal_condition_wait(&module->cond, &module->lock);
        }
    }

    header = (ompi_osc_pt2pt_header_get_t*) ptr;
    header->base.type = OMPI_OSC_PT2PT_HDR_TYPE_GET;
    header->base.flags = 0;
    header->len = frag_len;
    header->count = target_count;
    header->displacement = target_disp;
    header->tag = tag;
    ptr += sizeof(ompi_osc_pt2pt_header_get_t);

    do {
        ret = ompi_datatype_get_pack_description(target_dt, &packed_ddt);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        if (is_long_datatype) {
            /* the datatype does not fit in an eager message. send it seperately */
            header->base.flags |= OMPI_OSC_PT2PT_HDR_FLAG_LARGE_DATATYPE;

            OBJ_RETAIN(target_dt);

            ret = ompi_osc_pt2pt_isend_w_cb ((void *) packed_ddt, ddt_len, MPI_BYTE, target,
                                            tag, module->comm, ompi_osc_pt2pt_dt_send_complete,
                                            target_dt);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }

            *((uint64_t *) ptr) = ddt_len;
            ptr += 8;
        } else {
            memcpy((unsigned char*) ptr, packed_ddt, ddt_len);
            ptr += ddt_len;
        }

        /* TODO -- store the request somewhere so we can cancel it on error */
        pt2pt_request->outstanding_requests = 1;
        ret = ompi_osc_pt2pt_irecv_w_cb (origin_addr, origin_count, origin_dt, target, tag,
                                        module->comm, NULL, ompi_osc_pt2pt_req_comm_complete, pt2pt_request);
    } while (0);

    if (OMPI_SUCCESS == ret) {
        header->base.flags |= OMPI_OSC_PT2PT_HDR_FLAG_VALID;
        *request = &pt2pt_request->super;
    }

    ret = ompi_osc_pt2pt_frag_finish(module, frag);

    if (!release_req) {
        /* need to flush now in case the caller decides to wait on the request */
        ompi_osc_pt2pt_frag_flush_target (module, target);
    }

    return ret;
}

int ompi_osc_pt2pt_rget (void *origin_addr, int origin_count, struct ompi_datatype_t *origin_dt,
                        int target, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                        struct ompi_datatype_t *target_dt, struct ompi_win_t *win,
                        struct ompi_request_t **request)
{
    /* NTH: need to check for a passive access epoch and return the appropriate error if nececcesary */
    return ompi_osc_pt2pt_rget_internal (origin_addr, origin_count, origin_dt, target, target_disp,
                                        target_count, target_dt, win, false, request);
}


int ompi_osc_pt2pt_get (void *origin_addr, int origin_count, struct ompi_datatype_t *origin_dt,
                       int target, OPAL_PTRDIFF_TYPE target_disp, int target_count,
                       struct ompi_datatype_t *target_dt, struct ompi_win_t *win)
{
    ompi_request_t *request;

    return ompi_osc_pt2pt_rget_internal (origin_addr, origin_count, origin_dt, target, target_disp,
                                        target_count, target_dt, win, true, &request);
}

int ompi_osc_pt2pt_raccumulate(void *origin_addr, int origin_count,
                              struct ompi_datatype_t *origin_dt, int target,
                              OPAL_PTRDIFF_TYPE target_disp, int target_count,
                              struct ompi_datatype_t *target_dt, struct ompi_op_t *op,
                              struct ompi_win_t *win, struct ompi_request_t **request)
{
    ompi_osc_pt2pt_request_t *pt2pt_request;
    int ret;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "raccumulate: 0x%lx, %d, %s, %d, %d, %d, %s, %s, %s",
                         (unsigned long) origin_addr, origin_count,
                         origin_dt->name, target, (int) target_disp,
                         target_count, target_dt->name, op->o_name,
                         win->w_name));

    OMPI_OSC_PT2PT_REQUEST_ALLOC(win, pt2pt_request);
    if (NULL == pt2pt_request) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* short-circuit case */
    if (0 == origin_count || 0 == target_count) {
        ompi_osc_pt2pt_request_complete (pt2pt_request, MPI_SUCCESS);
        *request = (ompi_request_t *) pt2pt_request;
        return OMPI_SUCCESS;
    }

    pt2pt_request->type = OMPI_OSC_PT2PT_HDR_TYPE_ACC;

    ret = ompi_osc_pt2pt_accumulate_w_req (origin_addr, origin_count, origin_dt, target,
                                          target_disp, target_count, target_dt, op, win,
                                          pt2pt_request);
    if (OMPI_SUCCESS != ret) {
        OMPI_OSC_PT2PT_REQUEST_RETURN(pt2pt_request);
        return ret;
    }

    *request = (ompi_request_t *) pt2pt_request;

    return OMPI_SUCCESS;
}


static inline
int ompi_osc_pt2pt_rget_accumulate_internal (void *origin_addr, int origin_count,
                                            struct ompi_datatype_t *origin_datatype,
                                            void *result_addr, int result_count,
                                            struct ompi_datatype_t *result_datatype,
                                            int target_rank, MPI_Aint target_disp,
                                            int target_count, struct ompi_datatype_t *target_datatype,
                                            struct ompi_op_t *op, struct ompi_win_t *win,
                                            bool release_req, struct ompi_request_t **request)
{
    int ret;
    ompi_osc_pt2pt_module_t *module = GET_MODULE(win);
    ompi_proc_t *proc = ompi_comm_peer_lookup(module->comm, target_rank);
    bool is_long_datatype = false;
    bool is_long_msg = false;
    ompi_osc_pt2pt_frag_t *frag;
    ompi_osc_pt2pt_header_acc_t *header;
    size_t ddt_len, payload_len, frag_len;
    char *ptr;
    const void *packed_ddt;
    int tag;
    ompi_osc_pt2pt_request_t *pt2pt_request;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "rget_acc: 0x%lx, %d, %s, 0x%lx, %d, %s, 0x%x, %d, %d, %s, %s, %s",
                         (unsigned long) origin_addr, origin_count, origin_datatype->name,
                         (unsigned long) result_addr, result_count, result_datatype->name,
                         target_rank, (int) target_disp, target_count, target_datatype->name,
                         op->o_name, win->w_name));

    if (!ompi_osc_pt2pt_check_access_epoch (module, target_rank)) {
        return OMPI_ERR_RMA_SYNC;
    }

    /* get_accumulates are always request based, so that we know where to land the data */
    OMPI_OSC_PT2PT_REQUEST_ALLOC(win, pt2pt_request);
    if (OPAL_UNLIKELY(NULL == pt2pt_request)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    pt2pt_request->internal = release_req;

    /* short-circuit case. note that origin_count may be 0 if op is MPI_NO_OP */
    if (0 == result_count || 0 == target_count) {
        ompi_osc_pt2pt_request_complete (pt2pt_request, MPI_SUCCESS);
        *request = &pt2pt_request->super;
        return OMPI_SUCCESS;
    }

    /* optimize the self case. TODO: optimize the local case */
    if (ompi_comm_rank (module->comm) == target_rank) {
        *request = &pt2pt_request->super;
        return ompi_osc_pt2pt_gacc_self (origin_addr, origin_count, origin_datatype,
                                        result_addr, result_count, result_datatype,
                                        target_disp, target_count, target_datatype,
                                        op, module, pt2pt_request);
    }

    pt2pt_request->type = OMPI_OSC_PT2PT_HDR_TYPE_GET_ACC;
    pt2pt_request->origin_addr = origin_addr;
    pt2pt_request->origin_count = origin_count;
    OBJ_RETAIN(origin_datatype);
    pt2pt_request->origin_dt = origin_datatype;

    /* Compute datatype and payload lengths.  Note that the datatype description
     * must fit in a single frag */
    ddt_len = ompi_datatype_pack_description_length(target_datatype);

    if (&ompi_mpi_op_no_op.op != op) {
        payload_len = origin_datatype->super.size * origin_count;
    } else {
        payload_len = 0;
    }

    frag_len = sizeof(*header) + ddt_len + payload_len;
    ret = ompi_osc_pt2pt_frag_alloc(module, target_rank, frag_len, &frag, &ptr);
    if (OMPI_SUCCESS != ret) {
        frag_len = sizeof(*header) + ddt_len;
        ret = ompi_osc_pt2pt_frag_alloc(module, target_rank, frag_len, &frag, &ptr);
        if (OMPI_SUCCESS != ret) {
            /* allocate space for the header plus space to store ddt_len */
            frag_len = sizeof(*header) + 8;
            ret = ompi_osc_pt2pt_frag_alloc(module, target_rank, frag_len, &frag, &ptr);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            is_long_datatype = true;
        }

        is_long_msg = true;
    }

    tag = get_tag (module);

    /* If this is a long message then we need two completions before the
     * request is complete (1 for the send, 1 for the receive) */
    pt2pt_request->outstanding_requests = 1 + is_long_msg;

    /* increment the number of outgoing fragments */
    ompi_osc_signal_outgoing (module, target_rank, pt2pt_request->outstanding_requests);

    /* flush will be called at the end of this function. make sure the post message has
     * arrived. */
    if (!release_req && module->sc_group) {
        OPAL_THREAD_LOCK(&module->lock);
        while (0 != module->num_post_msgs) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "waiting for post messages. num_post_msgs = %d", module->num_post_msgs));
            opal_condition_wait(&module->cond, &module->lock);
        }
        OPAL_THREAD_UNLOCK(&module->lock);
    }

    header = (ompi_osc_pt2pt_header_acc_t *) ptr;
    header->base.flags = 0;
    header->len = frag_len;
    header->count = target_count;
    header->displacement = target_disp;
    header->op = op->o_f_to_c_index;
    header->tag = tag;
    ptr = (char *)(header + 1);

    do {
        ret = ompi_datatype_get_pack_description(target_datatype, &packed_ddt);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        if (is_long_datatype) {
            /* the datatype does not fit in an eager message. send it seperately */
            header->base.flags |= OMPI_OSC_PT2PT_HDR_FLAG_LARGE_DATATYPE;

            OBJ_RETAIN(target_datatype);

            ret = ompi_osc_pt2pt_isend_w_cb ((void *) packed_ddt, ddt_len, MPI_BYTE, target_rank,
                                            tag, module->comm, ompi_osc_pt2pt_dt_send_complete,
                                            target_datatype);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }

            *((uint64_t *) ptr) = ddt_len;
            ptr += 8;
        } else {
            memcpy((unsigned char*) ptr, packed_ddt, ddt_len);
            ptr += ddt_len;
        }

        ret = ompi_osc_pt2pt_irecv_w_cb (result_addr, result_count, result_datatype, target_rank, tag,
                                        module->comm, NULL, ompi_osc_pt2pt_req_comm_complete, pt2pt_request);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }

        if (!is_long_msg) {
            header->base.type = OMPI_OSC_PT2PT_HDR_TYPE_GET_ACC;

            if (&ompi_mpi_op_no_op.op != op) {
                osc_pt2pt_copy_for_send (ptr, payload_len, origin_addr, proc, origin_count,
                                        origin_datatype);
            }
        } else {
            header->base.type = OMPI_OSC_PT2PT_HDR_TYPE_GET_ACC_LONG;

            ret = ompi_osc_pt2pt_isend_w_cb (origin_addr, origin_count, origin_datatype, target_rank,
                                            tag, module->comm, ompi_osc_pt2pt_req_comm_complete, pt2pt_request);
        }
    } while (0);

    if (OMPI_SUCCESS == ret) {
        header->base.flags |= OMPI_OSC_PT2PT_HDR_FLAG_VALID;
        *request = (ompi_request_t *) pt2pt_request;
    }

    ret = ompi_osc_pt2pt_frag_finish(module, frag);

    if (!release_req) {
        /* need to flush now in case the caller decides to wait on the request */
        ompi_osc_pt2pt_frag_flush_target (module, target_rank);
    }

    return ret;
}

int ompi_osc_pt2pt_get_accumulate(void *origin_addr, int origin_count,
                                 struct ompi_datatype_t *origin_dt,
                                 void *result_addr, int result_count,
                                 struct ompi_datatype_t *result_dt,
                                 int target, MPI_Aint target_disp,
                                 int target_count, struct ompi_datatype_t *target_dt,
                                 struct ompi_op_t *op, struct ompi_win_t *win)
{
    ompi_request_t *request;

    return ompi_osc_pt2pt_rget_accumulate_internal (origin_addr, origin_count, origin_dt,
                                                   result_addr, result_count, result_dt,
                                                   target, target_disp, target_count,
                                                   target_dt, op, win, true, &request);
}


int ompi_osc_pt2pt_rget_accumulate(void *origin_addr, int origin_count,
                                  struct ompi_datatype_t *origin_dt,
                                  void *result_addr, int result_count,
                                  struct ompi_datatype_t *result_dt,
                                  int target, MPI_Aint target_disp,
                                  int target_count, struct ompi_datatype_t *target_dt,
                                  struct ompi_op_t *op, struct ompi_win_t *win,
                                  ompi_request_t **request)
{
    return ompi_osc_pt2pt_rget_accumulate_internal (origin_addr, origin_count, origin_dt,
                                                   result_addr, result_count, result_dt,
                                                   target, target_disp, target_count,
                                                   target_dt, op, win, false, request);
}

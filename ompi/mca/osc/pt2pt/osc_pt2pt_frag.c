/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/pml/pml.h"

#include "osc_pt2pt.h"
#include "osc_pt2pt_frag.h"
#include "osc_pt2pt_data_move.h"

static void ompi_osc_pt2pt_frag_constructor (ompi_osc_pt2pt_frag_t *frag){
    frag->buffer = frag->super.ptr;
}

OBJ_CLASS_INSTANCE(ompi_osc_pt2pt_frag_t, ompi_free_list_item_t,
                   ompi_osc_pt2pt_frag_constructor, NULL);

static int frag_send_cb (ompi_request_t *request)
{
    ompi_osc_pt2pt_frag_t *frag =
        (ompi_osc_pt2pt_frag_t*) request->req_complete_cb_data;
    ompi_osc_pt2pt_module_t *module = frag->module;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag_send complete to %d, frag = %p, request = %p",
                         frag->target, (void *) frag, (void *) request));

    mark_outgoing_completion(module);
    OMPI_FREE_LIST_RETURN_MT(&mca_osc_pt2pt_component.frags, &frag->super);


    /* put this request on the garbage colletion list */
    osc_pt2pt_gc_add_request (module, request);

    return OMPI_SUCCESS;
}

static int
frag_send(ompi_osc_pt2pt_module_t *module,
            ompi_osc_pt2pt_frag_t *frag)
{
    int count;

    count = (int)((uintptr_t) frag->top - (uintptr_t) frag->buffer);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag_send called to %d, frag = %p, count = %d",
                         frag->target, (void *) frag, count));

    /* we need to signal now that a frag is outgoing to ensure the count sent
     * with the unlock message is correct */
    ompi_osc_signal_outgoing (module, frag->target, 1);

    return ompi_osc_pt2pt_isend_w_cb (frag->buffer, count, MPI_BYTE, frag->target, OSC_PT2PT_FRAG_TAG,
                                     module->comm, frag_send_cb, frag);
}


int
ompi_osc_pt2pt_frag_start(ompi_osc_pt2pt_module_t *module,
                          ompi_osc_pt2pt_frag_t *frag)
{
    ompi_osc_pt2pt_peer_t *peer = module->peers + frag->target;
    int ret;

    assert(0 == frag->pending && peer->active_frag != frag);

    /* if eager sends are not active, can't send yet, so buffer and
       get out... */
    if (!(peer->eager_send_active || module->all_access_epoch)) {
        OPAL_THREAD_SCOPED_LOCK(&module->queued_frags_lock,
                                opal_list_append(&module->queued_frags, (opal_list_item_t *) frag));
        return OMPI_SUCCESS;
    }

    ret = frag_send(module, frag);

    opal_condition_broadcast(&module->cond);

    return ret;
}


int
ompi_osc_pt2pt_frag_flush_target(ompi_osc_pt2pt_module_t *module, int target)
{
    ompi_osc_pt2pt_frag_t *next, *frag = module->peers[target].active_frag;
    int ret = OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush target begin"));

    /* flush the active frag */
    if (NULL != frag) {
        if (1 != frag->pending) {
            /* communication going on while synchronizing; this is an rma usage bug */
            return OMPI_ERR_RMA_SYNC;
        }

        if (opal_atomic_cmpset (&module->peers[target].active_frag, frag, NULL)) {
            OPAL_THREAD_ADD32(&frag->pending, -1);
            ret = ompi_osc_pt2pt_frag_start(module, frag);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush target finished active frag"));

    /* walk through the pending list and send */
    OPAL_THREAD_LOCK(&module->queued_frags_lock);
    if (opal_list_get_size (&module->queued_frags)) {
        OPAL_LIST_FOREACH_SAFE(frag, next, &module->queued_frags, ompi_osc_pt2pt_frag_t) {
            if (frag->target == target) {
                opal_list_remove_item(&module->queued_frags, (opal_list_item_t *) frag);
                ret = frag_send(module, frag);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != frag)) {
                    break;
                }
            }
        }
    }
    OPAL_THREAD_UNLOCK(&module->queued_frags_lock);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush target finished"));

    return ret;
}


int
ompi_osc_pt2pt_frag_flush_all(ompi_osc_pt2pt_module_t *module)
{
    int ret = OMPI_SUCCESS;
    int i;
    ompi_osc_pt2pt_frag_t *frag, *next;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush all begin"));

    /* flush the active frag */
    for (i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
        ompi_osc_pt2pt_frag_t *frag = module->peers[i].active_frag;

        if (NULL != frag) {
            if (1 != frag->pending) {
                OPAL_THREAD_UNLOCK(&module->lock);
                /* communication going on while synchronizing; this is a bug */
                return OMPI_ERR_RMA_SYNC;
            }

            if (!opal_atomic_cmpset_ptr (&module->peers[i].active_frag, frag, NULL)) {
                continue;
            }

            OPAL_THREAD_ADD32(&frag->pending, -1);
            ret = ompi_osc_pt2pt_frag_start(module, frag);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush all finished active frag"));

    /* try to start all the queued frags */
    OPAL_THREAD_LOCK(&module->queued_frags_lock);
    if (opal_list_get_size (&module->queued_frags)) {
        OPAL_LIST_FOREACH_SAFE(frag, next, &module->queued_frags, ompi_osc_pt2pt_frag_t) {
            opal_list_remove_item(&module->queued_frags, (opal_list_item_t *) frag);
            ret = frag_send(module, frag);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }
        }
    }
    OPAL_THREAD_UNLOCK(&module->queued_frags_lock);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush all done"));

    return ret;
}

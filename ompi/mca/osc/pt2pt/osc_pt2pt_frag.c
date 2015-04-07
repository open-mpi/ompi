/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014-2015 Los Alamos National Security, LLC. All rights
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

OBJ_CLASS_INSTANCE(ompi_osc_pt2pt_frag_t, opal_free_list_item_t,
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
    opal_free_list_return (&mca_osc_pt2pt_component.frags, &frag->super);


    /* put this request on the garbage colletion list */
    osc_pt2pt_gc_add_request (module, request);

    return OMPI_SUCCESS;
}

static int frag_send (ompi_osc_pt2pt_module_t *module, ompi_osc_pt2pt_frag_t *frag)
{
    int count;

    count = (int)((uintptr_t) frag->top - (uintptr_t) frag->buffer);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag_send called to %d, frag = %p, count = %d",
                         frag->target, (void *) frag, count));

    return ompi_osc_pt2pt_isend_w_cb (frag->buffer, count, MPI_BYTE, frag->target, OSC_PT2PT_FRAG_TAG,
                                     module->comm, frag_send_cb, frag);
}


int ompi_osc_pt2pt_frag_start (ompi_osc_pt2pt_module_t *module,
                               ompi_osc_pt2pt_frag_t *frag)
{
    ompi_osc_pt2pt_peer_t *peer = module->peers + frag->target;
    int ret;

    assert(0 == frag->pending && peer->active_frag != frag);

    /* we need to signal now that a frag is outgoing to ensure the count sent
     * with the unlock message is correct */
    ompi_osc_signal_outgoing (module, frag->target, 1);

    /* if eager sends are not active, can't send yet, so buffer and
       get out... */
    if (!(peer->eager_send_active || module->all_access_epoch) || opal_list_get_size (&peer->queued_frags)) {
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output, "queuing fragment to peer %d",
                             frag->target));
        OPAL_THREAD_SCOPED_LOCK(&peer->lock,
                                opal_list_append(&peer->queued_frags, (opal_list_item_t *) frag));
        return OMPI_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output, "sending fragment to peer %d",
                         frag->target));

    ret = frag_send(module, frag);

    opal_condition_broadcast(&module->cond);

    return ret;
}

static int ompi_osc_pt2pt_flush_active_frag (ompi_osc_pt2pt_module_t *module, int target)
{
    ompi_osc_pt2pt_frag_t *active_frag = module->peers[target].active_frag;
    int ret = OMPI_SUCCESS;

    if (NULL == active_frag) {
        /* nothing to do */
        return OMPI_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: flushing active fragment to target. pending: %d", target,
                         active_frag->pending));

    if (opal_atomic_cmpset (&module->peers[target].active_frag, active_frag, NULL)) {
        if (0 != OPAL_THREAD_ADD32(&active_frag->pending, -1)) {
            /* communication going on while synchronizing; this is an rma usage bug */
            return OMPI_ERR_RMA_SYNC;
        }

        ompi_osc_signal_outgoing (module, target, 1);
        ret = frag_send (module, active_frag);
    }

    return ret;
}

int ompi_osc_pt2pt_frag_flush_target (ompi_osc_pt2pt_module_t *module, int target)
{
    ompi_osc_pt2pt_peer_t *peer = module->peers + target;
    ompi_osc_pt2pt_frag_t *next, *frag;
    int ret = OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush to target target %d. queue fragments: %u",
                         target, opal_list_get_size (&peer->queued_frags)));

    /* walk through the pending list and send */
    OPAL_THREAD_LOCK(&peer->lock);
    while (NULL != (frag = ((ompi_osc_pt2pt_frag_t *) opal_list_remove_first (&peer->queued_frags)))) {
        ret = frag_send(module, frag);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&peer->lock);

    /* XXX -- TODO -- better error handling */
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* flush the active frag */
    ret = ompi_osc_pt2pt_flush_active_frag (module, target);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush target %d finished", target));

    return ret;
}


int ompi_osc_pt2pt_frag_flush_all (ompi_osc_pt2pt_module_t *module)
{
    int ret = OMPI_SUCCESS;
    ompi_osc_pt2pt_frag_t *frag, *next;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush all begin"));

    /* try to start all the queued frags */
    for (int i = 0 ; i < ompi_comm_size (module->comm) ; ++i) {
        ompi_osc_pt2pt_peer_t *peer = module->peers + i;

        while (NULL != (frag = ((ompi_osc_pt2pt_frag_t *) opal_list_remove_first (&peer->queued_frags)))) {
            ret = frag_send(module, frag);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }
        }

        /* XXX -- TODO -- better error handling */
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: flushing all active fragments"));

    /* flush the active frag */
    for (int i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
        ret = ompi_osc_pt2pt_flush_active_frag (module, i);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc pt2pt: frag flush all done"));

    return ret;
}

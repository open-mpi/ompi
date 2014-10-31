/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
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

#include "osc_rdma.h"
#include "osc_rdma_frag.h"
#include "osc_rdma_data_move.h"

static void ompi_osc_rdma_frag_constructor (ompi_osc_rdma_frag_t *frag){
    frag->buffer = malloc (mca_osc_rdma_component.buffer_size + sizeof (ompi_osc_rdma_frag_header_t));
    assert (frag->buffer);
}

static void ompi_osc_rdma_frag_destructor (ompi_osc_rdma_frag_t *frag) {
    if (NULL != frag->buffer) {
        free (frag->buffer);
    }
}

OBJ_CLASS_INSTANCE(ompi_osc_rdma_frag_t, opal_list_item_t,
                   ompi_osc_rdma_frag_constructor, ompi_osc_rdma_frag_destructor);

static int frag_send_cb (ompi_request_t *request)
{
    ompi_osc_rdma_frag_t *frag =
        (ompi_osc_rdma_frag_t*) request->req_complete_cb_data;
    ompi_osc_rdma_module_t *module = frag->module;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: frag_send complete to %d, frag = %p, request = %p",
                         frag->target, (void *) frag, (void *) request));

    mark_outgoing_completion(module);
    OPAL_FREE_LIST_RETURN(&mca_osc_rdma_component.frags, &frag->super);


    /* put this request on the garbage colletion list */
    osc_rdma_gc_add_request (request);

    return OMPI_SUCCESS;
}

static int
frag_send(ompi_osc_rdma_module_t *module,
            ompi_osc_rdma_frag_t *frag)
{
    int count;

    count = (int)((uintptr_t) frag->top - (uintptr_t) frag->buffer);

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: frag_send called to %d, frag = %p, count = %d",
                         frag->target, (void *) frag, count));

    return ompi_osc_rdma_isend_w_cb (frag->buffer, count, MPI_BYTE, frag->target, OSC_RDMA_FRAG_TAG,
                                     module->comm, frag_send_cb, frag);
}


int
ompi_osc_rdma_frag_start(ompi_osc_rdma_module_t *module,
                        ompi_osc_rdma_frag_t *frag)
{
    int ret;

    assert(0 == frag->pending);
    assert(module->peers[frag->target].active_frag != frag);

    /* we need to signal now that a frag is outgoing to ensure the count sent
     * with the unlock message is correct */
    ompi_osc_signal_outgoing (module, frag->target, 1);

    /* if eager sends are not active, can't send yet, so buffer and
       get out... */
    if (module->passive_target_access_epoch) {
        if (!module->passive_eager_send_active[frag->target]) {
            opal_list_append(&module->queued_frags, &frag->super);
            return OMPI_SUCCESS;
        }
    } else {
        if (!module->active_eager_send_active) {
            opal_list_append(&module->queued_frags, &frag->super);
            return OMPI_SUCCESS;
        }
    }

    ret = frag_send(module, frag);

    opal_condition_broadcast(&module->cond);

    return ret;
}


int
ompi_osc_rdma_frag_flush_target(ompi_osc_rdma_module_t *module, int target)
{
    int ret = OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: frag flush target begin"));

    /* flush the active frag */
    if (NULL != module->peers[target].active_frag) {
        ompi_osc_rdma_frag_t *frag = module->peers[target].active_frag;

        if (0 != frag->pending) {
            /* communication going on while synchronizing; this is a bug */
            return OMPI_ERR_RMA_SYNC;
        }

        module->peers[target].active_frag = NULL;

        ret = ompi_osc_rdma_frag_start(module, frag);
        if (OMPI_SUCCESS != ret) return ret;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: frag flush target finished active frag"));

    /* walk through the pending list and send */
    ompi_osc_rdma_frag_t *frag, *next;
    OPAL_LIST_FOREACH_SAFE(frag, next, &module->queued_frags, ompi_osc_rdma_frag_t) {
        if (frag->target == target) {
            opal_list_remove_item(&module->queued_frags, &frag->super);
            ret = frag_send(module, frag);
            if (OMPI_SUCCESS != ret) return ret;
        }
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: frag flush target finished"));

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_frag_flush_all(ompi_osc_rdma_module_t *module)
{
    int ret = OMPI_SUCCESS;
    int i;
    ompi_osc_rdma_frag_t *frag, *next;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: frag flush all begin"));

    /* flush the active frag */
    for (i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
        if (NULL != module->peers[i].active_frag) {
            ompi_osc_rdma_frag_t *frag = module->peers[i].active_frag;

            if (0 != frag->pending) {
                /* communication going on while synchronizing; this is a bug */
                return OMPI_ERR_RMA_SYNC;
            }

            module->peers[i].active_frag = NULL;

            ret = ompi_osc_rdma_frag_start(module, frag);
            if (OMPI_SUCCESS != ret) return ret;
        }
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: frag flush all finished active frag"));

    /* try to start all the queued frags */
    OPAL_LIST_FOREACH_SAFE(frag, next, &module->queued_frags, ompi_osc_rdma_frag_t) {
        opal_list_remove_item(&module->queued_frags, &frag->super);
        ret = frag_send(module, frag);
        if (OMPI_SUCCESS != ret) {
            OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                                 "osc rdma: failure for frag send: %d", ret));
            return ret;
        }
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_framework.framework_output,
                         "osc rdma: frag flush all done"));

    return OMPI_SUCCESS;
}

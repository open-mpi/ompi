/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"

#include "pml_ob1_events.h"

#include "opal/mca/base/mca_base_pvar.h"
#include "opal/mca/base/mca_base_var.h"

mca_base_event_t *mca_pml_ob1_event[MCA_PML_OB1_EVENT_MAX];

/* Nanosecond ticks, matching the "ompi" source registered by the MPI layer
   in ompi_mpit_register_events.c.  Source registration is idempotent by
   name, so it does not matter whether ob1 or the MPI layer gets there first
   -- both end up describing the same clock. */
#define OB1_TICKS_PER_SECOND ((opal_count_t) 1000000000)

/* The payload layouts from pml_ob1_events.h, as the (type, offset) element
   arrays the event framework describes a payload with.  The framework has no
   notion of element names, so the field order here is the tool-visible order
   and must stay in step with the structs. */
static const mca_base_var_type_t request_types[] = {MCA_BASE_VAR_TYPE_UINT64_T};
static const ptrdiff_t request_offsets[] = {offsetof(mca_pml_ob1_request_event_t, request)};

static const mca_base_var_type_t transfer_types[] = {MCA_BASE_VAR_TYPE_UINT64_T,
                                                     MCA_BASE_VAR_TYPE_INT64_T};
static const ptrdiff_t transfer_offsets[] = {offsetof(mca_pml_ob1_transfer_event_t, request),
                                             offsetof(mca_pml_ob1_transfer_event_t, length)};

static const mca_base_var_type_t message_types[] = {MCA_BASE_VAR_TYPE_INT32_T,
                                                    MCA_BASE_VAR_TYPE_INT32_T,
                                                    MCA_BASE_VAR_TYPE_INT32_T,
                                                    MCA_BASE_VAR_TYPE_INT32_T};
static const ptrdiff_t message_offsets[] = {offsetof(mca_pml_ob1_message_event_t, source),
                                            offsetof(mca_pml_ob1_message_event_t, tag),
                                            offsetof(mca_pml_ob1_message_event_t, context_id),
                                            offsetof(mca_pml_ob1_message_event_t, sequence)};

struct ob1_event_desc_t {
    int which;
    const char *name;
    const char *description;
    int num_elements;
    const mca_base_var_type_t *types;
    const ptrdiff_t *offsets;
};

#define OB1_REQUEST_PAYLOAD 1, request_types, request_offsets
#define OB1_TRANSFER_PAYLOAD 2, transfer_types, transfer_offsets
#define OB1_MESSAGE_PAYLOAD 4, message_types, message_offsets

static const struct ob1_event_desc_t ob1_events[] = {
    {MCA_PML_OB1_EVENT_MESSAGE_ARRIVED, "message_arrived",
     "A message header for this communicator arrived from a peer", OB1_MESSAGE_PAYLOAD},

    {MCA_PML_OB1_EVENT_SEARCH_POSTED_BEGIN, "search_posted_begin",
     "Started searching the posted-receive queue for a match", OB1_MESSAGE_PAYLOAD},
    {MCA_PML_OB1_EVENT_SEARCH_POSTED_END, "search_posted_end",
     "Finished searching the posted-receive queue", OB1_MESSAGE_PAYLOAD},
    {MCA_PML_OB1_EVENT_SEARCH_UNEX_BEGIN, "search_unexpected_begin",
     "Started searching the unexpected-message queue for a match", OB1_REQUEST_PAYLOAD},
    {MCA_PML_OB1_EVENT_SEARCH_UNEX_END, "search_unexpected_end",
     "Finished searching the unexpected-message queue", OB1_REQUEST_PAYLOAD},

    {MCA_PML_OB1_EVENT_POSTED_INSERT, "posted_insert",
     "A receive request was inserted into the posted-receive queue", OB1_REQUEST_PAYLOAD},
    {MCA_PML_OB1_EVENT_POSTED_REMOVE, "posted_remove",
     "A receive request was removed from the posted-receive queue", OB1_REQUEST_PAYLOAD},

    {MCA_PML_OB1_EVENT_UNEX_INSERT, "unexpected_insert",
     "An unmatched message was inserted into the unexpected-message queue", OB1_MESSAGE_PAYLOAD},

    {MCA_PML_OB1_EVENT_TRANSFER_BEGIN, "transfer_begin",
     "Data movement for a request started", OB1_TRANSFER_PAYLOAD},
    {MCA_PML_OB1_EVENT_TRANSFER, "transfer",
     "A further fragment or RDMA step of a request's data movement", OB1_TRANSFER_PAYLOAD},
    {MCA_PML_OB1_EVENT_TRANSFER_END, "transfer_end", "Data movement for a request finished",
     OB1_TRANSFER_PAYLOAD},

    {MCA_PML_OB1_EVENT_REQUEST_ACTIVATE, "request_activate",
     "A point-to-point request was activated", OB1_REQUEST_PAYLOAD},
    {MCA_PML_OB1_EVENT_REQUEST_COMPLETE, "request_complete",
     "A point-to-point request completed", OB1_REQUEST_PAYLOAD},
    {MCA_PML_OB1_EVENT_REQUEST_FREE, "request_free",
     "The user released a point-to-point request handle (MPI_Request_free, or the "
     "implicit free when a nonblocking request completes). Marks the end of the "
     "handle's user-visible lifetime; it does not imply the transfer finished (an "
     "active request may be freed) and is not raised for blocking calls, which "
     "expose no request handle.", OB1_REQUEST_PAYLOAD},
    {MCA_PML_OB1_EVENT_RECEIVE_CANCEL, "receive_cancel", "A receive request was cancelled",
     OB1_REQUEST_PAYLOAD},
};

void mca_pml_ob1_events_register(const mca_base_component_t *component)
{
    mca_base_event_source_t *source = NULL;
    int source_index;
    size_t i;

    source_index = mca_base_event_source_register("ompi", "Open MPI runtime events (ordered)",
                                                  MCA_BASE_EVENT_SOURCE_ORDERED,
                                                  OB1_TICKS_PER_SECOND, OPAL_COUNT_MAX, NULL, true,
                                                  NULL);
    if (0 > source_index
        || OPAL_SUCCESS != mca_base_event_source_get_by_index(source_index, &source)) {
        /* No clock to stamp events with, so there is nothing to register
           against; every helper then sees a NULL event and stays quiet. */
        return;
    }

    for (i = 0; i < sizeof(ob1_events) / sizeof(ob1_events[0]); ++i) {
        const struct ob1_event_desc_t *desc = &ob1_events[i];
        int event_index;

        event_index = mca_base_component_event_register(component, desc->name, desc->description,
                                                        OPAL_INFO_LVL_4, desc->num_elements,
                                                        desc->types, desc->offsets, NULL,
                                                        MCA_BASE_VAR_BIND_MPI_COMM, 0, source,
                                                        NULL);
        if (0 > event_index) {
            continue;
        }

        (void) mca_base_event_get_by_index(event_index, &mca_pml_ob1_event[desc->which]);
    }
}

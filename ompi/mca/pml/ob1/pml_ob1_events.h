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

/*
 * MPI_T event types raised by ob1's matching engine.
 *
 * These are the point-to-point producers from open-mpi/ompi#13133, ported
 * onto the event framework that landed in #14083.  Between them they cover
 * what PERUSE reported: the arrival of a message, the two message queues
 * and the cost of searching them, the stages of a transfer, and the
 * lifecycle of a request.  Every event is bound to MPI_T_BIND_MPI_COMM, so
 * a tool is notified only for the communicator it registered.
 *
 * The request element in a payload is the request's address as an opaque
 * uint64 correlator -- it identifies one message across its events (so a
 * tool can measure match latency, queue dwell and wire time for a single
 * message) and must never be dereferenced.
 *
 * Raising must stay free on the critical path.  Each helper below reads the
 * event's listener gate first and returns before touching the payload, so
 * an unobserved event costs one load and a predicted branch -- the same
 * shape as the PERUSE handle check it sits beside.
 */

#ifndef MCA_PML_OB1_EVENTS_H
#define MCA_PML_OB1_EVENTS_H

#include "ompi_config.h"

#include <assert.h>

#include "opal/mca/base/mca_base_event.h"

BEGIN_C_DECLS

struct ompi_communicator_t;

enum {
    /* A message header came off the wire. */
    MCA_PML_OB1_EVENT_MESSAGE_ARRIVED,

    /* Walking the posted-receive queue to match an arriving message.  The
       begin/end pair is the cost of one match attempt. */
    MCA_PML_OB1_EVENT_SEARCH_POSTED_BEGIN,
    MCA_PML_OB1_EVENT_SEARCH_POSTED_END,

    /* Walking the unexpected queue on behalf of a newly posted receive. */
    MCA_PML_OB1_EVENT_SEARCH_UNEX_BEGIN,
    MCA_PML_OB1_EVENT_SEARCH_UNEX_END,

    /* Posted-receive queue residency. */
    MCA_PML_OB1_EVENT_POSTED_INSERT,
    MCA_PML_OB1_EVENT_POSTED_REMOVE,

    /* Unexpected queue residency. */
    MCA_PML_OB1_EVENT_UNEX_INSERT,

    /* Data movement for one request: begin, each further fragment or RDMA
       step, end. */
    MCA_PML_OB1_EVENT_TRANSFER_BEGIN,
    MCA_PML_OB1_EVENT_TRANSFER,
    MCA_PML_OB1_EVENT_TRANSFER_END,

    /* Request lifecycle. */
    MCA_PML_OB1_EVENT_REQUEST_ACTIVATE,
    MCA_PML_OB1_EVENT_REQUEST_COMPLETE,
    /* REQUEST_FREE marks the end of a request handle's user-visible lifetime
       (MPI_Request_free, or the implicit free when a nonblocking request
       completes) -- not the internal return to the free list, which may happen
       later for a still-active freed request and never happens for blocking
       calls that expose no handle. */
    MCA_PML_OB1_EVENT_REQUEST_FREE,
    MCA_PML_OB1_EVENT_RECEIVE_CANCEL,

    MCA_PML_OB1_EVENT_MAX,
};

/* One request's progress. */
struct mca_pml_ob1_request_event_t {
    uint64_t request;
};
typedef struct mca_pml_ob1_request_event_t mca_pml_ob1_request_event_t;

/* One request's progress, plus the bytes this step moves. */
struct mca_pml_ob1_transfer_event_t {
    uint64_t request;
    int64_t length;
};
typedef struct mca_pml_ob1_transfer_event_t mca_pml_ob1_transfer_event_t;

/* A message identified by its envelope rather than by a request -- used
   where ob1 has a wire header but no request yet. */
struct mca_pml_ob1_message_event_t {
    int32_t source;
    int32_t tag;
    int32_t context_id;
    int32_t sequence;
};
typedef struct mca_pml_ob1_message_event_t mca_pml_ob1_message_event_t;

/* Indexed by MCA_PML_OB1_EVENT_*; an entry is NULL if that event type failed
   to register (or the event framework is unavailable), which the helpers
   below treat exactly like "nobody is listening". */
extern mca_base_event_t *mca_pml_ob1_event[MCA_PML_OB1_EVENT_MAX];

/* Called from the component's register entry point. */
void mca_pml_ob1_events_register(const mca_base_component_t *component);

static inline bool mca_pml_ob1_event_wanted(int which)
{
    /* which is always a compile-time MCA_PML_OB1_EVENT_* enumerator, so it is
       in range by construction; assert it in debug builds (compiled out under
       NDEBUG) rather than paying for a runtime check on this hot gate. */
    assert(which >= 0 && which < MCA_PML_OB1_EVENT_MAX);
    const mca_base_event_t *event = mca_pml_ob1_event[which];

    return NULL != event && mca_base_event_active(event);
}

static inline void mca_pml_ob1_event_raise_request(int which, struct ompi_communicator_t *comm,
                                                   const void *request)
{
    if (OPAL_LIKELY(!mca_pml_ob1_event_wanted(which))) {
        return;
    }

    mca_pml_ob1_request_event_t payload = {.request = (uint64_t) (uintptr_t) request};
    mca_base_event_raise_bound(mca_pml_ob1_event[which], NULL, comm, &payload);
}

static inline void mca_pml_ob1_event_raise_transfer(int which, struct ompi_communicator_t *comm,
                                                    const void *request, size_t length)
{
    if (OPAL_LIKELY(!mca_pml_ob1_event_wanted(which))) {
        return;
    }

    mca_pml_ob1_transfer_event_t payload = {.request = (uint64_t) (uintptr_t) request,
                                            .length = (int64_t) length};
    mca_base_event_raise_bound(mca_pml_ob1_event[which], NULL, comm, &payload);
}

static inline void mca_pml_ob1_event_raise_message(int which, struct ompi_communicator_t *comm,
                                                   int32_t source, int32_t tag,
                                                   int32_t context_id, int32_t sequence)
{
    if (OPAL_LIKELY(!mca_pml_ob1_event_wanted(which))) {
        return;
    }

    mca_pml_ob1_message_event_t payload = {.source = source, .tag = tag,
                                           .context_id = context_id, .sequence = sequence};
    mca_base_event_raise_bound(mca_pml_ob1_event[which], NULL, comm, &payload);
}

END_C_DECLS

#endif /* MCA_PML_OB1_EVENTS_H */

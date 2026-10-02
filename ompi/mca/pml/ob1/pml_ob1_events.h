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

    /* Posted-receive queue residency, and the match that ends it. */
    MCA_PML_OB1_EVENT_POSTED_INSERT,
    MCA_PML_OB1_EVENT_POSTED_REMOVE,
    MCA_PML_OB1_EVENT_POSTED_MATCH,

    /* Unexpected queue residency, and the late receive that drains it. */
    MCA_PML_OB1_EVENT_UNEX_INSERT,
    MCA_PML_OB1_EVENT_UNEX_REMOVE,
    MCA_PML_OB1_EVENT_UNEX_MATCH,

    /* Data movement for one request: begin, each further fragment or RDMA
       step, end. */
    MCA_PML_OB1_EVENT_TRANSFER_BEGIN,
    MCA_PML_OB1_EVENT_TRANSFER,
    MCA_PML_OB1_EVENT_TRANSFER_END,

    /* Request lifecycle. */
    MCA_PML_OB1_EVENT_REQUEST_ACTIVATE,
    MCA_PML_OB1_EVENT_REQUEST_COMPLETE,
    MCA_PML_OB1_EVENT_REQUEST_FREE,
    MCA_PML_OB1_EVENT_RECEIVE_CANCEL,

    /* A send that completed inline and therefore never had a request.  See
       the comment on mca_pml_ob1_event_raise_immediate_send(). */
    MCA_PML_OB1_EVENT_IMMEDIATE_SEND,

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

/* A send with no request at all. */
struct mca_pml_ob1_immediate_event_t {
    int32_t peer;
    int32_t tag;
    int64_t length;
};
typedef struct mca_pml_ob1_immediate_event_t mca_pml_ob1_immediate_event_t;

/* Indexed by MCA_PML_OB1_EVENT_*; an entry is NULL if that event type failed
   to register (or the event framework is unavailable), which the helpers
   below treat exactly like "nobody is listening". */
extern mca_base_event_t *mca_pml_ob1_event[MCA_PML_OB1_EVENT_MAX];

/* Called from the component's register entry point. */
void mca_pml_ob1_events_register(const mca_base_component_t *component);

static inline bool mca_pml_ob1_event_wanted(int which)
{
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

/* The inline send path completes a short, non-synchronous send straight into
   the BTL and returns without ever allocating a request.  That is why PERUSE
   never reported these sends -- all of its send-side tracing hangs off a
   request -- and it is why this event carries the envelope and the byte count
   instead of a correlator: there is no request for a tool to correlate with.
   A tool that wants a complete send-side accounting needs this event plus
   MCA_PML_OB1_EVENT_REQUEST_ACTIVATE. */
static inline void mca_pml_ob1_event_raise_immediate_send(struct ompi_communicator_t *comm,
                                                          int32_t peer, int32_t tag, size_t length)
{
    if (OPAL_LIKELY(!mca_pml_ob1_event_wanted(MCA_PML_OB1_EVENT_IMMEDIATE_SEND))) {
        return;
    }

    mca_pml_ob1_immediate_event_t payload = {.peer = peer, .tag = tag,
                                             .length = (int64_t) length};
    mca_base_event_raise_bound(mca_pml_ob1_event[MCA_PML_OB1_EVENT_IMMEDIATE_SEND], NULL, comm,
                               &payload);
}

END_C_DECLS

#endif /* MCA_PML_OB1_EVENTS_H */

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
 * Two opaque uint64 correlators appear in these payloads, and neither may
 * ever be dereferenced.  The request element is a request's address: it ties
 * together the events of one request (activate, transfer, complete, ...).
 * The fragment element is the address of the unexpected-queue fragment: it
 * stays unique for as long as a message sits unmatched in the queue, so a
 * tool can pair an unexpected_insert with the unexpected_match that later
 * drains the same fragment -- measuring queue dwell from the two events'
 * timestamps and binding the message envelope (carried by the insert) to the
 * request that finally matched it (carried by the match).
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

    /* Posted-receive queue residency, and the match that ends it. */
    MCA_PML_OB1_EVENT_POSTED_INSERT,
    MCA_PML_OB1_EVENT_POSTED_REMOVE,
    MCA_PML_OB1_EVENT_POSTED_MATCH,

    /* Unexpected queue residency, and the late receive that drains it.  A
       fragment only ever leaves the queue by being matched, so the match is
       also the removal. */
    MCA_PML_OB1_EVENT_UNEX_INSERT,
    MCA_PML_OB1_EVENT_UNEX_MATCH,

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

/* A message entering the unexpected queue: its envelope, plus the address of
   the fragment that now holds it.  The fragment address is an opaque uint64
   token (never dereferenced) that stays unique while the message is queued, so
   it pairs this insert with the match that later drains the same fragment. */
struct mca_pml_ob1_unex_insert_event_t {
    uint64_t frag;
    int32_t source;
    int32_t tag;
    int32_t context_id;
    int32_t sequence;
};
typedef struct mca_pml_ob1_unex_insert_event_t mca_pml_ob1_unex_insert_event_t;

/* A late receive draining a message from the unexpected queue: the request
   doing the draining and the same fragment token its insert carried. */
struct mca_pml_ob1_unex_match_event_t {
    uint64_t request;
    uint64_t frag;
};
typedef struct mca_pml_ob1_unex_match_event_t mca_pml_ob1_unex_match_event_t;

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

/* unexpected_insert: the envelope (as mca_pml_ob1_event_raise_message) plus the
   fragment token that pairs this insert with its later match. */
static inline void mca_pml_ob1_event_raise_unex_insert(struct ompi_communicator_t *comm,
                                                       const void *frag, int32_t source,
                                                       int32_t tag, int32_t context_id,
                                                       int32_t sequence)
{
    if (OPAL_LIKELY(!mca_pml_ob1_event_wanted(MCA_PML_OB1_EVENT_UNEX_INSERT))) {
        return;
    }

    mca_pml_ob1_unex_insert_event_t payload = {.frag = (uint64_t) (uintptr_t) frag,
                                               .source = source, .tag = tag,
                                               .context_id = context_id, .sequence = sequence};
    mca_base_event_raise_bound(mca_pml_ob1_event[MCA_PML_OB1_EVENT_UNEX_INSERT], NULL, comm,
                               &payload);
}

/* unexpected_match: the draining request plus the same fragment token the
   matching unexpected_insert carried. */
static inline void mca_pml_ob1_event_raise_unex_match(struct ompi_communicator_t *comm,
                                                      const void *request, const void *frag)
{
    if (OPAL_LIKELY(!mca_pml_ob1_event_wanted(MCA_PML_OB1_EVENT_UNEX_MATCH))) {
        return;
    }

    mca_pml_ob1_unex_match_event_t payload = {.request = (uint64_t) (uintptr_t) request,
                                              .frag = (uint64_t) (uintptr_t) frag};
    mca_base_event_raise_bound(mca_pml_ob1_event[MCA_PML_OB1_EVENT_UNEX_MATCH], NULL, comm,
                               &payload);
}

END_C_DECLS

#endif /* MCA_PML_OB1_EVENTS_H */

/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * When what a peer published can be read.
 *
 * Every rank publishes what it takes to reach it -- BTL addresses,
 * locality, architecture, the PML it chose -- and reads the same about
 * every peer it wires up. Making those reads local is a collective fence
 * in one mode and a fetch per peer in the other; this layer tracks how far
 * along that is for a given peer. Its contract:
 *
 * 1. "Not yet" is distinct from "no": a miss reports OPAL_ERR_NOT_READY
 *    while the peer's data can still arrive. Conflating the two makes a
 *    BTL add_procs either declare a live peer unreachable or retry a dead
 *    one forever.
 * 2. Asking is what makes it true: where peers are fetched one at a time
 *    nothing runs in the background, so a caller that only re-asks each
 *    progress tick makes progress -- and one fetch per peer, however many
 *    ask, keeps that from issuing a Get per tick. The claim is one atomic.
 * 3. Nothing blocks except ompi_modex_wait_if_needed(): readiness is asked
 *    on send paths and from BTL callbacks.
 * 4. Session teardown must leave no flag saying the exchange has landed,
 *    or the next instance reads the previous one's addresses.
 *
 * Readiness is a predicate, not a subscription: parked work stays with
 * whoever parked it and is re-driven from the progress callback its owner
 * already registers. Hence no list, no callback and no lock here.
 */

#ifndef OMPI_MODEX_H
#define OMPI_MODEX_H

#include "ompi_config.h"

BEGIN_C_DECLS

struct ompi_proc_t;

/**
 * Start the connection-info exchange (background collect fence, or skip it
 * in direct-modex mode). Does not wait.
 *
 * PMIx names a collective by its participants alone, and this fence is
 * over every proc of the job: nothing may fence over that same set until
 * ompi_modex_wait_if_needed() has completed it.
 */
OMPI_DECLSPEC int ompi_modex_start_exchange(void);

/** True once the collect fence has completed, or there is no fence. */
OMPI_DECLSPEC bool ompi_modex_all_ready(void);

/**
 * True if this peer's complete blob is local (or all_ready / self).
 *
 * In the on-demand mode a false answer also starts the fetch that will
 * make it true, at most one in flight per peer, so polling this once per
 * progress tick is the intended use.
 */
OMPI_DECLSPEC bool ompi_modex_proc_ready(struct ompi_proc_t *proc);

/**
 * Ask for every peer's blob, whatever the mode would have been. Must be
 * called before the exchange starts: from parameter registration, or from
 * instance init as late as PML selection.
 *
 * For FT, whose subsystems reach for whichever rank is still alive from a
 * BTL callback: they can neither wait nor name that peer in advance, so
 * only a collecting fence up front makes those reaches local. And for a
 * BTL that takes all procs or none, which cannot be told to come back.
 */
OMPI_DECLSPEC void ompi_modex_require_all(void);

/**
 * Block in progress until the collect fence completes, and report how it
 * went. A failed fence collected nothing and leaves no way to get any
 * peer information: a failed startup, not a slower one.
 */
OMPI_DECLSPEC int ompi_modex_wait_if_needed(void);

/** Give back what this instance learned, so the next one starts blind. */
OMPI_DECLSPEC void ompi_modex_finalize(void);

END_C_DECLS

#endif /* OMPI_MODEX_H */

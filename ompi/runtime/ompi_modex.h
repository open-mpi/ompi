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
 * every peer it wires up. Making those reads local is a collective in one
 * mode and a fetch per peer in another, neither of them instant, and this
 * layer is what knows how far along that is for a given peer.
 *
 * It owes its callers five things.
 *
 * 1. "Not yet" told apart from "no". A read that misses reports
 *    OPAL_ERR_NOT_READY while the peer's data can still arrive, and misses
 *    plainly once it cannot. Without that line the two are the same event:
 *    a BTL add_procs either declares a live peer unreachable or retries a
 *    dead one forever.
 *
 * 2. Asking is what makes it true. Where peers are fetched one at a time
 *    nothing is running in the background; a peer's data becomes local
 *    because somebody wanted it. A caller that only ever asks -- again
 *    next tick, and the one after -- therefore makes progress without
 *    arranging anything.
 *
 * 3. One fetch per peer, however many ask. That caller is asking from a
 *    progress tick, so anything less turns a wait into a Get per tick
 *    against the server. The claim is a bit on the proc, taken with one
 *    atomic.
 *
 * 4. Nothing blocks, except the one call whose name says so. Readiness is
 *    asked on send paths and from BTL callbacks, where there is no
 *    progress to make and no stack to spare.
 *
 * 5. A second instance starts from nothing. Session teardown must leave no
 *    flag saying the exchange has landed, or the next instance reads the
 *    previous one's addresses.
 *
 * What this is deliberately not is a place to register a callback.
 * Readiness is a predicate; work that had to be parked stays with whoever
 * parked it -- ob1 its staged sends, cm its staged requests, the receive
 * path its fragments -- and is re-driven from the progress callback that
 * owner already registers while it has work outstanding.
 *
 * A subscription would be a second object naming work owned elsewhere, and
 * the questions that follow from that split are ones this layer is in no
 * position to answer: whether two arming callers mean one callback or two,
 * how long a subscription may name a proc its owner has finished with,
 * what happens when a callback re-enters the drain that fired it. Asked of
 * the owner they are already answered -- the parked list is the record, so
 * there is nothing to deduplicate; it holds what it parked, so there is no
 * lifetime to arrange; it drains from a tick, so nothing re-enters. What
 * remains here needs no list, no callback and no lock: three globals
 * written once, and per-peer state carried as flags on opal_proc_t, where
 * the readers already are.
 */

#ifndef OMPI_MODEX_H
#define OMPI_MODEX_H

#include "ompi_config.h"

BEGIN_C_DECLS

struct ompi_proc_t;

/**
 * Start the connection-info exchange (background collect fence, or
 * skip it in direct-modex mode). Does not wait.
 *
 * The fence it leaves in flight is over every proc of this job, and PMIx
 * tells one collective from another by its participants alone. So nothing
 * may fence over that same set until this one has completed -- see
 * ompi_modex_wait_if_needed(), which instance init calls before the
 * barrier that ends it.
 */
OMPI_DECLSPEC int ompi_modex_start_exchange(void);

/** True once the collect fence has completed, or there is no fence. */
OMPI_DECLSPEC bool ompi_modex_all_ready(void);

/**
 * True if this peer's complete blob is local (or all_ready / self).
 *
 * In the on-demand mode, asking is also wanting: a false answer starts the
 * fetch that will eventually make it true, so a caller that only ever
 * polls this still makes progress. At most one fetch is ever in flight per
 * peer, so polling it once per progress tick is what it looks like.
 */
OMPI_DECLSPEC bool ompi_modex_proc_ready(struct ompi_proc_t *proc);

/**
 * Ask for every peer's blob, whatever the mode would have been. Must be
 * called before the exchange starts, so from parameter registration.
 *
 * For a subsystem that cannot wait where it needs a peer, which is all of
 * FT: the reliable broadcast carries a revoke or a failure notice 2^i hops
 * around a communicator, an agreement talks to its tree parent and
 * children, the failure detector moves its heartbeat ring -- each from a
 * BTL callback, each reaching for whichever rank is still alive. One
 * collecting fence up front is what makes every one of those reaches
 * local, and it is the only way to have that: fetching per peer answers
 * the peer somebody asked about, and none of them can say in advance which
 * peer that will be.
 */
OMPI_DECLSPEC void ompi_modex_require_all(void);

/**
 * Block in progress until the collect fence completes, and report how it
 * went. A fence that failed collected nothing, so its caller has no peer
 * information and no way to get any: that is a failed startup, not a
 * slower one.
 */
OMPI_DECLSPEC int ompi_modex_wait_if_needed(void);

/** Give back what this instance learned, so the next one starts blind. */
OMPI_DECLSPEC void ompi_modex_finalize(void);

END_C_DECLS

#endif /* OMPI_MODEX_H */

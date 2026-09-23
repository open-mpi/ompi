<!--
  Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.

  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
-->

# Endpoint and proc lifecycle: nothing is reclaimed before finalize

A recurring question, most often raised by `MPI_Comm_disconnect`: once no
communicator names a peer any more, can its endpoints be torn down and
its resources returned? This note records the answer -- no, not without
a design that does not exist today -- and the reasoning, so the question
does not have to be re-derived.

## Where release happens today

BML endpoints are cleared in exactly one place,
`mca_bml_r2_del_procs()`, and the only PML-level `del_procs` caller is
`ompi_mpi_instance_cleanup_pml()` at instance finalize. Nothing
in `MPI_Comm_disconnect` or `MPI_Comm_free` reaches it:
`ompi_dpm_disconnect()` is a `PMIx_Fence`, and
`mca_pml_ob1_comm_destruct()` drops only the per-communicator OB1 state.

So a peer retired by a disconnect keeps a live `ompi_proc_t`, a live BML
endpoint, live per-BTL endpoints, and -- for tcp -- an open socket with
two libevent registrations, until the instance goes away. The
`ompi_proc_t` is not freed even then by refcount alone: `ompi_proc_list`
holds the original reference from `OBJ_NEW`.

## What MPI settles, and what it does not

MPI 5.1 §11.10.4 defines two processes as connected while they share a
communicator, window, or file handle, and -- clause 1(b) -- keeps them
connected indefinitely if the last shared communicator was freed with
`MPI_Comm_free` rather than `MPI_Comm_disconnect`. Only disconnect
retires the pair.

That is necessary but not sufficient for us. `MPI_Comm_create_from_group`
places no restriction on where its group came from, so a group held past
its communicator can name a peer again at any time: the `ompi_proc_t`
must outlive every group, which makes its OBJ refcount (groups,
communicators, in-flight requests, `ompi_proc_list`) useless as a
release trigger. Any scheme would need a separate per-peer binding
count, contributed by communicators and their window and file dups.

## A pair needs no agreement protocol

Communicator membership is symmetric and freeing is collective, so the
set of communicators binding two peers is the same set seen from either
end. If A's binding count for B reaches zero, B's count for A reaches
zero too. The two sides can differ on *when* -- destruction is deferred
locally by whatever requests still reference the communicator -- but not
on the answer. A pairwise decision therefore costs nothing beyond
tolerating skew.

## A set does, and there is nowhere to run it

Batching breaks that, because it makes a binding A cannot see relevant to
A's decision. Three peers, two communicators:

    comm1 = {A, B, C}      comm2 = {B, C}

All three disconnect `comm1`; B and C keep `comm2`. Every pair still
agrees: A-B and A-C are retired, B-C is not. But the *sets* do not
match --

    A wants to release {B, C}      B wants to release {A}
                                   C wants to release {A}

-- and a BTL whose unit of work is a group cannot be told "release these
two" by A and "release only A" by B. In the tree, sm is that BTL, and
concretely: `fini_sm_endpoint()` clears `mca_btl_sm_component.endpoints`,
the node-wide array the fragment paths resolve senders through. Retiring
one peer withdraws the claim for the whole node, and
`mca_btl_sm_attach_local_peers()` rebuilds all of it on the next
fragment. A's set here *is* the node, so honouring it means node-wide
teardown -- destroying state B-C traffic depends on, a binding A has no
way to learn about.

Reconciling the sets needs consensus over a membership that, by
construction, no longer shares a communicator: either a collective with
no place to be launched from, or a point-to-point agreement protocol.
Neither is worth building for this.

## What the tree actually promises about batching

Very little, and less than the code assumes.

`MCA_BTL_FLAGS_SINGLE_ADD_PROCS` is an add-side declaration -- "give me
one `add_procs` with every proc" -- set by portals4 and usnic always and
by tcp in `CONNECT_FULL` mode. It says nothing about the del side, and
there is no del-side equivalent.

`mca_bml_r2_del_procs()` decomposes every request into `nprocs == 1`
calls, and the two other `btl_del_procs` sites -- `mca_bml_r2_add_proc()`
and `mca_bml_r2_add_procs()`, undoing a wire-up that failed -- pass one
peer as well. A batching BTL has in fact never been handed its own batch
to retire. That is safe today only because a retirement means either
instance finalize, when every peer is tearing down at once, or a peer
that was never usable. Moving it to disconnect is precisely what removes
that guarantee.

## The decision

1. No BTL endpoint and no `ompi_proc_t` is released before instance
   finalize. Both are immortal for the life of the instance.

2. Correctness must therefore never depend on reclamation. A transport
   has to tolerate a peer that is gone while its endpoint remains: an
   EOF or a deliberate close on a retired peer is not a job failure.

3. The accepted cost is one `ompi_proc_t`, one BML endpoint, the per-BTL
   endpoints, and for tcp an fd and two event registrations, per peer
   ever contacted. It is bounded by distinct peers, not by disconnect
   count, so it does not grow with a spawn/disconnect loop over the same
   peers.

4. A BTL that could honour a partial retirement must say so explicitly
   before anything hands it one, and `del_procs` must refuse a set
   smaller than the BTL's unit rather than partially comply. See the
   `mca_btl_base_module_del_procs_fn_t()` documentation in `btl.h`.

## What would change the answer

Per-peer sm state, so that the only group-unit BTL in the tree becomes a
per-peer one, would reduce the problem to the pairwise case -- which, as
above, needs only skew tolerance in each transport. Absent that, or
absent a real teardown collective, endpoint reclamation is a rewrite,
not a patch.

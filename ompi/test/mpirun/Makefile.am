#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#
# Tests that have to be launched by mpirun.
#
# Everything run by "make check" elsewhere in the tree is either a
# non-MPI unit test or an MPI singleton, because Automake runs each test
# binary directly.  Some behavior, though, only exists between two ranks
# of a real job -- endpoint wire-up being the obvious example -- and
# cannot be reached without a launcher.  Those tests live here.
#
# Like ompi/test/mpi-abi's check-abi, this target requires Open MPI to
# have been INSTALLED first, and the installation's bin directory to be
# in the PATH.  "make check" deliberately runs before "make install", so
# these tests cannot be part of it.  Requiring the install is not just a
# scheduling convenience: the mpirun in the build tree is a thin wrapper
# that exec's $(bindir)/prterun, so it does not work until Open MPI has
# been installed.  Testing the installed tree also means the wrapper
# compiler, the launcher, and the component DSOs are all exactly what a
# user would get.
#
#     make install
#     PATH=$prefix/bin:$PATH make -C ompi/test/mpirun check-mpirun
#
# Ground rules for anything added here:
#
# 1. Use no more than 2 ranks, unless the test cannot distinguish its
#    cases with a pair (wireup_stress uses 4, oversubscribed).  These
#    run on CI machines with very few cores, alongside whatever else the
#    job is doing; a test that needs a big job does not belong here.
#
# 2. Always pass --timeout.  A bug in this area is at least as likely to
#    hang as it is to fail, and a hang must not stall CI until the
#    job-level timeout kills it.

EXTRA_DIST = \
        async_modex_wireup.c \
        any_source_oneway.c \
        late_first_contact.c \
        wireup_stress.c

# Deliberately built by the check-mpirun target rather than by Automake:
# these are compiled with the *installed* wrapper compiler.
MPICC = mpicc
MPIRUN = mpirun

# Rule 1 above: no more than 2 ranks.
MPIRUN_NP = 2

# Rule 2 above: never let a hung test run forever.
MPIRUN_TIMEOUT = 60

mpirun_runner = $(MPIRUN) --timeout $(MPIRUN_TIMEOUT) \
        -n $(MPIRUN_NP) --map-by ppr:$(MPIRUN_NP):node

# Four processes, oversubscribed onto one node.  Ring / star /
# all-to-all only differ with more than two ranks, and a test that keeps
# a peer in reserve while two others talk needs a third.  Four covers
# both without turning this into a "big job".
OVERSUB_NP = 4
oversub_runner = $(MPIRUN) --timeout $(MPIRUN_TIMEOUT) --oversubscribe \
        -n $(OVERSUB_NP) --map-by ppr:$(OVERSUB_NP):node

# The modex settings have to be in the *MPI processes'* environment;
# "mpirun --mca pmix_base_async_modex 1" does not reach them.
#
# ASYNC_MODEX keeps the job-wide data-collecting fence but runs it in the
# background, so a peer read before it lands reports "not yet" and
# whoever wanted that peer has to come back for it.
#
# DIRECT_MODEX removes the fence as well.  Nothing then makes a peer's
# data local except somebody asking about that one peer, and only the
# asking process's own progress finishes the job -- so every path that
# parks work has to re-drive it itself.  Both modes are run, because the
# fence is the safety net that hides a missing retry: a wire-up that
# only ever succeeds because the fence landed passes the first mode and
# hangs in the second.
ASYNC_MODEX = OMPI_MCA_pmix_base_async_modex=1
DIRECT_MODEX = OMPI_MCA_pmix_base_async_modex=1 \
        OMPI_MCA_pmix_base_collect_data=0

# Run each wire-up test twice: once with only the shared memory BTL,
# where a wire-up failure shows up as a prompt "unable to reach" abort,
# and once with the default transports, where a fallback transport turns
# the same failure into a hang.  Both must pass.
#
# The rma pattern is the one a hang would not catch.  A window picks one
# BTL for the whole window when it is created, from per-peer
# reachability, and a rank that had not resolved a peer yet would pick
# differently from its peers -- then address their state at an offset
# computed from its own choice.  Nothing fails; it writes into the wrong
# rank's state.  It is worst within a node, where the ranks share the
# segment they are sizing differently, hence the shared-memory-only run.
#
# Everything below goes through the "run" wrapper, which fails the target
# on a run that exits 0 while announcing that it could not reach a peer.
# That is not a pedantic reading of the output: a wire-up bug reaches the
# user as exactly this message, and these tests are two ranks doing the
# simplest thing they can, so nothing here has an unreachable peer to
# report.  It is worth checking for because a peer wrongly given up on is
# often retried by whoever asked next and never seen again -- which is
# how one of these runs spent a while passing while printing it.
#
# Not an Automake-recognized hook, so declare it phony to keep make from
# skipping it if a like-named file ever appears here.
.PHONY: check-mpirun

check-mpirun:
	@if ! command -v $(MPICC) >/dev/null 2>&1 || \
	    ! command -v $(MPIRUN) >/dev/null 2>&1; then \
	    echo "SKIP: $(MPICC)/$(MPIRUN) not found in PATH (is Open MPI installed?)"; \
	else \
	    set -e; \
	    run() { \
	        st=0; \
	        out=`"$$@" 2>&1` || st=$$?; \
	        printf '%s\n' "$$out"; \
	        if test 0 -ne $$st; then exit $$st; fi; \
	        case "$$out" in \
	            *"unable to reach"*) \
	                echo "FAIL: a reachable peer was reported unreachable"; \
	                exit 1;; \
	        esac; \
	    }; \
	    $(MPICC) -g "$(srcdir)/async_modex_wireup.c" -o async_modex_wireup; \
	    $(MPICC) -g "$(srcdir)/any_source_oneway.c" -o any_source_oneway; \
	    $(MPICC) -g "$(srcdir)/late_first_contact.c" -o late_first_contact; \
	    echo "=== async modex wire-up, shared memory only ==="; \
	    run env $(ASYNC_MODEX) \
	        $(mpirun_runner) --mca btl self,sm ./async_modex_wireup; \
	    echo "=== async modex wire-up, default transports ==="; \
	    run env $(ASYNC_MODEX) \
	        $(mpirun_runner) ./async_modex_wireup; \
	    echo "=== direct modex wire-up, shared memory only ==="; \
	    run env $(DIRECT_MODEX) \
	        $(mpirun_runner) --mca btl self,sm ./async_modex_wireup; \
	    echo "=== direct modex wire-up, default transports ==="; \
	    run env $(DIRECT_MODEX) \
	        $(mpirun_runner) ./async_modex_wireup; \
	    echo "=== ANY_SOURCE one-way, shared memory only ==="; \
	    run $(mpirun_runner) --mca btl self,sm ./any_source_oneway; \
	    echo "=== ANY_SOURCE one-way, default transports ==="; \
	    run $(mpirun_runner) ./any_source_oneway; \
	    echo "=== ANY_SOURCE one-way, direct modex, shared memory only ==="; \
	    run env $(DIRECT_MODEX) \
	        $(mpirun_runner) --mca btl self,sm ./any_source_oneway; \
	    echo "=== ANY_SOURCE one-way, direct modex, default transports ==="; \
	    run env $(DIRECT_MODEX) $(mpirun_runner) ./any_source_oneway; \
	    echo "=== late first contact, full modex ==="; \
	    run $(oversub_runner) ./late_first_contact; \
	    echo "=== late first contact, direct modex, shared memory only ==="; \
	    run env $(DIRECT_MODEX) \
	        $(oversub_runner) --mca btl self,sm ./late_first_contact; \
	    echo "=== late first contact, direct modex, default transports ==="; \
	    run env $(DIRECT_MODEX) $(oversub_runner) ./late_first_contact; \
	    $(MPICC) -g "$(srcdir)/wireup_stress.c" -o wireup_stress; \
	    for pattern in ring bcast reduce alltoall; do \
	        echo "=== wireup stress p2p $$pattern ==="; \
	        run $(oversub_runner) ./wireup_stress $$pattern p2p; \
	    done; \
	    for pattern in bcast reduce alltoall; do \
	        echo "=== wireup stress coll $$pattern ==="; \
	        run $(oversub_runner) ./wireup_stress $$pattern coll; \
	    done; \
	    for flavor in p2p coll; do \
	        echo "=== wireup stress alltoall $$flavor, direct modex ==="; \
	        run env $(DIRECT_MODEX) \
	            $(oversub_runner) ./wireup_stress alltoall $$flavor; \
	    done; \
	    echo "=== wireup stress rma ==="; \
	    run $(oversub_runner) ./wireup_stress rma; \
	    echo "=== wireup stress rma, direct modex ==="; \
	    run env $(DIRECT_MODEX) $(oversub_runner) ./wireup_stress rma; \
	    echo "=== wireup stress rma, direct modex, shared memory only ==="; \
	    run env $(DIRECT_MODEX) \
	        $(oversub_runner) --mca btl self,sm ./wireup_stress rma; \
	fi

clean-local:
	rm -rf async_modex_wireup async_modex_wireup.dSYM \
	    any_source_oneway any_source_oneway.dSYM \
	    late_first_contact late_first_contact.dSYM \
	    wireup_stress wireup_stress.dSYM

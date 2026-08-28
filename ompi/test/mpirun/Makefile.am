#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
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

# Ring / star / all-to-all only differ with more than two ranks.  Four
# processes, oversubscribed onto one node, is enough to tell those
# topologies apart without turning this into a "big job".
WIREUP_NP = 4
wireup_runner = $(MPIRUN) --timeout $(MPIRUN_TIMEOUT) --oversubscribe \
        -n $(WIREUP_NP) --map-by ppr:$(WIREUP_NP):node

# The asynchronous modex has to be enabled in the *MPI processes'*
# environment; "mpirun --mca pmix_base_async_modex 1" does not reach
# them.
#
# Run the wire-up test twice: once with only the shared memory BTL,
# where a wire-up failure shows up as a prompt "unable to reach" abort,
# and once with the default transports, where a fallback transport turns
# the same failure into a hang.  Both must pass.
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
	    $(MPICC) -g "$(srcdir)/async_modex_wireup.c" -o async_modex_wireup; \
	    echo "=== async modex wire-up, shared memory only ==="; \
	    OMPI_MCA_pmix_base_async_modex=1 \
	        $(mpirun_runner) --mca btl self,sm ./async_modex_wireup; \
	    echo "=== async modex wire-up, default transports ==="; \
	    OMPI_MCA_pmix_base_async_modex=1 \
	        $(mpirun_runner) ./async_modex_wireup; \
	    $(MPICC) -g "$(srcdir)/wireup_stress.c" -o wireup_stress; \
	    for pattern in ring bcast reduce alltoall; do \
	        echo "=== wireup stress p2p $$pattern ==="; \
	        $(wireup_runner) ./wireup_stress $$pattern p2p; \
	    done; \
	    for pattern in bcast reduce alltoall; do \
	        echo "=== wireup stress coll $$pattern ==="; \
	        $(wireup_runner) ./wireup_stress $$pattern coll; \
	    done; \
	fi

clean-local:
	rm -rf async_modex_wireup async_modex_wireup.dSYM \
	    wireup_stress wireup_stress.dSYM

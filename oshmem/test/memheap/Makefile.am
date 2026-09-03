#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# Tests that exercise OSHMEM memheap behavior through an installed runtime.
# See ompi/test/mpirun for the installed-runtime target conventions.

EXTRA_DIST = \
        run-static-anonymous-mapping.sh \
        static_anonymous_mapping.c \
        check_memheap.sh \
        layout_small.c \
        layout_large.c

SHMEMCC = shmemcc
OSHRUN = oshrun

# Use no more than two PEs and bound every launched test.
OSHRUN_NP = 2
OSHRUN_TIMEOUT = 60

.PHONY: check-memheap

# Deliberately use this installed-runtime target rather than ordinary
# pre-install "make check".  The runner's status is translated explicitly:
# 77 is SKIP, zero is PASS, and every other status fails this target.
check-memheap:
	@srcdir='$(srcdir)' \
	 SHMEMCC='$(SHMEMCC)' \
	 OSHRUN='$(OSHRUN)' \
	 OSHRUN_NP='$(OSHRUN_NP)' \
	 OSHRUN_TIMEOUT='$(OSHRUN_TIMEOUT)' \
	 "$(srcdir)/check_memheap.sh"; \
	rc=$$?; \
	case $$rc in \
	    0) echo "PASS: memheap" ;; \
	    77) echo "SKIP: memheap (runner exit 77)" ;; \
	    *) echo "FAIL: memheap (runner exit $$rc)"; exit $$rc ;; \
	esac

clean-local:
	rm -rf static_anonymous_mapping static_anonymous_mapping.dSYM \
	       layout_small layout_small.dSYM layout_large layout_large.dSYM

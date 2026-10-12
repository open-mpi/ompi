#
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# Functional tests of the OSHMEM C API, run through an installed runtime.
# Every *.c file in this directory is one test.  A test may request a PE
# count with a line of the form "NP: <n>" in a comment (default 2) and may
# exit with status 77 to report that it was skipped.

EXTRA_DIST = check_shmem.sh

SHMEMCC = shmemcc
OSHRUN = oshrun

# Bound every launched test.
OSHRUN_TIMEOUT = 120

.PHONY: check-shmem

# Like the memheap tests, this is an installed-runtime target rather than
# part of the pre-install "make check".  Set TESTS to a space-separated list
# of test names (without ".c") to run a subset.
check-shmem:
	@srcdir='$(srcdir)' \
	 SHMEMCC='$(SHMEMCC)' \
	 OSHRUN='$(OSHRUN)' \
	 OSHRUN_TIMEOUT='$(OSHRUN_TIMEOUT)' \
	 TESTS='$(TESTS)' \
	 "$(srcdir)/check_shmem.sh"

dist-hook:
	cp -p $(srcdir)/*.c $(distdir)

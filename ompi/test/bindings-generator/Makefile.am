#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Unit tests for the MPI binding generator (ompi/mpi/bindings/).  These are
# pure Python: they import the generator directly out of the source tree and
# render a few real .c.in templates in memory.  Nothing is compiled, no MPI
# is launched, and no build products are needed -- so this runs anywhere,
# including in a tree that has not been built yet.

EXTRA_DIST = test_bindings_generator.py

# Mirrors ompi/test/mpi-abi: skip cleanly when configure could not find a usable
# Python rather than failing the build.
check-local:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(PYTHON) -B "$(srcdir)/test_bindings_generator.py"; \
	fi

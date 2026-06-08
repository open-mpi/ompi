#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = \
        README.md \
        requirements.txt \
        mpi_abi_tests.py \
        manual/README.md \
        templates/c_probe.c.in \
        templates/fortran_probe.f90.in \
        templates/README.md

abi_test_runner = $(PYTHON) "$(srcdir)/mpi_abi_tests.py" \
        --srcdir "$(top_srcdir)" \
        --builddir "$(abs_top_builddir)" \
        --outdir "$(abs_builddir)/.mpi-abi"

check-local:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) check-fast; \
	fi

check-abi:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) check-abi; \
	fi

check-abi-cross:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) check-abi-cross; \
	fi

manifest:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) manifest; \
	fi

coverage:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) coverage; \
	fi

clean-local:
	rm -rf .mpi-abi

distclean-local:
	rm -rf .mpi-abi

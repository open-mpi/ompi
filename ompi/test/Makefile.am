#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#
# Unit tests for the OMPI layer.  These tests live inside the ompi/
# subtree, alongside the code that they exercise, and are run by "make
# check" (both a top-level "make check" and one run solely inside
# ompi/).
#
# Most tests here are self-checking single-process programs: they need no
# launcher, and any MPI communication they perform is on MPI_COMM_SELF.
# Tests are grouped into subdirectories by the MPI object or interface
# that they exercise.  mpi-abi is the exception: it is a suite of
# multi-process MPI programs, launched by its own driver, that exercise
# the MPI Standard ABI (see its check-local target).
#

# t, file, datatype, general, mpi-abi, reexport, and bindings-generator are run by
# "make check".  spc and monitoring are multi-process tests: they are
# built, but not run by "make check".

#
# Test coverage in ABI-only mode (--disable-ompi-abi):
#
# In ABI-only mode only the MPI Forum standard ABI library (libmpi_abi)
# and header are built; libopen_mpi and libmpi are not.  Tests that use
# only the public MPI C API still run: their Makefiles compile them with
# -DMPI_ABI (exactly as the mpicc_abi wrapper does) so that <mpi.h>
# resolves to the Forum ABI header, and link them against libmpi_abi.
# The file, part, general, and datatype subdirectories each build their
# public-API-only subset this way (see their Makefile.am files).
#
# Subdirectories dropped entirely in ABI-only mode:
#  - t: excluded for now pending the MPI_T fixes in
#    https://github.com/open-mpi/ompi/pull/14306
#  - monitoring, spc: multi-process tests that link internal Open MPI
#    symbols; not run by "make check" in any case
#  - reexport: dlsym()s internal handles (e.g. ompi_mpi_comm_world) out
#    of libmpi, which does not exist in ABI-only mode
#
# Always-included subdirectories:
#  - mpi-abi: comprehensive suite for the MPI Forum standard ABI,
#    including cross-ABI checks, installed artifact validation, and
#    conformance tests
#  - mpirun: tests for the launcher, which works with both ABIs
#  - bindings-generator: tests for the binding generation infrastructure
#

if OMPI_ABI_ONLY
SUBDIRS = file datatype general part mpi-abi mpirun bindings-generator
else
if OMPI_BUILD_OMPI_ABI
SUBDIRS = t file datatype general monitoring part spc mpi-abi mpirun reexport bindings-generator
else
SUBDIRS = file datatype general monitoring part spc mpi-abi mpirun reexport bindings-generator
endif
endif

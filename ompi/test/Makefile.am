#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
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
SUBDIRS = t file datatype general monitoring spc mpi-abi reexport bindings-generator

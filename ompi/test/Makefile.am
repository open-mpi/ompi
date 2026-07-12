#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# Single-process unit tests for the OMPI layer.  These tests live inside
# the ompi/ subtree, alongside the code that they exercise, and are run
# by "make check" (both a top-level "make check" and one run solely
# inside ompi/).
#
# Every test here is a self-checking single-process program: it needs no
# launcher, and any MPI communication it performs is on MPI_COMM_SELF.
# Tests are grouped into subdirectories by the MPI object or interface
# that they exercise.
#

# t, file, part, datatype, and general are run by "make check".  spc and
# monitoring are multi-process tests: they are built, but not run by
# "make check".
SUBDIRS = t file part datatype general monitoring spc

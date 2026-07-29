# -*- makefile -*-
#
# Copyright (c) 2026      Technical University of Darmstadt.  All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# This Makefile is only relevant if we're building the ignore-tkr "use
# mpi" MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPI_IGNORE_TKR_BINDINGS

# This directory only exists so that we can separate C compilation from
# Fortran compilation, for the same reason as
# ompi/mpi/fortran/use-mpi-f08/base: the parent directory zeroes out
# CPPFLAGS / AM_CPPFLAGS (see https://github.com/open-mpi/ompi/issues/7253).
#
# The one .c file here backs the mpi module's TS29113 choice-buffer
# procedures.
# This directory + Makefile.am is separate to have non-zeroed CPPFLAGS/AM_CPPFLAGS
if OMPI_FORTRAN_HAVE_TS

noinst_LTLIBRARIES = libusempi_ignore_tkr_ccode.la

libusempi_ignore_tkr_ccode_la_SOURCES = api_f90_generated.c

endif

include ../Makefile.prototype_files
template_files =${prototype_files:%=$(abs_top_srcdir)/ompi/mpi/fortran/use-mpi-f08/%}

api_f90_generated.c: $(template_files) $(OMPI_BINDINGS_GENERATOR)
	$(OMPI_V_GEN) $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/bindings.py \
	    --builddir $(abs_top_builddir) \
	    --srcdir $(abs_top_srcdir) \
	    --output $(abs_builddir)/$@ \
	    fortran \
	    --generate-ts-suffix \
	    --fort-std f90 \
	    code \
	    --lang c \
	    --prototype-files $(template_files)

MAINTAINERCLEANFILES = api_f90_generated.c
if OPAL_DEVEL_BUILD
CLEANFILES += api_f90_generated.c
endif

endif

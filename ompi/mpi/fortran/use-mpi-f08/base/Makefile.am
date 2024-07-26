# -*- makefile -*-
#
# Copyright (c) 2019      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2020      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# This Makefile is only relevant if we're building the "use mpi_f08"
# MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS

# This directory only exists so that we can separate C compilation
# from Fortran compilation.  Specifically: note that Automake's
# Fortran-buidling rules uses CPPFLAGS and AM_CPPFLAGS.  This can
# cause weirdness (e.g.,
# https://github.com/open-mpi/ompi/issues/7253).  So when compiling
# Fortran, we should zero out CPPFLAGS and AM_CPPFLAGS.

# HOWEVER, we have one .c file in the use-mpi-f08 library.  So we have
# to split it out to its own directory / Makefile.am where CPPFLAGS /
# AM_CPPFLAGS are *not* zeroed out.

noinst_LTLIBRARIES = libusempif08_ccode.la

libusempif08_ccode_la_SOURCES = \
        buffer_detach.c


if OMPI_FORTRAN_HAVE_TS
libusempif08_ccode_la_SOURCES += \
        ts.h \
        ts.c \
        api_f08_ts_generated.c
else
libusempif08_ccode_la_SOURCES += api_f08_generated.c
endif

#
# We generate two versions of the C files, one with TS 29113 support and the
# other without. This ensures that the end user is not required to run the
# binding scripts in order to generate the specific version for their compiler.
#
if OMPI_GENERATE_BINDINGS
api_f08_generated.c: ../interface.json
	$(OMPI_V_GEN) $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/bindings.py \
	    --builddir $(abs_top_builddir) \
	    --srcdir $(abs_top_srcdir) \
	    --output $(abs_builddir)/$@ \
	    fortran \
	    --prototypes $(abs_srcdir)/$< \
	    code \
	    c

api_f08_ts_generated.c: ../interface.json
	$(OMPI_V_GEN) $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/bindings.py \
	    --builddir $(abs_top_builddir) \
	    --srcdir $(abs_top_srcdir) \
	    --output $(abs_builddir)/$@ \
	    fortran \
	    --ts \
	    --prototypes $(abs_srcdir)/$< \
	    code \
	    c

# Delete generated files on maintainer-clean
MAINTAINERCLEANFILES = api_f08_generated.c api_f08_ts_generated.c
endif

# Always distribute both versions of the C files
EXTRA_DIST = \
        api_f08_generated.c \
        api_f08_ts_generated.c

endif

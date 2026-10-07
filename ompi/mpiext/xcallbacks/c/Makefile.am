#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      NVIDIA, Inc. All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

# This file builds the C bindings for MPI extensions.  It must be
# present in all MPI extensions.

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

# Convenience libtool library that will be slurped up into libmpi.la.
noinst_LTLIBRARIES = libmpiext_xcallbacks_c.la libmpiext_xcallbacks_profile_c.la 

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/mpiext

SUFFIXES= _ompi_generated.c _abi_generated.c .c.in

# This is the header file that is installed.
ompi_HEADERS = mpiext_xcallbacks_c.h

#
# the following must be processed by binding framework
# to generated both ompi and standard abi variants
#
prototype_sources = \
        op_create_x.c.in


# This list is spelled out explicitly (rather than derived from
# prototype_sources with a substitution reference) because BSD make
# does not support GNU make's pattern substitution.
interface_profile_sources = \
        op_create_x_ompi_generated.c

if OMPI_GENERATE_BINDINGS
# See OMPI_BINDINGS_GENERATOR in Makefile.ompi-rules.
$(interface_profile_sources): $(OMPI_BINDINGS_GENERATOR) abi_xcallbacks_c.h

#
# The suppress bc is to suppress generation of _c suffixes to entry
# points.
#
.c.in_ompi_generated.c:
	$(OMPI_V_GEN) $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/bindings.py \
	    --builddir $(abs_top_builddir) \
	    --srcdir $(abs_top_srcdir) \
	    --output $@ \
	    c \
	    source \
	    ompi \
            --suppress_bc \
	    --bindings-prefix=mpix \
	    $<

.c.in_abi_generated.c:
	$(OMPI_V_GEN) $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/bindings.py \
	    --builddir $(abs_top_builddir) \
	    --srcdir $(abs_top_srcdir) \
	    --output $@ \
	    c \
	    source \
	    standard \
            --suppress_bc \
	    --bindings-prefix=mpix \
	    $<
# Mangled version of mpi_xcallbacks_c.h
abi_xcallbacks_c.h: $(top_srcdir)/ompi/mpiext/xcallbacks/c/mpix-callback-apis.json $(top_srcdir)/ompi/mpi/bindings/c_header.py $(top_srcdir)/ompi/mpiext/xcallbacks/c/abi_xcallbacks_c.h.in
	$(OMPI_V_GEN) $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/c_header.py \
            --input $(top_srcdir)/ompi/mpiext/xcallbacks/c/abi_xcallbacks_c.h.in\
	    --apis-json $(top_srcdir)/ompi/mpiext/xcallbacks/c/mpix-callback-apis.json \
	    --pympistd-dir $(top_srcdir)/3rd-party/pympistandard \
	    --prototypes-only \
	    --mangle-names \
	    --output $@

endif

libmpiext_xcallbacks_c_la_SOURCES = \
        $(interface_profile_sources) \
        $(ompi_HEADERS) 
libmpiext_xcallbacks_c_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=0
libmpiext_xcallbacks_c_la_LDFLAGS = -module -avoid-version

libmpiext_xcallbacks_profile_c_la_SOURCES = \
        $(interface_profile_sources) \
        $(ompi_HEADERS)
libmpiext_xcallbacks_profile_c_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1
libmpiext_xcallbacks_profile_c_la_LDFLAGS = -module -avoid-version

libmpiext_xcallbacks_c_la_LIBADD = libmpiext_xcallbacks_profile_c.la

EXTRA_DIST = $(prototype_sources)

MAINTAINERCLEANFILES = *_generated.c 
if OPAL_DEVEL_BUILD
CLEANFILES += *_generated.c 
endif

if OMPI_STANDARD_ABI
include Makefile_abi.include
endif


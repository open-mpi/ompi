#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# A word of explanation...
#
# This library is linked against various MCA components because the
# support for CUDA device pointers is needed in various places.
# There's two cases:
#
# 1. libmca_common_cuda.la is a shared library.  By linking that shared
# library to all components that need it, the OS linker will
# automatically load it into the process as necessary, and there will
# only be one copy (i.e., all the components will share *one* copy of
# the code and data).
#
# 2. libmca_common_cuda.la is a static library.  In this case, it will
# be rolled up into the top-level libmpi.la.  It will also be rolled
# into each component, but then the component will also be rolled up
# into the upper-level libmpi.la.  Linkers universally know how to
# "figure this out" so that we end up with only one copy of the code
# and data.
#
# Note that building this common component statically and linking
# against other dynamic components is *not* supported!

AM_CPPFLAGS = $(common_cuda_CPPFLAGS)

# Header files

headers = \
        common_cuda.h

# Source files

sources = \
        common_cuda.c

dist_opaldata_DATA = help-mpi-common-cuda.txt

# As per above, we'll either have an installable or noinst result.
# The installable one should follow the same MCA prefix naming rules
# (i.e., libmca_<type>_<name>.la).  The noinst one can be named
# whatever it wants, although libmca_<type>_<name>_noinst.la is
# recommended.

# To simplify components that link to this library, we will *always*
# have an output libtool library named libmca_<type>_<name>.la -- even
# for case 2) described above (i.e., so there's no conditional logic
# necessary in component Makefile.am's that link to this library).
# Hence, if we're creating a noinst version of this library (i.e.,
# case 2), we sym link it to the libmca_<type>_<name>.la name
# (libtool will do the Right Things under the covers).  See the
# all-local and clean-local rules, below, for how this is effected.

lib_LTLIBRARIES =
noinst_LTLIBRARIES =
comp_inst = lib@OPAL_LIB_PREFIX@mca_common_cuda.la
comp_noinst = lib@OPAL_LIB_PREFIX@mca_common_cuda_noinst.la

if MCA_BUILD_opal_common_cuda_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

lib@OPAL_LIB_PREFIX@mca_common_cuda_la_SOURCES = $(headers) $(sources)
lib@OPAL_LIB_PREFIX@mca_common_cuda_la_LDFLAGS = \
        -version-info $(libmca_opal_common_cuda_so_version)
lib@OPAL_LIB_PREFIX@mca_common_cuda_la_LIBADD = $(common_cuda_LIBS)

lib@OPAL_LIB_PREFIX@mca_common_cuda_noinst_la_SOURCES = $(headers) $(sources)
lib@OPAL_LIB_PREFIX@mca_common_cuda_noinst_la_LDFLAGS = $(common_cuda_LDFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_cuda_noinst_la_LIBADD = $(common_cuda_LIBS)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
opal_HEADERS = $(headers)
endif

# These two rules will sym link the "noinst" libtool library filename
# to the installable libtool library filename in the case where we are
# compiling this component statically (case 2), described above).

V=0
OMPI_V_LN_SCOMP = $(ompi__v_LN_SCOMP_$V)
ompi__v_LN_SCOMP_ = $(ompi__v_LN_SCOMP_$AM_DEFAULT_VERBOSITY)
ompi__v_LN_SCOMP_0 = @echo "  LN_S    " `basename $(comp_inst)`;

all-local:
	$(OMPI_V_LN_SCOMP) if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	  $(LN_S) "$(comp_noinst)" "$(comp_inst)"; \
	fi

clean-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	fi

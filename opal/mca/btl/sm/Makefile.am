#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2009 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_opaldata_DATA = help-mpi-btl-sm.txt

libmca_btl_sm_la_sources = btl_sm_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_btl_sm_DSO
component_noinst =
component_install = mca_btl_sm.la
else
component_noinst = libmca_btl_sm.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_sm_la_SOURCES = $(libmca_btl_sm_la_sources)
mca_btl_sm_la_LDFLAGS = -module -avoid-version
mca_btl_sm_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la
mca_btl_sm_la_CPPFLAGS = $(btl_sm_CPPFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_sm_la_SOURCES = $(libmca_btl_sm_la_sources)
libmca_btl_sm_la_LDFLAGS = -module -avoid-version
libmca_btl_sm_la_CPPFLAGS = $(btl_sm_CPPFLAGS)

#
# Copyright (c) 2004-2008 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2009      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_opaldata_DATA = help-opal-crs-none.txt

sources = \
        crs_none.h \
        crs_none_component.c \
        crs_none_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_crs_none_DSO
component_noinst =
component_install = mca_crs_none.la
else
component_noinst = libmca_crs_none.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_crs_none_la_SOURCES = $(sources)
mca_crs_none_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_crs_none_la_SOURCES = $(sources)
libmca_crs_none_la_LDFLAGS = -module -avoid-version

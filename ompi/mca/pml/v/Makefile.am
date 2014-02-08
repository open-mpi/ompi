#
# Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

EXTRA_DIST = .windows

local_sources = \
        pml_v.h \
        pml_v_component.c \
        pml_v_output.h \
        pml_v_output.c

if MCA_BUILD_ompi_pml_v_DSO
component_noinst =
component_install = mca_pml_v.la
else
component_noinst = libmca_pml_v.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pml_v_la_SOURCES = $(local_sources)
mca_pml_v_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pml_v_la_SOURCES = $(local_sources)
libmca_pml_v_la_LDFLAGS = -module -avoid-version


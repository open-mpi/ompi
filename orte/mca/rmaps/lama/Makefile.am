#
# Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
#
# Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-orte-rmaps-lama.txt

sources = \
        rmaps_lama_module.c \
        rmaps_lama_max_tree.c \
        rmaps_lama_params.c \
        rmaps_lama.h \
        rmaps_lama_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_rmaps_lama_DSO
component_noinst =
component_install = mca_rmaps_lama.la
else
component_noinst = libmca_rmaps_lama.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_rmaps_lama_la_SOURCES = $(sources)
mca_rmaps_lama_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rmaps_lama_la_SOURCES =$(sources)
libmca_rmaps_lama_la_LDFLAGS = -module -avoid-version

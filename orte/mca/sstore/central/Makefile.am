#
# Copyright (c)      2010 The Trustees of Indiana University.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-orte-sstore-central.txt

sources = \
        sstore_central.h \
        sstore_central_component.c \
        sstore_central_module.c \
        sstore_central_global.c \
        sstore_central_local.c \
        sstore_central_app.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_sstore_central_DSO
component_noinst =
component_install = mca_sstore_central.la
else
component_noinst = libmca_sstore_central.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sstore_central_la_SOURCES = $(sources)
mca_sstore_central_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sstore_central_la_SOURCES = $(sources)
libmca_sstore_central_la_LDFLAGS = -module -avoid-version

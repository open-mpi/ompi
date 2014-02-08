#
# Copyright (c)      2010 The Trustees of Indiana University.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-orte-sstore-stage.txt

sources = \
        sstore_stage.h \
        sstore_stage_component.c \
        sstore_stage_module.c \
        sstore_stage_global.c \
        sstore_stage_local.c \
        sstore_stage_app.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_sstore_stage_DSO
component_noinst =
component_install = mca_sstore_stage.la
else
component_noinst = libmca_sstore_stage.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sstore_stage_la_SOURCES = $(sources)
mca_sstore_stage_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sstore_stage_la_SOURCES = $(sources)
libmca_sstore_stage_la_LDFLAGS = -module -avoid-version

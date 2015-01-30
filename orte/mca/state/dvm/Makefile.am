#
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        state_dvm.h \
        state_dvm_component.c \
        state_dvm.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_state_dvm_DSO
component_noinst =
component_install = mca_state_dvm.la
else
component_noinst = libmca_state_dvm.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_state_dvm_la_SOURCES = $(sources)
mca_state_dvm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_state_dvm_la_SOURCES =$(sources)
libmca_state_dvm_la_LDFLAGS = -module -avoid-version

#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        state_dbm.h \
        state_dbm_component.c \
        state_dbm.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_state_dbm_DSO
component_noinst =
component_install = mca_state_dbm.la
else
component_noinst = libmca_state_dbm.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_state_dbm_la_SOURCES = $(sources)
mca_state_dbm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_state_dbm_la_SOURCES =$(sources)
libmca_state_dbm_la_LDFLAGS = -module -avoid-version

#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        state_db.h \
        state_db_component.c \
        state_db.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_state_db_DSO
component_noinst =
component_install = mca_state_db.la
else
component_noinst = libmca_state_db.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_state_db_la_SOURCES = $(sources)
mca_state_db_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_state_db_la_SOURCES =$(sources)
libmca_state_db_la_LDFLAGS = -module -avoid-version

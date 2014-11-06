#
# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        dstore_sm.h \
        dstore_sm_component.c \
        dstore_sm.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_dstore_sm_DSO
component_noinst =
component_install = mca_dstore_sm.la
else
component_noinst = libmca_dstore_sm.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_dstore_sm_la_SOURCES = $(sources)
mca_dstore_sm_la_LDFLAGS = -module -avoid-version
mca_dstore_sm_la_LIBADD = $(dstore_sm_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_dstore_sm_la_SOURCES =$(sources)
libmca_dstore_sm_la_LDFLAGS = -module -avoid-version

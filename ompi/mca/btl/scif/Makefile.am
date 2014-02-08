# -*- indent-tabs-mode:nil -*-
#
# Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
#                         reserved.
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(btl_scif_CPPFLAGS)

if MCA_BUILD_ompi_btl_scif_DSO
component_noinst =
component_install = mca_btl_scif.la
else
component_noinst = libmca_btl_scif.la
component_install =
endif

scif_SOURCES = \
        btl_scif_component.c \
        btl_scif_module.c \
        btl_scif_add_procs.c \
        btl_scif_endpoint.h \
        btl_scif_endpoint.c \
        btl_scif_frag.c \
        btl_scif_frag.h \
        btl_scif_send.c \
        btl_scif_put.c \
        btl_scif_get.c \
        btl_scif.h

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_btl_scif_la_SOURCES = $(scif_SOURCES)
nodist_mca_btl_scif_la_SOURCES = $(scif_nodist_SOURCES)
mca_btl_scif_la_LIBADD = $(btl_scif_LIBS) 
mca_btl_scif_la_LDFLAGS = -module -avoid-version $(btl_scif_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_btl_scif_la_SOURCES = $(scif_SOURCES)
nodist_libmca_btl_scif_la_SOURCES = $(scif_nodist_SOURCES)
libmca_btl_scif_la_LIBADD = $(btl_scif_LIBS)
libmca_btl_scif_la_LDFLAGS = -module -avoid-version $(btl_scif_LDFLAGS)

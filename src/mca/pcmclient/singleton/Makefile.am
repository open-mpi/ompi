#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

EXTRA_DIST = VERSION

sources = \
        pcmclient_singleton.h \
        pcmclient_singleton.c \
        pcmclient_singleton_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pcmclient_singleton_DSO
component_noinst =
component_install = mca_pcmclient_singleton.la
else
component_noinst = libmca_pcmclient_singleton.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pcmclient_singleton_la_SOURCES = $(sources)
mca_pcmclient_singleton_la_LIBADD =
mca_pcmclient_singleton_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pcmclient_singleton_la_SOURCES = $(sources)
libmca_pcmclient_singleton_la_LIBADD =
libmca_pcmclient_singleton_la_LDFLAGS = -module -avoid-version

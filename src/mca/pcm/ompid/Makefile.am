#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
    pcm_ompid.c \
    pcm_ompid.h \
    pcm_ompid_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pcm_ompid_DSO
component_noinst =
component_install = mca_pcm_ompid.la
else
component_noinst = libmca_pcm_ompid.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pcm_ompid_la_SOURCES = $(sources)
mca_pcm_ompid_la_LIBADD = $(LIBOMPI_LA)
mca_pcm_ompid_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pcm_ompid_la_SOURCES = $(sources)
libmca_pcm_ompid_la_LDFLAGS = -module -avoid-version


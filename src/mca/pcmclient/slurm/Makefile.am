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

EXTRA_DIST = VERSION

sources = \
        pcmclient_slurm.h \
        pcmclient_slurm.c \
        pcmclient_slurm_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pcmclient_slurm_DSO
component_noinst =
component_install = mca_pcmclient_slurm.la
else
component_noinst = libmca_pcmclient_slurm.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pcmclient_slurm_la_SOURCES = $(sources)
mca_pcmclient_slurm_la_LIBADD =
mca_pcmclient_slurm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pcmclient_slurm_la_SOURCES = $(sources)
libmca_pcmclient_slurm_la_LIBADD =
libmca_pcmclient_slurm_la_LDFLAGS = -module -avoid-version

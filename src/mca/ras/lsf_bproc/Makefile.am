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

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ras_lsf_bproc_DSO
component_noinst =
component_install = mca_ras_lsf_bproc.la
else
component_noinst = libmca_ras_lsf_bproc.la
component_install =
endif

proxy_SOURCES = \
    ras_lsf_bproc.c \
    ras_lsf_bproc.h \
    ras_lsf_bproc_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ras_lsf_bproc_la_SOURCES = $(proxy_SOURCES)
mca_ras_lsf_bproc_la_LIBADD = 
mca_ras_lsf_bproc_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ras_lsf_bproc_la_SOURCES = $(proxy_SOURCES)
libmca_ras_lsf_bproc_la_LIBADD =
libmca_ras_lsf_bproc_la_LDFLAGS = -module -avoid-version

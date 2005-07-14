#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

if OMPI_BUILD_soh_bproc_DSO
component_noinst =
component_install = mca_soh_bproc.la
else
component_noinst = libmca_soh_bproc.la
component_install =
endif

host_SOURCES = \
    soh_bproc.c \
    soh_bproc.h \
    soh_bproc_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_soh_bproc_la_SOURCES = $(host_SOURCES)
mca_soh_bproc_la_LIBADD = 
mca_soh_bproc_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_soh_bproc_la_SOURCES = $(host_SOURCES)
libmca_soh_bproc_la_LIBADD =
libmca_soh_bproc_la_LDFLAGS = -module -avoid-version

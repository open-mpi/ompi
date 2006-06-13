#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if OMPI_BUILD_soh_xcpu_DSO
component_noinst =
component_install = mca_soh_xcpu.la
else
component_noinst = libmca_soh_xcpu.la
component_install =
endif

xcpu_SOURCES = \
    soh_xcpu.c \
    soh_xcpu.h \
    soh_xcpu_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_soh_xcpu_la_SOURCES = $(xcpu_SOURCES)
mca_soh_xcpu_la_LIBADD = \
    $(top_ompi_builddir)/orte/liborte.la \
    $(top_ompi_builddir)/opal/libopal.la
mca_soh_xcpu_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_soh_xcpu_la_SOURCES = $(xcpu_SOURCES)
libmca_soh_xcpu_la_LIBADD =
libmca_soh_xcpu_la_LDFLAGS = -module -avoid-version

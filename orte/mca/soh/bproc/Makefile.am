#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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



headers = \
	soh_bproc.h

if OMPI_BUILD_soh_bproc_DSO
component_noinst =
component_install = mca_soh_bproc.la
else
component_noinst = libmca_soh_bproc.la
component_install =
endif

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/mca/soh/bproc
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif

host_SOURCES = \
    soh_bproc.c \
    soh_bproc.h \
    soh_bproc_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_soh_bproc_la_SOURCES = $(host_SOURCES)
mca_soh_bproc_la_LIBADD = \
    $(top_ompi_builddir)/orte/liborte.la \
    $(top_ompi_builddir)/opal/libopal.la
mca_soh_bproc_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_soh_bproc_la_SOURCES = $(host_SOURCES)
libmca_soh_bproc_la_LIBADD =
libmca_soh_bproc_la_LDFLAGS = -module -avoid-version

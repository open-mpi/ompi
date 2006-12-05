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
	smr_bproc.h

if OMPI_BUILD_smr_bproc_DSO
component_noinst =
component_install = mca_smr_bproc.la
else
component_noinst = libmca_smr_bproc.la
component_install =
endif

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/mca/smr/bproc
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif

smr_SOURCES = \
    smr_bproc.c \
    smr_bproc.h \
    smr_bproc_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_smr_bproc_la_SOURCES = $(smr_SOURCES)
mca_smr_bproc_la_LIBADD = \
    $(smr_bproc_LIBS) \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
mca_smr_bproc_la_LDFLAGS = -module -avoid-version $(smr_bproc_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_smr_bproc_la_SOURCES = $(smr_SOURCES)
libmca_smr_bproc_la_LIBADD = $(smr_bproc_LIBS)
libmca_smr_bproc_la_LDFLAGS = -module -avoid-version $(smr_bproc_LDFLAGS)

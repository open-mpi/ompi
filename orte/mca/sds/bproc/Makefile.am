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

# Use the top-level Makefile.options



AM_CPPFLAGS = $(sds_bproc_CPPFLAGS)

sources = \
        sds_bproc.h \
        sds_bproc_component.c \
        sds_bproc_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_sds_bproc_DSO
component_noinst =
component_install = mca_sds_bproc.la
else
component_noinst = libmca_sds_bproc.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_sds_bproc_la_SOURCES = $(sources)
mca_sds_bproc_la_LDFLAGS = -module -avoid-version $(sds_bproc_LDFLAGS)
mca_sds_bproc_la_LIBADD = \
    $(sds_bproc_LIBS) \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_sds_bproc_la_SOURCES =$(sources)
libmca_sds_bproc_la_LDFLAGS = -module -avoid-version $(sds_bproc_LDFLAGS)
libmca_sds_bproc_la_LIBADD =  $(sds_bproc_LIBS)

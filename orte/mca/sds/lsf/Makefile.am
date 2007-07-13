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
# Copyright (c) 2007      Cisco, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(pls_lsf_CPPFLAGS)

sources = \
        sds_lsf.h \
        sds_lsf_component.c \
        sds_lsf_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_sds_lsf_DSO
component_noinst =
component_install = mca_sds_lsf.la
else
component_noinst = libmca_sds_lsf.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sds_lsf_la_SOURCES = $(sources)
mca_sds_lsf_la_LDFLAGS = -module -avoid-version $(sds_lsf_LDFLAGS)
mca_sds_lsf_la_LIBADD = \
    $(sds_lsf_LIBS) \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_sds_lsf_la_SOURCES =$(sources)
libmca_sds_lsf_la_LDFLAGS = -module -avoid-version $(sds_lsf_LDFLAGS)
libmca_sds_lsf_la_LIBADD = $(sds_lsf_LIBS)

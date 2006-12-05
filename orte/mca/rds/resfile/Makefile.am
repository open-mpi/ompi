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



# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_rds_resfile_DSO
component_noinst =
component_install = mca_rds_resfile.la
else
component_noinst = libmca_rds_resfile.la
component_install =
endif

proxy_SOURCES = \
    rds_resfile.c \
    rds_resfile_parse_attributes.c \
    rds_resfile.h \
    rds_resfile_component.c

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_rds_resfile_la_SOURCES = $(proxy_SOURCES)
mca_rds_resfile_la_LIBADD = \
    $(top_ompi_builddir)/orte/libopen-rte.la \
    $(top_ompi_builddir)/opal/libopen-pal.la
mca_rds_resfile_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_rds_resfile_la_SOURCES = $(proxy_SOURCES)
libmca_rds_resfile_la_LIBADD =
libmca_rds_resfile_la_LDFLAGS = -module -avoid-version

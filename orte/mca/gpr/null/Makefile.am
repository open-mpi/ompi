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

sources = \
        gpr_null_component.c \
        gpr_null.c \
	gpr_null.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_gpr_null_DSO
component_noinst =
component_install = mca_gpr_null.la
else
component_noinst = libmca_gpr_null.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_gpr_null_la_SOURCES = $(sources)
mca_gpr_null_la_LDFLAGS = -module -avoid-version
mca_gpr_null_la_LIBADD = \
        $(top_ompi_builddir)/orte/libopen-rte.la \
        $(top_ompi_builddir)/opal/libopen-pal.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_gpr_null_la_SOURCES =$(sources)
libmca_gpr_null_la_LDFLAGS = -module -avoid-version

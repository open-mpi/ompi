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
# Copyright (c) 2008-2011 University of Houston. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_fbtl_plfs_DSO
component_noinst =
component_install = mca_fbtl_plfs.la
else
component_noinst = libmca_fbtl_plfs.la
component_install =
endif

# Source files

fbtl_plfs_sources = \
        fbtl_plfs.h \
        fbtl_plfs.c \
        fbtl_plfs_component.c \
        fbtl_plfs_preadv.c \
        fbtl_plfs_ipreadv.c \
        fbtl_plfs_pwritev.c \
        fbtl_plfs_ipwritev.c

AM_CPPFLAGS = $(fbtl_plfs_CPPFLAGS)

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fbtl_plfs_la_SOURCES = $(fbtl_plfs_sources)
mca_fbtl_plfs_la_LIBADD = $(fbtl_plfs_LIBS)
mca_fbtl_plfs_la_LDFLAGS = -module -avoid-version $(fbtl_plfs_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_fbtl_plfs_la_SOURCES = $(fbtl_plfs_sources)
libmca_fbtl_plfs_la_LIBADD = $(fbtl_plfs_LIBS)
libmca_fbtl_plfs_la_LDFLAGS = -module -avoid-version $(fbtl_plfs_LDFLAGS)

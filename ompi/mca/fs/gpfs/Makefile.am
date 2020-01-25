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

if MCA_BUILD_ompi_fs_gpfs_DSO
component_noinst =
component_install = mca_fs_gpfs.la
else
component_noinst = libmca_fs_gpfs.la
component_install =
endif

# Source files

fs_gpfs_sources = \
		fs_gpfs_component.c \
		fs_gpfs_file_open.c \
		fs_gpfs_file_set_info.c\
		fs_gpfs_file_get_info.c\
		fs_gpfs.c \
		fs_gpfs.h

AM_CPPFLAGS = $(fs_gpfs_CPPFLAGS)

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fs_gpfs_la_SOURCES = $(fs_gpfs_sources)
mca_fs_gpfs_la_LIBADD = $(fs_gpfs_LIBS)
mca_fs_gpfs_la_LDFLAGS = -module -avoid-version $(fs_gpfs_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_fs_gpfs_la_SOURCES = $(fs_gpfs_sources)
libmca_fs_gpfs_la_LIBADD = $(fs_gpfs_LIBS)
libmca_fs_gpfs_la_LDFLAGS = -module -avoid-version $(fs_gpfs_LDFLAGS)


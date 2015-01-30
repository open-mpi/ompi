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
# Copyright (c) 2008-2014 University of Houston. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_fs_plfs_DSO
component_noinst =
component_install = mca_fs_plfs.la
else
component_noinst = libmca_fs_plfs.la
component_install =
endif

# Source files

fs_plfs_sources = \
        fs_plfs.h \
        fs_plfs.c \
        fs_plfs_component.c \
        fs_plfs_file_open.c \
	fs_plfs_file_close.c \
	fs_plfs_file_delete.c \
	fs_plfs_file_sync.c \
	fs_plfs_file_set_size.c \
        fs_plfs_file_get_size.c 

AM_CPPFLAGS = $(fs_plfs_CPPFLAGS)

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fs_plfs_la_SOURCES = $(fs_plfs_sources)
mca_fs_plfs_la_LIBADD = $(fs_plfs_LIBS)
mca_fs_plfs_la_LDFLAGS = -module -avoid-version $(fs_plfs_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_fs_plfs_la_SOURCES = $(fs_plfs_sources)
libmca_fs_plfs_la_LIBADD = $(fs_plfs_LIBS)
libmca_fs_plfs_la_LDFLAGS = -module -avoid-version $(fs_plfs_LDFLAGS)

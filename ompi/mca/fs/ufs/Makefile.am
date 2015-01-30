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

if MCA_BUILD_ompi_fs_ufs_DSO
component_noinst =
component_install = mca_fs_ufs.la
else
component_noinst = libmca_fs_ufs.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fs_ufs_la_SOURCES = $(sources)
mca_fs_ufs_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_fs_ufs_la_SOURCES = $(sources)
libmca_fs_ufs_la_LDFLAGS = -module -avoid-version

# Source files

sources = \
        fs_ufs.h \
        fs_ufs.c \
        fs_ufs_component.c \
        fs_ufs_file_open.c \
	fs_ufs_file_close.c \
	fs_ufs_file_delete.c \
	fs_ufs_file_sync.c \
	fs_ufs_file_set_size.c \
        fs_ufs_file_get_size.c

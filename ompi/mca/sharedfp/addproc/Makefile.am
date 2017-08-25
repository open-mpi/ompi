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
# Copyright (c) 2013      University of Houston. All rights reserved.
# Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_sharedfp_addproc_DSO
component_noinst =
component_install = mca_sharedfp_addproc.la
else
component_noinst = libmca_sharedfp_addproc.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sharedfp_addproc_la_SOURCES = $(sources)
mca_sharedfp_addproc_la_LDFLAGS = -module -avoid-version
mca_sharedfp_addproc_la_LIBADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_sharedfp_addproc_la_SOURCES = $(sources)
libmca_sharedfp_addproc_la_LDFLAGS = -module -avoid-version

# Source files

#IMPORTANT: Update here when adding new source code files to the library
sources = \
	sharedfp_addproc.h \
	sharedfp_addproc.c \
	sharedfp_addproc_component.c \
	sharedfp_addproc_seek.c \
        sharedfp_addproc_request_position.c \
	sharedfp_addproc_write.c \
	sharedfp_addproc_iwrite.c \
	sharedfp_addproc_read.c \
        sharedfp_addproc_iread.c \
	sharedfp_addproc_file_open.c

#The additional process is spawned by executing this executable
bin_PROGRAMS = mca_sharedfp_addproc_control

mca_sharedfp_addproc_control_SOURCES = \
    sharedfp_addproc_control.h \
    sharedfp_addproc_control.c

mca_sharedfp_addproc_control_LDADD = $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la

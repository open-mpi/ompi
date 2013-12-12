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
# Copyright (c) 2008      University of Houston. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_sharedfp_individual_DSO
component_noinst =
component_install = mca_sharedfp_individual.la
else
component_noinst = libmca_sharedfp_individual.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sharedfp_individual_la_SOURCES = $(sources)
mca_sharedfp_individual_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sharedfp_individual_la_SOURCES = $(sources)
libmca_sharedfp_individual_la_LDFLAGS = -module -avoid-version

# Source files

#IMPORTANT: Update here when adding new source code files to the library
sources = \
	sharedfp_individual.h \
	sharedfp_individual.c \
	sharedfp_individual_component.c \
	sharedfp_individual_seek.c \
	sharedfp_individual_get_position.c \
	sharedfp_individual_collaborate_data.c \
	sharedfp_individual_write.c \
	sharedfp_individual_iwrite.c \
	sharedfp_individual_read.c \
	sharedfp_individual_insert_metadata.c   \
	sharedfp_individual_file_open.c \
	sharedfp_individual_gettime.c

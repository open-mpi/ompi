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

if MCA_BUILD_ompi_fbtl_posix_DSO
component_noinst =
component_install = mca_fbtl_posix.la
else
component_noinst = libmca_fbtl_posix.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fbtl_posix_la_SOURCES = $(sources)
mca_fbtl_posix_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_fbtl_posix_la_SOURCES = $(sources)
libmca_fbtl_posix_la_LDFLAGS = -module -avoid-version

# Source files

sources = \
        fbtl_posix.h \
        fbtl_posix.c \
        fbtl_posix_component.c \
        fbtl_posix_preadv.c \
        fbtl_posix_ipreadv.c \
        fbtl_posix_pwritev.c \
        fbtl_posix_ipwritev.c

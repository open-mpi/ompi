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

if MCA_BUILD_ompi_fcache_ux_DSO
component_noinst =
component_install = mca_fcache_ux.la
else
component_noinst = libmca_fcache_ux.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fcache_ux_la_SOURCES = $(sources)
mca_fcache_ux_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_fcache_ux_la_SOURCES = $(sources)
libmca_fcache_ux_la_LDFLAGS = -module -avoid-version

# Source files

sources = \
        fcache_ux.h \
        fcache_ux.c \
        fcache_ux_component.c \
        fcache_ux_get_file_layout.c \
        fcache_ux_set_file_layout.c \
        fcache_ux_get_io_servers.c

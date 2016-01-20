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
# Copyright (c) 2008-2015 University of Houston. All rights reserved.
# Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        fcoll_dynamic_gen2.h \
        fcoll_dynamic_gen2_module.c \
        fcoll_dynamic_gen2_component.c \
        fcoll_dynamic_gen2_file_read_all.c \
        fcoll_dynamic_gen2_file_write_all.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_ompi_fcoll_dynamic_gen2_DSO
component_noinst =
component_install = mca_fcoll_dynamic_gen2.la
else
component_noinst = libmca_fcoll_dynamic_gen2.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_fcoll_dynamic_gen2_la_SOURCES = $(sources)
mca_fcoll_dynamic_gen2_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_fcoll_dynamic_gen2_la_SOURCES =$(sources)
libmca_fcoll_dynamic_gen2_la_LDFLAGS = -module -avoid-version

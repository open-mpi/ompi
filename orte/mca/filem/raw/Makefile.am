#
# Copyright (c) 2004-2007 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_ortedata_DATA = help-orte-filem-raw.txt

sources = \
        filem_raw.h \
        filem_raw_component.c \
        filem_raw_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_filem_raw_DSO
component_noinst =
component_install = mca_filem_raw.la
else
component_noinst = libmca_filem_raw.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_filem_raw_la_SOURCES = $(sources)
mca_filem_raw_la_LDFLAGS = -module -avoid-version
mca_filem_raw_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_filem_raw_la_SOURCES = $(sources)
libmca_filem_raw_la_LDFLAGS = -module -avoid-version

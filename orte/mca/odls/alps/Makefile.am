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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Los Alamos National Security, LLC.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ortedata_DATA = help-orte-odls-alps.txt

sources = \
        odls_alps.h \
        odls_alps_component.c \
        odls_alps_module.c \
        odls_alps_utils.c 

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_odls_alps_DSO
component_noinst =
component_install = mca_odls_alps.la
else
component_noinst = libmca_odls_alps.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_odls_alps_la_SOURCES = $(sources)
mca_odls_alps_la_CPPFLAGS = $(odls_alps_CPPFLAGS) 
mca_odls_alps_la_LDFLAGS = -module -avoid-version $(odls_alps_LDFLAGS)
mca_odls_alps_la_LIBADD = $(odls_alps_LIBS) \
    $(ORTE_TOP_BUILDDIR)/orte/mca/common/alps/lib@ORTE_LIB_PREFIX@mca_common_alps.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_odls_alps_la_SOURCES =$(sources)
libmca_odls_alps_la_CPPFLAGS = $(odls_alps_CPPFLAGS)
libmca_odls_alps_la_LDFLAGS = -module -avoid-version $(odls_alps_LDFLAGS)
libmca_odls_alps_la_LIBADD = $(odls_alps_LIBS)


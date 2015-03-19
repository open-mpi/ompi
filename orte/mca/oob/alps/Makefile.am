#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2015 Los Alamos National Security, LLC.
#                         All rights reserved
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
          oob_alps_component.c

if MCA_BUILD_orte_oob_alps_DSO
component_noinst =
component_install = mca_oob_alps.la
else
component_noinst = libmca_oob_alps.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_oob_alps_la_SOURCES = $(sources)
mca_oob_alps_la_LDFLAGS = -module -avoid-version
mca_oob_alps_la_LIBADD = $(ooob_alps_LIBS) \
    $(ORTE_TOP_BUILDDIR)/orte/mca/common/alps/lib@ORTE_LIB_PREFIX@mca_common_alps.la


noinst_LTLIBRARIES = $(component_noinst)
libmca_oob_alps_la_SOURCES = $(sources)
libmca_oob_alps_la_LDFLAGS = -module -avoid-version


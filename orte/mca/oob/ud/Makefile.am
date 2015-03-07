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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = $(orte_oob_ud_CPPFLAGS)

dist_ortedata_DATA = help-oob-ud.txt

sources = \
	oob_ud_component.h \
	oob_ud_component.c \
	oob_ud.c \
	oob_ud.h \
	oob_ud_event.c \
	oob_ud_peer.c \
	oob_ud_peer.h \
	oob_ud_ping.c \
	oob_ud_ping.h \
	oob_ud_qp.c \
	oob_ud_qp.h \
	oob_ud_recv.c \
	oob_ud_req.c \
	oob_ud_req.h \
	oob_ud_send.c \
        oob_ud_send.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_oob_ud_DSO
component_noinst =
component_install = mca_oob_ud.la
else
component_noinst = libmca_oob_ud.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_oob_ud_la_SOURCES = $(sources)
mca_oob_ud_la_LDFLAGS = -module -avoid-version $(orte_oob_ud_LDFLAGS)
mca_oob_ud_la_LIBADD = $(orte_oob_ud_LIBS) \
					   $(OPAL_TOP_BUILDDIR)/opal/mca/common/verbs/lib@OPAL_LIB_PREFIX@mca_common_verbs.la

noinst_LTLIBRARIES = $(component_noinst)
libmca_oob_ud_la_SOURCES = $(sources)
libmca_oob_ud_la_LDFLAGS = -module -avoid-version $(orte_oob_ud_LDFLAGS)
libmca_oob_ud_la_LIBADD = $(orte_oob_ud_LIBS)


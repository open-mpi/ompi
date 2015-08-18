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
# Copyright (c) 2012-2013 Los Alamos National Security, LLC.
#                         All rights reserved
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
          oob_usock_component.h \
          oob_usock.h \
          oob_usock_component.c \
          oob_usock_connection.h \
          oob_usock_sendrecv.h \
          oob_usock_hdr.h \
          oob_usock_peer.h \
          oob_usock_ping.h \
          oob_usock.c \
          oob_usock_connection.c \
          oob_usock_sendrecv.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_oob_usock_DSO
component_noinst =
component_install = mca_oob_usock.la
else
component_noinst = libmca_oob_usock.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_oob_usock_la_SOURCES = $(sources)
mca_oob_usock_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_oob_usock_la_SOURCES = $(sources)
libmca_oob_usock_la_LDFLAGS = -module -avoid-version


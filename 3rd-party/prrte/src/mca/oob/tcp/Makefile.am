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
# Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2012-2013 Los Alamos National Security, LLC.
#                         All rights reserved
# Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_prtedata_DATA = help-oob-tcp.txt

sources = \
          oob_tcp_component.h \
          oob_tcp.h \
          oob_tcp_listener.h \
          oob_tcp_common.h \
          oob_tcp_component.c \
          oob_tcp_connection.h \
          oob_tcp_sendrecv.h \
          oob_tcp_hdr.h \
          oob_tcp_peer.h \
          oob_tcp.c \
          oob_tcp_listener.c \
          oob_tcp_common.c \
          oob_tcp_connection.c \
          oob_tcp_sendrecv.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_oob_tcp_DSO
component_noinst =
component_install = prte_mca_oob_tcp.la
else
component_noinst = libprtemca_oob_tcp.la
component_install =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
prte_mca_oob_tcp_la_SOURCES = $(sources)
prte_mca_oob_tcp_la_LDFLAGS = -module -avoid-version
prte_mca_oob_tcp_la_LIBADD = $(top_builddir)/src/libprrte.la

noinst_LTLIBRARIES = $(component_noinst)
libprtemca_oob_tcp_la_SOURCES = $(sources)
libprtemca_oob_tcp_la_LDFLAGS = -module -avoid-version

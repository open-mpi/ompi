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
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-oob-tcp.txt

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
          oob_tcp_ping.h \
          oob_tcp.c \
          oob_tcp_listener.c \
          oob_tcp_common.c \
          oob_tcp_connection.c \
          oob_tcp_sendrecv.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_oob_tcp_DSO
component_noinst =
component_install = mca_oob_tcp.la
else
component_noinst = libmca_oob_tcp.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_oob_tcp_la_SOURCES = $(sources)
mca_oob_tcp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_oob_tcp_la_SOURCES = $(sources)
libmca_oob_tcp_la_LDFLAGS = -module -avoid-version


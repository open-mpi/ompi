# -*- makefile -*-
#
# Copyright (c) 2003 The Trustees of Indiana University.  
#                    All rights reserved.
# 
# This file is part of the CMPI software package.  For license
# information, see the LICENSE file in the top level directory of the
# CMPI source distribution.
#
# $Id: Makefile.am,v 1.2 2003/12/22 16:29:21 twoodall Exp $
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS		= communicator datatype p2p

lib_LTLIBRARIES	= libmpi.la

libmpi_la_SOURCES	=
libmpi_la_LIBADD	= \
	$(top_builddir)/src/mpi/communicator/libcommunicator.la \
	$(top_builddir)/src/mpi/datatype/libdatatype.la \
	$(top_builddir)/src/mpi/p2p/libp2p.la

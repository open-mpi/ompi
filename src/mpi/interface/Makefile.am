# -*- makefile -*-
#
# Copyright (c) 2003 The Trustees of Indiana University.  
#                    All rights reserved.
# 
# This file is part of the CMPI software package.  For license
# information, see the LICENSE file in the top level directory of the
# CMPI source distribution.
#
# $Id: Makefile.am,v 1.1 2003/11/22 16:36:25 jsquyres Exp $
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS		= communicator datatype

lib_LTLIBRARIES	= libmpi.la

libmpi_la_SOURCES	=
libmpi_la_LIBADD	= \
	$(top_builddir)/src/mpi/communicator/libcommunicator.la \
	$(top_builddir)/src/mpi/datatype/libdatatype.la

# -*- makefile -*-
#
# Copyright (c) 2003 The Trustees of Indiana University.  
#                    All rights reserved.
# 
# This file is part of the CMPI software package.  For license
# information, see the LICENSE file in the top level directory of the
# CMPI source distribution.
#
# $Id: Makefile.am,v 1.1 2003/11/22 16:36:58 jsquyres Exp $
#

include $(top_srcdir)/config/Makefile.options

# This is ugly, but we need it so that people can change things at
# "make" time (e.g., "make sysconfdir=/foo/bar all") as documented in
# autoconf.

AM_CPPFLAGS		= \
			-DLAM_PREFIX="\"$(prefix)\"" \
			-DLAM_INCDIR="\"$(includedir)\"" \
			-DLAM_LIBDIR="\"$(libdir)\""

# Stay tuned...
#bin_PROGRAMS		= mpicc mpic++ mpif77 mpif90

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

noinst_HEADERS	= \
		communicator.h \
		datatype.h

include_HEADERS	= \
		cmpi_config.h \
		mpi.h \
		mpif.h

# Add a hook to run *after* the file cmpi_config.h has been installed
# out to the target location.  It changes the pesky PACKAGE_* macros
# that autoconf automatically generates (and there is no way of
# turning off) into CMPI_PACKAGE_* in order to make <mpi.h> safe to
# include with other files.

install-data-hook:
	sed -e 's/define PACKAGE/define CMPI_PACKAGE/' \
		$(DESTDIR)$(includedir)/cmpi_config.h \
		> $(DESTDIR)$(includedir)/cmpi_config.h.install
	mv $(DESTDIR)$(includedir)/cmpi_config.h.install \
		$(DESTDIR)$(includedir)/cmpi_config.h

# -*- makefile -*-
#
# Copyright (c) 2003 The Trustees of Indiana University.  
#                    All rights reserved.
# 
# This file is part of the CMPI software package.  For license
# information, see the LICENSE file in the top level directory of the
# CMPI source distribution.
#
# $Id: Makefile.am,v 1.3 2003/12/22 16:29:11 twoodall Exp $
#

include $(top_srcdir)/config/Makefile.options

noinst_HEADERS	= \
		communicator.h \
		datatype.h \
		group.h \
		totalview.h

include_HEADERS	= \
		lam_config.h \
		lam_stdint.h \
		mpi.h \
		mpif.h

# Add a hook to run *after* the file lam_config.h has been installed
# out to the target location.  It changes the pesky PACKAGE_* macros
# that autoconf automatically generates (and there is no way of
# turning off) into CMPI_PACKAGE_* in order to make <mpi.h> safe to
# include with other files.

install-data-hook:
	sed -e 's/define PACKAGE/define CMPI_PACKAGE/' \
		$(DESTDIR)$(includedir)/lam_config.h \
		> $(DESTDIR)$(includedir)/lam_config.h.install
	mv $(DESTDIR)$(includedir)/lam_config.h.install \
		$(DESTDIR)$(includedir)/lam_config.h

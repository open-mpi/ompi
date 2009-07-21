#
# Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

noinst_LTLIBRARIES = libmca_installdirs_autodetect.la

libmca_installdirs_autodetect_la_SOURCES = opal_installdirs_autodetect_component.c

EXTRA_libmca_installdirs_autodetect_la_SOURCES = \
        opal_installdirs_solaris.c \
	opal_installdirs_linux.c \
	opal_installdirs_backtrace.c \
	opal_installdirs_walkcontext.c

libmca_installdirs_autodetect_la_LIBADD = \
	$(OMPI_INSTALLDIRS_AUTODETECT_PATH) \
	$(OMPI_INSTALLDIRS_AUTODETECT_PC)

libmca_installdirs_autodetect_la_DEPENDENCIES = \
	$(OMPI_INSTALLDIRS_AUTODETECT_PATH) \
	$(OMPI_INSTALLDIRS_AUTODETECT_PC)

# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = config include src test

EXTRA_DIST = README INSTALL VERSION Doxyfile

dist-hook:
	csh config/distscript.csh

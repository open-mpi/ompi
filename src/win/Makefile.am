# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libwin.la

# Source code files

headers = \
        win.h 

libwin_la_SOURCES = \
        $(headers) \
	win.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/win
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

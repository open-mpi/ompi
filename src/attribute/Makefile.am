# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libattribute.la

# Source code files

headers = \
        attribute.h

libattribute_la_SOURCES = \
        $(headers) \
        attribute.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/attribute
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

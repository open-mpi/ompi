# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

#libraries
noinst_LTLIBRARIES = libmpi_attribute.la
# Source code files

headers = \
        attribute.h

libmpi_attribute_la_SOURCES = \
        $(headers) \
        attribute.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/attribute
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

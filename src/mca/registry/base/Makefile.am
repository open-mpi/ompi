#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_registry_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h

# Library

libmca_registry_base_la_SOURCES = \
        $(headers) \
        registry_base_close.c \
        registry_base_open.c \
        registry_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/mca/registry/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

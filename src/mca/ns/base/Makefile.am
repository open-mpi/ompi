#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_ns_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h

# Library

libmca_ns_base_la_SOURCES = \
        $(headers) \
        ns_base_close.c \
        ns_base_select.c \
        ns_base_open.c \
        ns_base_functions.c


# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/ns/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

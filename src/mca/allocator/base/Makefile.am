#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_allocator_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h 

libmca_allocator_base_la_SOURCES = \
        $(headers) \
        allocator_base_open.c \
        allocator_base_close.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/mca/allocator/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif


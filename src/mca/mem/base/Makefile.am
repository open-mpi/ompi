#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_mem_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h \
	mem_base_allocator.h 

libmca_mem_base_la_SOURCES = \
        $(headers) \
        mem_base_open.c \
        mem_base_close.c \
	mem_base_allocator.c \
        mem_base_select.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/mca/mem/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif


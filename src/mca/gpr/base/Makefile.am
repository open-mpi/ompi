#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_gpr_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h

libmca_gpr_base_la_SOURCES = \
        $(headers) \
        gpr_base_open.c \
        gpr_base_close.c \
        gpr_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/gpr/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif


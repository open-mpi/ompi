#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_svc_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h 

libmca_svc_base_la_SOURCES = \
        $(headers) \
        svc_base_open.c \
        svc_base_close.c \
        svc_base_init.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/mca/svc/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif


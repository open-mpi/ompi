#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_pcmclient_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h

# Library

libmca_pcmclient_base_la_SOURCES = \
        $(headers) \
        pcmclient_base_close.c \
        pcmclient_base_open.c \
        pcmclient_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/pcmclient/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

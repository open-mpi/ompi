#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libfile.la

# Source code files

headers = \
        file.h

libfile_la_SOURCES = \
        $(headers) \
        file.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/file
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

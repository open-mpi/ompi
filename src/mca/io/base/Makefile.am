#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_io_base.la

headers = \
        base.h \
        io_base_request.h

libmca_io_base_la_SOURCES = \
	$(headers) \
	io_base_close.c \
        io_base_delete.c \
	io_base_file_select.c \
	io_base_find_available.c \
	io_base_open.c \
        io_base_request.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/io/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

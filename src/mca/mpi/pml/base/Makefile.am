#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_mpi_pml_base.la

# Source code files

headers = \
        base.h \
	pml_base_request.h

libmca_mpi_pml_base_la_SOURCES = \
        $(headers) \
	pml_base_close.c \
	pml_base_request.c \
	pml_base_open.c \
	pml_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/mpi/pml/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif


#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_mpi_base.la

headers = \
        base.h

libmca_mpi_base_la_SOURCES = \
        $(headers) \
        mca_mpi_base_open.c \
        mca_mpi_init_select_modules.c \
        mca_mpi_mem.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/mpi/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_mpi_coll_base.la

headers = \
        base.h

libmca_mpi_coll_base_la_SOURCES = \
        coll_base_open.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/mpi/coll/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = lam mpi

# Source code files

headers = mca.h

# Library

noinst_LTLIBRARIES = libmca_lam.la libmca_mpi.la

libmca_mpi_la_SOURCES =
libmca_mpi_la_LIBADD = mpi/libmca_mpi_convenience.la
libmca_mpi_la_DEPENDENCIES = $(libmca_mpi_la_LIBADD)

libmca_lam_la_SOURCES =
libmca_lam_la_LIBADD = lam/libmca_lam_convenience.la
libmca_lam_la_DEPENDENCIES = $(libmca_lam_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

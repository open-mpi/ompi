#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_ptl_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_ptl_ALL_SUBDIRS)

# Source code files

headers = ptl.h

noinst_LTLIBRARIES = libmca_mpi_ptl.la
libmca_mpi_ptl_la_SOURCES =
libmca_mpi_ptl_la_LIBADD = \
        base/libmca_mpi_ptl_base.la \
        $(MCA_ptl_STATIC_LTLIBS)
libmca_mpi_ptl_la_DEPENDENCIES = $(libmca_mpi_ptl_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/mpi/ptl
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

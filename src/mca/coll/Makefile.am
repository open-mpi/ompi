#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_coll_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_coll_ALL_SUBDIRS)

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = coll.h

noinst_LTLIBRARIES = libmca_mpi_coll.la
libmca_mpi_coll_la_SOURCES =
libmca_mpi_coll_la_LIBADD = \
        base/libmca_mpi_coll_base.la \
        $(MCA_coll_STATIC_LTLIBS)
libmca_mpi_coll_la_DEPENDENCIES = $(libmca_mpi_coll_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/mpi/coll
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

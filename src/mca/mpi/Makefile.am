#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# base should be listed first because it should be built first
# (probably not for any technical rason, but it makes sense to build
# the base first).  common_mpi must be second, however, because
# modules under there may be statically linked to other modules.

SUBDIRS = base common_mpi coll io one pml ptl 

# Library

noinst_LTLIBRARIES = libmca_mpi_convenience.la
libmca_mpi_convenience_la_SOURCES =
libmca_mpi_convenience_la_LIBADD = \
        base/libmca_mpi_base.la \
        coll/libmca_mpi_coll.la \
        pml/libmca_mpi_pml.la \
        ptl/libmca_mpi_ptl.la
libmca_mpi_convenience_la_DEPENDENCIES = $(libmca_mpi_convenience_la_LIBADD)

#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

SUBDIRS = include adio mpi-io 
DIST_SUBDIRS = $(SUBDIRS) doc

EXTRA_DIST = README COPYRIGHT README_OMPI

noinst_LTLIBRARIES = libromio_dist.la 
libromio_dist_la_SOURCES = 
libromio_dist_la_LIBADD  = \
	adio/libadio.la \
	mpi-io/libmpi-io.la \
	$(MCA_io_romio_STATIC_LTLIBS)
libromio_dist_la_DEPENDENCIES = \
	$(libmca_io_romio_romio_dist_la_LIBADD)
libromio_dist_la_LDFLAGS = -module -avoid-version

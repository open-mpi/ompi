#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# Source code files
headers = acconfig.h

AM_CPPFLAGS = -DHAVE_MPI_INFO #-DMPIO_INCLUDE

#SUBDIRS = include adio mpi2-other mpi-io util 
SUBDIRS = include adio mpi-io 

noinst_LTLIBRARIES = libmca_io_romio_romio_dist.la 

libmca_io_romio_romio_dist_la_SOURCES = 
libmca_io_romio_romio_dist_la_LIBADD  = \
	adio/libmca_io_romio_romio_dist_adio.la \
	mpi-io/libmca_io_romio_romio_dist_mpi_io.la 
	$(MCA_io_romio_STATIC_LTLIBS)

libmca_io_romio_romio_dist_la_DEPENDENCIES = \
	$(libmca_io_romio_romio_dist_la_LIBADD)

libmca_io_romio_romio_dist_la_LDFLAGS = -module -avoid-version

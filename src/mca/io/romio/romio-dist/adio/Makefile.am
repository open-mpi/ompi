#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DHAVE_MPI_INFO \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

SUBDIRS = common ad_ufs ad_nfs #ad_pvfs ad_xfs

# Library
noinst_LTLIBRARIES = libmca_io_romio_romio_dist_adio.la
libmca_io_romio_romio_dist_adio_la_SOURCES =
libmca_io_romio_romio_dist_adio_la_LIBADD = \
        common/libmca_io_romio_romio_dist_adio_common.la \
        ad_ufs/libmca_io_romio_romio_dist_adio_ad_ufs.la \
        ad_nfs/libmca_io_romio_romio_dist_adio_ad_nfs.la 
	
libmca_io_romio_romio_dist_adio_la_DEPENDENCIES = \
        $(libmca_io_romio_romio_dist_adio_la_LIBADD)


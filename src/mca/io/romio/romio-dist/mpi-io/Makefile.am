#
# $HEADER$
#
# Open MPI: Note that we ignore the fortran subdirectory here; we only
# need C bindings.  We also ignore the f2c / c2f source files; we
# don't need them.
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

# It is unfortunately necessary to make all the filenames in the ROMIO
# distribtion confrom to the MCA prefix name rule -- there is at least
# one or two filename conflicts that potentially create problems when
# merging everything into onelibmpi.

# In developer's copies, we have the real ROMIO files, and sym links
# are made.  In distribution tarballs, the sym-linked files turn into
# real files and the original filenames do not appear.

noinst_LTLIBRARIES = libmpi-io.la
libmpi_io_la_SOURCES = \
        mca_io_romio_dist_close.c \
        mca_io_romio_dist_delete.c \
        mca_io_romio_dist_fsync.c \
        mca_io_romio_dist_get_amode.c \
        mca_io_romio_dist_get_atom.c \
        mca_io_romio_dist_get_bytoff.c \
        mca_io_romio_dist_get_errh.c \
        mca_io_romio_dist_get_extent.c \
        mca_io_romio_dist_get_group.c \
        mca_io_romio_dist_get_info.c \
        mca_io_romio_dist_get_posn.c \
        mca_io_romio_dist_get_posn_sh.c \
        mca_io_romio_dist_get_size.c \
        mca_io_romio_dist_get_view.c \
        mca_io_romio_dist_ioreq_c2f.c \
        mca_io_romio_dist_ioreq_f2c.c \
        mca_io_romio_dist_iotest.c \
        mca_io_romio_dist_iowait.c \
        mca_io_romio_dist_iread_at.c \
        mca_io_romio_dist_iread.c \
        mca_io_romio_dist_iread_sh.c \
        mca_io_romio_dist_iwrite_at.c \
        mca_io_romio_dist_iwrite.c \
        mca_io_romio_dist_iwrite_sh.c \
        mpioimpl.h \
        mpioprof.h \
        mca_io_romio_dist_open.c \
        mca_io_romio_dist_prealloc.c \
        mca_io_romio_dist_rd_atallb.c \
        mca_io_romio_dist_rd_atalle.c \
        mca_io_romio_dist_read_allb.c \
        mca_io_romio_dist_read_all.c \
        mca_io_romio_dist_read_alle.c \
        mca_io_romio_dist_read_atall.c \
        mca_io_romio_dist_read_at.c \
        mca_io_romio_dist_read.c \
        mca_io_romio_dist_read_ordb.c \
        mca_io_romio_dist_read_ord.c \
        mca_io_romio_dist_read_orde.c \
        mca_io_romio_dist_read_sh.c \
        mca_io_romio_dist_seek.c \
        mca_io_romio_dist_seek_sh.c \
        mca_io_romio_dist_set_atom.c \
        mca_io_romio_dist_set_errh.c \
        mca_io_romio_dist_set_info.c \
        mca_io_romio_dist_set_size.c \
        mca_io_romio_dist_set_view.c \
        mca_io_romio_dist_wr_atallb.c \
        mca_io_romio_dist_wr_atalle.c \
        mca_io_romio_dist_write_allb.c \
        mca_io_romio_dist_write_all.c \
        mca_io_romio_dist_write_alle.c \
        mca_io_romio_dist_write_atall.c \
        mca_io_romio_dist_write_at.c \
        mca_io_romio_dist_write.c \
        mca_io_romio_dist_write_ordb.c \
        mca_io_romio_dist_write_ord.c \
        mca_io_romio_dist_write_orde.c \
        mca_io_romio_dist_write_sh.c

#
# Sym link the sources to conform to the MCA prefix name rule
#
dir=$(top_srcdir)/mpi-io
$(libmpi_io_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '19-'` ; \
		ln -s $(dir)/$$pname $@ ; \
	fi

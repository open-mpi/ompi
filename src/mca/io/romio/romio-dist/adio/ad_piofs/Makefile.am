#
# $HEADER$
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

noinst_LTLIBRARIES = libadio_piofs.la
libadio_piofs_la_SOURCES = \
        mca_io_romio_dist_ad_piofs.c \
        ad_piofs.h \
        mca_io_romio_dist_ad_piofs_close.c \
        mca_io_romio_dist_ad_piofs_done.c \
        mca_io_romio_dist_ad_piofs_fcntl.c \
        mca_io_romio_dist_ad_piofs_flush.c \
        mca_io_romio_dist_ad_piofs_hints.c \
        mca_io_romio_dist_ad_piofs_iread.c \
        mca_io_romio_dist_ad_piofs_iwrite.c \
        mca_io_romio_dist_ad_piofs_open.c \
        mca_io_romio_dist_ad_piofs_rdcoll.c \
        mca_io_romio_dist_ad_piofs_read.c \
        mca_io_romio_dist_ad_piofs_resize.c \
        mca_io_romio_dist_ad_piofs_seek.c \
        mca_io_romio_dist_ad_piofs_wait.c \
        mca_io_romio_dist_ad_piofs_wrcoll.c \
        mca_io_romio_dist_ad_piofs_write.c

#
# Sym link the sources to conform to the MCA prefix name rule
#
dir=$(top_srcdir)/adio/ad_piofs
$(libadio_piofs_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '19-'` ; \
		ln -s $(dir)/$$pname $@ ; \
	fi

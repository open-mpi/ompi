#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include \
        -I$(top_srcdir)/adio/include

# It is unfortunately necessary to make all the filenames in the ROMIO
# distribtion confrom to the MCA prefix name rule -- there is at least
# one or two filename conflicts that potentially create problems when
# merging everything into onelibmpi.

# In developer's copies, we have the real ROMIO files, and sym links
# are made.  In distribution tarballs, the sym-linked files turn into
# real files and the original filenames do not appear.

noinst_LTLIBRARIES = libadio_ufs.la
libadio_ufs_la_SOURCES = \
        mca_io_romio_dist_ad_ufs.c \
        mca_io_romio_dist_ad_ufs_close.c \
        mca_io_romio_dist_ad_ufs_done.c \
        mca_io_romio_dist_ad_ufs_fcntl.c \
        mca_io_romio_dist_ad_ufs_flush.c \
        ad_ufs.h \
        mca_io_romio_dist_ad_ufs_hints.c \
        mca_io_romio_dist_ad_ufs_iread.c \
        mca_io_romio_dist_ad_ufs_iwrite.c \
        mca_io_romio_dist_ad_ufs_open.c \
        mca_io_romio_dist_ad_ufs_rdcoll.c \
        mca_io_romio_dist_ad_ufs_read.c \
        mca_io_romio_dist_ad_ufs_resize.c \
        mca_io_romio_dist_ad_ufs_seek.c \
        mca_io_romio_dist_ad_ufs_wait.c \
        mca_io_romio_dist_ad_ufs_wrcoll.c \
        mca_io_romio_dist_ad_ufs_write.c

#
# Sym link the sources to conform to the MCA prefix name rule
#
dir=$(top_srcdir)/adio/ad_ufs
$(libadio_ufs_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '19-'` ; \
		rm -f $@; \
		ln -s $(dir)/$$pname $@ ; \
	fi

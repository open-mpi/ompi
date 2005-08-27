#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_srcdir) \
        -I$(top_ompi_srcdir)/include \
        -I$(top_ompi_builddir)/include \
        -I$(top_ompi_srcdir)/opal \
        -I$(top_ompi_srcdir)/opal/include \
        -I$(top_ompi_srcdir)/orte \
        -I$(top_ompi_srcdir)/orte/include \
        -I$(top_ompi_srcdir)/ompi \
        -I$(top_ompi_srcdir)/ompi/include \
        -I$(top_srcdir)/adio/include

# It is unfortunately necessary to make all the filenames in the ROMIO
# distribtion confrom to the MCA prefix name rule -- there is at least
# one or two filename conflicts that potentially create problems when
# merging everything into onelibmpi.

# In developer's copies, we have the real ROMIO files, and sym links
# are made.  In distribution tarballs, the sym-linked files turn into
# real files and the original filenames do not appear.

noinst_LTLIBRARIES = libadio_xfs.la
libadio_xfs_la_SOURCES = \
        io_romio_ad_xfs.c \
        ad_xfs.h \
        io_romio_ad_xfs_close.c \
        io_romio_ad_xfs_done.c \
        io_romio_ad_xfs_fcntl.c \
        io_romio_ad_xfs_flush.c \
        io_romio_ad_xfs_hints.c \
        io_romio_ad_xfs_iread.c \
        io_romio_ad_xfs_iwrite.c \
        io_romio_ad_xfs_open.c \
        io_romio_ad_xfs_rdcoll.c \
        io_romio_ad_xfs_read.c \
        io_romio_ad_xfs_resize.c \
        io_romio_ad_xfs_seek.c \
        io_romio_ad_xfs_wait.c \
        io_romio_ad_xfs_wrcoll.c \
        io_romio_ad_xfs_write.c


#
# Sym link the sources to conform to the MCA prefix name rule
#
dir=$(top_srcdir)/adio/ad_xfs
$(libadio_xfs_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '10-'` ; \
		rm -f $@; \
		ln -s $(dir)/$$pname $@ ; \
	fi

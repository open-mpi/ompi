#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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

include $(top_srcdir)/Makefile.options

# It is unfortunately necessary to make all the filenames in the ROMIO
# distribtion confrom to the MCA prefix name rule -- there is at least
# one or two filename conflicts that potentially create problems when
# merging everything into onelibmpi.

# In developer's copies, we have the real ROMIO files, and sym links
# are made.  In distribution tarballs, the sym-linked files turn into
# real files and the original filenames do not appear.

noinst_LTLIBRARIES = libadio_testfs.la
libadio_testfs_la_SOURCES = \
        io_romio_ad_testfs.c \
        ad_testfs.h \
        io_romio_ad_testfs_close.c \
        io_romio_ad_testfs_delete.c \
        io_romio_ad_testfs_done.c \
        io_romio_ad_testfs_fcntl.c \
        io_romio_ad_testfs_flush.c \
        io_romio_ad_testfs_getsh.c \
        io_romio_ad_testfs_hints.c \
        io_romio_ad_testfs_iread.c \
        io_romio_ad_testfs_iwrite.c \
        io_romio_ad_testfs_open.c \
        io_romio_ad_testfs_rdcoll.c \
        io_romio_ad_testfs_read.c \
        io_romio_ad_testfs_resize.c \
        io_romio_ad_testfs_seek.c \
        io_romio_ad_testfs_setsh.c \
        io_romio_ad_testfs_wait.c \
        io_romio_ad_testfs_wrcoll.c \
        io_romio_ad_testfs_write.c

#
# Sym link the sources to conform to the MCA prefix name rule
#
dir=$(top_srcdir)/adio/ad_testfs
$(libadio_testfs_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '10-'` ; \
		rm -f $@; \
		ln -s $(dir)/$$pname $@ ; \
	fi

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
        -I$(top_ompi_srcdir)/include \
        -I$(top_ompi_builddir)/include \
        -I$(top_ompi_builddir)/opal \
        -I$(top_ompi_builddir)/opal/include \
        -I$(top_ompi_srcdir)/opal \
        -I$(top_ompi_srcdir)/opal/include \
        -I$(top_ompi_builddir)/orte \
        -I$(top_ompi_builddir)/orte/include \
        -I$(top_ompi_srcdir)/orte \
        -I$(top_ompi_srcdir)/orte/include \
        -I$(top_ompi_builddir)/ompi \
        -I$(top_ompi_builddir)/ompi/include \
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

noinst_LTLIBRARIES = libadio_common.la
libadio_common_la_SOURCES = \
        mca_io_romio_dist_ad_aggregate.c \
        mca_io_romio_dist_ad_close.c \
        mca_io_romio_dist_ad_delete.c \
        mca_io_romio_dist_ad_end.c \
        mca_io_romio_dist_ad_flush.c \
        mca_io_romio_dist_ad_fstype.c \
        mca_io_romio_dist_ad_get_sh_fp.c \
        mca_io_romio_dist_ad_hints.c \
        mca_io_romio_dist_ad_init.c \
        mca_io_romio_dist_ad_open.c \
        mca_io_romio_dist_ad_read_coll.c \
        mca_io_romio_dist_ad_read_str.c \
        mca_io_romio_dist_ad_read_str_naive.c \
        mca_io_romio_dist_ad_seek.c \
        mca_io_romio_dist_ad_set_sh_fp.c \
        mca_io_romio_dist_ad_set_view.c \
        mca_io_romio_dist_ad_write_coll.c \
        mca_io_romio_dist_ad_write_str.c \
        mca_io_romio_dist_async_list.c \
        mca_io_romio_dist_byte_offset.c \
        mca_io_romio_dist_cb_config_list.c \
        mca_io_romio_dist_eof_offset.c \
        mca_io_romio_dist_error.c \
        mca_io_romio_dist_flatten.c \
        mca_io_romio_dist_gencheck.c \
        mca_io_romio_dist_get_fp_posn.c \
        mca_io_romio_dist_iscontig.c \
        mca_io_romio_dist_lock.c \
        mca_io_romio_dist_malloc.c \
        mca_io_romio_dist_req_malloc.c \
        mca_io_romio_dist_setfn.c \
        mca_io_romio_dist_shfp_fname.c \
        mca_io_romio_dist_status_setb.c

#
# Sym link the sources to conform to the MCA prefix name rule
#
dir=$(top_srcdir)/adio/common
$(libadio_common_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '19-'` ; \
		rm -f $@; \
		ln -s $(dir)/$$pname $@ ; \
	fi

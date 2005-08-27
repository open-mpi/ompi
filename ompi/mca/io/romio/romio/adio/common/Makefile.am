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

noinst_LTLIBRARIES = libadio_common.la
libadio_common_la_SOURCES = \
        io_romio_ad_aggregate.c \
        io_romio_ad_close.c \
        io_romio_ad_delete.c \
        io_romio_ad_end.c \
        io_romio_ad_flush.c \
        io_romio_ad_fstype.c \
        io_romio_ad_get_sh_fp.c \
        io_romio_ad_hints.c \
        io_romio_ad_init.c \
        io_romio_ad_open.c \
        io_romio_ad_read_coll.c \
        io_romio_ad_read_str.c \
        io_romio_ad_read_str_naive.c \
        io_romio_ad_seek.c \
        io_romio_ad_set_sh_fp.c \
        io_romio_ad_set_view.c \
        io_romio_ad_write_coll.c \
        io_romio_ad_write_str.c \
        io_romio_async_list.c \
        io_romio_byte_offset.c \
        io_romio_cb_config_list.c \
        io_romio_eof_offset.c \
        io_romio_error.c \
        io_romio_flatten.c \
        io_romio_gencheck.c \
        io_romio_get_fp_posn.c \
        io_romio_iscontig.c \
        io_romio_lock.c \
        io_romio_malloc.c \
        io_romio_req_malloc.c \
        io_romio_setfn.c \
        io_romio_shfp_fname.c \
        io_romio_status_setb.c

#
# Sym link the sources to conform to the MCA prefix name rule
#
dir=$(top_srcdir)/adio/common
$(libadio_common_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '10-'` ; \
		rm -f $@; \
		ln -s $(dir)/$$pname $@ ; \
	fi

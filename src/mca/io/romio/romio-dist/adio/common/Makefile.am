# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DHAVE_MPI_INFO \
        -DNO_AIO -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include \
        -I../../adio/include -I../../include -I../../../src

noinst_LTLIBRARIES = libmca_io_romio_romio_dist_adio_common.la
libmca_io_romio_romio_dist_adio_common_la_SOURCES = \
        ad_close.c ad_init.c ad_end.c ad_open.c setfn.c flatten.c \
        async_list.c req_malloc.c malloc.c ad_read_coll.c eof_offset.c \
        ad_write_coll.c lock.c ad_read_str.c ad_write_str.c iscontig.c \
        get_fp_posn.c ad_seek.c ad_delete.c ad_flush.c ad_hints.c error.c \
        ad_fstype.c ad_get_sh_fp.c ad_set_sh_fp.c shfp_fname.c \
        byte_offset.c status_setb.c ad_aggregate.c cb_config_list.c \
        ad_read_str_naive.c gencheck.c ad_set_view.c


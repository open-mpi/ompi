#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libadio_common.la
libadio_common_la_SOURCES = \
        ad_aggregate.c \
        ad_close.c \
        ad_delete.c \
        ad_end.c \
        ad_flush.c \
        ad_fstype.c \
        ad_get_sh_fp.c \
        ad_hints.c \
        ad_init.c \
        ad_open.c \
        ad_read_coll.c \
        ad_read_str.c \
        ad_read_str_naive.c \
        ad_seek.c \
        ad_set_sh_fp.c \
        ad_set_view.c \
        ad_write_coll.c \
        ad_write_str.c \
        async_list.c \
        byte_offset.c \
        cb_config_list.c \
        eof_offset.c \
        error.c \
        flatten.c \
        gencheck.c \
        get_fp_posn.c \
        iscontig.c \
        lock.c \
        malloc.c \
        req_malloc.c \
        setfn.c \
        shfp_fname.c \
        status_setb.c

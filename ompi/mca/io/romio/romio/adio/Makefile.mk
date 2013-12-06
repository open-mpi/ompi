## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

AM_CPPFLAGS += -I$(top_builddir)/adio/include -I$(top_srcdir)/adio/include

noinst_HEADERS +=                      \
    adio/include/adio.h                \
    adio/include/adio_cb_config_list.h \
    adio/include/adio_extern.h         \
    adio/include/adioi.h               \
    adio/include/adioi_errmsg.h        \
    adio/include/adioi_error.h         \
    adio/include/adioi_fs_proto.h      \
    adio/include/heap-sort.h           \
    adio/include/mpio_error.h          \
    adio/include/mpipr.h               \
    adio/include/mpiu_greq.h           \
    adio/include/nopackage.h           \
    adio/include/mpiu_external32.h     \
    adio/include/romioconf-undefs.h

include $(top_srcdir)/adio/ad_bg/Makefile.mk
include $(top_srcdir)/adio/ad_bgl/Makefile.mk
include $(top_srcdir)/adio/ad_bglockless/Makefile.mk
include $(top_srcdir)/adio/ad_gridftp/Makefile.mk
include $(top_srcdir)/adio/ad_hfs/Makefile.mk
include $(top_srcdir)/adio/ad_lustre/Makefile.mk
include $(top_srcdir)/adio/ad_nfs/Makefile.mk
## NTFS builds are handled entirely by the separate Windows build system
##include $(top_srcdir)/adio/ad_ntfs/Makefile.mk
include $(top_srcdir)/adio/ad_panfs/Makefile.mk
include $(top_srcdir)/adio/ad_pfs/Makefile.mk
include $(top_srcdir)/adio/ad_piofs/Makefile.mk
include $(top_srcdir)/adio/ad_pvfs/Makefile.mk
include $(top_srcdir)/adio/ad_pvfs2/Makefile.mk
include $(top_srcdir)/adio/ad_sfs/Makefile.mk
include $(top_srcdir)/adio/ad_testfs/Makefile.mk
include $(top_srcdir)/adio/ad_ufs/Makefile.mk
include $(top_srcdir)/adio/ad_xfs/Makefile.mk
include $(top_srcdir)/adio/ad_zoidfs/Makefile.mk
include $(top_srcdir)/adio/common/Makefile.mk


##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
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
    adio/include/ad_tuning.h           \
    adio/include/heap_sort.h           \
    adio/include/lock_internal.h       \
    adio/include/mpio_error.h          \
    adio/include/mpipr.h               \
    adio/include/mpiu_greq.h           \
    adio/include/nopackage.h           \
    adio/include/romioconf-undefs.h    \
    adio/include/mpiu_external32.h     \
    adio/include/hint_fns.h

include $(top_srcdir)/adio/ad_daos/Makefile.mk
include $(top_srcdir)/adio/ad_gpfs/Makefile.mk
include $(top_srcdir)/adio/ad_gpfs/bg/Makefile.mk
include $(top_srcdir)/adio/ad_gpfs/pe/Makefile.mk
include $(top_srcdir)/adio/ad_lustre/Makefile.mk
include $(top_srcdir)/adio/ad_nfs/Makefile.mk
## NTFS builds are handled entirely by the separate Windows build system
##include $(top_srcdir)/adio/ad_ntfs/Makefile.mk
include $(top_srcdir)/adio/ad_panfs/Makefile.mk
include $(top_srcdir)/adio/ad_pvfs2/Makefile.mk
include $(top_srcdir)/adio/ad_testfs/Makefile.mk
include $(top_srcdir)/adio/ad_ufs/Makefile.mk
include $(top_srcdir)/adio/ad_xfs/Makefile.mk
include $(top_srcdir)/adio/ad_ime/Makefile.mk
include $(top_srcdir)/adio/ad_quobytefs/Makefile.mk
include $(top_srcdir)/adio/common/Makefile.mk

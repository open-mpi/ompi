## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

romio_other_sources +=                  \
    adio/common/ad_aggregate.c          \
    adio/common/ad_aggregate_new.c      \
    adio/common/ad_close.c              \
    adio/common/ad_coll_build_req_new.c \
    adio/common/ad_coll_exch_new.c      \
    adio/common/ad_darray.c             \
    adio/common/ad_delete.c             \
    adio/common/ad_done.c               \
    adio/common/ad_done_fake.c          \
    adio/common/ad_end.c                \
    adio/common/ad_fcntl.c              \
    adio/common/ad_features.c           \
    adio/common/ad_flush.c              \
    adio/common/ad_fstype.c             \
    adio/common/ad_get_sh_fp.c          \
    adio/common/ad_hints.c              \
    adio/common/ad_init.c               \
    adio/common/ad_io_coll.c            \
    adio/common/ad_iopen.c              \
    adio/common/ad_iread.c              \
    adio/common/ad_iread_coll.c         \
    adio/common/ad_iread_fake.c         \
    adio/common/ad_iwrite.c             \
    adio/common/ad_iwrite_coll.c        \
    adio/common/ad_iwrite_fake.c        \
    adio/common/ad_open.c               \
    adio/common/ad_opencoll.c           \
    adio/common/ad_opencoll_failsafe.c  \
    adio/common/ad_opencoll_scalable.c  \
    adio/common/ad_prealloc.c           \
    adio/common/ad_read.c               \
    adio/common/ad_read_coll.c          \
    adio/common/ad_read_str.c           \
    adio/common/ad_read_str_naive.c     \
    adio/common/ad_resize.c             \
    adio/common/ad_seek.c               \
    adio/common/ad_set_sh_fp.c          \
    adio/common/ad_set_view.c           \
    adio/common/ad_subarray.c           \
    adio/common/ad_wait.c               \
    adio/common/ad_wait_fake.c          \
    adio/common/ad_write.c              \
    adio/common/ad_write_coll.c         \
    adio/common/ad_write_nolock.c       \
    adio/common/ad_write_str.c          \
    adio/common/ad_write_str_naive.c    \
    adio/common/adi_close.c             \
    adio/common/byte_offset.c           \
    adio/common/cb_config_list.c        \
    adio/common/eof_offset.c            \
    adio/common/error.c                 \
    adio/common/flatten.c               \
    adio/common/get_fp_posn.c           \
    adio/common/greq_fns.c              \
    adio/common/heap-sort.c             \
    adio/common/iscontig.c              \
    adio/common/lock.c                  \
    adio/common/malloc.c                \
    adio/common/shfp_fname.c            \
    adio/common/status_setb.c           \
    adio/common/strfns.c                \
    adio/common/system_hints.c          \
    adio/common/hint_fns.c              \
    adio/common/ad_threaded_io.c        \
    adio/common/p2p_aggregation.c       \
    adio/common/onesided_aggregation.c  \
    adio/common/utils.c


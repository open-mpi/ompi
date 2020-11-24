/*
 *  Copyright 2016 Cray Inc. All Rights Reserved.
 */
#include "adio.h"

#include "ad_lustre.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdint.h>

/* If necessary (older luster client headers) define the new
   locking structures. */






//#define LOCK_AHEAD_DEBUG

#ifndef LL_IOC_LADVISE
#define LL_IOC_LADVISE                  _IOR('f', 250, struct llapi_lu_ladvise)

enum lu_ladvise_type {
    LU_LADVISE_INVALID = 0,
    LU_LADVISE_WILLREAD = 1,
    LU_LADVISE_DONTNEED = 2,
    LU_LADVISE_LOCKNOEXPAND = 3,
    LU_LADVISE_LOCKAHEAD = 4,
    LU_LADVISE_MAX
};

#define LU_LADVISE_NAMES {                                              \
        [LU_LADVISE_WILLREAD]   = "willread",                           \
        [LU_LADVISE_DONTNEED]   = "dontneed",                           \
        [LU_LADVISE_LOCKNOEXPAND] = "locknoexpand",                     \
        [LU_LADVISE_LOCKAHEAD] = "lockahead",                       \
}

/* This is the userspace argument for ladvise.  It is currently the same as
 * what goes on the wire (struct lu_ladvise), but is defined separately as we
 * may need info which is only used locally. */
struct llapi_lu_ladvise {
    __u16 lla_advice;           /* advice type */
    __u16 lla_value1;           /* values for different advice types */
    __u32 lla_value2;
    __u64 lla_start;            /* first byte of extent for advice */
    __u64 lla_end;              /* last byte of extent for advice */
    __u32 lla_value3;
    __u32 lla_value4;
};
enum ladvise_flag {
    LF_ASYNC = 0x00000001,
    LF_UNSET = 0x00000002,
    /* For lock requests */
    LF_NONBLOCK = 0x00000003,
};

#define LADVISE_MAGIC 0x1ADF1CE0
/* Masks of valid flags for each advice */
#define LF_LOCKNOEXPAND_MASK LF_UNSET
#define LF_LOCKAHEAD_MASK LF_NONBLOCK
/* Flags valid for all advices not explicitly specified */
#define LF_DEFAULT_MASK LF_ASYNC
/* All flags */
#define LF_MASK (LF_ASYNC | LF_UNSET | LF_NONBLOCK)

#define lla_lockahead_mode   lla_value1
#define lla_peradvice_flags    lla_value2
#define lla_lockahead_result lla_value3

/* This is the userspace argument for ladvise, corresponds to ladvise_hdr which
 * is used on the wire.  It is defined separately as we may need info which is
 * only used locally. */
struct llapi_ladvise_hdr {
    __u32 lah_magic;            /* LADVISE_MAGIC */
    __u32 lah_count;            /* number of advices */
    __u64 lah_flags;            /* from enum ladvise_flag */
    __u32 lah_value1;           /* unused */
    __u32 lah_value2;           /* unused */
    __u64 lah_value3;           /* unused */
    struct llapi_lu_ladvise lah_advise[0];      /* advices in this header */
};

#define LAH_COUNT_MAX   (1024)

enum lock_mode_user {
    MODE_READ_USER = 1,
    MODE_WRITE_USER,
    MODE_MAX_USER,
};

#define LOCK_MODE_NAMES { \
        [MODE_READ_USER]  = "READ",\
        [MODE_WRITE_USER] = "WRITE"\
}

enum lockahead_results {
    LLA_RESULT_SENT = 0,
    LLA_RESULT_DIFFERENT,
    LLA_RESULT_SAME,
};
#endif


int llapi_ladvise_lock(ADIO_File fd, unsigned long long flags, int num_advise,
                       ADIO_Offset * offset, int stripe_size, int num_extents,
                       ADIO_Offset step_size)
{
    struct llapi_ladvise_hdr *ladvise_hdr;
    int rc;
    int i;
    enum lock_mode_user mode = 0;

    if (num_advise < 1 || num_advise >= LAH_COUNT_MAX) {
        errno = EINVAL;
        /*llapi_error(LLAPI_MSG_ERROR, -EINVAL,
         * "bad advice number %d", num_advise); */
        return -1;
    }

    ladvise_hdr =
        ADIOI_Malloc(sizeof(struct llapi_ladvise_hdr) +
                     sizeof(struct llapi_lu_ladvise) * num_advise);

    if (ladvise_hdr == NULL) {
        errno = ENOMEM;
        //llapi_error(LLAPI_MSG_ERROR, -ENOMEM, "not enough memory");
        return -1;
    }
    ladvise_hdr->lah_magic = LADVISE_MAGIC;
    ladvise_hdr->lah_count = num_advise;
    ladvise_hdr->lah_flags = flags & LF_MASK;
    ladvise_hdr->lah_value1 = 0;
    ladvise_hdr->lah_value2 = 0;
    ladvise_hdr->lah_value3 = 0;

    if (fd->hints->fs_hints.lustre.lock_ahead_write)
        mode = MODE_WRITE_USER;
    else if (fd->hints->fs_hints.lustre.lock_ahead_read)        /* read only */
        mode = MODE_READ_USER;
    else
        MPI_Abort(MPI_COMM_WORLD, 1);

    for (i = 0; i < num_extents; ++i) {
        ladvise_hdr->lah_advise[i].lla_advice = LU_LADVISE_LOCKAHEAD;
        ladvise_hdr->lah_advise[i].lla_lockahead_mode = mode;
        ladvise_hdr->lah_advise[i].lla_peradvice_flags = flags | LF_ASYNC;
        ladvise_hdr->lah_advise[i].lla_start = *offset;
        ladvise_hdr->lah_advise[i].lla_end = *offset + stripe_size - 1;;
        ladvise_hdr->lah_advise[i].lla_value3 = 0;
        ladvise_hdr->lah_advise[i].lla_value4 = 0;
        ladvise_hdr->lah_advise[i].lla_lockahead_result = 0;
        *offset += step_size;
    }


    rc = ioctl(fd->fd_sys, LL_IOC_LADVISE, ladvise_hdr);

    if (rc < 0) {
        ADIOI_Free(ladvise_hdr);
        //llapi_error(LLAPI_MSG_ERROR, -errno, "cannot give advice");
        return -1;
    }


    /* Simply save the new start/end extents, forget what we aleady had locked
     * since lustre may reclaim it at any time. */
    fd->hints->fs_hints.lustre.lock_ahead_start_extent = ladvise_hdr->lah_advise[0].lla_start;
    fd->hints->fs_hints.lustre.lock_ahead_end_extent =
        ladvise_hdr->lah_advise[num_extents - 1].lla_end;


#ifdef LOCK_AHEAD_DEBUG
    /* Print any per extent errors */
    for (i = 0; i < num_extents; ++i) {
        if (ladvise_hdr->lah_advise[i].lla_lockahead_result) {
            fprintf(stderr, "%s(%d) "
                    "lock ahead extent[%4.4d] {%ld,%ld} stripe {%lld,%lld} error %d\n",
                    __func__, __LINE__,
                    i,
                    (long int) ladvise_hdr->lah_advise[i].lla_start,
                    (long int) ladvise_hdr->lah_advise[i].lla_end,
                    (long int) ladvise_hdr->lah_advise[i].lla_start / stripe_size,
                    (long int) ladvise_hdr->lah_advise[i].lla_end / stripe_size,
                    ladvise_hdr->lah_advise[i].lla_lockahead_result);
        }
    }

#endif
    ADIOI_Free(ladvise_hdr);

    return 0;
}







/* Set lustre locks to only lock the requested byte range, do not
   extend any locks to 'infinity' which is the normal behavior.
   This will enhance 'lock ahead' extent locking, which we do not
   want to auto-extend. */
int ADIOI_LUSTRE_request_only_lock_ioctl(ADIO_File fd)
{
    int err = 0;

    struct llapi_ladvise_hdr *noexpand_hdr;
    noexpand_hdr = ADIOI_Malloc(sizeof(struct llapi_ladvise_hdr) + sizeof(struct llapi_lu_ladvise));
    if (!noexpand_hdr) {
        err = -ENOMEM;
        goto out;
    }

    noexpand_hdr->lah_magic = LADVISE_MAGIC;
    noexpand_hdr->lah_count = 1;
    noexpand_hdr->lah_flags = 0;
    noexpand_hdr->lah_value1 = 0;
    noexpand_hdr->lah_value2 = 0;
    noexpand_hdr->lah_value3 = 0;
    noexpand_hdr->lah_advise[0].lla_advice = LU_LADVISE_LOCKNOEXPAND;
    noexpand_hdr->lah_advise[0].lla_peradvice_flags = 0;

    noexpand_hdr->lah_advise[0].lla_value1 = 0;
    noexpand_hdr->lah_advise[0].lla_start = 0;
    noexpand_hdr->lah_advise[0].lla_end = 0;
    noexpand_hdr->lah_advise[0].lla_value3 = 0;
    noexpand_hdr->lah_advise[0].lla_value4 = 0;

    int rc = ioctl(fd->fd_sys, LL_IOC_LADVISE, noexpand_hdr);
    if (rc < 0) {
        ADIOI_Free(noexpand_hdr);
        //llapi_error(LLAPI_MSG_ERROR, -errno, "cannot give advice");
        return -1;
    }

    ADIOI_Free(noexpand_hdr);


  out:

    return err;
}

/* Use group locks to 'clear' existing locks on the file
   before attempting 'lock ahead' extent locking. */
int ADIOI_LUSTRE_clear_locks(ADIO_File fd)
{
    int err = 0;
    int id;

    if (!fd->my_cb_nodes_index) {
        srand(time(NULL));
        id = rand();
        err = ioctl(fd->fd_sys, LL_IOC_GROUP_LOCK, id);
        err = ioctl(fd->fd_sys, LL_IOC_GROUP_UNLOCK, id);
    }
    return err;
}

/* Lock a predefined series of 'extents' in the file.
   The intent is to match the aggregator locking pattern. */
void ADIOI_LUSTRE_lock_ahead_ioctl(ADIO_File fd, int avail_cb_nodes, ADIO_Offset next_offset,
                                   int *error_code)
{

    int err = 0;
    int num_extents = fd->hints->fs_hints.lustre.lock_ahead_num_extents;
    int flags = fd->hints->fs_hints.lustre.lock_ahead_flags;
    ADIO_Offset offset = 0, step_size = 0;
    int stripe_size = fd->hints->striping_unit;

    int agg_idx = fd->my_cb_nodes_index;

    /* Not a collective aggregator? Do nothing and return
     * since current code is based on aggregator/stripes */
    if (agg_idx < 0) {
        /* Disable further lock ahead ...
         * fd->hints->fs_hints.lustre.lock_ahead_read  = 0;
         * fd->hints->fs_hints.lustre.lock_ahead_write = 0;
         * fd->hints->fs_hints.lustre.lock_ahead_start_extent = 0;
         * fd->hints->fs_hints.lustre.lock_ahead_end_extent   = INT64_MAX;
         */
        return;
    }
#ifdef LOCK_AHEAD_DEBUG
    {
        /* Debug check.  Calculate the expected rank for this stripe */
        int rank_index;
        rank_index = (int) ((next_offset / stripe_size) % avail_cb_nodes);
        /* Not sure why, but this happens in the generic read coll?
         * It doesn't do the aggregation striped quite as expected.
         * We'll probably lock the wrong stripes for this read ...
         * but we're more interested in write locks than read locks
         * so stick with the lustre specific calculations for now.
         * Consider dropping read support if performance isn't improved
         * or ad_lustre doesn't add read coll code.
         */
        if (agg_idx != rank_index) {
            fprintf(stderr, "%s(%d) rank[%d] file system %d "
                    "lock ahead debug R(%d)/W(%d), "
                    "aggregator %d(%d)/%d(%d), "
                    "offset %lld, start offset %lld, stripe %lld "
                    "num_extents %d\n",
                    __func__, __LINE__,
                    fd->hints->ranklist[agg_idx],
                    fd->file_system,
                    fd->hints->fs_hints.lustre.lock_ahead_read,
                    fd->hints->fs_hints.lustre.lock_ahead_write,
                    agg_idx, rank_index,
                    avail_cb_nodes, fd->hints->cb_nodes,
                    (long long) next_offset, (long long) (next_offset / stripe_size * stripe_size),
                    (long long) next_offset / stripe_size, num_extents);
        }
        /* Just checking the config vs what was passed in */
        if (agg_idx >= avail_cb_nodes) {
            fprintf(stderr, "%s(%d) file system %d "
                    "lock ahead debug R(%d)/W(%d), "
                    "aggregator %d(%d)/%d(%d), "
                    "num_extents %d\n",
                    __func__, __LINE__, fd->file_system,
                    fd->hints->fs_hints.lustre.lock_ahead_read,
                    fd->hints->fs_hints.lustre.lock_ahead_write,
                    agg_idx, rank_index, avail_cb_nodes, fd->hints->cb_nodes, num_extents);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }
#endif

    /* Check file access vs requested lock ahead */
    if (fd->access_mode & ADIO_RDONLY) {
        /* Don't need write lock ahead */
        fd->hints->fs_hints.lustre.lock_ahead_write = 0;

        /* Do need read lock ahead or give up. */
        if (!(fd->hints->fs_hints.lustre.lock_ahead_read)) {
            fd->hints->fs_hints.lustre.lock_ahead_start_extent = 0;
            fd->hints->fs_hints.lustre.lock_ahead_end_extent = INT64_MAX;
            return;
        }
    }
    if (fd->access_mode & ADIO_WRONLY) {
        /* Don't need read lock ahead */
        fd->hints->fs_hints.lustre.lock_ahead_read = 0;

        /* Do need write lock ahead or give up. */
        if (!(fd->hints->fs_hints.lustre.lock_ahead_write)) {
            fd->hints->fs_hints.lustre.lock_ahead_start_extent = 0;
            fd->hints->fs_hints.lustre.lock_ahead_end_extent = INT64_MAX;
            return;
        }
    }


    step_size = (ADIO_Offset) avail_cb_nodes *stripe_size;

    if (next_offset == 0) {     /* 1st call, calculate our starting offset */
        offset = (ADIO_Offset) agg_idx *stripe_size;
    } else      /* Have to assume we're writing to one of our stripes */
        offset = next_offset / stripe_size * stripe_size;       /* start of stripe */

    err = llapi_ladvise_lock(fd, flags, num_extents, &offset, stripe_size, num_extents, step_size);


    if (err == -1) {    /* turn off lock ahead after a failure */
#ifdef LOCK_AHEAD_DEBUG
        fprintf(stderr, "%s(%d) file system %d "
                "lock ahead failure R(%d)/W(%d), "
                "aggregator %d/%d, "
                "next offset %lld, stripe %lld, "
                "last offset %lld, stripe %lld, "
                "step %lld, stripe size %lld "
                "num_extents %d\n",
                __func__, __LINE__, fd->file_system,
                fd->hints->fs_hints.lustre.lock_ahead_read,
                fd->hints->fs_hints.lustre.lock_ahead_write,
                agg_idx,
                avail_cb_nodes,
                (long long) next_offset, (long long) next_offset / stripe_size,
                (long long) offset, (long long) offset / stripe_size,
                (long long) step_size, (long long) stripe_size, num_extents);
#endif
        fd->hints->fs_hints.lustre.lock_ahead_read = 0;
        fd->hints->fs_hints.lustre.lock_ahead_write = 0;
        fd->hints->fs_hints.lustre.lock_ahead_start_extent = 0;
        fd->hints->fs_hints.lustre.lock_ahead_end_extent = INT64_MAX;

        *error_code = ADIOI_Err_create_code("ADIOI_LUSTRE_lock_ahead_ioctl", fd->filename, errno);
        if (agg_idx == 0) {
            fprintf(stderr, "%s: ioctl(LL_IOC_LADVISE) \'%s\'\n", __func__, strerror(errno));
        }
        /* Note: it's too late to turn off 'request only' locking, which
         * could affect performance without also having 'lock ahead'.
         *
         * We expect lustre to support this (turning it off) later */
    }


    return;
}

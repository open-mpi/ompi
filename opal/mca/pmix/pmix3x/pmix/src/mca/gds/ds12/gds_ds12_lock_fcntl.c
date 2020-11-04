/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2016-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "include/pmix_common.h"

#include "src/mca/common/dstore/dstore_common.h"
#include "src/mca/gds/base/base.h"

#include "src/util/error.h"
#include "src/util/output.h"

#include "gds_ds12_lock.h"

#define _ESH_12_FCNTL_LOCK(lockfd, operation)               \
__pmix_attribute_extension__ ({                             \
    pmix_status_t ret = PMIX_SUCCESS;                       \
    int i;                                                  \
    struct flock fl = {0};                                  \
    fl.l_type = operation;                                  \
    fl.l_whence = SEEK_SET;                                 \
    for(i = 0; i < 10; i++) {                               \
        if( 0 > fcntl(lockfd, F_SETLKW, &fl) ) {            \
            switch( errno ){                                \
                case EINTR:                                 \
                    continue;                               \
                case ENOENT:                                \
                case EINVAL:                                \
                    ret = PMIX_ERR_NOT_FOUND;               \
                    break;                                  \
                case EBADF:                                 \
                    ret = PMIX_ERR_BAD_PARAM;               \
                    break;                                  \
                case EDEADLK:                               \
                case EFAULT:                                \
                case ENOLCK:                                \
                    ret = PMIX_ERR_RESOURCE_BUSY;           \
                    break;                                  \
                default:                                    \
                    ret = PMIX_ERROR;                       \
                    break;                                  \
            }                                               \
        }                                                   \
        break;                                              \
    }                                                       \
    if (ret) {                                              \
        pmix_output(0, "%s %d:%s lock failed: %s",          \
            __FILE__, __LINE__, __func__, strerror(errno)); \
    }                                                       \
    ret;                                                    \
})

typedef struct {
    char *lockfile;
    int lockfd;
} ds12_lock_fcntl_ctx_t;

pmix_status_t pmix_gds_ds12_lock_init(pmix_common_dstor_lock_ctx_t *ctx, const char *base_path,
                                      const char *name, uint32_t local_size, uid_t uid, bool setuid)
{
    pmix_status_t rc = PMIX_SUCCESS;
    ds12_lock_fcntl_ctx_t *lock_ctx;

    if (*ctx != NULL) {
        return PMIX_SUCCESS;
    }

    lock_ctx = (ds12_lock_fcntl_ctx_t*)malloc(sizeof(ds12_lock_fcntl_ctx_t));
    if (NULL == lock_ctx) {
        rc = PMIX_ERR_INIT;
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    *ctx = lock_ctx;
    memset(lock_ctx, 0, sizeof(ds12_lock_fcntl_ctx_t));
    lock_ctx->lockfd = -1;

    /* create a lock file to prevent clients from reading while server is writing
     * to the shared memory. This situation is quite often, especially in case of
     * direct modex when clients might ask for data simultaneously. */
    if(0 > asprintf(&lock_ctx->lockfile, "%s/dstore_sm.lock", base_path)) {
        rc = PMIX_ERR_OUT_OF_RESOURCE;
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    PMIX_OUTPUT_VERBOSE((10, pmix_gds_base_framework.framework_output,
        "%s:%d:%s _lockfile_name: %s", __FILE__, __LINE__, __func__, lock_ctx->lockfile));

    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
        lock_ctx->lockfd = open(lock_ctx->lockfile, O_CREAT | O_RDWR | O_EXCL, 0600);

        /* if previous launch was crashed, the lockfile might not be deleted and unlocked,
         * so we delete it and create a new one. */
        if (lock_ctx->lockfd < 0) {
            unlink(lock_ctx->lockfile);
            lock_ctx->lockfd = open(lock_ctx->lockfile, O_CREAT | O_RDWR, 0600);
            if (lock_ctx->lockfd < 0) {
                rc = PMIX_ERROR;
                PMIX_ERROR_LOG(rc);
                goto error;
            }
        }
        if (0 != setuid) {
            if (0 > chown(lock_ctx->lockfile, uid, (gid_t) -1)) {
                rc = PMIX_ERROR;
                PMIX_ERROR_LOG(rc);
                goto error;
            }
            if (0 > chmod(lock_ctx->lockfile, S_IRUSR | S_IWGRP | S_IRGRP)) {
                rc = PMIX_ERROR;
                PMIX_ERROR_LOG(rc);
                goto error;
            }
        }
    }
    else {
        lock_ctx->lockfd = open(lock_ctx->lockfile, O_RDONLY);
        if (0 > lock_ctx->lockfd) {
            rc = PMIX_ERROR;
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    return rc;

error:
    if (NULL != lock_ctx) {
        if (NULL != lock_ctx->lockfile) {
            free(lock_ctx->lockfile);
        }
        if (0 > lock_ctx->lockfd) {
            close(lock_ctx->lockfd);
            if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
                unlink(lock_ctx->lockfile);
            }
        }
        free(lock_ctx);
        lock_ctx = NULL;
    }
    *ctx = NULL;

    return rc;
}

void pmix_ds12_lock_finalize(pmix_common_dstor_lock_ctx_t *lock_ctx)
{
    ds12_lock_fcntl_ctx_t *fcntl_lock = (ds12_lock_fcntl_ctx_t*)*lock_ctx;

    if (NULL == fcntl_lock) {
        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
        return;
    }

    close(fcntl_lock->lockfd);

    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
        unlink(fcntl_lock->lockfile);
    }
    free(fcntl_lock);
    *lock_ctx = NULL;
}

pmix_status_t pmix_ds12_lock_rd_get(pmix_common_dstor_lock_ctx_t lock_ctx)
{    ds12_lock_fcntl_ctx_t *fcntl_lock = (ds12_lock_fcntl_ctx_t*)lock_ctx;
     pmix_status_t rc;

     if (NULL == fcntl_lock) {
         rc = PMIX_ERR_NOT_FOUND;
         PMIX_ERROR_LOG(rc);
         return rc;
     }
     rc = _ESH_12_FCNTL_LOCK(fcntl_lock->lockfd, F_RDLCK);

     return rc;

}

pmix_status_t pmix_ds12_lock_wr_get(pmix_common_dstor_lock_ctx_t lock_ctx)
{    ds12_lock_fcntl_ctx_t *fcntl_lock = (ds12_lock_fcntl_ctx_t*)lock_ctx;
     pmix_status_t rc;

     if (NULL == fcntl_lock) {
         rc = PMIX_ERR_NOT_FOUND;
         PMIX_ERROR_LOG(rc);
         return rc;
     }
     rc = _ESH_12_FCNTL_LOCK(fcntl_lock->lockfd, F_WRLCK);

     return rc;

}

pmix_status_t pmix_ds12_lock_rw_rel(pmix_common_dstor_lock_ctx_t lock_ctx)
{    ds12_lock_fcntl_ctx_t *fcntl_lock = (ds12_lock_fcntl_ctx_t*)lock_ctx;
     pmix_status_t rc;

     if (NULL == fcntl_lock) {
         rc = PMIX_ERR_NOT_FOUND;
         PMIX_ERROR_LOG(rc);
         return rc;
     }
     rc = _ESH_12_FCNTL_LOCK(fcntl_lock->lockfd, F_UNLCK);

     return rc;

}

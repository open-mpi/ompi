/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include <errno.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#include <string.h>
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif /* HAVE_NETDB_H */
#include <time.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "orte/util/show_help.h"

#include "oshmem/proc/proc.h"
#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"
#include "oshmem/util/oshmem_util.h"

#include "sshmem_mmap.h"

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#    define MAP_ANONYMOUS MAP_ANON
#endif /* MAP_ANONYMOUS and MAP_ANON */

#if !defined(MAP_FAILED)
#    define MAP_FAILED ((char*)-1)
#endif /* MAP_FAILED */

/* ////////////////////////////////////////////////////////////////////////// */
/*local functions */
/* local functions */
static int
module_init(void);

static int
segment_create(map_segment_t *ds_buf,
               const char *file_name,
               size_t size);

static int
ds_copy(const map_segment_t *from,
        map_segment_t *to);

static void *
segment_attach(map_segment_t *ds_buf, sshmem_mkey_t *mkey);

static int
segment_detach(map_segment_t *ds_buf, sshmem_mkey_t *mkey);

static int
segment_unlink(map_segment_t *ds_buf);

static int
module_finalize(void);

/*
 * mmap shmem module
 */
mca_sshmem_mmap_module_t mca_sshmem_mmap_module = {
    /* super */
    {
        module_init,
        segment_create,
        ds_copy,
        segment_attach,
        segment_detach,
        segment_unlink,
        module_finalize
    }
};

/* ////////////////////////////////////////////////////////////////////////// */
/* private utility functions */
/* ////////////////////////////////////////////////////////////////////////// */

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * completely resets the contents of *ds_buf
 */
static inline void
shmem_ds_reset(map_segment_t *ds_buf)
{
    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: shmem_ds_resetting "
         "(id: %d, size: %lu, name: %s)\n",
         mca_sshmem_mmap_component.super.base_version.mca_type_name,
         mca_sshmem_mmap_component.super.base_version.mca_component_name,
         ds_buf->seg_id, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
    );

    MAP_SEGMENT_RESET_FLAGS(ds_buf);
    ds_buf->seg_id = MAP_SEGMENT_SHM_INVALID;
    ds_buf->seg_base_addr = 0;
    ds_buf->end = 0;
    ds_buf->seg_size = 0;
    ds_buf->type = MAP_SEGMENT_UNKNOWN;
    unlink(ds_buf->seg_name);
    memset(ds_buf->seg_name, '\0', sizeof(ds_buf->seg_name));
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
module_init(void)
{
    /* nothing to do */
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
module_finalize(void)
{
    /* nothing to do */
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
ds_copy(const map_segment_t *from,
        map_segment_t *to)
{
    memcpy(to, from, sizeof(map_segment_t));

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: ds_copy complete "
         "from: (id: %d, size: %lu, "
         "name: %s flags: 0x%02x) "
         "to: (id: %d, size: %lu, "
         "name: %s flags: 0x%02x)\n",
         mca_sshmem_mmap_component.super.base_version.mca_type_name,
         mca_sshmem_mmap_component.super.base_version.mca_component_name,
         from->seg_id, (unsigned long)from->seg_size, from->seg_name,
         from->flags, to->seg_id, (unsigned long)to->seg_size, to->seg_name,
         to->flags)
    );

    return OSHMEM_SUCCESS;
}
/* ////////////////////////////////////////////////////////////////////////// */

static int
segment_create(map_segment_t *ds_buf,
               const char *file_name,
               size_t size)
{
    int rc = OSHMEM_SUCCESS;
    void *addr = NULL;

    assert(ds_buf);

    /* init the contents of map_segment_t */
    shmem_ds_reset(ds_buf);

    addr = mmap((void *)mca_sshmem_base_start_address,
                size,
                PROT_READ | PROT_WRITE,
                MAP_PRIVATE |
#if defined(MAP_ANONYMOUS)
                MAP_ANONYMOUS |
#endif
                MAP_FIXED,
                -1,
                0);

    if (MAP_FAILED == addr) {
        opal_show_help("help-oshmem-sshmem.txt",
                "create segment failure",
                true,
                "mmap",
                orte_process_info.nodename, (unsigned long long) size,
                strerror(errno), errno);
        opal_show_help("help-oshmem-sshmem-mmap.txt",
                       "mmap:create segment failure",
                       true);
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    ds_buf->type = MAP_SEGMENT_ALLOC_MMAP;
    if (mca_sshmem_mmap_component.is_anonymous) {
        /*
         * Segment attach is not called for anonymous mmap
         */
        ds_buf->seg_id = MAP_SEGMENT_SHM_INVALID;
    } else {
        /*
         * Warning: implied that input file name has a fixed format
         * and pe which is stored as segment identifier is used in file name
         * generation during segment attachment
         */
        ds_buf->seg_id = oshmem_my_proc_id();
    }
    ds_buf->seg_base_addr = addr;
    ds_buf->seg_size = size;
    ds_buf->end = (void*)((uintptr_t)ds_buf->seg_base_addr + ds_buf->seg_size);

    OPAL_OUTPUT_VERBOSE(
          (70, oshmem_sshmem_base_framework.framework_output,
           "%s: %s: create %s "
           "(id: %d, addr: %p size: %lu, name: %s)\n",
           mca_sshmem_mmap_component.super.base_version.mca_type_name,
           mca_sshmem_mmap_component.super.base_version.mca_component_name,
           (rc ? "failure" : "successful"),
           ds_buf->seg_id, ds_buf->seg_base_addr, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
      );

    return rc;
}

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * segment_attach can only be called after a successful call to segment_create
 */
static void *
segment_attach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    void *addr = NULL;

    assert(ds_buf);
    assert(mkey->va_base == 0);

    if (MAP_SEGMENT_SHM_INVALID == (int)(mkey->u.key)) {
        return (mkey->va_base);
    }

    if (mca_sshmem_mmap_component.is_anonymous) {
        /*
         * Note: segment attach for anonymous mmap
         * is not called due to invalid segment id
         */
        addr = mmap((void *)mca_sshmem_base_start_address,
                    ds_buf->seg_size,
                    PROT_READ | PROT_WRITE,
                    MAP_SHARED |
#if defined(MAP_ANONYMOUS)
                    MAP_ANONYMOUS |
#endif
                    MAP_FIXED,
                    -1,
                    0);
    } else {
        char *file_name = NULL;
        if (NULL == (file_name = oshmem_get_unique_file_name(mkey->u.key))) {
            OPAL_OUTPUT(
                (oshmem_sshmem_base_framework.framework_output,
                "Can't get file name")
            );
            return NULL;
        }

        int fd;
        if (-1 == (fd = open(file_name, O_CREAT | O_RDWR, 0600))) {
            OPAL_OUTPUT(
                (oshmem_sshmem_base_framework.framework_output,
                 "file open failed: %s", strerror(errno))
            );
            free(file_name);
            return NULL;
        }
        free(file_name);

        addr = mmap((void *)NULL,
                    ds_buf->seg_size,
                    PROT_READ | PROT_WRITE,
                    MAP_SHARED,
                    fd,
                    0);

        if (0 != close(fd)) {
            OPAL_OUTPUT(
                (oshmem_sshmem_base_framework.framework_output,
                "file close failed: %s", strerror(errno))
            );
        }
    }

    if (MAP_FAILED == addr) {
        OPAL_OUTPUT(
            (oshmem_sshmem_base_framework.framework_output,
             "Failed to mmap() %llu bytes (errno=%d)",
             (unsigned long long)ds_buf->seg_size, errno)
            );
        return NULL;
    }

    mkey->va_base = addr;

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: attach successful "
            "(id: %d, addr: %p size: %lu, name: %s | va_base: 0x%p len: %d key %llx)\n",
            mca_sshmem_mmap_component.super.base_version.mca_type_name,
            mca_sshmem_mmap_component.super.base_version.mca_component_name,
            ds_buf->seg_id, ds_buf->seg_base_addr, (unsigned long)ds_buf->seg_size, ds_buf->seg_name,
            mkey->va_base, mkey->len, (unsigned long long)mkey->u.key)
        );

    /* update returned base pointer with an offset that hides our stuff */
    return (mkey->va_base);
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_detach(map_segment_t *ds_buf, sshmem_mkey_t *mkey)
{
    int rc = OSHMEM_SUCCESS;

    assert(ds_buf);

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: detaching "
            "(id: %d, addr: %p size: %lu, name: %s)\n",
            mca_sshmem_mmap_component.super.base_version.mca_type_name,
            mca_sshmem_mmap_component.super.base_version.mca_component_name,
            ds_buf->seg_id, ds_buf->seg_base_addr, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
    );

    munmap((void *)ds_buf->seg_base_addr, ds_buf->seg_size);

    /* reset the contents of the map_segment_t associated with this
     * shared memory segment.
     */
    shmem_ds_reset(ds_buf);

    return rc;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_unlink(map_segment_t *ds_buf)
{
    /* not much unlink work needed for sysv */

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: unlinking "
            "(id: %d, addr: %p size: %lu, name: %s)\n",
            mca_sshmem_mmap_component.super.base_version.mca_type_name,
            mca_sshmem_mmap_component.super.base_version.mca_component_name,
            ds_buf->seg_id, ds_buf->seg_base_addr, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
    );

    /* don't completely reset.  in particular, only reset
     * the id and flip the invalid bit.  size and name values will remain valid
     * across unlinks. other information stored in flags will remain untouched.
     */
    ds_buf->seg_id = MAP_SEGMENT_SHM_INVALID;
    /* note: this is only changing the valid bit to 0. */
    MAP_SEGMENT_INVALIDATE(ds_buf);

    return OSHMEM_SUCCESS;
}


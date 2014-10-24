/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
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
#ifdef HAVE_SYS_IPC_H
#include <sys/ipc.h>
#endif /* HAVE_SYS_IPC_H */
#if HAVE_SYS_SHM_H
#include <sys/shm.h>
#endif /* HAVE_SYS_SHM_H */
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif /* HAVE_NETDB_H */

#include "opal/constants.h"
#include "opal_stdint.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "orte/util/show_help.h"

#include "oshmem/proc/proc.h"
#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"

#include "sshmem_sysv.h"


/* ////////////////////////////////////////////////////////////////////////// */
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

/* sysv shmem module */
mca_sshmem_sysv_module_t mca_sshmem_sysv_module = {
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
           mca_sshmem_sysv_component.super.base_version.mca_type_name,
           mca_sshmem_sysv_component.super.base_version.mca_component_name,
           ds_buf->seg_id, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
      );

      MAP_SEGMENT_RESET_FLAGS(ds_buf);
      ds_buf->seg_id = MAP_SEGMENT_SHM_INVALID;
      ds_buf->seg_base_addr = 0;
      ds_buf->end = 0;
      ds_buf->seg_size = 0;
      ds_buf->type = MAP_SEGMENT_UNKNOWN;
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
         mca_sshmem_sysv_component.super.base_version.mca_type_name,
         mca_sshmem_sysv_component.super.base_version.mca_component_name,
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
    int shmid = MAP_SEGMENT_SHM_INVALID;
    int flags;

    assert(ds_buf);

    /* init the contents of map_segment_t */
    shmem_ds_reset(ds_buf);

    /* for sysv shared memory we don't have to worry about the backing store
     * being located on a network file system... so no check is needed here.
     */

    /* create a new shared memory segment and save the shmid. note the use of
     * real_size here
     */
    flags = IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR;
#if defined (SHM_HUGETLB)
    flags |= ((0 != mca_sshmem_sysv_component.use_hp) ? SHM_HUGETLB : 0);
    size = ((size + sshmem_sysv_gethugepagesize() - 1) / sshmem_sysv_gethugepagesize()) * sshmem_sysv_gethugepagesize();
#endif

    /* Create a new shared memory segment and save the shmid. */
    shmid = shmget(IPC_PRIVATE, size, flags);
    if (shmid == MAP_SEGMENT_SHM_INVALID) {
        opal_show_help("help-oshmem-sshmem.txt",
                       "create segment failure",
                       true,
                       "sysv",
                       orte_process_info.nodename, (unsigned long long) size,
                       strerror(errno), errno);
        opal_show_help("help-oshmem-sshmem-sysv.txt",
                       "sysv:create segment failure",
                       true);
        return OSHMEM_ERROR;
    }

    /* Attach to the segment */
    addr = shmat(shmid, (void *) mca_sshmem_base_start_address, 0);
    if (addr == (void *) -1L) {
        opal_show_help("help-oshmem-sshmem.txt",
                       "create segment failure",
                       true,
                       "sysv",
                       orte_process_info.nodename, (unsigned long long) size,
                       strerror(errno), errno);
        opal_show_help("help-oshmem-sshmem-sysv.txt",
                       "sysv:create segment failure",
                       true);
        shmctl(shmid, IPC_RMID, NULL);
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    shmctl(shmid, IPC_RMID, NULL );

    ds_buf->type = MAP_SEGMENT_ALLOC_SHM;
    ds_buf->seg_id = shmid;
    ds_buf->seg_base_addr = addr;
    ds_buf->seg_size = size;
    ds_buf->end = (void*)((uintptr_t)ds_buf->seg_base_addr + ds_buf->seg_size);

    OPAL_OUTPUT_VERBOSE(
          (70, oshmem_sshmem_base_framework.framework_output,
           "%s: %s: create %s "
           "(id: %d, addr: %p size: %lu, name: %s)\n",
           mca_sshmem_sysv_component.super.base_version.mca_type_name,
           mca_sshmem_sysv_component.super.base_version.mca_component_name,
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
    assert(ds_buf);
    assert(mkey->va_base == 0);

    if (MAP_SEGMENT_SHM_INVALID == (int)(mkey->u.key)) {
        return (mkey->va_base);
    }

    mkey->va_base = shmat((int)(mkey->u.key), 0, 0);

    OPAL_OUTPUT_VERBOSE(
        (70, oshmem_sshmem_base_framework.framework_output,
         "%s: %s: attach successful "
            "(id: %d, addr: %p size: %lu, name: %s | va_base: 0x%p len: %d key %llx)\n",
            mca_sshmem_sysv_component.super.base_version.mca_type_name,
            mca_sshmem_sysv_component.super.base_version.mca_component_name,
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
            mca_sshmem_sysv_component.super.base_version.mca_type_name,
            mca_sshmem_sysv_component.super.base_version.mca_component_name,
            ds_buf->seg_id, ds_buf->seg_base_addr, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
    );

    if (ds_buf->seg_id != MAP_SEGMENT_SHM_INVALID) {
        shmctl(ds_buf->seg_id, IPC_RMID, NULL );
    }

    if (mca_sshmem_sysv_component.use_hp != 0) {
        /**
         *  Workaround kernel panic when detaching huge pages from user space simultanously from several processes
         *  dont detach here instead let kernel do it during process cleanup
         */
        /* shmdt((void *)ds_buf->seg_base_addr); */
    }

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
         "(id: %d, size: %lu, name: %s)\n",
         mca_sshmem_sysv_component.super.base_version.mca_type_name,
         mca_sshmem_sysv_component.super.base_version.mca_component_name,
         ds_buf->seg_id, (unsigned long)ds_buf->seg_size, ds_buf->seg_name)
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

/*
 * Get current huge page size
 *   
 */
size_t sshmem_sysv_gethugepagesize(void)
{
    static size_t huge_page_size = 0;
    char buf[256];
    int size_kb;
    FILE *f;

    /* Cache the huge page size value */
    if (huge_page_size == 0) {
        f = fopen("/proc/meminfo", "r");
        if (f != NULL) {
            while (fgets(buf, sizeof(buf), f)) {
                if (sscanf(buf, "Hugepagesize: %d kB", &size_kb) == 1) {
                    huge_page_size = size_kb * 1024L;
                    break;
                }
            }
            fclose(f);
        }

        if (huge_page_size == 0) {
            huge_page_size = 2 * 1024L *1024L;
        }
    }

    return huge_page_size;
}



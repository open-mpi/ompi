/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

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
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */

#include "opal/constants.h"
#include "opal_stdint.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/shmem/shmem.h"
#include "opal/mca/shmem/base/base.h"

#include "opal/mca/shmem/windows/shmem_windows.h"

/* for tons of debug output: -mca shmem_base_verbose 70 */

/* ////////////////////////////////////////////////////////////////////////// */
/*local functions */
/* local functions */
static int
module_init(void);

static int
segment_create(opal_shmem_ds_t *ds_buf,
               const char *file_name,
               size_t size);

static int
ds_copy(const opal_shmem_ds_t *from,
        opal_shmem_ds_t *to);

static void *
segment_attach(opal_shmem_ds_t *ds_buf);

static int
segment_detach(opal_shmem_ds_t *ds_buf);

static int
segment_unlink(opal_shmem_ds_t *ds_buf);

static int
module_finalize(void);

/*
 * windows shmem module
 */
opal_shmem_windows_module_t opal_shmem_windows_module = {
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
shmem_ds_reset(opal_shmem_ds_t *ds_buf)
{
    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: shmem_ds_resetting "
         "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
         ds_buf->seg_name)
    );

    ds_buf->opid = 0;
    ds_buf->seg_cpid = 0;
    OPAL_SHMEM_DS_RESET_FLAGS(ds_buf);
    ds_buf->seg_id = OPAL_SHMEM_DS_ID_INVALID;
    ds_buf->seg_size = 0;
    memset(ds_buf->seg_name, '\0', OPAL_PATH_MAX);
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
module_init(void)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
module_finalize(void)
{
    /* nothing to do */
    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
ds_copy(const opal_shmem_ds_t *from,
        opal_shmem_ds_t *to)
{
    pid_t my_pid = getpid();

    /* inter-process copy - exclude process-specific data */
    if (from->opid != my_pid) {
        /* mask out internal flags */
        to->flags = (from->flags & OPAL_SHMEM_DS_FLAGS_INTERNAL_MASK);
        to->seg_base_addr = NULL;
    }
    /* i am the owner process, so i can safely copy all the information */
    else {
        to->flags = from->flags;
        to->seg_base_addr = from->seg_base_addr;
    }

    to->opid = my_pid;
    to->seg_id = from->seg_id;
    to->seg_size = from->seg_size;
    to->seg_cpid = from->seg_cpid;
    memcpy(to->seg_name, from->seg_name, OPAL_PATH_MAX);

    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: ds_copy complete "
         "from: (opid: %lu, id: %d, size: %"PRIsize_t", "
         "name: %s flags: 0x%02x) "
         "to: (opid: %lu, id: %d, size: %"PRIsize_t", "
         "name: %s flags: 0x%02x)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)from->opid, from->seg_id, from->seg_size,
         from->seg_name, from->flags, (unsigned long)to->opid, to->seg_id,
         to->seg_size, to->seg_name, to->flags)
    );

    return OPAL_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
/* mmap equivalent implementation on Windows */

#ifdef __USE_FILE_OFFSET64
# define DWORD_HI(x) (x >> 32)
# define DWORD_LO(x) ((x) & 0xffffffff)
#else
# define DWORD_HI(x) (0)
# define DWORD_LO(x) (x)
#endif

/* define mmap flags */
#define PROT_READ     0x1
#define PROT_WRITE    0x2
#define MAP_SHARED    0x01
#define MAP_PRIVATE   0x02
#define MAP_ANONYMOUS 0x20
#define MAP_ANON      MAP_ANONYMOUS
#define MAP_FAILED    ((void *) -1)
/* This flag is only available in WinXP+ */
#ifdef FILE_MAP_EXECUTE
#define PROT_EXEC     0x4
#else
#define PROT_EXEC        0x0
#define FILE_MAP_EXECUTE 0
#endif

static void *mmap(void *start, size_t length, int prot, int flags, int fd, off_t offset)
{
    DWORD flProtect;
    HANDLE mmap_fd, h;
    DWORD dwDesiredAccess;
    off_t end;
    void *ret;

    if (prot & ~(PROT_READ | PROT_WRITE | PROT_EXEC))
        return MAP_FAILED;
    if (fd == -1) {
        if (!(flags & MAP_ANON) || offset)
            return MAP_FAILED;
    } else if (flags & MAP_ANON)
        return MAP_FAILED;

    if (prot & PROT_WRITE) {
        if (prot & PROT_EXEC)
            flProtect = PAGE_EXECUTE_READWRITE;
        else
            flProtect = PAGE_READWRITE;
    } else if (prot & PROT_EXEC) {
        if (prot & PROT_READ)
            flProtect = PAGE_EXECUTE_READ;
        else if (prot & PROT_EXEC)
            flProtect = PAGE_EXECUTE;
    } else
        flProtect = PAGE_READONLY;

    end = length + offset;
    if (fd == -1)
        mmap_fd = INVALID_HANDLE_VALUE;
    else
        mmap_fd = (HANDLE)_get_osfhandle(fd);
    h = CreateFileMapping(mmap_fd, NULL, flProtect, DWORD_HI(end), DWORD_LO(end), NULL);
    if (h == NULL)
        return MAP_FAILED;

    if (prot & PROT_WRITE)
        dwDesiredAccess = FILE_MAP_WRITE;
    else
        dwDesiredAccess = FILE_MAP_READ;
    if (prot & PROT_EXEC)
        dwDesiredAccess |= FILE_MAP_EXECUTE;
    if (flags & MAP_PRIVATE)
        dwDesiredAccess |= FILE_MAP_COPY;
    ret = MapViewOfFile(h, dwDesiredAccess, DWORD_HI(offset), DWORD_LO(offset), length);
    if (ret == NULL) {
        CloseHandle(h);
        ret = MAP_FAILED;
    }
    return ret;
}

/* ////////////////////////////////////////////////////////////////////////// */
/* SKG will not compile, but it's a start */
static int
segment_create(opal_shmem_ds_t *ds_buf,
               const char *file_name,
               size_t size)
{
    int rc = OPAL_SUCCESS;
    bool file_previously_opened = false;
    pid_t my_pid = getpid();
    char *temp1 = NULL, *temp2 = NULL;
    /* the real size of the shared memory segment.  this includes enough space
     * to store our segment header.
     */
    size_t real_size = size + sizeof(opal_shmem_seg_hdr_t);
    opal_shmem_seg_hdr_t *seg_hdrp = MAP_FAILED;
    HANDLE hMapObject = INVALID_HANDLE_VALUE;
    LPVOID lpvMem = NULL;

    /* init the contents of opal_shmem_ds_t */
    shmem_ds_reset(ds_buf);

    /* On Windows the shared file will be created by the OS directly on the
     * system ressources. Therefore, no file get involved in the operation.
     * However, a unique key should be used as name for the shared memory object
     * in order to allow all processes to access the same unique shared memory
     * region. The key will be obtained from the original file_name by replacing
     * all path separator occurences by '/' (as '\' is not allowed on the object
     * name).
     */
    temp1 = strdup(file_name);
    temp2 = temp1;
    while (NULL != (temp2 = strchr(temp2, OPAL_PATH_SEP[0])) ) {
        *temp2 = '/';
    }
    /* update path change in ds_buf */
    memcpy(ds_buf->seg_name, temp1, OPAL_PATH_MAX);
    /* relase the temporary file name */
    free(temp1);  /* relase the temporary file name */

                                   /* use paging file */
    hMapObject = CreateFileMapping(INVALID_HANDLE_VALUE,
                                   /* no security attributes */
                                   NULL,
                                   /* read/write access */
                                   PAGE_READWRITE,
                                   /* size: high 32-bits */
                                   0,
                                   /* size: low 32-bits */
                                   (DWORD)real_size,
                                   /* name of map object */
                                   ds_buf->seg_name);
    if (NULL == hMapObject) {
        rc = GetLastError();
        goto out;
    }
    if (ERROR_ALREADY_EXISTS == GetLastError()) {
        file_previously_opened = true;
    }

    /* Get a pointer to the file-mapped shared memory. */
    lpvMem = MapViewOfFile(hMapObject,          /* object to map view of */
                           FILE_MAP_WRITE,      /* read/write access */
                           0,                   /* high offset:  map from */
                           0,                   /* low offset:   beginning */
                           0);                  /* default: map entire file */
    if (NULL == lpvMem) {
        rc = GetLastError();
        goto out;
    }

    seg_hdrp = (opal_shmem_seg_hdr_t *)lpvMem;

    /* all is well */
    {
        /* -- initialize the shared memory segment -- */
        opal_atomic_rmb();

        /* init segment lock */
        opal_atomic_init(&seg_hdrp->lock, OPAL_ATOMIC_UNLOCKED);
        /* i was the creator of this segment, so note that fact */
        seg_hdrp->cpid = my_pid;

        opal_atomic_wmb();

        /* -- initialize the contents of opal_shmem_ds_t -- */
        ds_buf->opid = my_pid;
        ds_buf->seg_cpid = my_pid;
        ds_buf->seg_size = real_size;
        ds_buf->seg_base_addr = (unsigned char *)seg_hdrp;
        /* ds_buf->seg_name already set above */

        /* set "valid" bit because setment creation was successful */
        OPAL_SHMEM_DS_SET_VALID(ds_buf);

        OPAL_OUTPUT_VERBOSE(
            (70, opal_shmem_base_output,
             "%s: %s: create successful "
             "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
             mca_shmem_windows_component.super.base_version.mca_type_name,
             mca_shmem_windows_component.super.base_version.mca_component_name,
             (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
             ds_buf->seg_name)
        );
    }

out:
    /* in this component, the id is the file descriptor returned by open.  this
     * check is here to see if it is safe to call close on the file descriptor.
     * that is, we are making sure that our call to open was successful and
     * we are not not in an error path.
     */
    if (-1 != ds_buf->seg_id) {
        if (0 != close(ds_buf->seg_id)) {
            int err = errno;
            char hn[MAXHOSTNAMELEN];
            gethostname(hn, MAXHOSTNAMELEN - 1);
            hn[MAXHOSTNAMELEN - 1] = '\0';
            opal_show_help("help-opal-shmem-windows.txt", "sys call fail", 1, hn,
                           "close(2)", "", strerror(err), err);
            rc = OPAL_ERROR;
         }
     }

    /* an error occured, so invalidate the shmem object and munmap if needed */
    if (OPAL_SUCCESS != rc) {
        if (MAP_FAILED != seg_hdrp) {
            UnmapViewOfFile(seg_hdrp);
        }
        shmem_ds_reset(ds_buf);
    }
    return rc;
}

/* ////////////////////////////////////////////////////////////////////////// */
/**
 * segment_attach can only be called after a successful call to segment_create
 */
static void *
segment_attach(opal_shmem_ds_t *ds_buf)
{
    pid_t my_pid = getpid();

    if (my_pid != ds_buf->seg_cpid) {
        if (-1 == (ds_buf->seg_id = open(ds_buf->seg_name, O_CREAT | O_RDWR,
                                         0600))) {
            int err = errno;
            char hn[MAXHOSTNAMELEN];
            gethostname(hn, MAXHOSTNAMELEN - 1);
            hn[MAXHOSTNAMELEN - 1] = '\0';
            opal_show_help("help-opal-shmem-windows.txt", "sys call fail", 1, hn,
                           "open(2)", "", strerror(err), err);
            return NULL;
        }
        else if (MAP_FAILED == (ds_buf->seg_base_addr =
                                mmap(NULL, ds_buf->seg_size,
                                     PROT_READ | PROT_WRITE, MAP_SHARED,
                                     ds_buf->seg_id, 0))) {
            int err = errno;
            char hn[MAXHOSTNAMELEN];
            gethostname(hn, MAXHOSTNAMELEN - 1);
            hn[MAXHOSTNAMELEN - 1] = '\0';
            opal_show_help("help-opal-shmem-windows.txt", "sys call fail", 1, hn,
                           "mmap(2)", "", strerror(err), err);
            /* windows module failed, so close the file and return NULL - no error check
             * here because we are already in an error path...
             */
            close(ds_buf->seg_id);
            return NULL;
        }
        /* all is well */
        else {
            /* if close fails here, that's okay.  just let the user know and
             * continue.  if we got this far, open and mmap were successful...
             */
            if (0 != close(ds_buf->seg_id)) {
                int err = errno;
                char hn[MAXHOSTNAMELEN];
                gethostname(hn, MAXHOSTNAMELEN - 1);
                hn[MAXHOSTNAMELEN - 1] = '\0';
                opal_show_help("help-opal-shmem-windows.txt", "sys call fail", 1,
                               hn, "close(2)", "", strerror(err), err);
            }
        }
    }
    /* else i was the segment creator.  nothing to do here because all the hard
     * work was done in segment_create :-).
     */

    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: attach successful "
         "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
         ds_buf->seg_name)
    );

    /* update returned base pointer with an offset that hides our stuff */
    return (ds_buf->seg_base_addr + sizeof(opal_shmem_seg_hdr_t));
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_detach(opal_shmem_ds_t *ds_buf)
{
    int rc = OPAL_SUCCESS;

    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: detaching "
         "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
         ds_buf->seg_name)
    );

    if (0 != UnmapViewOfFile(ds_buf->seg_base_addr)) {
        int err = errno;
        char hn[MAXHOSTNAMELEN];
        gethostname(hn, MAXHOSTNAMELEN - 1);
        hn[MAXHOSTNAMELEN - 1] = '\0';
        opal_show_help("help-opal-shmem-windows.txt", "sys call fail", 1, hn,
                       "munmap(2)", "", strerror(err), err);
        rc = OPAL_ERROR;
    }
    /* reset the contents of the opal_shmem_ds_t associated with this
     * shared memory segment.
     */
    shmem_ds_reset(ds_buf);
    return rc;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
segment_unlink(opal_shmem_ds_t *ds_buf)
{
    OPAL_OUTPUT_VERBOSE(
        (70, opal_shmem_base_output,
         "%s: %s: unlinking "
         "(opid: %lu id: %d, size: %"PRIsize_t", name: %s)\n",
         mca_shmem_windows_component.super.base_version.mca_type_name,
         mca_shmem_windows_component.super.base_version.mca_component_name,
         (unsigned long)ds_buf->opid, ds_buf->seg_id, ds_buf->seg_size,
         ds_buf->seg_name)
    );

/*    if (-1 == unlink(ds_buf->seg_name)) {
        int err = errno;
        char hn[MAXHOSTNAMELEN];
        gethostname(hn, MAXHOSTNAMELEN - 1);
        hn[MAXHOSTNAMELEN - 1] = '\0';
        opal_show_help("help-opal-shmem-windows.txt", "sys call fail", 1, hn,
                       "unlink(2)", ds_buf->seg_name, strerror(err), err);
        return OPAL_ERROR;
    }
	*/

    /* don't completely reset the opal_shmem_ds_t.  in particular, only reset
     * the id and flip the invalid bit.  size and name values will remain valid
     * across unlinks. other information stored in flags will remain untouched.
     */
    ds_buf->seg_id = OPAL_SHMEM_DS_ID_INVALID;
    /* note: this is only chaning the valid bit to 0.  this is not the same
     * as calling invalidate(ds_buf).
     */
    OPAL_SHMEM_DS_INVALIDATE(ds_buf);
    return OPAL_SUCCESS;
}


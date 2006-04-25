/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_TIME_H
#include <time.h>
#endif  /* HAVE_TIME_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include "ompi/constants.h"
#include "common_sm_mmap.h"
#include "opal/util/output.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"


OBJ_CLASS_INSTANCE(
    mca_common_sm_mmap_t,
    opal_object_t,
    NULL,
    NULL
);

/*
 * Instance that is shared between components that use shared memory
 */
mca_common_sm_mmap_t *mca_common_sm_mmap = NULL;

#if !defined(__WINDOWS__)
static int mca_common_sm_mmap_open(char* path)
{
    int fd = -1;
    struct timespec ts;

    /* loop until file can be opened, or until an error, other than
     * access error, occurs */
    while (fd < 0) {
        fd = open(path, O_CREAT|O_RDWR, 0000); 
        if (fd < 0 && errno != EACCES) {
            opal_output(0, 
                        "mca_ptl_sm_mmap_open: open %s failed with errno=%d\n",
                        path, errno);
            return -1;
        }
        ts.tv_sec = 0; 
        ts.tv_nsec = 500000;
        nanosleep(&ts, NULL);
    }

    return fd;
}
#endif  /* !defined(__WINDOWS__) */

mca_common_sm_mmap_t* mca_common_sm_mmap_init(size_t size, char *file_name, 
        size_t size_ctl_structure, size_t data_seg_alignment)
{
    int fd = -1, return_code = OMPI_SUCCESS;
    bool file_previously_opened;
    mca_common_sm_file_header_t* seg = NULL;
    mca_common_sm_mmap_t* map = NULL;
    struct stat s_stat;
    unsigned char *addr = NULL;
    size_t tmp,mem_offset;

#if !defined(__WINDOWS__)
    /* input parameter error checks */
    if( (size < sizeof(mca_common_sm_file_header_t) ) ||
                ( file_name == NULL ) || 
                ( size_ctl_structure <
                  sizeof(mca_common_sm_file_header_t ) )) {
        return NULL;
    }

    /* open the backing file.  The first process to succeed here will
       effectively block the others until most of the rest of the
       setup in this function is complete because the initial perms
       are 000 (an fchmod() is executed below, enabling the other
       processes to get in) */
    fd = mca_common_sm_mmap_open(file_name);
    if( -1 == fd ) {
        opal_output(0, "mca_common_sm_mmap_init: mca_common_sm_mmap_open failed \n");
        return NULL;
    }

    /* figure out if I am first to attach to file */
    file_previously_opened=false;
    return_code=fstat(fd,&s_stat);
    if( 0 > return_code ) {
        opal_output(0, "mca_common_sm_mmap_init: fstat failed with errno=%d\n", errno);
        goto return_error;
    }
    if( s_stat.st_size > 0 )
        file_previously_opened=true;

    /* first process to open the file, so needs to initialize it */
    if( !file_previously_opened ) {
        /* truncate the file to the requested size */
        if(ftruncate(fd, size) != 0) {
            opal_output(0, 
                    "mca_common_sm_mmap_init: ftruncate failed with errno=%d\n",
                    errno);
            goto return_error;
        }
    }

    /* map the file and initialize segment state */
    seg = (mca_common_sm_file_header_t*)
        mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if( (void*)-1 == seg ) {
        opal_output(0, "mca_common_sm_mmap_init: mmap failed with errno=%d\n",
                errno);
        goto return_error;
    }
#else
    HANDLE hMapObject = INVALID_HANDLE_VALUE;
    LPVOID lpvMem = NULL;

    hMapObject = CreateFileMapping( INVALID_HANDLE_VALUE, /* use paging file */
                                    NULL,                 /* no security attributes */
                                    PAGE_READWRITE,       /* read/write access */
                                    0,                    /* size: high 32-bits */
                                    size,                 /* size: low 32-bits */
                                    file_name);           /* name of map object */
    if( ERROR_ALREADY_EXISTS == GetLastError() )
        file_previously_opened=true;

    if( NULL != hMapObject ) {
        /* Get a pointer to the file-mapped shared memory. */
        lpvMem = MapViewOfFile( hMapObject,     /* object to map view of */
                                FILE_MAP_WRITE, /* read/write access */
                                0,              /* high offset:  map from */
                                0,              /* low offset:   beginning */
                                0);             /* default: map entire file */
    }
    seg = (mca_common_sm_file_header_t*)lpvMem;
    if( NULL == lpvMem ) {
        int rc = GetLastError();
        char* localbuf = NULL;
        FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                       NULL, rc, 0, (LPTSTR)&localbuf, 1024, NULL );
        opal_output( 0, "%s\n", localbuf );
        LocalFree( localbuf );
        goto return_error;
    }
#endif  /* !defined(__WINDOWS__) */

    /* set up the map object */
    map = OBJ_NEW(mca_common_sm_mmap_t);
    strncpy(map->map_path, file_name, OMPI_PATH_MAX);
    /* the first entry in the file is the control structure. The first
       entry in the control structure is an mca_common_sm_file_header_t
       element */
    map->map_seg = seg;

    /* If we have a data segment (i.e., if 0 != data_seg_alignment),
       then make it the first aligned address after the control
       structure. */
    if (0 != data_seg_alignment) {
        addr = ((unsigned char *) seg) + size_ctl_structure;
        /* calculate how far off alignment we are */
        tmp = ((size_t) addr) % data_seg_alignment;
        /* if we're off alignment, then move up to the next alignment */
        if( tmp > 0 )
            addr += (data_seg_alignment - tmp);

        /* is addr past end of file ? */
        if( (unsigned char*)seg+size < addr ) {
            opal_output(0, "mca_common_sm_mmap_init: memory region too small len %d  addr %p\n",
                        size,addr);
            goto return_error;
        }
        map->data_addr = addr;
    } else {
        map->data_addr = NULL;
    }
    mem_offset = addr-(unsigned char *)seg;
    map->map_addr = (unsigned char *)seg;
    map->map_size = size;

    /* initialize the segment - only the first process to open the file */
    if( !file_previously_opened ) {
        opal_atomic_unlock(&seg->seg_lock);
        seg->seg_inited = false;
        seg->seg_offset = mem_offset;
        seg->seg_size = size;
    }

#if !defined(__WINDOWS__)
    /* enable access by other processes on this host */
    if(fchmod(fd, 0600) != 0) {
        opal_output(0, "mca_common_sm_mmap_init: fchmod failed with errno=%d :: fd %d\n",
                errno,fd);
        OBJ_RELEASE(map);
        goto return_error;
    }
    close(fd);
#else
    CloseHandle(hMapObject);
#endif  /* !defined(__WINDOWS__) */

    return map;

  return_error:
#if !defined(__WINDOWS__)
    if( -1 != fd ) {
        fchmod(fd, 0600);
        close(fd);
    }
    if( NULL != seg ) munmap((void*) seg,size);
#else
    if( NULL != lpvMem ) UnmapViewOfFile( lpvMem );
    if( NULL != hMapObject ) CloseHandle(hMapObject);
#endif  /* !defined(__WINDOWS__) */

    return NULL;
}

int mca_common_sm_mmap_fini( mca_common_sm_mmap_t* sm_mmap )
{
    int rc = OMPI_SUCCESS;

    if( NULL != sm_mmap->map_seg ) {
#if !defined(__WINDOWS__)
        rc = munmap((void*) sm_mmap->map_addr, sm_mmap->map_size );
        sm_mmap->map_addr = NULL;
        sm_mmap->map_size = 0;
#else
        BOOL return_error = UnmapViewOfFile( sm_mmap->map_addr );
        if( false == return_error ) {
            rc = GetLastError();
        }
#endif  /* !defined(__WINDOWS__) */
    }
    return rc;
}

/**
 *  allocate memory from a previously allocated shared memory
 *  block.
 *
 *  @param size size of request, in bytes (IN)
 * 
 *  @retval addr virtual address
 */

void* mca_common_sm_mmap_seg_alloc(
    struct mca_mpool_base_module_t* mpool,
    size_t* size,
    mca_mpool_base_registration_t** registration)
{
    mca_common_sm_mmap_t* map = mca_common_sm_mmap;
    mca_common_sm_file_header_t* seg = map->map_seg;
    void* addr;

    opal_atomic_lock(&seg->seg_lock);
    if(seg->seg_offset + *size > map->map_size) {
        addr = NULL;
    } else {
        size_t fixup;

        /* add base address to segment offset */
        addr = map->data_addr + seg->seg_offset;
        seg->seg_offset += *size;

        /* fix up seg_offset so next allocation is aligned on a
           sizeof(long) boundry.  Do it here so that we don't have to
           check before checking remaining size in buffer */
        if ((fixup = (seg->seg_offset & (sizeof(long) - 1))) > 0) {
            seg->seg_offset += sizeof(long) - fixup;
        }
    }
    if (NULL != registration) {
        *registration = NULL;
    }
    opal_atomic_unlock(&seg->seg_lock);
    return addr;
}


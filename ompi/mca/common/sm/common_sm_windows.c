/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Los Alamos National Security, LLC.
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

#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/align.h"
#include "opal/threads/mutex.h"
#include "opal/util/opal_sos.h"

#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ompi/constants.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/mpool/sm/mpool_sm.h"
#include "common_sm_windows.h"

OBJ_CLASS_INSTANCE(
    mca_common_sm_module_windows_t,
    opal_object_t,
    NULL,
    NULL
);

/******************************************************************************/
/**
 * mca_common_sm_windows_component_query 
 */
int 
mca_common_sm_windows_component_query(void)
{
    return OMPI_SUCCESS;
}

mca_common_sm_module_t * 
mca_common_sm_windows_init(ompi_proc_t **sorted_procs,
                        size_t num_local_procs,
                        size_t size, char *file_name,
                        size_t size_ctl_structure,
                        size_t data_seg_alignment)
{
    int fd = -1, return_code = OMPI_SUCCESS;
    bool file_previously_opened = false;
    mca_common_sm_seg_header_t* seg = NULL;
    mca_common_sm_module_windows_t* map = NULL;
    unsigned char *addr = NULL;
    size_t tmp, mem_offset;

    HANDLE hMapObject = INVALID_HANDLE_VALUE;
    LPVOID lpvMem = NULL;
    char *temp1, *temp2;
    int rc;

    /**
     * On Windows the shared file will be created by the OS directly on
     * the system ressources. Therefore, no file get involved in the
     * operation. However, a unique key should be used as name for the
     * shared memory object in order to allow all processes to access
     * the same unique shared memory region. The key will be obtained
     * from the original file_name by replacing all path separator
     * occurences by '/' (as '\' is not allowed on the object name).
     */
    temp1 = strdup(file_name);
    temp2 = temp1;
    while( NULL != (temp2 = strchr(temp2, OPAL_PATH_SEP[0])) ) {
        *temp2 = '/';
    }
    hMapObject = CreateFileMapping( INVALID_HANDLE_VALUE, /* use paging file */
                                    NULL,                 /* no security attributes */
                                    PAGE_READWRITE,       /* read/write access */
                                    0,                    /* size: high 32-bits */
                                    (DWORD)size,          /* size: low 32-bits */
                                    temp1);               /* name of map object */
    if( NULL == hMapObject ) {
        rc = GetLastError();
        goto return_error;
    }
    if( ERROR_ALREADY_EXISTS == GetLastError() )
        file_previously_opened=true;
    free(temp1);  /* relase the temporary file name */

    /* Get a pointer to the file-mapped shared memory. */
    lpvMem = MapViewOfFile( hMapObject,          /* object to map view of */
                            FILE_MAP_WRITE,      /* read/write access */
                            0,                   /* high offset:  map from */
                            0,                   /* low offset:   beginning */
                            0);                  /* default: map entire file */
    if( NULL == lpvMem ) {
        rc = GetLastError();
        goto return_error;
    }
    seg = (mca_common_sm_seg_header_t*)lpvMem;

    /* set up the map object */
    map = OBJ_NEW(mca_common_sm_module_windows_t);
    strncpy(map->super.module_seg_path, file_name, OPAL_PATH_MAX);
    /* the first entry in the file is the control structure. The first
       entry in the control structure is an mca_common_sm_seg_header_t
       element */
    map->super.module_seg  = seg;

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
            opal_output(0, "mca_common_sm_init: memory region too small len %d  addr %p\n",
                        size,addr);
            goto return_error;
        }
        map->super.module_data_addr = addr;
    } else {
        map->super.module_data_addr = NULL;
    }
    mem_offset = addr-(unsigned char *)seg;
    map->super.module_seg_addr = (unsigned char *)seg;
    map->super.module_size = size;

    /* initialize the segment - only the first process to open the file */
    if( !file_previously_opened ) {
        opal_atomic_unlock(&seg->seg_lock);
        seg->seg_inited = false;
        seg->seg_offset = mem_offset;
	/* initialize size after subtracting out space used by the header */
        seg->seg_size = size - mem_offset;
    }

    map->super.hMappedObject = hMapObject;

    return (mca_common_sm_module_t *)map;

  return_error:
    {
        char* localbuf = NULL;
        FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                       NULL, rc, 0, (LPTSTR)&localbuf, 1024, NULL );
        opal_output( 0, "%s\n", localbuf );
        LocalFree( localbuf );
    }
    if( NULL != lpvMem ) UnmapViewOfFile( lpvMem );
    if( NULL != hMapObject ) CloseHandle(hMapObject);

    return NULL;
}

int 
mca_common_sm_windows_fini(mca_common_sm_module_t *mca_common_sm_module)
{
    mca_common_sm_module_windows_t *windows_module = 
        (mca_common_sm_module_windows_t *)mca_common_sm_module;
    int rc = OMPI_SUCCESS;

    if( NULL != windows_module->super.module_seg ) {
        BOOL return_error = UnmapViewOfFile( windows_module->super.module_seg_addr );
        if( false == return_error ) {
            rc = GetLastError();
        }
        CloseHandle(windows_module->super.hMappedObject);

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

void * 
mca_common_sm_windows_seg_alloc(struct mca_mpool_base_module_t* mpool,
                             size_t* size,
                             mca_mpool_base_registration_t** registration)
{
    mca_mpool_sm_module_t *sm_module = (mca_mpool_sm_module_t*) mpool;
    mca_common_sm_module_windows_t *map = 
        (mca_common_sm_module_windows_t *)sm_module->sm_common_module;
    mca_common_sm_seg_header_t* seg = map->super.module_seg;
    void* addr;

    opal_atomic_lock(&seg->seg_lock);
    if(seg->seg_offset + *size > seg->seg_size) {
        addr = NULL;
    } else {
        size_t fixup;

        /* add base address to segment offset */
        addr = map->super.module_data_addr + seg->seg_offset;
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


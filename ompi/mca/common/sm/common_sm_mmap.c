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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
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
#include "ompi/proc/proc.h"
#include "common_sm_mmap.h"
#include "opal/util/basename.h"
#include "opal/util/output.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rml/base/base.h"

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

mca_common_sm_mmap_t* mca_common_sm_mmap_init(size_t size, char *file_name, 
        size_t size_ctl_structure, size_t data_seg_alignment)
{
#if !defined(__WINDOWS__)
    int fd = -1;
    mca_common_sm_file_header_t* seg = NULL;
    mca_common_sm_mmap_t* map = NULL;
    unsigned char *addr = NULL;
    size_t tmp,mem_offset;

    bool i_create_shared_file=false;
    ompi_proc_t **procs;
    size_t n_local_procs=0, n_total_procs=0,n,p;
    ompi_proc_t *my_proc;
    int rc=0, sm_file_inited=0;
    struct iovec iov[2]; 
    int sm_file_created;

    /* figure out how many local procs are on this host */
    procs=ompi_proc_world(&n_total_procs);
    for(p=0 ; p < n_total_procs ; p++ ) {
        if( procs[p]->proc_flags & OMPI_PROC_FLAG_LOCAL){
            n_local_procs++;
        }
    }

    /* create list of local proc_t pointers - compress the original
       list */
    n=0;
    for(p=0; p < n_total_procs ; p++ ) {
        if( procs[p]->proc_flags & OMPI_PROC_FLAG_LOCAL) {
            procs[n]=procs[p];
            n++;
        }
    }
    
    /* figure out if I am the lowest rank on host, who will create
       the shared file */
    my_proc=ompi_proc_local();
    if( my_proc == procs[0] ) {
        i_create_shared_file=true;
    }

    /* open the backing file. */
    if( i_create_shared_file ) {
        /* process initializing the file */
        fd = open(file_name, O_CREAT|O_RDWR, 0600);
            if (fd < 0) {
            opal_output(0,"mca_common_sm_mmap_init: open %s failed with errno=%d\n",                      
                        file_name, errno);
            goto file_opened;
        }

        /* truncate the file to the requested size */
        if(ftruncate(fd, size) != 0) {
            opal_output(0, 
                    "mca_common_sm_mmap_init: ftruncate failed with errno=%d\n",
                    errno);
            goto file_opened;
        }
        /* map the file and initialize segment state */
        seg = (mca_common_sm_file_header_t*)
            mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
        if( (void*)-1 == seg ) {
            opal_output(0, "mca_common_sm_mmap_init: mmap failed with errno=%d\n",
                    errno);
            goto file_opened;
        }
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
                goto file_opened;
            }
            map->data_addr = addr;
        } else {
            map->data_addr = NULL;
        }
        mem_offset = addr-(unsigned char *)seg;
        map->map_addr = (unsigned char *)seg;
        map->map_size = size;
    
        /* initialize the segment - only the first process to open the file */
        seg->seg_offset = mem_offset;
	/* initialize size after subtracting out space used by the header */
        seg->seg_size = size - mem_offset;
        opal_atomic_unlock(&seg->seg_lock);
        seg->seg_inited = false;

        /* if we got this far, the file has been initialized correctly */
        sm_file_inited=1;

        file_opened:

        /* signal the rest of the local procs that the backing file
           has been created */
        for(p=1 ; p < n_local_procs ; p++ ) {
            sm_file_created=ORTE_RML_TAG_SM_BACK_FILE_CREATED;
            iov[0].iov_base=&sm_file_created;
            iov[0].iov_len=sizeof(sm_file_created);
            iov[1].iov_base=&sm_file_inited;
            iov[1].iov_len=sizeof(sm_file_inited);
            rc=orte_rml.send(&(procs[p]->proc_name),iov,2,
                ORTE_RML_TAG_SM_BACK_FILE_CREATED,0);
            if( rc < 0 ) {
                opal_output(0,
                    "mca_common_sm_mmap_init: orte_rml.send failed to %l with errno=%d\n",
                    p,errno);
                goto return_error;
            }
        }
        if ( 0 == sm_file_inited ) {
            /* error - the sm backing file did not get opened correctly */
            goto return_error;
        }
    } else {
        /* all other procs wait for the file to be initialized
           before using the backing file */
        iov[0].iov_base=&sm_file_created;
        iov[0].iov_len=sizeof(sm_file_created);
        iov[1].iov_base=&sm_file_inited;
        iov[1].iov_len=sizeof(sm_file_inited);
        rc=orte_rml.recv(&(procs[0]->proc_name),iov,2,
              ORTE_RML_TAG_SM_BACK_FILE_CREATED,0);
        if( rc < 0 ) {
            opal_output(0,                "mca_common_sm_mmap_init: orte_rml.recv failed from %l with errno=%d\n",            
                0,errno);
            goto return_error;
        }
        /* check to see if file inited correctly */
        if( 0 == sm_file_inited ) {
            goto return_error;
        }

        /* open backing file */
        fd = open(file_name, O_RDWR, 0600);
            if (fd < 0) {
            opal_output(0,"mca_common_sm_mmap_init: open %s failed with errno=%d\n",                      
                        file_name, errno);
            return NULL;
        }

        /* map the file and initialize segment state */
        seg = (mca_common_sm_file_header_t*)
            mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
        if( (void*)-1 == seg ) {
            opal_output(0, "mca_common_sm_mmap_init: mmap failed with errno=%d\n",
                    errno);
            goto return_error;
        }
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
    
    } 

    /* enable access by other processes on this host */
    close(fd);

    return map;

  return_error:
    if( -1 != fd ) {
        close(fd);
    }

    if( NULL != seg ) munmap((void*) seg,size);

    return NULL;

#else
    int fd = -1, return_code = OMPI_SUCCESS;
    bool file_previously_opened = false;
    mca_common_sm_file_header_t* seg = NULL;
    mca_common_sm_mmap_t* map = NULL;
    unsigned char *addr = NULL;
    size_t tmp,mem_offset;

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
                                    size,                 /* size: low 32-bits */
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
    seg = (mca_common_sm_file_header_t*)lpvMem;

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
	/* initialize size after subtracting out space used by the header */
        seg->seg_size = size - mem_offset;
    }

    map->hMappedObject = hMapObject;

    return map;

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
#endif
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
        CloseHandle(sm_mmap->hMappedObject);

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
    if(seg->seg_offset + *size > seg->seg_size) {
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


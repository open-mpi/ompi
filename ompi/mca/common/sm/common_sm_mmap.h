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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _COMMON_SM_MMAP_H_
#define _COMMON_SM_MMAP_H_

#include "ompi_config.h"

#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/proc/proc.h"
#include "ompi/group/group.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_mpool_base_module_t;

typedef struct mca_common_sm_file_header_t {
    /* lock to control atomic access */
    opal_atomic_lock_t seg_lock;

    /* is the segment ready for use */
    volatile int32_t seg_inited;

    /* Offset to next available memory location available for allocation */
    size_t seg_offset;

    /* total size of the segment */
    size_t seg_size;
} mca_common_sm_file_header_t;


typedef struct mca_common_sm_mmap_t {
    /* double link list element */
    opal_list_item_t map_item;
    /* pointer to header imbeded in the shared memory file */
    mca_common_sm_file_header_t *map_seg;
    /* base address of the mmap'ed file */
    unsigned char *map_addr;
    /* base address of data segment */
    unsigned char *data_addr;
    /* How big it is (in bytes) */
    size_t map_size;
    /* Filename */
    char map_path[OMPI_PATH_MAX];
#if defined(__WINDOWS__)
    /* Handle to the object */
    HANDLE hMappedObject;
#endif  /* defined(__WINDOWS__) */
} mca_common_sm_mmap_t;

OBJ_CLASS_DECLARATION(mca_common_sm_mmap_t);


/**
 *  This routine is used to set up a shared memory file, backed
 *  by a specified file.  It is assumed that the file does not
 *  exist before any of the current set of processes try and open
 *  it.
 *
 *  @param procs - array of (ompi_proc_t*)'s to create this shared
 *  memory segment for.  This array must be writable; it may be edited
 *  (in undefined ways) if the array contains procs that are not on
 *  this host.  It is assumed that the caller will simply free this
 *  array upon return.  (INOUT)
 *
 *  @param num_procs - length of the procs array (IN)
 *
 *  @param size - size of the file, in bytes (IN)
 *
 *  @param file_name  name of file to be opened. (IN)
 *
 *  @param size_ctl_structure  size of the control structure at
 *                             the head of the file. The control structure
 *                             is assumed to have mca_common_sm_file_header_t
 *                             as its first segment (IN)
 *
 *  @param data_set_alignment  alignment of the data segment.  this
 *                             follows the control structure.  If this 
 *                             value if 0, then assume that there will 
 *                             be no data segment following the control 
 *                             structure. (IN)
 *
 *  @return value pointer to control structure at head of file.
 */
OMPI_DECLSPEC extern
mca_common_sm_mmap_t* mca_common_sm_mmap_init(
    ompi_proc_t **procs,
    size_t num_procs,
    size_t size, 
    char *file_name,
    size_t size_ctl_structure, 
    size_t data_seg_alignment);

/**
 *  This routine is used to set up a shared memory file, backed
 *  by a specified file.  It is assumed that the file does not
 *  exist before any of the current set of processes try and open
 *  it.
 *
 * This routine is the same as mca_common_sm_mmap_init() except that
 * it takes an (ompi_group_t*) parameter to specify the peers rather
 * than an array of procs.  Unlike mca_common_sm_mmap_init(), the
 * group must contain *only* local peers, or this function will return
 * NULL and not create any shared memory segment.
 */
OMPI_DECLSPEC extern
mca_common_sm_mmap_t* mca_common_sm_mmap_init_group(
    ompi_group_t *group,
    size_t size, 
    char *file_name,
    size_t size_ctl_structure, 
    size_t data_seg_alignment);

/*
 * Callback from the sm mpool
 */
OMPI_DECLSPEC extern
void* mca_common_sm_mmap_seg_alloc(
    struct mca_mpool_base_module_t* mpool, 
    size_t* size, 
    mca_mpool_base_registration_t** registration);

/**
 * This function will release all local ressources attached to the
 * mmapped file. We assume that the operating system will destroy the
 * file when the last process release it.
 *
 * @param sm_mmap - the control structure at head of file.
 *
 * @returnvalue 0 if everything was OK, otherwise a negative value.
 */

OMPI_DECLSPEC extern
int mca_common_sm_mmap_fini( mca_common_sm_mmap_t* sm_mmap );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif


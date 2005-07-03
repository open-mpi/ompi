/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef _COMMON_SM_MMAP_H_
#define _COMMON_SM_MMAP_H_

#include "ompi_config.h"

#include "opal/class/opal_object.h"
#include "class/ompi_list.h"
#include "include/sys/atomic.h"
#include "mca/mpool/mpool.h" 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_mpool_base_module_t;

struct mca_common_sm_file_header_t {

    /* lock to control atomic access */
    ompi_lock_t seg_lock;
    /* is the segment ready for use */
    
    volatile bool seg_inited;
    /* Offset to next available memory location available for allocation */
    size_t seg_offset;

    /* total size of the segment */
    size_t seg_size;

    /* array of pointers to the base of the shared memory address - one per
     * local process */
    volatile char **base_shared_mem_segment;

    /* array of flags indicating base_shared_mem_segment is set */
    volatile int *base_shared_mem_flags;
};
typedef struct mca_common_sm_file_header_t mca_common_sm_file_header_t;


struct mca_common_sm_mmap_t {
    /* double link list element */
    ompi_list_item_t map_item;
    /* pointer to header imbeded in the shared memory file */
    mca_common_sm_file_header_t* map_seg;
    /* base address of the mmap'ed file */
    unsigned char  *map_addr;
    /* base address of data segment */
    unsigned char  *data_addr;
    size_t map_size;
    char map_path[OMPI_PATH_MAX];
};
typedef struct mca_common_sm_mmap_t mca_common_sm_mmap_t;

OBJ_CLASS_DECLARATION(mca_common_sm_mmap_t);


/**
 *  This routine is used to set up a shared memory file, backed
 *  by a specified file.  It is assumed that the file does not
 *  exist before any of the current set of processes try and open
 *  it.
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
 *                             follows the control structure (IN)
 *
 *  @returnvalue pointer to control structure at head of file.
 */

extern mca_common_sm_mmap_t* mca_common_sm_mmap_init(
    size_t size, 
    char *file_name,
    size_t size_ctl_structure, 
    size_t data_seg_alignment);

extern void* mca_common_sm_mmap_seg_alloc(
    struct mca_mpool_base_module_t* mpool, 
    size_t* size, 
    mca_mpool_base_registration_t** registration);

/*
 * Instance that is shared between components that use shared memory
 */

OMPI_DECLSPEC extern mca_common_sm_mmap_t *mca_common_sm_mmap;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif


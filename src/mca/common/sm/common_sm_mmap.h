#ifndef _COMMON_SM_MMAP_H_
#define _COMMON_SM_MMAP_H_

#include "class/ompi_object.h"
#include "class/ompi_list.h"
#include "include/sys/atomic.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct mca_common_sm_file_header_t {
    /* lock to control atomic access */
    ompi_lock_t seg_lock;
    /* is the segment ready for use */
    volatile bool seg_inited;
    /* Offset to next available memory location available for allocation */
    size_t seg_offset;
    /* total size of the segment */
    size_t seg_size;
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
    char map_path[PATH_MAX];
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

mca_common_sm_mmap_t* mca_common_sm_mmap_init(size_t size, char *file_name,
                size_t size_ctl_structure, size_t data_seg_alignment);
void* mca_common_sm_mmap_alloc(size_t* size);
void  mca_common_sm_mmap_free(void* addr);

/*
 * Instance that is shared between components that use shared memory
 */

OMPI_DECLSPEC extern mca_common_sm_mmap_t *mca_common_sm_mmap;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif


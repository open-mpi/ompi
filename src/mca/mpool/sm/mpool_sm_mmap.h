#ifndef _MPOOL_SM_MMAP_H_
#define _MPOOL_SM_MMAP_H_

#include "class/ompi_object.h"
#include "os/atomic.h"
#include "class/ompi_list.h"

struct mca_mpool_sm_segment_t {
    ompi_lock_data_t seg_lock;
    size_t seg_offset;
    size_t seg_size;
};
typedef struct mca_mpool_sm_segment_t mca_mpool_sm_segment_t;


struct mca_mpool_sm_mmap_t {
    ompi_list_item_t map_item;
    mca_mpool_sm_segment_t* map_seg;
    unsigned char  *map_addr;
    size_t map_size;
    char map_path[PATH_MAX];
};
typedef struct mca_mpool_sm_mmap_t mca_mpool_sm_mmap_t;

OBJ_CLASS_DECLARATION(mca_mpool_sm_mmap_t);


mca_mpool_sm_mmap_t* mca_mpool_sm_mmap_init(size_t size);
void* mca_mpool_sm_mmap_alloc(size_t* size);
void  mca_mpool_sm_mmap_free(void* addr);


#endif


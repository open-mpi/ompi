#ifndef _PTL_SM_MMAP_H_
#define _PTL_SM_MMAP_H_

#include "class/ompi_object.h"
#include "mem/allocator.h"
#include "os/atomic.h"


struct mca_ptl_sm_segment {
    ompi_lock_data_t seg_lock;
    size_t seg_offset;
    size_t seg_size;
};
typedef struct mca_ptl_sm_segment mca_ptl_sm_segment_t;


struct mca_ptl_sm_mmap {
    ompi_object_t sm_base;
    mca_ptl_sm_segment_t* sm_segment;
    unsigned char* sm_addr;
    size_t sm_size;
};
typedef struct mca_ptl_sm_mmap mca_ptl_sm_mmap_t;

OBJ_CLASS_DECLARATION(mca_ptl_sm_mmap_t);



mca_ptl_sm_mmap_t* mca_ptl_sm_mmap_init(size_t size);
void* mca_ptl_sm_mmap_alloc(ompi_allocator_t*, size_t size);
void  mca_ptl_sm_mmap_free(ompi_allocator_t*, void* alloc);


#endif


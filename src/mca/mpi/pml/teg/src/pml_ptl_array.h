/*
 * $HEADER$
 */

#ifndef LAM_PTL_ARRAY_H
#define LAM_PTL_ARRAY_H

#include "lam/util/output.h"
#include "mca/mpi/ptl/ptl.h"

extern lam_class_info_t mca_ptl_array_cls;

struct mca_ptl_proc_t {
    double ptl_weight;               /* PTL weight for scheduling */
    struct mca_ptl_peer_t* ptl_peer; /* PTL addressing info */
    mca_ptl_t *ptl;                  /* PTL implementation */
};
typedef struct mca_ptl_proc_t mca_ptl_proc_t;

struct mca_ptl_array_t {
    lam_object_t     super;
    mca_ptl_proc_t*  ptl_procs;   /* array of ptl procs */
    size_t           ptl_size;    /* number available */
    size_t           ptl_reserve;
    size_t           ptl_index;   /* last used index*/
};
typedef struct mca_ptl_array_t mca_ptl_array_t;


void mca_ptl_array_init(mca_ptl_array_t*);
void mca_ptl_array_destroy(mca_ptl_array_t*);
int  mca_ptl_array_reserve(mca_ptl_array_t*, size_t);

static inline size_t mca_ptl_array_get_size(mca_ptl_array_t* array)
{
    return array->ptl_size;
}

static inline void mca_ptl_array_set_size(mca_ptl_array_t* array, size_t size)
{
    if(array->ptl_size > array->ptl_reserve)
        mca_ptl_array_reserve(array, size);
    array->ptl_size = size;
}

static inline mca_ptl_proc_t* mca_ptl_array_insert(mca_ptl_array_t* array)
{
#if LAM_ENABLE_DEBUG
    if(array->ptl_size >= array->ptl_reserve) {
        lam_output(0, "mca_ptl_array_insert: invalid array index %d >= %d", 
            array->ptl_size, array->ptl_reserve);
        return 0;
    }
#endif
    return &array->ptl_procs[array->ptl_size++];
}

static inline mca_ptl_proc_t* mca_ptl_array_get_index(mca_ptl_array_t* array, size_t index)
{
#if LAM_ENABLE_DEBUG
    if(index >= array->ptl_size) {
        lam_output(0, "mca_ptl_array_get_index: invalid array index %d >= %d",
            index, array->ptl_size);
        return 0;
    }
#endif
    return &array->ptl_procs[index];
}

static inline mca_ptl_proc_t* mca_ptl_array_get_next(mca_ptl_array_t* array)
{
#if LAM_ENABLE_DEBUG
    if(array->ptl_size == 0) {
        lam_output(0, "mca_ptl_array_get_next: invalid array size");
        return 0;
    }
#endif
    mca_ptl_proc_t* ptl_proc = &array->ptl_procs[array->ptl_index++];
    if(array->ptl_index == array->ptl_size)
        array->ptl_index = 0;
    return ptl_proc;
}


#endif


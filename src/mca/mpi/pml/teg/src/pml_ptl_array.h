/*
 * $HEADER$
 */

#ifndef LAM_PTL_ARRAY_H
#define LAM_PTL_ARRAY_H

#include "mca/mpi/ptl/ptl.h"

extern lam_class_info_t mca_ptl_array_cls;

struct mca_ptl_info_t {
    double ptl_weight;   /* PTL weight for scheduling */
    mca_ptl_t *ptl;      /* PTL implementation */
};
typedef struct mca_ptl_info_t mca_ptl_info_t;

struct mca_ptl_array_t {
    lam_object_t     super;
    mca_ptl_info_t*  ptl_array;   /* array of PTL info */
    size_t           ptl_size;    /* number available */
    size_t           ptl_reserve;
    size_t           ptl_index;   /* last used index*/
};
typedef struct mca_ptl_array_t mca_ptl_array_t;


void mca_ptl_array_init(mca_ptl_array_t*);
void mca_ptl_array_destroy(mca_ptl_array_t*);
int  mca_ptl_array_reserve(mca_ptl_array_t*, size_t);

static inline void mca_ptl_array_set_size(mca_ptl_array_t* array, size_t size)
{
    if(array->ptl_size > array->ptl_reserve)
        mca_ptl_array_reserve(array, size);
    array->ptl_size = size;
}

static inline mca_ptl_info_t* mca_ptl_array_get_next(mca_ptl_array_t* ptl_array)
{
    mca_ptl_info_t* ptl_info = &ptl_array->ptl_array[ptl_array->ptl_index++];
    if(ptl_array->ptl_index == ptl_array->ptl_size)
        ptl_array->ptl_index = 0;
    return ptl_info;
}

#endif


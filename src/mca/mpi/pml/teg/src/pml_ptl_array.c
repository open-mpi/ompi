/*
 * $HEADER$
 */

#include <string.h>
#include "lam/mem/malloc.h"
#include "pml_ptl_array.h"


lam_class_info_t mca_pml_teg_array_cls = {
    "mca_ptl_array_t",
    &lam_object_cls,
    (class_init_t) mca_ptl_array_init,
    (class_destroy_t) mca_ptl_array_destroy
};
                                                                                                                             

void mca_ptl_array_init(mca_ptl_array_t* array)
{
    SUPER_INIT(array, &lam_object_cls);
    array->ptl_procs = 0;
    array->ptl_size = 0;
    array->ptl_index = 0;
    array->ptl_reserve = 0;
}


void mca_ptl_array_destroy(mca_ptl_array_t* array)
{
    if(array->ptl_procs != 0)
        LAM_FREE(array->ptl_procs);
    SUPER_DESTROY(array, &lam_object_cls);
}


int mca_ptl_array_reserve(mca_ptl_array_t* array, size_t size)
{
    mca_ptl_proc_t *procs;
    if(array->ptl_reserve >= size)
        return LAM_SUCCESS;
    
    procs = (mca_ptl_proc_t*)LAM_MALLOC(sizeof(mca_ptl_array_t)*size);
    if(array == 0)
        return LAM_ERR_OUT_OF_RESOURCE;
    if(array->ptl_size) {
         memcpy(procs, array->ptl_procs, array->ptl_size * sizeof(mca_ptl_proc_t));
         LAM_FREE(array->ptl_procs);
    }
    array->ptl_procs = procs;
    array->ptl_reserve = size;
    memset(array->ptl_procs+(size-array->ptl_size), 0, (size-array->ptl_size)*sizeof(mca_ptl_proc_t));
    return LAM_SUCCESS;
}


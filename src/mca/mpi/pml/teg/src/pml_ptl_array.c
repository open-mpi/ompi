/*
 * $HEADER$
 */

#include <string.h>
#include "lam/util/malloc.h"
#include "pml_ptl_array.h"


lam_class_info_t mca_pml_teg_proc_cls = {
    "mca_ptl_array_t",
    &lam_object_cls,
    (class_init_t) mca_ptl_array_init,
    (class_destroy_t) mca_ptl_array_destroy
};
                                                                                                                             

void mca_ptl_array_init(mca_ptl_array_t* ptl_array)
{
    SUPER_INIT(ptl_array, &lam_object_cls);
    ptl_array->ptl_array = 0;
    ptl_array->ptl_size = 0;
    ptl_array->ptl_index = 0;
    ptl_array->ptl_reserve = 0;
}


void mca_ptl_array_destroy(mca_ptl_array_t* ptl_array)
{
    if(ptl_array->ptl_array != 0)
        LAM_FREE(ptl_array->ptl_array);
    SUPER_DESTROY(ptl_array, &lam_object_cls);
}


int mca_ptl_array_reserve(mca_ptl_array_t* ptl_array, size_t size)
{
    mca_ptl_info_t *array;
    if(ptl_array->ptl_reserve >= size)
        return LAM_SUCCESS;
    
    array = (mca_ptl_info_t*)LAM_MALLOC(sizeof(mca_ptl_array_t)*size);
    if(array == 0)
        return LAM_ERR_OUT_OF_RESOURCE;
    if(ptl_array->ptl_size) {
         memcpy(ptl_array->ptl_array, array, ptl_array->ptl_size * sizeof(mca_ptl_info_t));
         LAM_FREE(ptl_array->ptl_array);
    }
    memset(ptl_array->ptl_array+(size-ptl_array->ptl_size), 0, (size-ptl_array->ptl_size)*sizeof(mca_ptl_info_t));
    ptl_array->ptl_reserve = size;
    return LAM_SUCCESS;
}


/*
 * $HEADER$
 */

#include <string.h>
#include "pml_ptl_array.h"


lam_class_info_t mca_pml_teg_array_t_class_info = {
    "mca_ptl_array_t",
    CLASS_INFO(lam_object_t),
    (lam_construct_t) mca_ptl_array_construct,
    (lam_destruct_t) mca_ptl_array_destruct
};
                                                                                                                             

void mca_ptl_array_construct(mca_ptl_array_t* array)
{
    OBJ_CONSTRUCT_SUPER(array, lam_object_t);
    array->ptl_procs = 0;
    array->ptl_size = 0;
    array->ptl_index = 0;
    array->ptl_reserve = 0;
}


void mca_ptl_array_destruct(mca_ptl_array_t* array)
{
    if(array->ptl_procs != 0)
        free(array->ptl_procs);
    OBJ_DESTRUCT_SUPER(array, lam_object_t);
}


int mca_ptl_array_reserve(mca_ptl_array_t* array, size_t size)
{
    mca_ptl_proc_t *procs;
    if(array->ptl_reserve >= size)
        return LAM_SUCCESS;
    
    procs = malloc(sizeof(mca_ptl_array_t)*size);
    if(array == 0)
        return LAM_ERR_OUT_OF_RESOURCE;
    if(array->ptl_size) {
         memcpy(procs, array->ptl_procs, array->ptl_size * sizeof(mca_ptl_proc_t));
         free(array->ptl_procs);
    }
    array->ptl_procs = procs;
    array->ptl_reserve = size;
    memset(array->ptl_procs+(size-array->ptl_size), 0, (size-array->ptl_size)*sizeof(mca_ptl_proc_t));
    return LAM_SUCCESS;
}


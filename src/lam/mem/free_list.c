/*
 * $HEADER$
 */
#include "lam_config.h"
#include "lam/mem/free_list.h"


lam_class_info_t lam_free_list_t_class_info = {
    "lam_free_list_t", 
    CLASS_INFO(lam_list_t),
    (lam_construct_t)lam_free_list_construct, 
    (lam_destruct_t)lam_free_list_destruct
};
                                                                                                                             

void lam_free_list_construct(lam_free_list_t* fl)
{
    OBJ_CONSTRUCT_SUPER(fl, lam_list_t);
}

void lam_free_list_destruct(lam_free_list_t* fl)
{
    OBJ_DESTRUCT_SUPER(fl, lam_object_t);
}

int lam_free_list_construct_with(
    lam_free_list_t *flist,
    size_t elem_size,
    lam_class_info_t* elem_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    lam_allocator_t* allocator)
{
    flist->fl_elem_size = elem_size;
    flist->fl_elem_class = elem_class;
    flist->fl_max_to_alloc = max_elements_to_alloc;
    flist->fl_num_allocated = 0;
    flist->fl_num_per_alloc = num_elements_per_alloc;
    flist->fl_allocator = allocator;
    return lam_free_list_grow(flist, num_elements_to_alloc);
}


int lam_free_list_grow(lam_free_list_t* flist, size_t num_elements)
{
    unsigned char* ptr;
    size_t i;
    if (flist->fl_max_to_alloc > 0 && flist->fl_num_allocated + num_elements > flist->fl_max_to_alloc)
        return LAM_ERR_TEMP_OUT_OF_RESOURCE;

    if (NULL != flist->fl_allocator)
        ptr = (unsigned char*)lam_allocator_alloc(flist->fl_allocator, num_elements * flist->fl_elem_size);
    else
        ptr = malloc(num_elements * flist->fl_elem_size);
    if(NULL == ptr)
        return LAM_ERR_TEMP_OUT_OF_RESOURCE;

    for(i=0; i<num_elements; i++) {
        lam_list_item_t* item = (lam_list_item_t*)ptr;
        if (NULL != flist->fl_elem_class) {
            /* by-pass OBJ_CONSTRUCT() in this case */
            OBJECT(item)->obj_class_info = flist->fl_elem_class;
            OBJECT(item)->obj_class_info->cls_construct(OBJECT(item));
        }
        lam_list_append(&flist->super, item);
        ptr += flist->fl_elem_size;
    }
    flist->fl_num_allocated += num_elements;
    return LAM_SUCCESS;
}




/*
 * $HEADER$
 */

#ifdef HAVE_CONFIG_H
#include "ompi_config.h"
#endif

#include "class/ompi_free_list.h"


static void ompi_free_list_construct(ompi_free_list_t* fl);
static void ompi_free_list_destruct(ompi_free_list_t* fl);


ompi_class_t ompi_free_list_t_class = {
    "ompi_free_list_t", 
    OBJ_CLASS(ompi_list_t),
    (ompi_construct_t)ompi_free_list_construct, 
    (ompi_destruct_t)ompi_free_list_destruct
};


static void ompi_free_list_construct(ompi_free_list_t* fl)
{
    OBJ_CONSTRUCT(&fl->fl_lock, ompi_mutex_t);
    fl->fl_max_to_alloc = 0;
    fl->fl_num_allocated = 0;
    fl->fl_num_per_alloc = 0;
    fl->fl_num_waiting = 0;
    fl->fl_elem_size = 0;
    fl->fl_elem_class = 0;
    fl->fl_mpool = 0;
}

static void ompi_free_list_destruct(ompi_free_list_t* fl)
{
    OBJ_DESTRUCT(&fl->fl_lock);
}

int ompi_free_list_init(
    ompi_free_list_t *flist,
    size_t elem_size,
    ompi_class_t* elem_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc,
    mca_mpool_base_module_t* mpool)
{
    flist->fl_elem_size = elem_size;
    flist->fl_elem_class = elem_class;
    flist->fl_max_to_alloc = max_elements_to_alloc;
    flist->fl_num_allocated = 0;
    flist->fl_num_per_alloc = num_elements_per_alloc;
    flist->fl_mpool = mpool;
    if(num_elements_to_alloc)
        return ompi_free_list_grow(flist, num_elements_to_alloc);
    return OMPI_SUCCESS;
}


int ompi_free_list_grow(ompi_free_list_t* flist, size_t num_elements)
{
    unsigned char* ptr;
    size_t i;
    if (flist->fl_max_to_alloc > 0 && flist->fl_num_allocated + num_elements > flist->fl_max_to_alloc)
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    if (NULL != flist->fl_mpool)
        ptr = (unsigned char*)flist->fl_mpool->mpool_alloc(num_elements * flist->fl_elem_size, 0);
    else
        ptr = (unsigned char *)malloc(num_elements * flist->fl_elem_size);
    if(NULL == ptr)
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    for(i=0; i<num_elements; i++) {
        ompi_list_item_t* item = (ompi_list_item_t*)ptr;
        if (NULL != flist->fl_elem_class) {
            OBJ_CONSTRUCT_INTERNAL(item, flist->fl_elem_class);
        }
        ompi_list_append(&flist->super, item);
        ptr += flist->fl_elem_size;
    }
    flist->fl_num_allocated += num_elements;
    return OMPI_SUCCESS;
}




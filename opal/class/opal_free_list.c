/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/class/opal_free_list.h"
#include "opal/include/sys/cache.h"


static void opal_free_list_construct(opal_free_list_t* fl);
static void opal_free_list_destruct(opal_free_list_t* fl);


opal_class_t opal_free_list_t_class = {
    "opal_free_list_t", 
    OBJ_CLASS(ompi_list_t),
    (opal_construct_t)opal_free_list_construct, 
    (opal_destruct_t)opal_free_list_destruct
};


static void opal_free_list_construct(opal_free_list_t* fl)
{
    OBJ_CONSTRUCT(&fl->fl_lock, ompi_mutex_t);
    fl->fl_max_to_alloc = 0;
    fl->fl_num_allocated = 0;
    fl->fl_num_per_alloc = 0;
    fl->fl_num_waiting = 0;
    fl->fl_elem_size = 0;
    fl->fl_elem_class = 0;
}

static void opal_free_list_destruct(opal_free_list_t* fl)
{
    OBJ_DESTRUCT(&fl->fl_lock);
}

int opal_free_list_init(
    opal_free_list_t *flist,
    size_t elem_size,
    opal_class_t* elem_class,
    int num_elements_to_alloc,
    int max_elements_to_alloc,
    int num_elements_per_alloc)
{
    flist->fl_elem_size = elem_size;
    flist->fl_elem_class = elem_class;
    flist->fl_max_to_alloc = max_elements_to_alloc;
    flist->fl_num_allocated = 0;
    flist->fl_num_per_alloc = num_elements_per_alloc;
    if(num_elements_to_alloc)
        return opal_free_list_grow(flist, num_elements_to_alloc);
    return OMPI_SUCCESS;
}


int opal_free_list_grow(opal_free_list_t* flist, size_t num_elements)
{
    unsigned char* ptr;
    size_t i;
    size_t mod;

    if (flist->fl_max_to_alloc > 0 && flist->fl_num_allocated + num_elements > flist->fl_max_to_alloc)
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    ptr = (unsigned char *)malloc((num_elements * flist->fl_elem_size) + CACHE_LINE_SIZE);
    if(NULL == ptr)
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    mod = (unsigned long)ptr % CACHE_LINE_SIZE;
    if(mod != 0) {
        ptr += (CACHE_LINE_SIZE - mod);
    }

    for(i=0; i<num_elements; i++) {
        opal_free_list_item_t* item = (opal_free_list_item_t*)ptr;
        item->user_data = NULL; 
        if (NULL != flist->fl_elem_class) {
            OBJ_CONSTRUCT_INTERNAL(item, flist->fl_elem_class);
        }
        ompi_list_append(&(flist->super), &(item->super));
        ptr += flist->fl_elem_size;
    }
    flist->fl_num_allocated += num_elements;
    return OMPI_SUCCESS;
}



